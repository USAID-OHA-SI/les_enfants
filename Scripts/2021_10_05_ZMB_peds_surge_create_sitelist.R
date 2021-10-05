# PURPOSE: Reboot on Pediatric Analysis for Zambia
# COLLABORATOR: T Essam | K Srikanth
# DATE: 2021-09-08
# NOTES: Request from Zambia SCA


# GLOBALS -----------------------------------------------------------------

  library(tidyverse)
  library(gophr)
  library(sf)
  library(glitr)
  library(glamr)
  library(gisr)
  library(scales)
  library(here)
  library(gt)
  library(ggbeeswarm)
  library(readxl)
  library(fuzzyjoin)
  library(ggnewscale)
  library(extrafont)
  library(ggdist)
  
  datain <- "Data"
  dataout <- "Dataout"
  graphics <- "Graphics"
  
  #path to Q3 msd output file
  merdata <- glamr::si_path("path_msd") 
  zmb_msd <- list.files(merdata, pattern = "Site_IM_FY19-22_20210917_v2_1_Zambia", full.names = T)

  
  #caption info for plotting
  source <- source_info(zmb_msd)
  
  #current FY and quarter
  curr_fy <- source_info(zmb_msd, return = "fiscal_year")
  curr_qtr <- source_info(zmb_msd, return = "quarter")
  
  # indicators of focus
  indic_list <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "TX_NET_NEW", "TX_NEW", "TX_CURR") 
  vlc_list <- c("TX_CURR", "TX_PVLS")
  

# LOAD MSD DATA ---------------------------------------------------------------
  
  msd_site <- read_msd(zmb_msd)

  
  # HTS_TST   -> "Modality/Age/Sex/Result"
  # TX        -> "Age/Sex/HIVStatus"
  # HTS_INDEX -> "1:Age/Sex/IndexCasesOffered" 
  
  disagg_fltr <- c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus", "1:Age/Sex/IndexCasesOffered")
  
  # Filtering down to sites
  msd_site_fltr <- msd_site %>% 
    filter(indicator %in% c("TX_CURR", "HTS_TST"),
           fiscal_year >= 2021) %>%
    mutate(ip = case_when(
      str_detect(mech_name, "DISCOVER") ~ "DISCOVER-H",
      TRUE ~ mech_name
    ))

  # How many sites serving PEDS? 
  msd_site_fltr %>% 
    distinct(orgunituid, sitename, fiscal_year, psnu) %>% 
    dim()

  # Get unique sites, removing any Military ones
  msd_fac_list <- 
    msd_site_fltr %>% 
    distinct(sitename, orgunituid, psnu, snu1) %>% 
    filter(str_detect(snu1, "Military", negate = T))
  

# LOAD SURGE DATA ---------------------------------------------------------

  
  # Original Surge sites
  surge_old <- 
    read_excel("Data/Pediatric Surge IP Mapping_12.05.2020.xls", skip = 3) %>% 
    rename(peds_surge_ovc = `Priority Level for Pediatric Surge - OVC`) %>% 
    mutate(peds_surge_site = if_else(`Pediatric Surge Facilities` %in% c("x", "X"), 1, 0),
           peds_surge_ovc = if_else(is.na(peds_surge_ovc), "NONE", peds_surge_ovc))


  # According to emails there are
  # 143 surge sites across USAID supported provinces
  # 25 DISCOVER HEALTH
  # 31 EQUIP
  # 87 SAFE
  # Additional fixes that were made to the site lists passed to us
  # Waya Facility reports under Katondo Urban Health Centre
  # UCZ Twatasha reports under the Twatasha Urban Health Centre
  # Namulundu is a health post that reports under Tazara Rural Health Centre
  # Riverside Urban Health Centre is in Tier 2 (removed from Tier 1 list)
  # Mpatamatu Section 26 Urban Health Centre is in Tier 3 (removed from Tier 3)
  
  
  # New Surge sites
  sheet_names <- excel_sheets("Data/USAID SAFE_Peds_Surge_Site_Tier 1.xlsx")[2:4]
  
  # SAFE
    # Read in SAFE and pluck out the tier number from the sheet name and enter it
    # as a new value so we can track tier effects (if they exist)
  safe_sites <- map_dfr(sheet_names, 
          ~read_excel("Data/USAID SAFE_Peds_Surge_Site_Tier 1.xlsx", sheet = .x) %>% 
            mutate(tier = str_extract(.x, "[:digit:]") %>% as.numeric())) %>% 
    select(1:5) %>% 
    mutate(partner = "SAFE")
    #        dup_tag = case_when(
    #          Facility == "Mpatamatu Section 26 Urban Health Centre" & tier == 3 ~ 1,
    #          Facility == "Riverside Urban Health Centre" & tier == 2 ~ 1, 
    #          TRUE ~ 0
    #        ))
  
  # DISCOVER - NOTES
    # These have been edited in the anti_join file to include the orgunituids
    # Anti-join w/ manual fixes
  discover_sites <- read_csv("Data/DISCOVER_peds_sites_20210917_TME.csv") %>% 
    rename(comments = !!names(.[8]), 
           `Facility Raw` = Facility,
           Facility = sitename, 
           District = psnu) %>% 
    mutate(dup_tag = case_when(
      str_detect(Facility, "(Waya|UCZ_Twatasha|Namulundu)") ~ 1, 
      TRUE ~ 0
    ))
  
  # discover_sites_raw <- 
  #   read_excel("Data/USAID DISCOVER HEALTH PEDS SURGE SITES_08092021.xlsx") %>%
  #   mutate(across(c(1:3), str_to_title)) %>% 
  #   mutate(partner = "DISCOVER") %>% 
  #   rename(District = Hub, not_sure = District)
           

  # EQUIP
    # PPT image sent -- seriously, who does that?
  equip_sites <- surge_old %>% 
    filter(Specifics == "EQUIP", peds_surge_site == 1) %>% 
    select(Province, District, 
           Facility = `Health Facility`,
           orgunitname = organisationunitname) %>% 
    mutate(partner = "EQUIP") %>% 
    mutate(dup_tag = 0) 

  # Are names consistent? Are there duplicates?
  # Two facilities show duplication b/c SAFE has them listed at multiple TIER sites -- fixed
  map(list(safe_sites, discover_sites, equip_sites), names)  
  # map(list(safe_sites, discover_sites, equip_sites), ~.x %>% 
  #       count(Facility) %>% 
  #       filter(n>1))  
  
  # What sites do not merge? (use anti_join)
  surge_sites <- 
    bind_rows(safe_sites, discover_sites, equip_sites) %>% 
    rename(psnu = District) %>% 
    clean_psnu %>% 
    mutate(Province = ifelse(Province == "North-western", "NorthWestern", Province),
           psnu = case_when(
             psnu == "Itezhi-Tezhi" ~ "Itezhi-tezhi",
             psnu == "Kapiri Mposhi" ~ "Kapiri-Mposhi", 
             TRUE~ psnu),
           psnu = str_remove(psnu, " Hub"))
  
  surge_sites %>% count(Facility) %>% arrange(desc(n))
  
  surge_sites_mtch <-
    surge_sites %>%
    left_join(surge_old, by = c("Province" = "Province",
                            "psnu" = "District",
                            "Facility" = "Health Facility"))

  # Now see how many of these match to DATIM!
  peds_fac_list <-   
    surge_sites_mtch %>% select(-orgunituid) %>%
    left_join(msd_site_fltr %>% distinct(sitename, orgunituid, psnu, snu1) %>% clean_psnu, 
                by = c("Facility" = "sitename", "psnu" = "psnu")) %>% 
    mutate(surge_site = 1)

  peds_fac_list %>% count(orgunituid, Facility) %>% filter(n == 2)
  
  peds_fac_list_shrt <- 
    peds_fac_list %>% 
    select(orgunituid, partner, surge_site)
  
  # Export site list for use, flaggin sites listed twice
  peds_fac_list %>% 
    group_by(orgunituid, Facility) %>% 
    mutate(dup = ifelse(max(n()) >1, 1, 0)) %>% 
    select(partner, orgunituid, sitename = Facility, psnu, snu1, surge_site, dup, everything()) %>% 
    write_csv(., "Dataout/ZMB_peds_surge_full_list.csv")
  

# # MUNGE KEY INDICATORS FOR SURGE ANALYSIS ---------------------------------
#   
#   # First consider TX_CURR for Under 15s
# 
#   tmp <- msd_site_fltr %>% 
#     left_join(peds_fac_list_shrt, by = "orgunituid") %>% 
#     mutate(treatment = ifelse(!is.na(surge_site), 1, 0)) %>% 
#     select(treatment, surge_site, everything()) %>% 
#     filter(indicator == "TX_CURR") %>% 
#     reshape_msd() %>% 
#     filter(period_type == "results") %>% 
#     group_by(snu1, psnu, orgunituid, sex, trendsfine, trendscoarse, 
#              period, period_type, treatment, partner, mech_name) %>% 
#     summarise(tx_curr = sum(value, na.rm = T))
#   
#   # Surge is only taking place in these Provinces
#   snu1_list <- c("Copp|Luap|Much|North")
#   
#   tmt_df_viz <- 
#     tmp %>% 
#     filter(str_detect(snu1, snu1_list), 
#            trendscoarse == "<15") %>%
#     group_by(orgunituid, sex, period, treatment, psnu, snu1, mech_name) %>% 
#     summarise(tx_curr_und15 = sum(tx_curr, na.rm = T)) %>% 
#     ungroup() %>% 
#     mutate(trt_cat = case_when(
#       treatment == 1 & sex == "Female" ~ "Surge - Females",
#       treatment == 1 & sex == "Male"   ~ "Surge - Males",
#       treatment == 0 & sex == "Female" ~ "Non-surge - Females",
#       treatment == 0 & sex == "Male" ~ "Non-surge - Males"
#     ),
#       trt_cat = fct_relevel(trt_cat, c("Surge - Males", "Non-surge - Males",
#                                       "Surge - Females", "Non-surge - Females")),
#     trt_color = case_when(
#       trt_cat == "Surge - Females" ~ moody_blue,
#       trt_cat == "Non-surge - Females" ~ moody_blue_light,
#       trt_cat == "Surge - Males" ~ genoa,
#       trt_cat == "Non-surge - Males" ~ genoa_light
#     ))
#     
#    
#   tmt_df_labels <- 
#     tmt_df_viz %>% 
#     group_by(trt_cat, snu1, trt_color, period) %>% 
#     summarise(median = median(tx_curr_und15, na.rm = T) %>% round(., 0),
#               tx_curr_mean = mean(tx_curr_und15, na.rm = T) %>% round(., 0),
#               tx_curr_und15 = median) %>% 
#     ungroup()
#     
#   tmt_df_viz %>% 
#     filter(orgunituid != "SLX0sExVdzz0", period == "FY21Q3") %>% 
#     ggplot(aes(x = tx_curr_und15, y = trt_cat, fill = trt_color)) +
#     stat_halfeye(color = grey80k, 
#                  normalize = "panels") +
#     ggdist::stat_dots(
#       ## orientation to the left
#       side = "left",
#       ## move geom to the left
#       justification = 1.1,
#       ## adjust grouping (binning) of observations
#       binwidth = 0.75,
#       color = NA, 
#       normalize = "panels"
#     ) +
#     geom_label(data = tmt_df_labels, 
#               aes(x = tx_curr_und15, y = period, label = tx_curr_und15), fill = NA, label.size = NA,
#               vjust = -0.5, hjust = .5, size = 9/.pt, family = "Source Sans Pro Light") +
#     facet_wrap(~snu1, nrow = 5) +
#     si_style_xgrid(facet_space = 0.5) +
#     scale_fill_identity()
#   
#   
#   
#   
#   
#   tmp %>% 
#     filter(str_detect(snu1, snu_list), 
#            sex == "Male", period == "FY21Q3", 
#            trendsfine != "<01") %>% 
#     ggplot(aes(y = factor(interaction(treatment, trendsfine)), x = tx_curr, fill = factor(treatment))) +
#     stat_halfeye() +
#     ggdist::stat_dots(
#       ## orientation to the left
#       side = "left", 
#       ## move geom to the left
#       justification = 1.1, 
#       ## adjust grouping (binning) of observations 
#       binwidth = .25
#     ) + 
#     facet_wrap(trendsfine~snu1, scales = "free") +
#     si_style_xgrid()
    
