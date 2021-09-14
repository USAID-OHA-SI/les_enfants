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
  
  datain <- "Data"
  dataout <- "Dataout"
  graphics <- "Graphics"
  
  #path to Q3 msd output file
  merdata <- glamr::si_path("path_msd") 
  zmb_msd <- list.files(merdata, pattern = "Site_IM_FY19-21_20210813_v1_1_Zambia.zip", full.names = T)
  
  
  #caption info for plotting
  source <- source_info(zmb_msd)
  
  #current FY and quarter
  curr_fy <- source_info(zmb_msd, return = "fiscal_year")
  curr_qtr <- source_info(zmb_msd, return = "quarter")
  
  # indicators of focus
  indic_list <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "TX_NET_NEW", "TX_NEW", "TX_CURR") 
  vlc_list <- c("TX_CURR", "TX_PLVS")
  

# LOAD DATA ---------------------------------------------------------------
  
  msd_site <- read_msd(zmb_msd)
  
  msd_site_fltr <- msd_site %>% 
    filter(indicator %in% indic_list, standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2021)
  
  # How many sites?
  msd_site_fltr %>% 
    distinct(orgunituid, sitename, fiscal_year) %>% 
    dim()
  
  # Original Surge sites
  surge <- 
    read_excel("Data/Pediatric Surge IP Mapping_12.05.2020.xls", skip = 3) %>% 
    rename(peds_surge_ovc = `Priority Level for Pediatric Surge - OVC`) %>% 
    mutate(peds_surge_site = if_else(`Pediatric Surge Facilities` %in% c("x", "X"), 1, 0),
           peds_surge_ovc = if_else(is.na(peds_surge_ovc), "NONE", peds_surge_ovc))



  # According to emails there are
  # 143 surge sites across USAID supported provinces
  # 25 DISCOVER HEALTH
  # 31 EQUIP
  # 87 SAFE
  
  # New Surge sites
  sheet_names <- excel_sheets("Data/USAID SAFE_Peds_Surge_Site_Tier 1.xlsx")[2:4]
  
  # Read in SAFE and pluck out the tier number from the sheet name and enter it
  # as a new value so we can track tier effects (if they exist)
  safe_sites <- map_dfr(sheet_names, 
          ~read_excel("Data/USAID SAFE_Peds_Surge_Site_Tier 1.xlsx", sheet = .x) %>% 
            mutate(tier = str_extract(.x, "[:digit:]") %>% as.numeric())) %>% 
    select(1:4) %>% 
    mutate(partner = "SAFE")
  
  discover_sites <- 
    read_excel("Data/USAID DISCOVER HEALTH PEDS SURGE SITES_08092021.xlsx") %>%
    mutate(across(c(1:3), str_to_title)) %>% 
    mutate(partner = "DISCOVER") %>% 
    rename(District = Hub, not_sure = District)
  
  
  equip_sites <- surge %>% 
    filter(Specifics == "EQUIP", peds_surge_site == 1) %>% 
    select(Province, District, 
           Facility = `Health Facility`,
           orgunitname = organisationunitname) %>% 
    mutate(partner = "EQUIP")

  map(list(safe_sites, discover_sites, equip_sites), names)  
  
  # What sites do not merge?
  surge_sites <- 
    bind_rows(safe_sites, discover_sites, equip_sites) %>% 
    rename(psnu = District) %>% 
    clean_psnu %>% 
    mutate(Province = ifelse(Province == "North-western", "NorthWestern", Province),
           psnu = ifelse(psnu == "Itezhi-Tezhi", "Itezhi-tezhi", psnu),
           psnu = str_remove(psnu, " Hub")) %>% 
    anti_join(surge, by = c("Province" = "Province", 
                            "psnu" = "District", 
                            "Facility" = "Health Facility"))
  
  write_csv(surge_sites, "Dataout/anti_join_sites.csv")

# Pull out distinct site list for FY21 sites
msd_site_fltr %>% select(1:9) %>% 
  distinct(orgunituid, sitename, snu1, psnu) %>% View()

