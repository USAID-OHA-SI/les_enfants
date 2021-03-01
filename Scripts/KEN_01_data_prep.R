# PURPOSE: TX_NET_NEW Analysis focused on PEDS for Kenya
# COLLABORATOR: T Essam | A Chafetz | B Kagniniwa
# DATE: 2021-02-26
# NOTES: BASED ON REQUEST FROM Kenya C&T Team


# LIBRARIES -----------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(sf)
library(glitr)
library(glamr)
library(gisr)
library(gt)
library(scales)
library(here)
library(gt)
library(ggbeeswarm)
library(extrafont)

# GLOBALS -----------------------------------------------------------------
  
  # Directories
  graphics <- "Graphics"
  
  # Output footnotes
  source <- "Source: FY21Q1 MSD"
  
  # indicators of focus
  indic_list <- c("TX_CURR", "TX_NET_NEW", "TX_NEW") 
  
  # Country
  country <- "Kenya"
  
  merdata <- glamr::si_path("path_msd")
  
  
  # Load credentials & project paths
  #glamr::si_path()
  #glamr::load_secrets()


# FUNCTIONS -------------------------------------------------------------

  # Return a distinct count of a filtered data frame to check numbers
  # Spread by funding agency
  distinct_count <- function(df) {
    df %>% 
      distinct(orgunituid, mech_code, fiscal_year, indicator, fundingagency) %>% 
      count(fiscal_year, fundingagency) %>% 
      spread(fundingagency, n)
  }

# SITE SHARE --------------------------------------------------------------
  
  
  #' @title Distribution of Site / TX_CURR Share
  #' 
  #' @param df_tx
  #' @param indicator Indicator name
  #' @param share_level Share levels: psnu, snu1 or null for country
  #' @return TX_CURR Results Achievements
  #' 
  sites_share <- function(df_tx, 
                          indicator = "TX_CURR",
                          share_level = "PSNU") {
    # Filter indicators
    df_tx <- df_tx %>% 
      filter(
        str_to_lower(fundingagency) != "dedup",
        str_to_lower(indicator) == str_to_lower({{indicator}}),                      # get specific indicator
        standardizeddisaggregate == "Total Numerator",
        !is.na(cumulative)                               # exclude sites with no cumulative results
      ) %>%
      select(fiscal_year, fundingagency, snu1uid, snu1,  # narrow down cols
             psnuuid, psnu, orgunituid, sitename, targets,
             results = cumulative, starts_with("qtr"))
    
    # Identify group level for sites share 
    if (str_to_upper(share_level) == "PSNU") {
      df_tx <- df_tx %>% 
        group_by(fiscal_year, fundingagency, snu1uid, snu1, psnuuid, psnu) 
    }
    else if (str_to_upper(share_level) == "SNU1") {
      df_tx <- df_tx %>% 
        group_by(fiscal_year, fundingagency, snu1uid, snu1)
    }
    else {
      df_tx <- df_tx %>% 
        group_by(fiscal_year, fundingagency)
    }
    
    # Calculate sites shares
    df_tx <- df_tx %>%   
      mutate(
        targets_share = targets / sum(targets, na.rm = TRUE),
        results_share = results / sum(results, na.rm = TRUE),
        qtr1_share = qtr1 / sum(qtr1, na.rm = TRUE), 
        qtr2_share = qtr2 / sum(qtr2, na.rm = TRUE), 
        qtr3_share = qtr3 / sum(qtr3, na.rm = TRUE), 
        qtr4_share = qtr4 / sum(qtr4, na.rm = TRUE) 
      ) %>% 
      ungroup() %>% 
      pivot_longer(cols = starts_with(c("target", "result", "qtr")), 
                   names_to = "metric", 
                   values_to = "val")
    
    return(df_tx)
  }
  
  #' @title Site Performance
  #' 
  #' @param df_tx
  #' @param indicator
  #' @param share_level
  #' @return Site Indicator / Performance Checks
  #' 
  sites_perf <- function(df_tx,
                         indicator = "TX_CURR",
                         share_level = "PSNU") {
    
    df_tx %>% 
      sites_share(indicator = {{indicator}},
                  share_level = {{share_level}}) %>% 
      pivot_wider(names_from = 'metric', 
                  values_from = "val",
                  values_fn = sum) %>%  
      rowwise() %>% 
      mutate(
        site_perf = if_else(results >= targets, 1, 0), # did site meet its target?
        site_perf_qtr = case_when(                     # When did the site meet it's target?
          site_perf == 1 & qtr1 >= targets ~ 1,
          site_perf == 1 & qtr2 >= targets ~ 2,
          site_perf == 1 & qtr3 >= targets ~ 3,
          site_perf == 1 & qtr4 >= targets ~ 4,
          TRUE ~ 0
        ),
        site_perf_loss = case_when(                   # When did the site missed to maintain perf?
          site_perf == 0 & qtr1 >= targets ~ 1,
          site_perf == 0 & qtr2 >= targets ~ 2,
          site_perf == 0 & qtr3 >= targets ~ 3,
          site_perf == 0 & qtr4 >= targets ~ 4,
          TRUE ~ 0
        )
      ) %>% 
      ungroup() %>% 
      group_by(fiscal_year, fundingagency, 
               snu1uid, snu1, psnuuid, psnu) %>% 
      mutate(                                                    # Summary PSNU Level Performance
        psnu_results = sum(results, na.rm = TRUE),
        psnu_results_qtr1 = sum(qtr1, na.rm = TRUE),
        psnu_results_qtr2 = sum(qtr2, na.rm = TRUE),
        psnu_results_qtr3 = sum(qtr3, na.rm = TRUE),
        psnu_results_qtr4 = sum(qtr4, na.rm = TRUE),
        psnu_targets = sum(targets, na.rm = TRUE),
        psnu_perf = if_else(psnu_results >= psnu_targets, 1, 0), # did psnu meet its target?
        psnu_perf_qtr = case_when(                               # When did the psnu meet it's target?
          psnu_results_qtr1 >= psnu_targets ~ 1,
          psnu_results_qtr2 >= psnu_targets ~ 2,
          psnu_results_qtr3 >= psnu_targets ~ 3,
          psnu_results_qtr4 >= psnu_targets ~ 4,
          TRUE ~ 0
        ),
        psnu_perf_loss = case_when(                              # When did psnu missed to maintain perf?
          psnu_perf == 0 & psnu_results_qtr1 >= psnu_targets ~ 1,
          psnu_perf == 0 & psnu_results_qtr2 >= psnu_targets ~ 2,
          psnu_perf == 0 & psnu_results_qtr3 >= psnu_targets ~ 3,
          psnu_perf == 0 & psnu_results_qtr4 >= psnu_targets ~ 4,
          TRUE ~ 0
        )
      ) %>% 
      ungroup() %>% 
      mutate(site_outperf = case_when(                          # What are the sites doing better / worse then their parent psnu
        site_perf > psnu_perf ~ "YES",
        site_perf < psnu_perf ~ "NO",
        site_perf == 1 & site_perf == 1 ~ "YYES",
        site_perf == 0 & psnu_perf == 0 ~ "NNO",
        TRUE ~ NA_character_
      )
      ) 
  }
  
  
  #' @title Treatment Sites location
  #' 
  #' @param df_msd reference df for TX_CURR Site x IM data
  #' @param country Country/OU 
  #' @param username Datim Passwork
  #' @param password Datim Username
  #' @param site_only return site only data, default = TRUE
  #' @return sites with valide location data
  #' 
  tx_sites <- function(df_msd, 
                       username = datim_user(), 
                       password = datim_pwd(),
                       peds_only = FALSE,
                       site_only = TRUE) {
    
    # Check credentials
    if (is.null(username) | is.null(password)) {
      cat(Wavelength::paint_red("ERROR - Your datim credentials are not set."))
      stop("Look into: glamr::set_datim()")
    }
    
    # Extract sites location from datim
    sites <- gisr::extract_locations(
      country = "Zambia", 
      username = username,
      password = password
    ) %>% 
      extract_facilities() %>%
      filter(!is.na(longitude) | !is.na(latitude)) %>%
      select(orgunituid = id, longitude, latitude)
    
    # Merge locations to mer data & check for peds
    if (peds_only == TRUE) {
      sites <- df_msd %>%
        dplyr::filter(indicator == "TX_CURR", trendscoarse == "<15") %>% 
        dplyr::left_join(sites, by = "orgunituid")
    } 
    else {
      sites <- df_msd %>%
        dplyr::filter(indicator == "TX_CURR") %>% 
        dplyr::left_join(sites, by = "orgunituid")
    }
    
    # Keep site info only
    if (site_only == TRUE) {
      sites <- sites %>% 
        dplyr::distinct(orgunituid, sitename, longitude, latitude)
    }
    
    return(sites)
  }
  
  
  
# LOAD & MUNGE MSD --------------------------------------------------------
  
  ken_msd <- return_latest(merdata, "Site_IM.*Kenya")
  
  # @essam import/first time handled in 99_data_import (read_msd)
  msd <- read_msd(ken_msd)
  
  # Subset to only select indicators
  msd <- msd %>% 
    filter(indicator %in% indic_list) %>%
    clean_agency()
  
  
  
# EXPLORE DISTRIBUTION & COUNTS OF PEDS TREATMENT SITES -----------------
    
  # Treatment Site locations
  df_tx_sites <- msd %>% 
    tx_sites()

  # Sites with targets only - 
  msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", 
           !is.na(targets), indicator == "TX_CURR") %>% 
    distinct_count()
  
  # Sites with results only
  msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", 
           !is.na(cumulative), indicator == "TX_CURR") %>% 
    distinct_count()
  
  # Sites with results OR targets
  tx_count <- msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", 
           !is.na(cumulative) | !is.na(targets), indicator == "TX_CURR") %>% 
    distinct_count()
  
  # Sites with results for under15s
  tx_count_peds <- msd %>% 
    filter(trendscoarse == "<15", !is.na(cumulative), indicator == "TX_CURR") %>% 
    mutate(fundingagency = paste0(fundingagency, "_peds")) %>% 
    distinct_count()
  
  # What proportion of sites are reporting PEDS numbers
  left_join(tx_count, tx_count_peds) %>% 
    select(`FY` = fiscal_year,
           `CDC sites` = CDC, 
           `USAID sites` =USAID, 
           `CDC peds sites` = CDC_peds, 
           `USAID peds sites` = USAID_peds) %>% 
    mutate(`USAID peds share` = (`USAID peds sites` / `USAID sites`), 
           `CDC peds share` = (`CDC peds sites` / `CDC sites`)) %>% 
    select(sort(current_vars())) %>% select(`FY`, everything()) %>% 
    gt() %>% 
    tab_options(table.font.names = "Source Sans Pro")  %>% 
    fmt_number(columns = c(3:4, 6:7), decimals = 0) %>% 
    fmt_percent(columns = c(2,5), decimals = 0) %>% 
    gtsave(here(graphics, "KEN_peds_share_tx_sites.png"))
  
  # FOCUS ON MECHS NOW
  # What does this look like by mech_code for only USAID?
  usaid_tx_count <- msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", 
           !is.na(cumulative) | !is.na(targets), 
           indicator == "TX_CURR", 
           fundingagency == "USAID") %>% 
    distinct(orgunituid, mech_code, mech_name, fiscal_year, indicator, fundingagency) %>% 
    count(fiscal_year, mech_name) %>% 
    spread(mech_name, n) %>%
    mutate(tag = "all")
  
  # What about for only peds?
  usaid_tx_count_peds <- msd %>% 
    filter(trendscoarse == "<15", !is.na(cumulative), 
           indicator == "TX_CURR", 
           fundingagency == "USAID") %>% 
    mutate(tag = "peds") %>% 
    distinct(orgunituid, mech_code, mech_name, fiscal_year, indicator, fundingagency, tag) %>% 
    count(fiscal_year, mech_name, tag) %>% 
    spread(mech_name, n)
  
  # Combining these, what do we get?
  rbind(usaid_tx_count[(names(usaid_tx_count) %in% names(usaid_tx_count_peds))], usaid_tx_count_peds) %>% 
    pivot_longer(cols = -c(fiscal_year, tag),
                 names_to = "mech", 
                 values_to = "site_count") %>% 
    pivot_wider(names_from = tag, values_from = site_count) %>% 
    ggplot(aes(factor(fiscal_year))) +
    geom_col(aes(y = all), fill = grey10k) +
    geom_col(aes(y = peds), fill = scooter) +
    geom_text(aes(label = peds, y = peds), vjust = 0, nudge_y = 1, color = scooter) +
    geom_text(aes(label = paste0("(", percent(peds/all, accuracy = 1), ")"), y = peds), 
              vjust = 1, nudge_y = -3, color = "white") +
    geom_text(aes(label = all, y = all), vjust = 0, nudge_y = 1, color = grey80k) +
    facet_wrap(~mech) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 325)) +
    si_style_xline() +
    theme(axis.text.y = element_blank()) +
    labs(x = NULL, y = NULL,
         title = "THE SHARE OF USAID TREATMENT SITES SERVING PEDS (<15) HAS INCREASED FOR AFYA JIJINI AND AFYA PWANI",
         caption = source)
  
  si_save(here(graphics, "KEN_share_tx_site_serving_peds"), scale = 1.45)
  
  # Main Takeaways
  # The share of sites that are reporting TX_CURR peds results has been increasing year over year
  # Next step is to look at the volume of results by partner 
  
  
  # What do targets / achievements look like by mech by age band?
  tx_curr_mech_peds <- 
    msd %>% 
    filter(trendscoarse == "<15", 
           !is.na(cumulative), 
           indicator == "TX_CURR", 
           fundingagency == "USAID", 
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year != 2019) %>% 
    group_by(mech_code, mech_name, fiscal_year, indicator, trendsfine) %>% 
    summarise(TX_CURR_peds = sum(cumulative, na.rm = TRUE),
              targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(trendsfine) %>% 
    mutate(maxval = max(targets, na.rm = TRUE)) %>% 
    ungroup()  

  tx_curr_mech_peds %>% 
    ggplot(aes(x = factor(fiscal_year), y = TX_CURR_peds, group = mech_name)) +
    geom_col(aes(y = targets), fill = grey10k) +
    geom_col(aes(fill = mech_name)) + 
    geom_point(aes(y = maxval), colour = "white") +
    geom_errorbar(data = . %>% filter(targets != 0), 
                  aes(ymin = targets, ymax = targets), 
                  colour = grey10k, size = 0.5, linetype = "dashed" ) +
    facet_wrap(paste0("Age band: ", trendsfine) ~ mech_name, scales = "free", nrow = 3) +
    scale_fill_si(palette = "siei", reverse = TRUE) +
    scale_y_continuous(expand = c(0, Inf), limits = c(0, NaN))+
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = "NO USAID PARTNER MET THEIR FY20 TREATMENT TARGETS FOR THE <1 AGE BAND",
         subtitle = "Treatment ahievement depicted by filled bar height, targets shown in light gray (or with dotted line when surpassed)",
         caption = source)+
    theme(legend.position = "none")  

  
  
  tx_curr_mech_under15s <- 
    msd %>% 
    # filter(!is.na(cumulative), 
    # indicator == "TX_CURR", 
    # fundingagency == "USAID", 
    # standardizeddisaggregate == "Total Numerator") %>% 
    filter(trendscoarse == "<15", 
           !is.na(cumulative), 
           indicator == "TX_CURR", 
           fundingagency == "USAID", 
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    #mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    group_by(mech_code, mech_name, fiscal_year, indicator) %>% 
    summarise(TX_CURR = sum(cumulative, na.rm = TRUE))
  
  # What does the composition of the mechs look like for under 15s? Is the mix of treatment patients
  # changing across time for each partner?
  tx_curr_mech_under15s %>% 
    left_join(., tx_curr_mech_peds) %>% 
    mutate(share = (TX_CURR_peds/TX_CURR)) %>% 
    ggplot(aes(x = factor(fiscal_year), y = share, group = trendsfine, color = trendsfine)) + 
    geom_line() + 
    ggrepel::geom_text_repel(data = . %>% filter(fiscal_year != 2019), 
                             aes(label = comma(TX_CURR_peds, accuracy = 1))) +
    facet_wrap(~mech_name) +
    scale_y_continuous(labels = percent) +
    si_style_ygrid() +
    scale_color_si(palette = "siei")+
    labs(x = NULL, y = NULL,
         title = "THE AGE COMPOSITION OF TREATMENT HAS CHANGED LITTLE ACROSS FISCAL YEARS",
         caption = source) +
    theme(legend.position = "none")
  
  si_save(here(graphics, "ZMB_age_composition_tx_curr_by_partner"))
  
  tx_curr_mech <- 
    msd %>% 
    filter(!is.na(cumulative), 
           indicator == "TX_CURR", 
           fundingagency == "USAID", 
           standardizeddisaggregate == "Total Numerator") %>% 
    mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    group_by(mech_code, mech_name, fiscal_year, indicator) %>% 
    summarise(TX_CURR = sum(cumulative, na.rm = TRUE)) 
  
  tx_curr_mech_under15s %>% 
    rename(TX_CURR_peds = TX_CURR) %>% 
    left_join(tx_curr_mech) %>% 
    mutate(share = TX_CURR_peds / TX_CURR)  
  
  

# PLOT --------------------------------------------------------------------

  # Run the ggplot code chunk below with the set.seed chunk to get reproducible jitter
  set.seed(20201215)  
  msd %>% 
    #filter(snu1 %in% c("Copperbelt Province", "Central Province")) %>% 
    filter(trendscoarse == "<15", 
           !is.na(cumulative), 
           indicator == "TX_CURR", 
           fundingagency == "USAID", 
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    #mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    group_by(orgunituid, mech_name, fiscal_year, trendsfine, snu1) %>% 
    summarise(tx_curr_peds = sum(cumulative, na.rm = TRUE),
              targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(target_achieved = if_else(tx_curr_peds > targets & targets !=0, 1, 0)) %>% 
    filter(fiscal_year == "2021", trendsfine != "<01") %>% 
    group_by(trendsfine) %>% 
    mutate(maxval = max(tx_curr_peds),
           average = mean(tx_curr_peds)
    ) %>% 
    ungroup() %>% 
    mutate(color_code = if_else(target_achieved == 0, "grey", mech_name)) %>% 
    ggplot(aes(x = mech_name, y = tx_curr_peds, color = mech_name)) +
    geom_point(aes(y = maxval, colour = NA)) +
    geom_hline(aes(yintercept = average), color = grey40k, linetype = "dotted", size = 1) + 
    geom_quasirandom(alpha = 0.75, method = "tukeyDense") +
    #geom_quasirandom(data = . %>% filter(target_achieved == 0), alpha = 0.75, method = "tukeyDense") +
    #geom_quasirandom(data = . %>% filter(target_achieved == 1), method = "tukeyDense") +
    facet_wrap(~paste0(trendsfine, "\n"), scales = "free_y", nrow = 3) +
    coord_flip() +
    si_style_xgrid() +
    scale_color_si(palette = "siei", reverse = TRUE) +
    #scale_color_manual(values = c("grey" = grey10k, "SAFE" = denim, "EQUIP" = burnt_sienna,
    # "DISCOVER-H" = scooter)) +
    labs(x = NULL, y = "TX_CURR Achieved",
         title = "IN FY20, TREATMENT SITES SERVED AN AVERAGE OF ABOUT 16 PATIENTS PER SITE",
         subtitle = "TX_CURR site level, age band average depicted by dotted line.",
         caption = source)  +
    scale_y_continuous(trans = log_trans(),
                       breaks =c(1, 5, 10, 15, 25, 50, 100, 300, 500, 5000)) +
    theme(legend.position = "none",
          axis.line.x = element_line(colour = color_gridline))   
  
  
# County level?
  msd %>% 
    #filter(snu1 %in% c("Copperbelt Province", "Central Province")) %>% 
    filter(trendscoarse == "<15", 
           !is.na(cumulative), 
           indicator == "TX_CURR", 
           fundingagency == "USAID", 
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           mech_name == "AMPATHplus") %>% 
    #mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    group_by(orgunituid, mech_name, fiscal_year, trendsfine, snu1) %>% 
    summarise(tx_curr_peds = sum(cumulative, na.rm = TRUE),
              targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(target_achieved = if_else(tx_curr_peds > targets & targets !=0, 1, 0)) %>% 
    filter(fiscal_year == "2021", trendsfine != "<01") %>% 
    group_by(trendsfine) %>% 
    mutate(maxval = max(tx_curr_peds),
           average = mean(tx_curr_peds)
    ) %>% 
    ungroup() %>% 
    mutate(color_code = if_else(target_achieved == 0, "grey", mech_name)) %>% 
    ggplot(aes(x = trendsfine, y = tx_curr_peds, color = trendsfine)) +
    geom_point(aes(y = maxval, colour = NA)) +
    geom_hline(aes(yintercept = average, color = trendsfine), linetype = "dotted", size = 1) + 
    geom_quasirandom(alpha = 0.75, method = "tukeyDense") +
    #geom_quasirandom(data = . %>% filter(target_achieved == 0), alpha = 0.75, method = "tukeyDense") +
    #geom_quasirandom(data = . %>% filter(target_achieved == 1), method = "tukeyDense") +
    facet_wrap(~snu1, scales = "free_y") +
    coord_flip() +
    si_style_xgrid() +
    scale_color_si(palette = "siei", reverse = TRUE) +
    #scale_color_manual(values = c("grey" = grey10k, "SAFE" = denim, "EQUIP" = burnt_sienna,
    # "DISCOVER-H" = scooter)) +
    labs(x = NULL, y = "TX_CURR Achieved",
         title = "IN FY21Q1, AMPATHplus SERVED AN AVERAGE OF ABOUT 11 PATIENTS PER SITE",
         subtitle = "TX_CURR site level, age band averages depicted by dotted line.",
         caption = source)  +
    scale_y_continuous(trans = log_trans(),
                       breaks =c(1, 5, 10, 15, 25, 50, 100, 300)) +
    theme(legend.position = "none",
          axis.line.x = element_line(colour = color_gridline))   
  
  