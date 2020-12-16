# PURPOSE: TX_NET_NEW Analysis focused on PEDS
# COLLABORATOR: T Essam | A Chafetz | B Kagniniwa
# DATE: 2020-12-09
# NOTES: BASED ON REQUEST FROM ZAMBIA SI LEAD


# GLOBALS -----------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(sf)
  library(glitr)
  library(glamr)
  library(gisr)
  library(scales)
  library(here)

  # indicators of focus
  indic_list <- c("TX_CURR", "TX_NET_NEW", "TX_NEW") 
  
  # Load credentials & project paths
    glamr::si_path()
  
    glamr::load_secrets()

  
  # FUNCTIONS
  # Return a distinct count of a filtered data frame to check numbers
  # Spread by funding agency
  distinct_count <- function(df) {
    df %>% 
      distinct(orgunituid, mech_code, fiscal_year, indicator, fundingagency) %>% 
      count(fiscal_year, fundingagency) %>% 
      spread(fundingagency, n)
  }
  
  #' @title Distribution of Site / TX_CURR Share
  #' 
  #' @param df_tx
  #' @param indicator Indicator name
  #' @param exclude_na Exclude sites with NA results/targets
  #' @param share_level Share levels: psnu, snu1 or null for country
  #' @return TX_CURR Results Achievements
  #' 
  sites_share <- function(df_tx, 
                          indicator = "TX_CURR",
                          exclude_na = TRUE,
                          share_level = "PSNU") {
    # Filter indicators
    df_tx <- df_tx %>% 
      filter(indicator == {{indicator}},
             standardizeddisaggregate == "Total Numerator")
    
    # Exclude results / targets with NA values
    if (exclude_na == TRUE) {
      df_tx <- df_tx %>%
        filter(!is.na(cumulative), !is.na(targets)) 
    }
    
    # Select vars of interest
    df_tx <- df_tx %>%  
      select(fiscal_year, fundingagency, snu1uid, snu1, 
             psnuuid, psnu, orgunituid, sitename, targets, 
             results = cumulative, starts_with("qtr")) 
    
    # Identify share level
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
        targets_share = targets / sum(targets),
        results_share = results / sum(results),
        qtr1_share = qtr1 / sum(targets), 
        qtr2_share = qtr2 / sum(targets), 
        qtr3_share = qtr3 / sum(targets), 
        qtr4_share = qtr4 / sum(targets) 
      ) %>% 
      ungroup() %>% 
      pivot_longer(cols = starts_with(c("targ", "res", "qtr")), 
                   names_to = "metric", 
                   values_to = "val") 
    
    return(df_tx)
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
  
  
# TASKS -------------------------------------------------------------------

  #1) Determine number of treatment sites (with results, with targets, with targets and results) 
  
  #2) Determine the number of treatment sites with PEDs
  
  #3) What is the distribution of TX_CURR within the sites? (results / targets share)
  
  
  
  
  
# LOAD & MUNGE MSD --------------------------------------------------------

  zmb_msd <- return_latest(si_path(), "Site_IM.*Zambia")
  
  # @essam import/first time handled in 99_data_import (read_msd)
  msd <- read_msd(zmb_msd)

  # Subset to only select indicators
  msd <- msd %>% 
    filter(indicator %in% indic_list) %>%
    clean_agency()
  
  # Treatment Site locations
  msd %>% tx_sites()
  
  # Treatment Site locations for peds 
  msd %>% tx_sites(peds_only = TRUE)
    
  # Check on different breakdowns across types of filters 
  # Sites with targets only - 
  msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", !is.na(targets), indicator == "TX_CURR") %>% 
    distinct_count()
    
  # Sites with results only
  msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", !is.na(cumulative), indicator == "TX_CURR") %>% 
    distinct_count()

  # Sites with results OR targets
  tx_count <- msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", !is.na(cumulative) | !is.na(targets), indicator == "TX_CURR") %>% 
    distinct_count()
  
  # Sites with results for under15s
  tx_count_peds <- msd %>% 
    filter(trendscoarse == "<15", !is.na(cumulative), indicator == "TX_CURR") %>% 
    mutate(fundingagency = paste0(fundingagency, "_peds")) %>% 
    distinct_count()
  
  # What proportion of sites are reporting PEDS numbers
  left_join(tx_count, tx_count_peds) %>% 
    mutate(peds_count_share = (USAID_peds / USAID), 
           peds_count_share_cdc = (CDC_peds / CDC)) 


# AGGREGATE TARGETS / RESULTS & CREATE SHARES -----------------------------
  
  # site results share by PSNU
  msd %>% sites_share(share_level = "PSNU")
  
  msd %>% 
    sites_share(share_level = "OU") %>% 
    clean_psnu() %>%
    filter(fiscal_year == 2020,
           fundingagency == "USAID",
           metric == "results_share") %>% 
    select(snu1, psnu, sitename, val) %>% 
    arrange(snu1, psnu, desc(val)) %>% 
    View()
  
  # site results share by snu1
  msd %>% sites_share(share_level = "SNU1") 
  
  msd %>% 
    sites_share(share_level = "OU") %>% 
    clean_psnu() %>%
    filter(fiscal_year == 2020,
           fundingagency == "USAID",
           metric == "results_share") %>% 
    select(snu1, sitename, val) %>% 
    arrange(snu1, desc(val)) %>% 
    View()
  
  # site results share by ou
  msd %>% sites_share(share_level = "OU") 
  
  # Sites with at least 1% of the country's TX_CURR Results
  msd %>% 
    sites_share(share_level = "OU") %>% 
    clean_psnu() %>%
    filter(fiscal_year == 2020,
           fundingagency == "USAID",
           metric == "results_share",
           val >= .01) %>% 
    select(snu1, psnu, sitename, val) %>% 
    arrange(desc(val)) %>% 
    View()
    