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

  
  # FUNCTIONS
  # Return a distinct count of a filtered data frame to check numbers
  # Spread by funding agency
  distinct_count <- function(df) {
    df %>% 
      distinct(orgunituid, mech_code, fiscal_year, indicator, fundingagency) %>% 
      count(fiscal_year, fundingagency) %>% 
      spread(fundingagency, n)
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
