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

  #TODO: @Achafetz -- this may need to be modified based on 1st run or not given what read_msd does
  return_latest(datim_path, "Site_IM.*Zambia") %>% unzip(., exdir = datim_path)
  zmb_msd <- return_latest(datim_path, "Site_IM.*Zambia")
  
  msd <- read_msd(zmb_msd)

  # Subset to only select indicators
  msd <- msd %>% 
    filter(indicator %in% indic_list) %>% 
    mutate(fundingagency = if_else(fundingagency == "HHS/CDC", "CDC", fundingagency))
    
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

  
  