


###################### INTRODUCTION ---------------------------------------------------
# This code creates the dataframes to be used in the Pediatric automated country reports

# Input: Indivitual country SITExIM files (downloaded from DATIM - Genie)
# Output: multiple dataframes with metrics summarized based on the subset (e.g., all data, USAID only, USAID x gender, etc) x (peds vs under 5 vs adolescent)

# Steps for creating the automated pediatric country report:
#1. Download the country SITE x IM files
#2. Run creating_95_metrics_df.R file to create the summary dataframes   ***** this code represents this step *****
#3. Run combining the 95s output dfs.R to combine the individual country dataframes into multiple, combined dataframes 
#4. Run the RMarkdown file to generate the report   
              #a. Note, there is a separate R file that runs all the reports at once. 


# Load necessary libraries
library(tidyverse)
library(janitor)
library(data.table)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)
library(readr)

# Clear the environment
rm(list = ls())
gc()


##################### FUNCTIONS ----------------------------------------------------
### Functions for the coding below


# Fiscal year and quarter two quarters prior
get_prev_two_quarters <- function(fiscal_year, quarter_num) {
  if (quarter_num == 1) {
    fiscal_year_prev <- fiscal_year - 1
    quarter_num_prev <- 3
  } else if (quarter_num == 2) {
    fiscal_year_prev <- fiscal_year - 1
    quarter_num_prev <- 4
  } else {
    fiscal_year_prev <- fiscal_year
    quarter_num_prev <- quarter_num - 2
  }
  return(data.frame(fiscal_year_prev = fiscal_year_prev, quarter_num_prev = quarter_num_prev))
}


# Function to create quarterly target variables
create_quarterly_targets <- function(data) {
  data %>%
    mutate(
      tgt1 = if_else(!is.na(targets), targets / 4, NA_real_),
      tgt2 = if_else(!is.na(targets), targets / 4, NA_real_),
      tgt3 = if_else(!is.na(targets), targets / 4, NA_real_),
      tgt4 = if_else(!is.na(targets), targets / 4, NA_real_)
    )
}



# Function to create dataframes to save

append_batch_dfs <- function() {
  # Assign variables globally using <<-
  metrics_hts_data_usaid_under5  <<- batch_metrics_hts_data_usaid$hts_data_to_saveunder5_usaid
  metrics_hts_data_usaid_peds  <<- batch_metrics_hts_data_usaid$hts_data_to_savepeds_usaid
  metrics_hts_data_usaid_adolescent  <<- batch_metrics_hts_data_usaid$hts_data_to_saveadolescent_usaid
  
  cumulative_metrics_95_under5  <<- batch_metrics_dfs$metrics_95_under5
  cumulative_metrics_95_peds  <<- batch_metrics_dfs$metrics_95_peds
  cumulative_metrics_95_adolescent  <<- batch_metrics_dfs$metrics_95_adolescent
  
  cumulative_metrics_95_under5_usaid  <<- batch_metrics_dfs_usaid$metrics_95_under5_usaid
  cumulative_metrics_95_peds_usaid  <<- batch_metrics_dfs_usaid$metrics_95_peds_usaid
  cumulative_metrics_95_adolescent_usaid  <<- batch_metrics_dfs_usaid$metrics_95_adolescent_usaid
  
  cumulative_metrics_95_under5_usaid_gender  <<- batch_metrics_dfs_usaid_gender$metrics_95_under5_usaid_gender
  cumulative_metrics_95_peds_usaid_gender  <<- batch_metrics_dfs_usaid_gender$metrics_95_peds_usaid_gender
  cumulative_metrics_95_adolescent_usaid_gender  <<- batch_metrics_dfs_usaid_gender$metrics_95_adolescent_usaid_gender
  
  cumulative_metrics_95_under5_cdc  <<- batch_metrics_dfs_cdc$metrics_95_under5_cdc
  cumulative_metrics_95_peds_cdc  <<- batch_metrics_dfs_cdc$metrics_95_peds_cdc
  cumulative_metrics_95_adolescent_cdc  <<- batch_metrics_dfs_cdc$metrics_95_adolescent_cdc
  
  cumulative_metrics_95_under5_usaid_pp  <<- batch_metrics_dfs_usaid_pp$metrics_95_under5_usaid_pp
  cumulative_metrics_95_peds_usaid_pp  <<- batch_metrics_dfs_usaid_pp$metrics_95_peds_usaid_pp
  cumulative_metrics_95_adolescent_usaid_pp  <<- batch_metrics_dfs_usaid_pp$metrics_95_adolescent_usaid_pp
  
  cumulative_metrics_95_under5_usaid_snu1  <<- batch_metrics_dfs_usaid_snu1$metrics_95_under5_usaid_snu1
  cumulative_metrics_95_peds_usaid_snu1  <<- batch_metrics_dfs_usaid_snu1$metrics_95_peds_usaid_snu1
  cumulative_metrics_95_adolescent_usaid_snu1  <<- batch_metrics_dfs_usaid_snu1$metrics_95_adolescent_usaid_snu1
  
  cumulative_target_metrics_95_under5  <<- batch_target_metrics_dfs$target_metrics_95_under5
  cumulative_target_metrics_95_peds  <<- batch_target_metrics_dfs$target_metrics_95_peds
  cumulative_target_metrics_95_adolescent  <<- batch_target_metrics_dfs$target_metrics_95_adolescent
  
  cumulative_target_metrics_95_under5_usaid  <<- batch_target_metrics_dfs_usaid$target_metrics_95_under5_usaid
  cumulative_target_metrics_95_peds_usaid  <<- batch_target_metrics_dfs_usaid$target_metrics_95_peds_usaid
  cumulative_target_metrics_95_adolescent_usaid  <<- batch_target_metrics_dfs_usaid$target_metrics_95_adolescent_usaid
  
  cumulative_target_metrics_95_under5_usaid_gender  <<- batch_target_metrics_dfs_usaid_gender$target_metrics_95_under5_usaid_gender
  cumulative_target_metrics_95_peds_usaid_gender  <<- batch_target_metrics_dfs_usaid_gender$target_metrics_95_peds_usaid_gender
  cumulative_target_metrics_95_adolescent_usaid_gender  <<- batch_target_metrics_dfs_usaid_gender$target_metrics_95_adolescent_usaid_gender
  
  cumulative_target_metrics_95_under5_cdc  <<- batch_target_metrics_dfs_cdc$target_metrics_95_under5_cdc
  cumulative_target_metrics_95_peds_cdc  <<- batch_target_metrics_dfs_cdc$target_metrics_95_peds_cdc
  cumulative_target_metrics_95_adolescent_cdc  <<- batch_target_metrics_dfs_cdc$target_metrics_95_adolescent_cdc
  
  cumulative_target_metrics_95_under5_usaid_pp  <<- batch_target_metrics_dfs_usaid_pp$target_metrics_95_under5_usaid_pp
  cumulative_target_metrics_95_peds_usaid_pp  <<- batch_target_metrics_dfs_usaid_pp$target_metrics_95_peds_usaid_pp
  cumulative_target_metrics_95_adolescent_usaid_pp  <<- batch_target_metrics_dfs_usaid_pp$target_metrics_95_adolescent_usaid_pp
  
  cumulative_target_metrics_95_under5_usaid_snu1  <<- batch_target_metrics_dfs_usaid_snu1$target_metrics_95_under5_usaid_snu1
  cumulative_target_metrics_95_peds_usaid_snu1  <<- batch_target_metrics_dfs_usaid_snu1$target_metrics_95_peds_usaid_snu1
  cumulative_target_metrics_95_adolescent_usaid_snu1  <<- batch_target_metrics_dfs_usaid_snu1$target_metrics_95_adolescent_usaid_snu1
  
  message("All data frames have been successfully appended to the global environment.")
}




###################### LOADING and RESHAPING THE DATA -----------------------------

# Clear the environment (except for the functions)
rm(list = ls()[!sapply(mget(ls(), .GlobalEnv), is.function)])


# Specify the file path and read the file 

load('C:/Users/georg/Desktop/databases/output/2024_Q4/Senegal_SITE_IM_2022_2024Q4.rda')


# create quarterly targets 

DATIM_combined = Senegal_SITE_IM_2022_2024Q4
rm(Senegal_SITE_IM_2022_2024Q4)


DATIM_combined = create_quarterly_targets(DATIM_combined)


 
# Reshape the data to long format
DATIM_long <- DATIM_combined %>%
    pivot_longer(
      cols = starts_with("qtr"),
      names_to = "quarter",
      names_prefix = "qtr",
      values_to = "value",
      values_drop_na = FALSE
    ) %>%
    mutate(
      quarter_num = as.numeric(quarter)
    )
  
DATIM_long_targets <- DATIM_combined %>%
    filter(is.na(targets) == FALSE) %>% 
    pivot_longer(
      cols = starts_with("tgt"),
      names_to = "quarter",
      names_prefix = "tgt",
      values_to = "target_value",
      values_drop_na = FALSE
    ) %>%
    mutate(
      quarter_num = as.numeric(quarter)
    )
  
  # Define the age groups and corresponding dataframe names
  age_groups <- list(
    under5 = c('<01', '01-04'),
    peds = c('<01', '01-04', '01-09', '05-09', '10-14'),
    adolescent = c('10-14', '15-19')
  )
  
  # Initialize lists to store metrics dataframes for this batch
  batch_metrics_dfs <- list()
  batch_metrics_dfs_usaid <- list()
  batch_metrics_dfs_usaid_gender <- list()
  batch_metrics_dfs_cdc <- list()
  batch_metrics_dfs_usaid_pp <- list()
  batch_metrics_dfs_usaid_snu1 <- list()
  
  batch_target_metrics_dfs <- list()
  batch_target_metrics_dfs_usaid <- list()
  batch_target_metrics_dfs_usaid_gender <- list()
  batch_target_metrics_dfs_cdc <- list()
  batch_target_metrics_dfs_usaid_pp <- list()
  batch_target_metrics_dfs_usaid_snu1 <- list()
  
  batch_metrics_hts_data_usaid <- list()
  
  
 # HTSdf = DATIM_long %>% filter(indicator == 'HTS_TST' | indicator == 'HTS_TST_POS')
  
  #table(HTSdf$indicator, HTSdf$standardizeddisaggregate)
  #table(HTSdf$modality)
  
 # target_ages = c('<01', '01-04', '01-09', '05-09', '10-14')
  
###################### CREATE the dataframes for Each Age Group and subset ----------------------------------

# Loop over each age group
for (group_name in names(age_groups)) {
  target_ages <- age_groups[[group_name]]
  
    # All agencies ----------------------------------------------------
  
  print('##### Processing All Funding Agencies #####')
  
  # First 95: Compute HTS_TST and HTS_TST_POS sums and Positivity Rate
  hts_data <- DATIM_long %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value
    ) %>%
    mutate(
      Positivity_Rate = HTS_TST_POS / HTS_TST
    )
  
  #Add total rows per quarter and year
  total_rows <- hts_data %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      HTS_TST = sum(HTS_TST, na.rm = TRUE),
      HTS_TST_POS = sum(HTS_TST_POS, na.rm = TRUE),
      modality = "ALL",  # Add modality = 'ALL' for total rows
      Positivity_Rate = HTS_TST_POS / HTS_TST,  # Recalculate Positivity Rate
      .groups = 'drop'
    )
  
  # Step 3: Combine original data with total rows
  hts_data <- bind_rows(hts_data, total_rows)
  
  
  # Step 4: Arrange for better readability (optional)
  hts_data <- hts_data %>%
    arrange(country, fiscal_year, quarter_num, modality)
  

  hts_data_targets <- DATIM_long_targets %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator) %>%
    summarize(
      total_value_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
    if(nrow(hts_data_targets) > 0) {
    hts_data_targets = hts_data_targets %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value_targets
    ) 
  }
  
  # Second 95: Compute TX_NEW and Linkage Rate
  tx_new_data <- DATIM_long %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_NEW = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  tx_new_data_targets <- DATIM_long_targets %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_NEW_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Merge hts_data and tx_new_data
  first_second_95_df <- hts_data %>%
    left_join(tx_new_data, by = c("country", "fiscal_year", "quarter_num")) %>%
    mutate(
      Linkage_Rate = TX_NEW / HTS_TST_POS
    ) 
  
  if(nrow(hts_data_targets) > 0) {
  first_second_95_df_targets <- hts_data_targets %>%
    left_join(tx_new_data_targets, by = c("country", "fiscal_year", "quarter_num")) %>% 
    rename(HTS_TST_targets = HTS_TST, 
           HTS_TST_POS_targets = HTS_TST_POS)
  } else {
    first_second_95_df_targets = data.frame()
  }
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N, and VL Coverage Rate and VL Suppression Rate
  # Get TX_PVLS_D and TX_PVLS_N
  tx_pvls_data <- DATIM_long %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_"
    )
  
  
  tx_pvls_data_targets <- DATIM_long_targets %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom) %>%
    summarize(
      total_value_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if(nrow(tx_pvls_data_targets) > 0) {
    tx_pvls_data_targets = tx_pvls_data_targets %>% 
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value_targets,
      names_prefix = "TX_PVLS_targets"
    )
  }
  
  
  # Get TX_CURR data
  tx_curr_data <- DATIM_long %>%
    filter(
      indicator == "TX_CURR",
      source_name == 'DATIM',
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_CURR = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  tx_curr_data_targets <- DATIM_long_targets %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_CURR_target = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Apply the function to tx_pvls_data to get previous fiscal_year and quarter_num
  vl_data <- tx_pvls_data %>%
    rowwise() %>%
    mutate(prev = list(get_prev_two_quarters(fiscal_year, quarter_num))) %>%
    unnest(prev) %>%
    ungroup()
  
  
  # Merge vl_data with tx_curr_data to get TX_CURR_prev
  vl_data <- vl_data %>%
    left_join(
      tx_curr_data %>%
        rename(
          fiscal_year_prev = fiscal_year,
          quarter_num_prev = quarter_num,
          TX_CURR_prev = TX_CURR
        ),
      by = c("country", "fiscal_year_prev", "quarter_num_prev")
    )
  
  # Compute VL Coverage Rate and VL Suppression Rate
  vl_data <- vl_data %>%
    mutate(
      VL_Coverage_Rate = TX_PVLS_D / TX_CURR_prev,
      VL_Suppression_Rate = TX_PVLS_N / TX_PVLS_D
    )
  
  # Merge first_second_95_df with vl_data
  metrics_df <- first_second_95_df %>%
    full_join(vl_data, by = c("country", "fiscal_year", "quarter_num"))
  
  # Now, join TX_CURR for the current quarter
  metrics_df <- metrics_df %>%
    left_join(tx_curr_data, by = c("country", "fiscal_year", "quarter_num"))
  
  # Select and arrange the columns
  metrics_df <- metrics_df %>%
    select(
      country,
      fiscal_year,
      quarter_num,
      HTS_TST,
      HTS_TST_POS,
      Positivity_Rate,
      TX_NEW,
      Linkage_Rate,
      TX_CURR,
      TX_PVLS_D,
      TX_PVLS_N,
      VL_Coverage_Rate,
      VL_Suppression_Rate
    ) %>%
    arrange(country, fiscal_year, quarter_num)
  
  
  # Perform the join operation
  if(nrow(first_second_95_df_targets > 0)) {
  metrics_df_targets <- first_second_95_df_targets %>%
    full_join(tx_curr_data_targets, by = c("country", "fiscal_year", "quarter_num")) %>%
    full_join(tx_new_data_targets, by = c("country", "fiscal_year", "quarter_num")) %>%
    full_join(tx_pvls_data_targets, by = c("country", "fiscal_year", "quarter_num"))
  
  metrics_df_targets$prime_partner_name <- 'all combined'
  metrics_df_targets$snu1 = 'all combined'
  
  batch_target_metrics_dfs[[paste0("target_metrics_95_", group_name)]] <- metrics_df_targets
  
  }
  
  # Add prime_partner_name column and snu1 name
  metrics_df$prime_partner_name <- 'all combined'
  metrics_df$snu1 = 'all combined'
  
  
  # Store the metrics_df in the batch_metrics_dfs list
  batch_metrics_dfs[[paste0("metrics_95_", group_name)]] <- metrics_df
  
  
    # USAID only ----------------------------------------------------
  
  print('##### Processing USAID Funding Agency #####')
  
  # Filter data for USAID funding agency
  DATIM_long_usaid <- DATIM_long %>%
    filter(funding_agency == 'USAID')
  
  DATIM_long_usaid_targets <- DATIM_long_targets %>%
    filter(funding_agency == 'USAID')
  
  
  
  # First 95: Compute HTS_TST and HTS_TST_POS sums and Positivity Rate
  
  
  hts_data_to_save <- DATIM_long_usaid %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator, modality) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value
    ) %>%
    mutate(
      Positivity_Rate = HTS_TST_POS / HTS_TST
    )
  
  # Step 2: Add total rows per quarter and year
  total_rows <- hts_data_to_save %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      HTS_TST = sum(HTS_TST, na.rm = TRUE),
      HTS_TST_POS = sum(HTS_TST_POS, na.rm = TRUE),
      modality = "ALL",  # Add modality = 'ALL' for total rows
      Positivity_Rate = HTS_TST_POS / HTS_TST,  # Recalculate Positivity Rate
      .groups = 'drop'
    )
  
  # Step 3: Combine original data with total rows
  hts_data_to_save <- bind_rows(hts_data_to_save, total_rows)
  
  # Step 4: Arrange for better readability (optional)
  hts_data_to_save <- hts_data_to_save %>%
    arrange(country, fiscal_year, quarter_num, modality)
  
  
  # Store the metrics_df in the batch_metrics_dfs_usaid_pp list
  batch_metrics_hts_data_usaid[[paste0("hts_data_to_save", group_name, "_usaid")]] <- hts_data_to_save
  
  
  
  hts_data_usaid <- DATIM_long_usaid %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value
    ) %>%
    mutate(
      Positivity_Rate = HTS_TST_POS / HTS_TST
    )
  
  
  hts_data_usaid_targets <- DATIM_long_usaid_targets %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator) %>%
    summarize(
      total_target_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if(nrow(hts_data_usaid_targets) > 0) {
    hts_data_usaid_targets = hts_data_usaid_targets %>% 
    pivot_wider(
      names_from = indicator,
      values_from = total_target_value
    ) %>% 
    rename(HTS_TST_targets = HTS_TST, 
           HTS_TST_POS_targets = HTS_TST_POS)
  
  }
  
  # Second 95: Compute TX_NEW and Linkage Rate
  tx_new_data_usaid <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_NEW = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  tx_new_data_usaid_targets <- DATIM_long_usaid_targets %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_NEW_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Merge hts_data and tx_new_data
  first_second_95_df_usaid <- hts_data_usaid %>%
    left_join(tx_new_data_usaid, by = c("country", "fiscal_year", "quarter_num")) %>%
    mutate(
      Linkage_Rate = TX_NEW / HTS_TST_POS
    )
  
  if(nrow(tx_new_data_usaid_targets) > 0) {
  first_second_95_df_usaid_targets <- hts_data_usaid_targets %>%
    left_join(tx_new_data_usaid_targets, by = c("country", "fiscal_year", "quarter_num")) 
  } else {
    first_second_95_df_usaid_targets = data.frame()
  }
  
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N, and VL Coverage Rate and VL Suppression Rate
  # Get TX_PVLS_D and TX_PVLS_N
  tx_pvls_data_usaid <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_"
    )
  
  tx_pvls_data_usaid_targets <- DATIM_long_usaid_targets %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom) %>%
    summarize(
      total_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if (nrow(tx_pvls_data_usaid_targets) > 0) {
    tx_pvls_data_usaid_targets = tx_pvls_data_usaid_targets %>% 
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_target"
    )
  } 
  
  
  
  
  # Get TX_CURR data
  tx_curr_data_usaid <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_CURR = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  tx_curr_data_usaid_targets <- DATIM_long_usaid_targets %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_CURR_target = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Apply the function to tx_pvls_data to get previous fiscal_year and quarter_num
  vl_data_usaid <- tx_pvls_data_usaid %>%
    rowwise() %>%
    mutate(prev = list(get_prev_two_quarters(fiscal_year, quarter_num))) %>%
    unnest(prev) %>%
    ungroup()
  
  # Merge vl_data with tx_curr_data to get TX_CURR_prev
  vl_data_usaid <- vl_data_usaid %>%
    left_join(
      tx_curr_data_usaid %>%
        rename(
          fiscal_year_prev = fiscal_year,
          quarter_num_prev = quarter_num,
          TX_CURR_prev = TX_CURR
        ),
      by = c("country", "fiscal_year_prev", "quarter_num_prev")
    )
  
  # Compute VL Coverage Rate and VL Suppression Rate
  vl_data_usaid <- vl_data_usaid %>%
    mutate(
      VL_Coverage_Rate = TX_PVLS_D / TX_CURR_prev,
      VL_Suppression_Rate = TX_PVLS_N / TX_PVLS_D
    )
  
  # Merge first_second_95_df with vl_data
  metrics_df_usaid <- first_second_95_df_usaid %>%
    full_join(vl_data_usaid, by = c("country", "fiscal_year", "quarter_num"))
  
  # Now, join TX_CURR for the current quarter
  metrics_df_usaid <- metrics_df_usaid %>%
    left_join(tx_curr_data_usaid, by = c("country", "fiscal_year", "quarter_num"))
  
  # Select and arrange the columns
  metrics_df_usaid <- metrics_df_usaid %>%
    select(
      country,
      fiscal_year,
      quarter_num,
      HTS_TST,
      HTS_TST_POS,
      Positivity_Rate,
      TX_NEW,
      Linkage_Rate,
      TX_CURR,
      TX_PVLS_D,
      TX_PVLS_N,
      VL_Coverage_Rate,
      VL_Suppression_Rate
    ) %>%
    arrange(country, fiscal_year, quarter_num)
  
  
  # Perform the join operation
  if (nrow(first_second_95_df_targets) > 0) {
  metrics_df_usaid_targets <- first_second_95_df_usaid_targets %>%
    full_join(tx_curr_data_usaid_targets, by = c("country", "fiscal_year", "quarter_num")) %>%
    full_join(tx_new_data_usaid_targets, by = c("country", "fiscal_year", "quarter_num")) %>%
    full_join(tx_pvls_data_usaid_targets, by = c("country", "fiscal_year", "quarter_num"))
  
  metrics_df_usaid_targets$prime_partner_name <- 'all USAID'
  metrics_df_usaid_targets$snu1 <- 'all USAID'
  batch_target_metrics_dfs_usaid[[paste0("target_metrics_95_", group_name, "_usaid")]] <- metrics_df_usaid_targets
  }
  
  # Add prime_partner_name column and snu1 name
  metrics_df_usaid$prime_partner_name <- 'all USAID'
  metrics_df_usaid$snu1 <- 'all USAID'

  
  # Store the metrics_df in the batch_metrics_dfs_usaid list
  batch_metrics_dfs_usaid[[paste0("metrics_95_", group_name, "_usaid")]] <- metrics_df_usaid
  
  
    # USAID Funding Agency - by Gender ----------------------------------------------------
  
  print('##### Processing USAID Funding Agency - by Gender #####')
  
  # Filter data for USAID funding agency
  DATIM_long_usaid <- DATIM_long %>%
    filter(funding_agency == 'USAID')
  
  DATIM_long_usaid_targets <- DATIM_long_targets %>%
    filter(funding_agency == 'USAID')
  
  
  
  # First 95: Compute HTS_TST and HTS_TST_POS sums and Positivity Rate
  hts_data_usaid_gender <- DATIM_long_usaid %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator, sex) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value
    ) %>%
    mutate(
      Positivity_Rate = HTS_TST_POS / HTS_TST
    )
  
  
  hts_data_usaid_targets_gender <- DATIM_long_usaid_targets %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator, sex) %>%
    summarize(
      total_target_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if (nrow(hts_data_usaid_targets_gender) > 0) {
    hts_data_usaid_targets_gender = hts_data_usaid_targets_gender %>% 
      pivot_wider(
      names_from = indicator,
      values_from = total_target_value
    ) %>% 
    rename(HTS_TST_targets = HTS_TST, 
           HTS_TST_POS_targets = HTS_TST_POS)
  }
  
  
  # Second 95: Compute TX_NEW and Linkage Rate
  tx_new_data_usaid_gender <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, sex) %>%
    summarize(
      TX_NEW = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  tx_new_data_usaid_targets_gender <- DATIM_long_usaid_targets %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, sex) %>%
    summarize(
      TX_NEW_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Merge hts_data and tx_new_data
  first_second_95_df_usaid_gender <- hts_data_usaid_gender %>%
    left_join(tx_new_data_usaid_gender, by = c("country", "fiscal_year", "quarter_num", 'sex')) %>%
    mutate(
      Linkage_Rate = TX_NEW / HTS_TST_POS
    )
  
  if (nrow(hts_data_usaid_targets_gender) > 0) {
  first_second_95_df_usaid_targets_gender <- hts_data_usaid_targets_gender %>%
    left_join(tx_new_data_usaid_targets_gender, by = c("country", "fiscal_year", "quarter_num", 'sex')) 
  } else {
    first_second_95_df_usaid_targets_gender = data.frame()
  }
  
  
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N, and VL Coverage Rate and VL Suppression Rate
  # Get TX_PVLS_D and TX_PVLS_N
  tx_pvls_data_usaid_gender <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom, sex) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_"
    )
  
  tx_pvls_data_usaid_targets_gender <- DATIM_long_usaid_targets %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom, sex) %>%
    summarize(
      total_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if (nrow(tx_pvls_data_usaid_targets_gender) > 0) {
    tx_pvls_data_usaid_targets_gender = tx_pvls_data_usaid_targets_gender %>% 
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_target"
    ) 
  }
  
  
  
  
  # Get TX_CURR data
  tx_curr_data_usaid_gender <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, sex) %>%
    summarize(
      TX_CURR = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  tx_curr_data_usaid_targets_gender <- DATIM_long_usaid_targets %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, sex) %>%
    summarize(
      TX_CURR_target = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Apply the function to tx_pvls_data to get previous fiscal_year and quarter_num
  vl_data_usaid_gender <- tx_pvls_data_usaid_gender %>%
    rowwise() %>%
    mutate(prev = list(get_prev_two_quarters(fiscal_year, quarter_num))) %>%
    unnest(prev) %>%
    ungroup()
  
  # Merge vl_data with tx_curr_data to get TX_CURR_prev
  vl_data_usaid_gender <- vl_data_usaid_gender %>%
    left_join(
      tx_curr_data_usaid_gender %>%
        rename(
          fiscal_year_prev = fiscal_year,
          quarter_num_prev = quarter_num,
          TX_CURR_prev = TX_CURR
        ),
      by = c("country", "fiscal_year_prev", "quarter_num_prev", 'sex')
    )
  
  # Compute VL Coverage Rate and VL Suppression Rate
  vl_data_usaid_gender <- vl_data_usaid_gender %>%
    mutate(
      VL_Coverage_Rate = TX_PVLS_D / TX_CURR_prev,
      VL_Suppression_Rate = TX_PVLS_N / TX_PVLS_D
    )
  
  # Merge first_second_95_df with vl_data
  metrics_df_usaid_gender <- first_second_95_df_usaid_gender %>%
    full_join(vl_data_usaid_gender, by = c("country", "fiscal_year", "quarter_num", 'sex'))
  
  # Now, join TX_CURR for the current quarter
  metrics_df_usaid_gender <- metrics_df_usaid_gender %>%
    left_join(tx_curr_data_usaid_gender, by = c("country", "fiscal_year", "quarter_num", 'sex'))
  
  # Select and arrange the columns
  metrics_df_usaid_gender <- metrics_df_usaid_gender %>%
    select(
      country,
      fiscal_year,
      quarter_num,
      sex,
      HTS_TST,
      HTS_TST_POS,
      Positivity_Rate,
      TX_NEW,
      Linkage_Rate,
      TX_CURR,
      TX_PVLS_D,
      TX_PVLS_N,
      VL_Coverage_Rate,
      VL_Suppression_Rate
    ) %>%
    arrange(country, fiscal_year, quarter_num)
  
  
  # Perform the join operation
  if(nrow(first_second_95_df_targets) > 0) {
  metrics_df_usaid_targets_gender <- first_second_95_df_usaid_targets_gender %>%
    full_join(tx_curr_data_usaid_targets_gender, by = c("country", "fiscal_year", "quarter_num", 'sex')) %>%
    full_join(tx_new_data_usaid_targets_gender, by = c("country", "fiscal_year", "quarter_num", 'sex')) %>%
    full_join(tx_pvls_data_usaid_targets_gender, by = c("country", "fiscal_year", "quarter_num", 'sex'))
  
  metrics_df_usaid_targets_gender$prime_partner_name <- 'all USAID'
  metrics_df_usaid_targets_gender$snu1 <- 'all USAID'
  batch_target_metrics_dfs_usaid_gender[[paste0("target_metrics_95_", group_name, "_usaid_gender")]] <- metrics_df_usaid_targets_gender
  
  }
  
  # Add prime_partner_name column and snu1 name
  metrics_df_usaid_gender$prime_partner_name <- 'all USAID'
  metrics_df_usaid_gender$snu1 <- 'all USAID'

  
  # Store the metrics_df in the batch_metrics_dfs_usaid list
  batch_metrics_dfs_usaid_gender[[paste0("metrics_95_", group_name, "_usaid_gender")]] <- metrics_df_usaid_gender
  
  
  
    # CDC only ----------------------------------------------------
  
  
  print('##### Processing CDC Funding Agency #####')
  # Filter data for CDC funding agency
  DATIM_long_cdc <- DATIM_long %>%
    filter(funding_agency == 'HHS/CDC')
  
  DATIM_long_cdc_targets <- DATIM_long_targets %>%
    filter(funding_agency == 'HHS/CDC')
  
  if(nrow(DATIM_long_cdc) > 0)
    {
  
  # First 95: Compute HTS_TST and HTS_TST_POS sums and Positivity Rate
  hts_data_cdc <- DATIM_long_cdc %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value
    )
  
  if(nrow(hts_data_cdc) > 0)
  { hts_data_cdc = hts_data_cdc %>% 
      mutate(Positivity_Rate = HTS_TST_POS / HTS_TST
    )
  
  
  hts_data_cdc_targets <- DATIM_long_cdc_targets %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, indicator) %>%
    summarize(
      total_target_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if(nrow(hts_data_cdc_targets) > 0){
    hts_data_cdc_targets = hts_data_cdc_targets %>% 
    pivot_wider(
      names_from = indicator,
      values_from = total_target_value
    ) %>% 
    rename(HTS_TST_targets = HTS_TST, 
           HTS_TST_POS_targets = HTS_TST_POS)
  }
  
  
  # Second 95: Compute TX_NEW and Linkage Rate
  tx_new_data_cdc <- DATIM_long_cdc %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_NEW = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  tx_new_data_cdc_targets <- DATIM_long_cdc_targets %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_NEW_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Merge hts_data and tx_new_data
  first_second_95_df_cdc <- hts_data_cdc %>%
    left_join(tx_new_data_cdc, by = c("country", "fiscal_year", "quarter_num")) %>%
    mutate(
      Linkage_Rate = TX_NEW / HTS_TST_POS
    )
  
  if (nrow(hts_data_cdc_targets) > 0) {
  first_second_95_df_cdc_targets <- hts_data_cdc_targets %>%
    left_join(tx_new_data_cdc_targets, by = c("country", "fiscal_year", "quarter_num")) 
  } else {
    first_second_95_df_cdc_targets = data.frame()
  }
  
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N, and VL Coverage Rate and VL Suppression Rate
  # Get TX_PVLS_D and TX_PVLS_N
  tx_pvls_data_cdc <- DATIM_long_cdc %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_"
    )
  
  tx_pvls_data_cdc_targets <- DATIM_long_cdc_targets %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, numeratordenom) %>%
    summarize(
      total_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if(nrow(tx_pvls_data_cdc_targets) > 0){
    tx_pvls_data_cdc_targets = tx_pvls_data_cdc_targets %>% 
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_target"
    )
  }
  
  
  
  
  # Get TX_CURR data
  tx_curr_data_cdc <- DATIM_long_cdc %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_CURR = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  tx_curr_data_cdc_targets <- DATIM_long_cdc_targets %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num) %>%
    summarize(
      TX_CURR_target = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  
  # Apply the function to tx_pvls_data to get previous fiscal_year and quarter_num
  vl_data_cdc <- tx_pvls_data_cdc %>%
    rowwise() %>%
    mutate(prev = list(get_prev_two_quarters(fiscal_year, quarter_num))) %>%
    unnest(prev) %>%
    ungroup()
  
  # Merge vl_data with tx_curr_data to get TX_CURR_prev
  vl_data_cdc <- vl_data_cdc %>%
    left_join(
      tx_curr_data_cdc %>%
        rename(
          fiscal_year_prev = fiscal_year,
          quarter_num_prev = quarter_num,
          TX_CURR_prev = TX_CURR
        ),
      by = c("country", "fiscal_year_prev", "quarter_num_prev")
    )
  
  # Compute VL Coverage Rate and VL Suppression Rate
  vl_data_cdc <- vl_data_cdc %>%
    mutate(
      VL_Coverage_Rate = TX_PVLS_D / TX_CURR_prev,
      VL_Suppression_Rate = TX_PVLS_N / TX_PVLS_D
    )
  
  # Merge first_second_95_df with vl_data
  metrics_df_cdc <- first_second_95_df_cdc %>%
    full_join(vl_data_cdc, by = c("country", "fiscal_year", "quarter_num"))
  
  # Now, join TX_CURR for the current quarter
  metrics_df_cdc <- metrics_df_cdc %>%
    left_join(tx_curr_data_cdc, by = c("country", "fiscal_year", "quarter_num"))
  
  # Select and arrange the columns
  metrics_df_cdc <- metrics_df_cdc %>%
    select(
      country,
      fiscal_year,
      quarter_num,
      HTS_TST,
      HTS_TST_POS,
      Positivity_Rate,
      TX_NEW,
      Linkage_Rate,
      TX_CURR,
      TX_PVLS_D,
      TX_PVLS_N,
      VL_Coverage_Rate,
      VL_Suppression_Rate
    ) %>%
    arrange(country, fiscal_year, quarter_num)
  
  
  # Perform the join operation
  
  if(nrow(first_second_95_df_cdc_targets) > 0) {
  metrics_df_cdc_targets <- first_second_95_df_cdc_targets %>%
    full_join(tx_curr_data_cdc_targets, by = c("country", "fiscal_year", "quarter_num")) %>%
    full_join(tx_new_data_cdc_targets, by = c("country", "fiscal_year", "quarter_num")) %>%
    full_join(tx_pvls_data_cdc_targets, by = c("country", "fiscal_year", "quarter_num"))
  
  metrics_df_cdc_targets$prime_partner_name <- 'all HHS/CDC'
  metrics_df_cdc_targets$snu1 <- 'all HHS/CDC'
  batch_target_metrics_dfs_cdc[[paste0("target_metrics_95_", group_name, "_cdc")]] <- metrics_df_cdc_targets
  
  }
  
  # Add prime_partner_name column and snu1 name
  metrics_df_cdc$prime_partner_name <- 'all HHS/CDC'
  metrics_df_cdc$snu1 <- 'all HHS/CDC'

  
  # Store the metrics_df in the batch_metrics_dfs_cdc list
  batch_metrics_dfs_cdc[[paste0("metrics_95_", group_name, "_cdc")]] <- metrics_df_cdc
  }
  }
  
  
    # USAID Funding Agency - by prime partner  ----------------------------------------------------
  
  print('##### Processing USAID Funding Agency by Prime Partner #####')
  
  
  
  # First 95: Compute HTS_TST and HTS_TST_POS sums and Positivity Rate by prime_partner_name
  hts_data_usaid_pp <- DATIM_long_usaid %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name, indicator) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value
    ) %>%
    mutate(
      Positivity_Rate = HTS_TST_POS / HTS_TST
    )
  
  # Second 95: Compute TX_NEW and Linkage Rate
  tx_new_data_usaid_pp <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name) %>%
    summarize(
      TX_NEW = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge hts_data and tx_new_data
  first_second_95_df_usaid_pp <- hts_data_usaid_pp %>%
    left_join(tx_new_data_usaid_pp, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name")) %>%
    mutate(
      Linkage_Rate = TX_NEW / HTS_TST_POS
    )
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N, and VL Coverage Rate and VL Suppression Rate
  # Get TX_PVLS_D and TX_PVLS_N
  tx_pvls_data_usaid_pp <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name, numeratordenom) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_"
    )
  
  # Get TX_CURR data
  tx_curr_data_usaid_pp <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name) %>%
    summarize(
      TX_CURR = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Apply the function to tx_pvls_data to get previous fiscal_year and quarter_num
  vl_data_usaid_pp <- tx_pvls_data_usaid_pp %>%
    rowwise() %>%
    mutate(prev = list(get_prev_two_quarters(fiscal_year, quarter_num))) %>%
    unnest(prev) %>%
    ungroup()
  
  # Merge vl_data with tx_curr_data to get TX_CURR_prev
  vl_data_usaid_pp <- vl_data_usaid_pp %>%
    left_join(
      tx_curr_data_usaid_pp %>%
        rename(
          fiscal_year_prev = fiscal_year,
          quarter_num_prev = quarter_num,
          TX_CURR_prev = TX_CURR
        ),
      by = c("country", "prime_partner_name", "fiscal_year_prev", "quarter_num_prev")
    )
  
  # Compute VL Coverage Rate and VL Suppression Rate
  vl_data_usaid_pp <- vl_data_usaid_pp %>%
    mutate(
      VL_Coverage_Rate = TX_PVLS_D / TX_CURR_prev,
      VL_Suppression_Rate = TX_PVLS_N / TX_PVLS_D
    )
  
  # Merge first_second_95_df with vl_data
  metrics_df_usaid_pp <- first_second_95_df_usaid_pp %>%
    full_join(vl_data_usaid_pp, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name"))
  
  # Now, join TX_CURR for the current quarter
  metrics_df_usaid_pp <- metrics_df_usaid_pp %>%
    left_join(tx_curr_data_usaid_pp, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name"))
  
  # Select and arrange the columns
  metrics_df_usaid_pp <- metrics_df_usaid_pp %>%
    select(
      country,
      fiscal_year,
      quarter_num,
      prime_partner_name,
      HTS_TST,
      HTS_TST_POS,
      Positivity_Rate,
      TX_NEW,
      Linkage_Rate,
      TX_CURR,
      TX_PVLS_D,
      TX_PVLS_N,
      VL_Coverage_Rate,
      VL_Suppression_Rate
    ) %>%
    arrange(country, fiscal_year, quarter_num, prime_partner_name)
  
  # Store the metrics_df in the batch_metrics_dfs_usaid_pp list
  batch_metrics_dfs_usaid_pp[[paste0("metrics_95_", group_name, "_usaid_pp")]] <- metrics_df_usaid_pp
  
  
  print('##### Processing USAID Funding Agency by Prime Partner Targets #####')
  
  # Filter target data for USAID funding agency
  DATIM_long_usaid_targets_pp <- DATIM_long_usaid_targets %>%
    filter(!is.na(prime_partner_name))  # Ensure prime_partner_name is not NA
  
  # First 95: Compute HTS_TST and HTS_TST_POS target sums
  hts_data_usaid_pp_targets <- DATIM_long_usaid_targets_pp %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name, indicator) %>%
    summarize(
      total_target_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if(nrow(hts_data_usaid_pp_targets) > 0) {
    hts_data_usaid_pp_targets = hts_data_usaid_pp_targets %>% 
    pivot_wider(
      names_from = indicator,
      values_from = total_target_value
    ) %>%
    rename(
      HTS_TST_targets = HTS_TST,
      HTS_TST_POS_targets = HTS_TST_POS
    )
  }
  
  # Second 95: Compute TX_NEW target sums
  tx_new_data_usaid_pp_targets <- DATIM_long_usaid_targets_pp %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name) %>%
    summarize(
      TX_NEW_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N target sums
  tx_pvls_data_usaid_pp_targets <- DATIM_long_usaid_targets_pp %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name, numeratordenom) %>%
    summarize(
      total_target_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if(nrow(tx_pvls_data_usaid_pp_targets) > 0) {
    tx_pvls_data_usaid_pp_targets = tx_pvls_data_usaid_pp_targets %>% 
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_target_value,
      names_prefix = "TX_PVLS_target_"
    ) %>%
    rename(
      TX_PVLS_D_targets = TX_PVLS_target_D,
      TX_PVLS_N_targets = TX_PVLS_target_N
    )
  }
  
  
  # Get TX_CURR target data
  tx_curr_data_usaid_pp_targets <- DATIM_long_usaid_targets_pp %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, prime_partner_name) %>%
    summarize(
      TX_CURR_target = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge all target data
  if (nrow(hts_data_usaid_pp_targets) > 0 ) {
  metrics_df_usaid_pp_targets <- hts_data_usaid_pp_targets %>%
    full_join(tx_new_data_usaid_pp_targets, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name")) %>%
    full_join(tx_pvls_data_usaid_pp_targets, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name")) %>%
    full_join(tx_curr_data_usaid_pp_targets, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name"))
  
  # Add prime_partner_name and snu1 columns if necessary
  # (Assuming prime_partner_name is already present from grouping)
  
  # Store the target metrics_df in the batch_target_metrics_dfs_usaid_pp list
  batch_target_metrics_dfs_usaid_pp[[paste0("target_metrics_95_", group_name, "_usaid_pp")]] <- metrics_df_usaid_pp_targets
  
  } 
  
    # USAID Funding Agency - by SNU1 ----------------------------------------------------
  
  print('##### Processing USAID Funding Agency by snu1 #####') 
  
  
  # First 95: Compute HTS_TST and HTS_TST_POS sums and Positivity Rate by sub_national_unit
  hts_data_usaid_snu1 <- DATIM_long_usaid %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1, indicator) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = indicator,
      values_from = total_value
    ) %>%
    mutate(
      Positivity_Rate = HTS_TST_POS / HTS_TST
    )
  
  # Second 95: Compute TX_NEW and Linkage Rate
  tx_new_data_usaid_snu1 <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1) %>%
    summarize(
      TX_NEW = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge hts_data and tx_new_data
  first_second_95_df_usaid_snu1 <- hts_data_usaid_snu1 %>%
    left_join(tx_new_data_usaid_snu1, by = c("country", "fiscal_year", "quarter_num", "snu1")) %>%
    mutate(
      Linkage_Rate = TX_NEW / HTS_TST_POS
    )
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N, and VL Coverage Rate and VL Suppression Rate
  # Get TX_PVLS_D and TX_PVLS_N
  tx_pvls_data_usaid_snu1 <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1, numeratordenom) %>%
    summarize(
      total_value = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_value,
      names_prefix = "TX_PVLS_"
    )
  
  # Get TX_CURR data
  tx_curr_data_usaid_snu1 <- DATIM_long_usaid %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1) %>%
    summarize(
      TX_CURR = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Apply the function to tx_pvls_data to get previous fiscal_year and quarter_num
  vl_data_usaid_snu1 <- tx_pvls_data_usaid_snu1 %>%
    rowwise() %>%
    mutate(prev = list(get_prev_two_quarters(fiscal_year, quarter_num))) %>%
    unnest(prev) %>%
    ungroup()
  
  # Merge vl_data with tx_curr_data to get TX_CURR_prev
  vl_data_usaid_snu1 <- vl_data_usaid_snu1 %>%
    left_join(
      tx_curr_data_usaid_snu1 %>%
        rename(
          fiscal_year_prev = fiscal_year,
          quarter_num_prev = quarter_num,
          TX_CURR_prev = TX_CURR
        ),
      by = c("country", "snu1", "fiscal_year_prev", "quarter_num_prev")
    )
  
  # Compute VL Coverage Rate and VL Suppression Rate
  vl_data_usaid_snu1 <- vl_data_usaid_snu1 %>%
    mutate(
      VL_Coverage_Rate = TX_PVLS_D / TX_CURR_prev,
      VL_Suppression_Rate = TX_PVLS_N / TX_PVLS_D
    )
  
  # Merge first_second_95_df with vl_data
  metrics_df_usaid_snu1 <- first_second_95_df_usaid_snu1 %>%
    full_join(vl_data_usaid_snu1, by = c("country", "fiscal_year", "quarter_num", "snu1"))
  
  # Now, join TX_CURR for the current quarter
  metrics_df_usaid_snu1 <- metrics_df_usaid_snu1 %>%
    left_join(tx_curr_data_usaid_snu1, by = c("country", "fiscal_year", "quarter_num", "snu1"))
  
  # Select and arrange the columns
  metrics_df_usaid_snu1 <- metrics_df_usaid_snu1 %>%
    select(
      country,
      fiscal_year,
      quarter_num,
      snu1,
      HTS_TST,
      HTS_TST_POS,
      Positivity_Rate,
      TX_NEW,
      Linkage_Rate,
      TX_CURR,
      TX_PVLS_D,
      TX_PVLS_N,
      VL_Coverage_Rate,
      VL_Suppression_Rate
    ) %>%
    arrange(country, fiscal_year, quarter_num, snu1)
  
  # Store the metrics_df in the batch_metrics_dfs_usaid_snu1 list
  batch_metrics_dfs_usaid_snu1[[paste0("metrics_95_", group_name, "_usaid_snu1")]] <- metrics_df_usaid_snu1
  
  
      ##### Processing USAID Funding Agency by SNU1 Targets
  
  # Filter target data for USAID funding agency
  DATIM_long_usaid_targets_snu1 <- DATIM_long_usaid_targets %>%
    filter(!is.na(snu1))  # Ensure snu1 is not NA
  
  # First 95: Compute HTS_TST and HTS_TST_POS target sums
  hts_data_usaid_snu1_targets <- DATIM_long_usaid_targets_snu1 %>%
    filter(
      indicator %in% c("HTS_TST", "HTS_TST_POS"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1, indicator) %>%
    summarize(
      total_target_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  if(nrow(hts_data_usaid_snu1_targets) > 0) {
    hts_data_usaid_snu1_targets = hts_data_usaid_snu1_targets %>% 
    pivot_wider(
      names_from = indicator,
      values_from = total_target_value
    ) %>%
    rename(
      HTS_TST_targets = HTS_TST,
      HTS_TST_POS_targets = HTS_TST_POS
    )
  }
  
  # Second 95: Compute TX_NEW target sums
  tx_new_data_usaid_snu1_targets <- DATIM_long_usaid_targets_snu1 %>%
    filter(
      indicator == "TX_NEW",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1) %>%
    summarize(
      TX_NEW_targets = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Third 95: Compute TX_PVLS_D and TX_PVLS_N target sums
  tx_pvls_data_usaid_snu1_targets <- DATIM_long_usaid_targets_snu1 %>%
    filter(
      indicator == "TX_PVLS",
      numeratordenom %in% c("D", "N"),
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1, numeratordenom) %>%
    summarize(
      total_target_value = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    ) 
  if (nrow(tx_pvls_data_usaid_snu1_targets) > 0) {
    tx_pvls_data_usaid_snu1_targets = tx_pvls_data_usaid_snu1_targets %>% 
    pivot_wider(
      names_from = numeratordenom,
      values_from = total_target_value,
      names_prefix = "TX_PVLS_target_"
    ) %>%
    rename(
      TX_PVLS_D_targets = TX_PVLS_target_D,
      TX_PVLS_N_targets = TX_PVLS_target_N
    )
  }
  
  # Get TX_CURR target data
  tx_curr_data_usaid_snu1_targets <- DATIM_long_usaid_targets_snu1 %>%
    filter(
      indicator == "TX_CURR",
      ageasentered %in% target_ages
    ) %>%
    group_by(country, fiscal_year, quarter_num, snu1) %>%
    summarize(
      TX_CURR_target = sum(target_value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge all target data
  if (nrow(hts_data_usaid_snu1_targets) > 0) {
  metrics_df_usaid_snu1_targets <- hts_data_usaid_snu1_targets %>%
    full_join(tx_new_data_usaid_snu1_targets, by = c("country", "fiscal_year", "quarter_num", "snu1")) %>%
    full_join(tx_pvls_data_usaid_snu1_targets, by = c("country", "fiscal_year", "quarter_num", "snu1")) %>%
    full_join(tx_curr_data_usaid_snu1_targets, by = c("country", "fiscal_year", "quarter_num", "snu1"))
  
  # Add snu1 column if necessary
  # (Assuming snu1 is already present from grouping)
  
  # Store the target metrics_df in the batch_target_metrics_dfs_usaid_snu1 list
  batch_target_metrics_dfs_usaid_snu1[[paste0("target_metrics_95_", group_name, "_usaid_snu1")]] <- metrics_df_usaid_snu1_targets
  }
}
  
###################### APPEND and SAVE the dataframes ---------------------

append_batch_dfs()


# Combine and save each pair of metrics and target metrics dataframes
  

# Extract the country name from combined_95_metrics_targets_peds
country <- DATIM_combined$country[1]

# Define the output directory path
output_dir <- file.path("C:/Users/georg/Desktop/databases/output/Ninety95s_dfs", country)

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created directory: ", output_dir)
} else {
  message("Directory already exists: ", output_dir)
}

# Save metrics_hts_data_* objects
save(metrics_hts_data_usaid_under5, file = file.path(output_dir, "metrics_hts_data_usaid_under5.rda"))

save(metrics_hts_data_usaid_peds, file = file.path(output_dir, "metrics_hts_data_usaid_peds.rda"))

save(metrics_hts_data_usaid_adolescent, file = file.path(output_dir, "metrics_hts_data_usaid_adolescent.rda"))


# Combine and save combined_95_metrics_targets_peds
combined_95_metrics_targets_peds <<- cumulative_metrics_95_peds %>%
  left_join(cumulative_target_metrics_95_peds, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1"))%>% 
  distinct()
save(combined_95_metrics_targets_peds, file = file.path(output_dir, "combined_95_metrics_targets_peds.rda"))


# Combine and save combined_95_metrics_targets_peds_usaid
combined_95_metrics_targets_peds_usaid <<- cumulative_metrics_95_peds_usaid %>%
  left_join(cumulative_target_metrics_95_peds_usaid, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1")) %>% 
  distinct()
save(combined_95_metrics_targets_peds_usaid, file = file.path(output_dir, "combined_95_metrics_targets_peds_usaid.rda"))


# Combine and save combined_95_metrics_targets_peds_usaid_gender
combined_95_metrics_targets_peds_usaid_gender <<- cumulative_metrics_95_peds_usaid_gender %>%
  left_join(cumulative_target_metrics_95_peds_usaid_gender, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1", "sex")) %>% 
  distinct()
save(combined_95_metrics_targets_peds_usaid_gender, file = file.path(output_dir, "combined_95_metrics_targets_peds_usaid_gender.rda"))

# Conditional block for CDC data
if (nrow(DATIM_long_cdc) > 0) {
  if (nrow(hts_data_cdc) > 0) {
  combined_95_metrics_targets_peds_cdc <<- cumulative_metrics_95_peds_cdc %>%
    left_join(cumulative_target_metrics_95_peds_cdc, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1")) %>% 
    distinct()
  save(combined_95_metrics_targets_peds_cdc, file = file.path(output_dir, "combined_95_metrics_targets_peds_cdc.rda"))

  combined_95_metrics_targets_under5_cdc <<- cumulative_metrics_95_under5_cdc %>%
    left_join(cumulative_target_metrics_95_under5_cdc, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1"))
  save(combined_95_metrics_targets_under5_cdc, file = file.path(output_dir, "combined_95_metrics_targets_under5_cdc.rda"))

  combined_95_metrics_targets_adolescent_cdc <<- cumulative_metrics_95_adolescent_cdc %>%
    left_join(cumulative_target_metrics_95_adolescent_cdc, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1"))
  save(combined_95_metrics_targets_adolescent_cdc, file = file.path(output_dir, "combined_95_metrics_targets_adolescent_cdc.rda"))
}
}

# Continue saving remaining combined data frames
combined_95_metrics_targets_peds_usaid_pp <<- cumulative_metrics_95_peds_usaid_pp %>%
  left_join(cumulative_target_metrics_95_peds_usaid_pp, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name")) %>% 
  distinct()
save(combined_95_metrics_targets_peds_usaid_pp, file = file.path(output_dir, "combined_95_metrics_targets_peds_usaid_pp.rda"))


combined_95_metrics_targets_peds_usaid_snu1 <<- cumulative_metrics_95_peds_usaid_snu1 %>%
  left_join(cumulative_target_metrics_95_peds_usaid_snu1, by = c("country", "fiscal_year", "quarter_num", "snu1"))%>% 
  distinct()
save(combined_95_metrics_targets_peds_usaid_snu1, file = file.path(output_dir, "combined_95_metrics_targets_peds_usaid_snu1.rda"))


combined_95_metrics_targets_under5 <<- cumulative_metrics_95_under5 %>%
  left_join(cumulative_target_metrics_95_under5, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1"))  %>% 
  distinct()
save(combined_95_metrics_targets_under5, file = file.path(output_dir, "combined_95_metrics_targets_under5.rda"))


combined_95_metrics_targets_under5_usaid <<- cumulative_metrics_95_under5_usaid %>%
  left_join(cumulative_target_metrics_95_under5_usaid, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1"))
save(combined_95_metrics_targets_under5_usaid, file = file.path(output_dir, "combined_95_metrics_targets_under5_usaid.rda"))


combined_95_metrics_targets_under5_usaid_gender <<- cumulative_metrics_95_under5_usaid_gender %>%
  left_join(cumulative_target_metrics_95_under5_usaid_gender, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1", "sex"))  %>% 
  distinct()
save(combined_95_metrics_targets_under5_usaid_gender, file = file.path(output_dir, "combined_95_metrics_targets_under5_usaid_gender.rda"))


combined_95_metrics_targets_under5_usaid_pp <<- cumulative_metrics_95_under5_usaid_pp %>%
  left_join(cumulative_target_metrics_95_under5_usaid_pp, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name"))  %>% 
  distinct()
save(combined_95_metrics_targets_under5_usaid_pp, file = file.path(output_dir, "combined_95_metrics_targets_under5_usaid_pp.rda"))


combined_95_metrics_targets_under5_usaid_snu1 <<- cumulative_metrics_95_under5_usaid_snu1 %>%
  left_join(cumulative_target_metrics_95_under5_usaid_snu1, by = c("country", "fiscal_year", "quarter_num", "snu1"))  %>% 
  distinct()
save(combined_95_metrics_targets_under5_usaid_snu1, file = file.path(output_dir, "combined_95_metrics_targets_under5_usaid_snu1.rda"))


combined_95_metrics_targets_adolescent <<- cumulative_metrics_95_adolescent %>%
  left_join(cumulative_target_metrics_95_adolescent, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1"))  %>% 
  distinct()
save(combined_95_metrics_targets_adolescent, file = file.path(output_dir, "combined_95_metrics_targets_adolescent.rda"))


combined_95_metrics_targets_adolescent_usaid <<- cumulative_metrics_95_adolescent_usaid %>%
  left_join(cumulative_target_metrics_95_adolescent_usaid, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1"))  %>% 
  distinct()
save(combined_95_metrics_targets_adolescent_usaid, file = file.path(output_dir, "combined_95_metrics_targets_adolescent_usaid.rda"))


combined_95_metrics_targets_adolescent_usaid_gender <<- cumulative_metrics_95_adolescent_usaid_gender %>%
  left_join(cumulative_target_metrics_95_adolescent_usaid_gender, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name", "snu1", "sex"))  %>% 
  distinct()
save(combined_95_metrics_targets_adolescent_usaid_gender, file = file.path(output_dir, "combined_95_metrics_targets_adolescent_usaid_gender.rda"))


combined_95_metrics_targets_adolescent_usaid_pp <<- cumulative_metrics_95_adolescent_usaid_pp %>%
  left_join(cumulative_target_metrics_95_adolescent_usaid_pp, by = c("country", "fiscal_year", "quarter_num", "prime_partner_name"))  %>% 
  distinct()
save(combined_95_metrics_targets_adolescent_usaid_pp, file = file.path(output_dir, "combined_95_metrics_targets_adolescent_usaid_pp.rda"))


combined_95_metrics_targets_adolescent_usaid_snu1 <<- cumulative_metrics_95_adolescent_usaid_snu1 %>%
  left_join(cumulative_target_metrics_95_adolescent_usaid_snu1, by = c("country", "fiscal_year", "quarter_num", "snu1"))  %>% 
  distinct()
save(combined_95_metrics_targets_adolescent_usaid_snu1, file = file.path(output_dir, "combined_95_metrics_targets_adolescent_usaid_snu1.rda"))






  