

###################### INTRODUCTION ---------------------------------------------------

# Steps for creating the automated PVT country-specific report:

#1. Download the SITE xIM country file combined and save as .rda files

#2. Run PVT dataframe for each country.R file to create the summary dataframes    ***** this code represents this step - STEP 1 *****
#2b. Run the same code to combine the multiple individual country dataframes into one combined dataframes    ***** this code represents this step - STEP 2 *****

#4. Run the RMarkdown file to generate the report  
    #a. Note, there is a separate R file that runs all the reports at once. 

# This R code creates individual summary dataframes 

library(tidyverse)
library(ggplot2)
library(tidyr)

rm(list = ls())

list.files('C:/Users/georg/Desktop/databases/input/DATIM_Genie/2024_Q4/')

#################### #################### #################### #################### #################### 
### STEP 1 ----------------------------------------------------------------------------------------------------
#################### #################### #################### #################### #################### 


# Specify the directory where your .rda files live
input_dir <- "C:/Users/georg/Desktop/databases/input/DATIM_Genie/2024_Q4/"

# Get the full paths of all .rda files in that directory
rda_files <- list.files(input_dir, pattern = "*.rda$", full.names = TRUE)


# Loop over each .rda file ---------------------------------------------------------------------------------------

for (file_path in rda_files) {
  
  # We'll capture that and assume there is exactly ONE object inside the file.
  obj_name <- load(file_path) 
  
  # Now, 'obj_name' should be something like "Bur_2024" or "Esw_2024".
  # We rename that object to SITExIM_data.
  SITExIM_data <- get(obj_name)
  
  # Remove the original loaded object from the environment
  rm(list = obj_name)

  # this line of code gives the country, there is only 1 country in each SITE_IM_df
  table(SITExIM_data$country)
  
  current_country = SITExIM_data$country[1]
  
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
  
  
  SITExIM_data = create_quarterly_targets(SITExIM_data)
  
  
  ### creating DATIM_long -----------------------------------------------------------------------------------------------------
  # might be able to make this list shorter, especially when doing this with SITExIM files
  
  SITExIM_data_short <- SITExIM_data %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_PVLS", "PrEP_CT" , "PrEP_CURR", "PrEP_NEW") | str_starts(indicator, "PMTCT"))
  
  
  rm(SITExIM_data)
  
  
  # Create the new variable 'indicator_ND'
  SITExIM_data_short <- SITExIM_data_short %>%
    mutate(indicator_ND = if_else(!is.na(numeratordenom), 
                                  paste(indicator, numeratordenom, sep = "_"), 
                                  indicator))
  
  
  
  
  # Reshape the data to long format
  DATIM_long <- SITExIM_data_short %>%
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
  
  rm(SITExIM_data_short)
  
  #DATIM_long_targets <- SITExIM_data_short %>%
  #  filter(is.na(targets) == FALSE) %>% 
  #  pivot_longer(
  #    cols = starts_with("tgt"),
  #    names_to = "quarter",
  #    names_prefix = "tgt",
  #    values_to = "target_value",
  #    values_drop_na = FALSE
  #  ) %>%
  #  mutate(
  #    quarter_num = as.numeric(quarter)
  # )
  
  
  
  
  
  
  
  ### creating individual dataframes  -----------------------------------------------------------------------------------------------------
  
  table(DATIM_long$indicator_ND)
  
  ## PMTCT_STAT ----------------------
  
  PMTCT_STAT_df = DATIM_long %>% 
    filter(str_starts(indicator, "PMTCT_STAT")) %>% 
    filter(ageasentered != "")
  
  
  PMTCT_STAT_summary_df <- PMTCT_STAT_df %>%
    group_by(country, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_STAT_D = sum(value[indicator_ND == "PMTCT_STAT_D"], na.rm = TRUE),
      PMTCT_STAT_N = sum(value[indicator_ND == "PMTCT_STAT_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N = sum(value[indicator_ND == "PMTCT_STAT_POS_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_known_at_entry = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Known at Entry"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_newly_diagnosed = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Newly Identified"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  PMTCT_STAT_summary_df_pp <- PMTCT_STAT_df %>%
    group_by(prime_partner_name, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_STAT_D = sum(value[indicator_ND == "PMTCT_STAT_D"], na.rm = TRUE),
      PMTCT_STAT_N = sum(value[indicator_ND == "PMTCT_STAT_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N = sum(value[indicator_ND == "PMTCT_STAT_POS_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_known_at_entry = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Known at Entry"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_newly_diagnosed = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Newly Identified"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  PMTCT_STAT_summary_df_snu1 <- PMTCT_STAT_df %>%
    group_by(snu1, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_STAT_D = sum(value[indicator_ND == "PMTCT_STAT_D"], na.rm = TRUE),
      PMTCT_STAT_N = sum(value[indicator_ND == "PMTCT_STAT_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N = sum(value[indicator_ND == "PMTCT_STAT_POS_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_known_at_entry = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Known at Entry"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_newly_diagnosed = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Newly Identified"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  
  PMTCT_STAT_summary_age_df <- PMTCT_STAT_df %>%
    group_by(country, funding_agency, fiscal_year, quarter, ageasentered) %>%
    summarise(
      PMTCT_STAT_D = sum(value[indicator_ND == "PMTCT_STAT_D"], na.rm = TRUE),
      PMTCT_STAT_N = sum(value[indicator_ND == "PMTCT_STAT_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N = sum(value[indicator_ND == "PMTCT_STAT_POS_N"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_known_at_entry = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Known at Entry"], na.rm = TRUE),
      PMTCT_STAT_POS_N_HIV_newly_diagnosed = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Newly Identified"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  
  ## PMTCT_ART ----------------------
  
  PMTCT_ART_df = DATIM_long %>% 
    filter(str_starts(indicator, "PMTCT_ART")) %>% 
    filter(ageasentered != "")                                                      # if something goes wrong, check here 
  
  
  PMTCT_ART_summary_df <- PMTCT_ART_df %>%
    group_by(country, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_ART_D = sum(value[indicator_ND == "PMTCT_ART_D"], na.rm = TRUE),
      PMTCT_ART_N = sum(value[indicator_ND == "PMTCT_ART_N"], na.rm = TRUE),
      PMTCT_ART_N_ART_NEW = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, New"], na.rm = TRUE),
      PMTCT_ART_N_ART_Already = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, Already"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  PMTCT_ART_summary_df_pp <- PMTCT_ART_df %>%
    group_by(prime_partner_name, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_ART_D = sum(value[indicator_ND == "PMTCT_ART_D"], na.rm = TRUE),
      PMTCT_ART_N = sum(value[indicator_ND == "PMTCT_ART_N"], na.rm = TRUE),
      PMTCT_ART_N_ART_NEW = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, New"], na.rm = TRUE),
      PMTCT_ART_N_ART_Already = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, Already"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  PMTCT_ART_summary_df_snu1 <- PMTCT_ART_df %>%
    group_by(snu1, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_ART_D = sum(value[indicator_ND == "PMTCT_ART_D"], na.rm = TRUE),
      PMTCT_ART_N = sum(value[indicator_ND == "PMTCT_ART_N"], na.rm = TRUE),
      PMTCT_ART_N_ART_NEW = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, New"], na.rm = TRUE),
      PMTCT_ART_N_ART_Already = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, Already"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  
  
  
  ## PMTCT_EID ----------------------
  
  # note, for PMTCT_EID, in theory, PMTCT_EID_N should equal PMTCT_EID_Zero_to_Twelve_Mo, 
  #       however, in some cases, it seems not to.  this could be a data quality issue. 
  #       also, please note the difference between PMTCT_EID_N_first and PMTCT_EID_N_all 
  
  
  PMTCT_EID_df <- DATIM_long %>%
    filter(str_starts(indicator_ND, "PMTCT_EID")) %>%  # Keep rows where indicator_ND starts with "PMTCT_EID"
    filter(!(indicator_ND == "PMTCT_EID_N" & ageasentered == ""))  # Exclude rows with specific conditions
  
  
  
  PMTCT_EID_summary_df <- PMTCT_EID_df %>%
    group_by(country, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_EID_D = sum(value[indicator_ND == "PMTCT_EID_D"], na.rm = TRUE),
      PMTCT_EID_N_first = sum(
        value[
          (indicator_ND == "PMTCT_EID_N") &
            ((fiscal_year == 2024 & otherdisaggregate == "EID First Test") | fiscal_year != 2024)
        ],
        na.rm = TRUE
      ),
      PMTCT_EID_N_all = sum(
        value[
          (indicator_ND == "PMTCT_EID_N") &
            ((fiscal_year == 2024 & otherdisaggregate %in% c("EID First Test", "EID Second Test or more")) | fiscal_year != 2024)
        ],
        na.rm = TRUE
      ),
      PMTCT_EID_Less_Equal_Two_Months_N = sum(value[indicator_ND == "PMTCT_EID_Less_Equal_Two_Months_N"], na.rm = TRUE),
      PMTCT_EID_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_EID_Two_Twelve_Months_N"], na.rm = TRUE)
    ) %>%
    mutate(
      PMTCT_EID_Zero_to_Twelve_Mo = PMTCT_EID_Less_Equal_Two_Months_N + PMTCT_EID_Two_Twelve_Months_N
    ) %>%
    ungroup()
  
  
  PMTCT_EID_summary_df_pp <- PMTCT_EID_df %>%
    group_by(prime_partner_name, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_EID_D = sum(value[indicator_ND == "PMTCT_EID_D"], na.rm = TRUE),
      PMTCT_EID_N_first = sum(
        value[
          (indicator_ND == "PMTCT_EID_N") &
            ((fiscal_year == 2024 & otherdisaggregate == "EID First Test") | fiscal_year != 2024)
        ],
        na.rm = TRUE
      ),
      PMTCT_EID_N_all = sum(
        value[
          (indicator_ND == "PMTCT_EID_N") &
            ((fiscal_year == 2024 & otherdisaggregate %in% c("EID First Test", "EID Second Test or more")) | fiscal_year != 2024)
        ],
        na.rm = TRUE
      ),
      PMTCT_EID_Less_Equal_Two_Months_N = sum(value[indicator_ND == "PMTCT_EID_Less_Equal_Two_Months_N"], na.rm = TRUE),
      PMTCT_EID_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_EID_Two_Twelve_Months_N"], na.rm = TRUE)
    ) %>%
    mutate(
      PMTCT_EID_Zero_to_Twelve_Mo = PMTCT_EID_Less_Equal_Two_Months_N + PMTCT_EID_Two_Twelve_Months_N
    ) %>%
    ungroup()
  
  
  
  PMTCT_EID_summary_df_snu1 <- PMTCT_EID_df %>%
    group_by(snu1, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_EID_D = sum(value[indicator_ND == "PMTCT_EID_D"], na.rm = TRUE),
      PMTCT_EID_N_first = sum(
        value[
          (indicator_ND == "PMTCT_EID_N") &
            ((fiscal_year == 2024 & otherdisaggregate == "EID First Test") | fiscal_year != 2024)
        ],
        na.rm = TRUE
      ),
      PMTCT_EID_N_all = sum(
        value[
          (indicator_ND == "PMTCT_EID_N") &
            ((fiscal_year == 2024 & otherdisaggregate %in% c("EID First Test", "EID Second Test or more")) | fiscal_year != 2024)
        ],
        na.rm = TRUE
      ),
      PMTCT_EID_Less_Equal_Two_Months_N = sum(value[indicator_ND == "PMTCT_EID_Less_Equal_Two_Months_N"], na.rm = TRUE),
      PMTCT_EID_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_EID_Two_Twelve_Months_N"], na.rm = TRUE)
    ) %>%
    mutate(
      PMTCT_EID_Zero_to_Twelve_Mo = PMTCT_EID_Less_Equal_Two_Months_N + PMTCT_EID_Two_Twelve_Months_N
    ) %>%
    ungroup()
  
  
  
  
  ## PMTCT_HEI ----------------------
  
  ### note - this one gives me some trouble, there have been a lot of switches in how MER data is collected/disaggregated, etc, so proceed with caution and double check with tableau/pano!  
  
  
  PMTCT_HEI_df <- DATIM_long %>%
    filter(str_starts(indicator_ND, "PMTCT_HEI")) %>%  # Keep rows where indicator_ND starts with "PMTCT_HEI"
    filter(!(indicator_ND %in% c("PMTCT_HEI_N", "PMTCT_HEI_POS_ART_N", "PMTCT_HEI_POS_N") & ageasentered == ""))
  
  
  
  PMTCT_HEI_summary_df <- PMTCT_HEI_df %>%
    group_by(country, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_HEI_N = sum(value[indicator_ND == "PMTCT_HEI_N"], na.rm = TRUE),
      PMTCT_HEI_N_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_N" & ageasentered == "<=02 Months"], na.rm = TRUE), 
      PMTCT_HEI_NEG_N = sum(value[indicator_ND == "PMTCT_HEI_NEG_N"], na.rm = TRUE),
      PMTCT_HEI_POS_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N"], na.rm = TRUE),
      PMTCT_HEI_POS_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_POS_2MO_N"], na.rm = TRUE),
      PMTCT_HEI_POS_ART_N = sum(value[indicator_ND == "PMTCT_HEI_POS_ART_N"], na.rm = TRUE),
      PMTCT_HEI_POS_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N" & ageasentered == "02 - 12 Months"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  PMTCT_HEI_summary_df_pp <- PMTCT_HEI_df %>%
    group_by(prime_partner_name, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_HEI_N = sum(value[indicator_ND == "PMTCT_HEI_N"], na.rm = TRUE),
      PMTCT_HEI_N_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_N" & ageasentered == "<=02 Months"], na.rm = TRUE), 
      PMTCT_HEI_NEG_N = sum(value[indicator_ND == "PMTCT_HEI_NEG_N"], na.rm = TRUE),
      PMTCT_HEI_POS_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N"], na.rm = TRUE),
      PMTCT_HEI_POS_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_POS_2MO_N"], na.rm = TRUE),
      PMTCT_HEI_POS_ART_N = sum(value[indicator_ND == "PMTCT_HEI_POS_ART_N"], na.rm = TRUE),
      PMTCT_HEI_POS_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N" & ageasentered == "02 - 12 Months"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  PMTCT_HEI_summary_df_snu1 <- PMTCT_HEI_df %>%
    group_by(snu1, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PMTCT_HEI_N = sum(value[indicator_ND == "PMTCT_HEI_N"], na.rm = TRUE),
      PMTCT_HEI_N_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_N" & ageasentered == "<=02 Months"], na.rm = TRUE), 
      PMTCT_HEI_NEG_N = sum(value[indicator_ND == "PMTCT_HEI_NEG_N"], na.rm = TRUE),
      PMTCT_HEI_POS_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N"], na.rm = TRUE),
      PMTCT_HEI_POS_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_POS_2MO_N"], na.rm = TRUE),
      PMTCT_HEI_POS_ART_N = sum(value[indicator_ND == "PMTCT_HEI_POS_ART_N"], na.rm = TRUE),
      PMTCT_HEI_POS_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N" & ageasentered == "02 - 12 Months"], na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  
  ## HTS_TST ----------------------
  #    note, looks like in some years/countries (perhaps later year/countries), you have to add up HTS_TST_N_PMTCT_breastfeeding + HTS_TST_N_POSTANC1_PregLD to get the correct numbers?
  
  HTS_TST_df <- DATIM_long %>%
    filter(str_starts(indicator_ND, "HTS_TST")) %>% 
    filter(modality %in% c("PMTCT ANC", "PMTCT Post ANC1 Breastfeeding", 
                           "PMTCT Post ANC1 Pregnant/L&D", "Post ANC1"))
  
  
  
  HTS_TST_summary_df <- HTS_TST_df %>%
    group_by(country, funding_agency, fiscal_year, quarter) %>%
    summarise(
      HTS_TST_N = sum(value[indicator_ND == "HTS_TST_N"], na.rm = TRUE),
      HTS_TST_POS_N = sum(value[indicator_ND == "HTS_TST_POS_N"], na.rm = TRUE),
      HTS_TST_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
      HTS_TST_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
      HTS_TST_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
      HTS_TST_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_N" & modality == "Post ANC1" ], na.rm = TRUE),
      HTS_TST_POS_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "Post ANC1" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  
  HTS_TST_summary_df_pp <- HTS_TST_df %>%
    group_by(prime_partner_name, funding_agency, fiscal_year, quarter) %>%
    summarise(
      HTS_TST_N = sum(value[indicator_ND == "HTS_TST_N"], na.rm = TRUE),
      HTS_TST_POS_N = sum(value[indicator_ND == "HTS_TST_POS_N"], na.rm = TRUE),
      HTS_TST_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
      HTS_TST_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
      HTS_TST_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
      HTS_TST_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_N" & modality == "Post ANC1" ], na.rm = TRUE),
      HTS_TST_POS_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "Post ANC1" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  
  
  HTS_TST_summary_df_snu1 <- HTS_TST_df %>%
    group_by(snu1, funding_agency, fiscal_year, quarter) %>%
    summarise(
      HTS_TST_N = sum(value[indicator_ND == "HTS_TST_N"], na.rm = TRUE),
      HTS_TST_POS_N = sum(value[indicator_ND == "HTS_TST_POS_N"], na.rm = TRUE),
      HTS_TST_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
      HTS_TST_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
      HTS_TST_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
      HTS_TST_POS_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
      HTS_TST_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_N" & modality == "Post ANC1" ], na.rm = TRUE),
      HTS_TST_POS_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "Post ANC1" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  
  ## TX_PVLS ----------------------
  
  
  TX_PVLS_df <- DATIM_long %>%
    filter(str_starts(indicator_ND, "TX_PVLS")) %>% 
    filter(standardizeddisaggregate == "PregnantOrBreastfeeding/Indication/HIVStatus" | standardizeddisaggregate == "Sex/PregnantOrBreastfeeding/HIVStatus")
  
  
  TX_PVLS_summary_df <- TX_PVLS_df %>%
    group_by(country, funding_agency, fiscal_year, quarter) %>%
    summarise(
      TX_PVLS_D = sum(value[indicator_ND == "TX_PVLS_D"], na.rm = TRUE),
      TX_PVLS_N = sum(value[indicator_ND == "TX_PVLS_N"], na.rm = TRUE),
      TX_PVLS_D_pregnant = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
      TX_PVLS_N_pregnant = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
      TX_PVLS_D_breastfeeding = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
      TX_PVLS_N_breastfeeding = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  TX_PVLS_summary_df_pp <- TX_PVLS_df %>%
    group_by(prime_partner_name, funding_agency, fiscal_year, quarter) %>%
    summarise(
      TX_PVLS_D = sum(value[indicator_ND == "TX_PVLS_D"], na.rm = TRUE),
      TX_PVLS_N = sum(value[indicator_ND == "TX_PVLS_N"], na.rm = TRUE),
      TX_PVLS_D_pregnant = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
      TX_PVLS_N_pregnant = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
      TX_PVLS_D_breastfeeding = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
      TX_PVLS_N_breastfeeding = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  TX_PVLS_summary_df_snu1 <- TX_PVLS_df %>%
    group_by(snu1, funding_agency, fiscal_year, quarter) %>%
    summarise(
      TX_PVLS_D = sum(value[indicator_ND == "TX_PVLS_D"], na.rm = TRUE),
      TX_PVLS_N = sum(value[indicator_ND == "TX_PVLS_N"], na.rm = TRUE),
      TX_PVLS_D_pregnant = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
      TX_PVLS_N_pregnant = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
      TX_PVLS_D_breastfeeding = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
      TX_PVLS_N_breastfeeding = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  
  
  
  ## PrEP ----------------------
  #    note, the pregnant/breastfeeding disaggregate is optional 
  
  PrEP_df <- DATIM_long %>%
    filter(str_starts(indicator_ND, "PrEP")) %>% 
    filter(standardizeddisaggregate == "Sex/PregnantBreastfeeding") 
  
  
  
  
  
  PrEP_summary_df <- PrEP_df %>%
    group_by(country, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PrEP_CT_N = sum(value[indicator_ND == "PrEP_CT_N"], na.rm = TRUE),
      PrEP_NEW_N = sum(value[indicator_ND == "PrEP_NEW_N"], na.rm = TRUE),
      PrEP_CT_N_pregnant = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
      PrEP_NEW_N_pregnant = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
      PrEP_CT_N_breastfeeding = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
      PrEP_NEW_N_breastfeeding = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  
  PrEP_summary_df_pp <- PrEP_df %>%
    group_by(prime_partner_name, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PrEP_CT_N = sum(value[indicator_ND == "PrEP_CT_N"], na.rm = TRUE),
      PrEP_NEW_N = sum(value[indicator_ND == "PrEP_NEW_N"], na.rm = TRUE),
      PrEP_CT_N_pregnant = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
      PrEP_NEW_N_pregnant = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
      PrEP_CT_N_breastfeeding = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
      PrEP_NEW_N_breastfeeding = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  
  PrEP_summary_df_snu1 <- PrEP_df %>%
    group_by(snu1, funding_agency, fiscal_year, quarter) %>%
    summarise(
      PrEP_CT_N = sum(value[indicator_ND == "PrEP_CT_N"], na.rm = TRUE),
      PrEP_NEW_N = sum(value[indicator_ND == "PrEP_NEW_N"], na.rm = TRUE),
      PrEP_CT_N_pregnant = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
      PrEP_NEW_N_pregnant = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
      PrEP_CT_N_breastfeeding = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
      PrEP_NEW_N_breastfeeding = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
    ) %>%
    ungroup()
  
  
  
  
  
  ### bind_cols by $country, funding_agency, fiscal_year, quarter to create the final PVT OU dataframe -----------------------------------------------------------------------------------------------------
  
  PVT_SITExIM_df <- full_join(
    PMTCT_STAT_summary_df, 
    PMTCT_ART_summary_df, 
    by = c("country", "funding_agency", "fiscal_year", "quarter")
  ) %>% 
    full_join(
      PMTCT_EID_summary_df, 
      by = c("country", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      PMTCT_HEI_summary_df, 
      by = c("country", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      HTS_TST_summary_df, 
      by = c("country", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      TX_PVLS_summary_df, 
      by = c("country", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      PrEP_summary_df, 
      by = c("country", "funding_agency", "fiscal_year", "quarter")
    )
  
  
  PVT_SITExIM_df_pp <- full_join(
    PMTCT_STAT_summary_df_pp, 
    PMTCT_ART_summary_df_pp, 
    by = c("prime_partner_name", "funding_agency", "fiscal_year", "quarter")
  ) %>% 
    full_join(
      PMTCT_EID_summary_df_pp, 
      by = c("prime_partner_name", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      PMTCT_HEI_summary_df_pp, 
      by = c("prime_partner_name", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      HTS_TST_summary_df_pp, 
      by = c("prime_partner_name", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      TX_PVLS_summary_df_pp, 
      by = c("prime_partner_name", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      PrEP_summary_df_pp, 
      by = c("prime_partner_name", "funding_agency", "fiscal_year", "quarter")
    )
  
  
  PVT_SITExIM_df_snu1 <- full_join(
    PMTCT_STAT_summary_df_snu1, 
    PMTCT_ART_summary_df_snu1, 
    by = c("snu1", "funding_agency", "fiscal_year", "quarter")
  ) %>% 
    full_join(
      PMTCT_EID_summary_df_snu1, 
      by = c("snu1", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      PMTCT_HEI_summary_df_snu1, 
      by = c("snu1", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      HTS_TST_summary_df_snu1, 
      by = c("snu1", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      TX_PVLS_summary_df_snu1, 
      by = c("snu1", "funding_agency", "fiscal_year", "quarter")
    ) %>% 
    full_join(
      PrEP_summary_df_snu1, 
      by = c("snu1", "funding_agency", "fiscal_year", "quarter")
    )
  
  
  
  # lets make some new new variables
  
  PVT_SITExIM_df <- PVT_SITExIM_df %>%
    mutate(
      PMTCT_STAT_testing_coverage = round((PMTCT_STAT_N / PMTCT_STAT_D) * 100, 1),
      positivity_overall_ANC = round((PMTCT_STAT_POS_N / PMTCT_STAT_N) * 100, 1),
      ANC1_missed_number = PMTCT_STAT_D - PMTCT_STAT_N, 
      positivity_new = round((PMTCT_STAT_POS_N_HIV_newly_diagnosed / PMTCT_STAT_N) * 100, 1),
      positivity_postANC1_breastfeeding = round((HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding / HTS_TST_N_PMTCT_POSTANC1_breastfeeding) * 100, 1),
      positivity_postANC1_preg = round((HTS_TST_POS_N_PMTCT_POSTANC1_PregLD / HTS_TST_N_PMTCT_POSTANC1_PregLD) * 100, 1),
      HIV_not_on_ART = PMTCT_STAT_POS_N - PMTCT_ART_N, 
      HEI_pos_linkage_ART_proxy = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1),
      PMTCT_ART_linkage_new_proxy = round((PMTCT_ART_N_ART_NEW / PMTCT_STAT_POS_N_HIV_newly_diagnosed) * 100, 1),
      PMTCT_ART_coverage_total = round(PMTCT_ART_N/PMTCT_ART_D * 100, 1), 
      EID_coverage_2mo = round((PMTCT_EID_Less_Equal_Two_Months_N / PMTCT_EID_D) * 100, 1),
      EID_coverage = round((PMTCT_EID_N_first / PMTCT_EID_D) * 100, 1),
      HEI_pos_2mo = round((PMTCT_HEI_POS_2MO_N / (PMTCT_HEI_N_2MO_N)) * 100, 1),
      HEI_pos_12mo = round((PMTCT_HEI_POS_N / PMTCT_HEI_N) * 100, 1),
      Missed_reporting_proxy = round((1 - (PMTCT_HEI_N / PMTCT_EID_N_all)) * 100, 1),
      viral_suppression = round((TX_PVLS_N / TX_PVLS_D) * 100, 1),
      viral_suppression_pregnant = round((TX_PVLS_N_pregnant / TX_PVLS_D_pregnant) * 100, 1),
      viral_suppression_breastfeeding = round((TX_PVLS_N_breastfeeding / TX_PVLS_D_breastfeeding) * 100, 1), 
      HEI_POS_on_ART = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1)
    )
  
  
  PVT_SITExIM_df_pp <- PVT_SITExIM_df_pp %>%
    mutate(
      PMTCT_STAT_testing_coverage = round((PMTCT_STAT_N / PMTCT_STAT_D) * 100, 1),
      positivity_overall_ANC = round((PMTCT_STAT_POS_N / PMTCT_STAT_N) * 100, 1),
      ANC1_missed_number = PMTCT_STAT_D - PMTCT_STAT_N, 
      positivity_new = round((PMTCT_STAT_POS_N_HIV_newly_diagnosed / PMTCT_STAT_N) * 100, 1),
      positivity_postANC1_breastfeeding = round((HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding / HTS_TST_N_PMTCT_POSTANC1_breastfeeding) * 100, 1),
      positivity_postANC1_preg = round((HTS_TST_POS_N_PMTCT_POSTANC1_PregLD / HTS_TST_N_PMTCT_POSTANC1_PregLD) * 100, 1),
      HIV_not_on_ART = PMTCT_STAT_POS_N - PMTCT_ART_N, 
      HEI_pos_linkage_ART_proxy = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1),
      PMTCT_ART_linkage_new_proxy = round((PMTCT_ART_N_ART_NEW / PMTCT_STAT_POS_N_HIV_newly_diagnosed) * 100, 1),
      PMTCT_ART_coverage_total = round(PMTCT_ART_N/PMTCT_ART_D * 100, 1), 
      EID_coverage_2mo = round((PMTCT_EID_Less_Equal_Two_Months_N / PMTCT_EID_D) * 100, 1),
      EID_coverage = round((PMTCT_EID_N_first / PMTCT_EID_D) * 100, 1),
      HEI_pos_2mo = round((PMTCT_HEI_POS_2MO_N / (PMTCT_HEI_N_2MO_N)) * 100, 1),
      HEI_pos_12mo = round((PMTCT_HEI_POS_N / PMTCT_HEI_N) * 100, 1),
      Missed_reporting_proxy = round((1 - (PMTCT_HEI_N / PMTCT_EID_N_all)) * 100, 1),
      viral_suppression = round((TX_PVLS_N / TX_PVLS_D) * 100, 1),
      viral_suppression_pregnant = round((TX_PVLS_N_pregnant / TX_PVLS_D_pregnant) * 100, 1),
      viral_suppression_breastfeeding = round((TX_PVLS_N_breastfeeding / TX_PVLS_D_breastfeeding) * 100, 1), 
      HEI_POS_on_ART = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1), 
      country = current_country
    )
  
  
  PVT_SITExIM_df_snu1 <- PVT_SITExIM_df_snu1 %>%
    mutate(
      PMTCT_STAT_testing_coverage = round((PMTCT_STAT_N / PMTCT_STAT_D) * 100, 1),
      positivity_overall_ANC = round((PMTCT_STAT_POS_N / PMTCT_STAT_N) * 100, 1),
      ANC1_missed_number = PMTCT_STAT_D - PMTCT_STAT_N, 
      positivity_new = round((PMTCT_STAT_POS_N_HIV_newly_diagnosed / PMTCT_STAT_N) * 100, 1),
      positivity_postANC1_breastfeeding = round((HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding / HTS_TST_N_PMTCT_POSTANC1_breastfeeding) * 100, 1),
      positivity_postANC1_preg = round((HTS_TST_POS_N_PMTCT_POSTANC1_PregLD / HTS_TST_N_PMTCT_POSTANC1_PregLD) * 100, 1),
      HIV_not_on_ART = PMTCT_STAT_POS_N - PMTCT_ART_N, 
      HEI_pos_linkage_ART_proxy = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1),
      PMTCT_ART_linkage_new_proxy = round((PMTCT_ART_N_ART_NEW / PMTCT_STAT_POS_N_HIV_newly_diagnosed) * 100, 1),
      PMTCT_ART_coverage_total = round(PMTCT_ART_N/PMTCT_ART_D * 100, 1), 
      EID_coverage_2mo = round((PMTCT_EID_Less_Equal_Two_Months_N / PMTCT_EID_D) * 100, 1),
      EID_coverage = round((PMTCT_EID_N_first / PMTCT_EID_D) * 100, 1),
      HEI_pos_2mo = round((PMTCT_HEI_POS_2MO_N / (PMTCT_HEI_N_2MO_N)) * 100, 1),
      HEI_pos_12mo = round((PMTCT_HEI_POS_N / PMTCT_HEI_N) * 100, 1),
      Missed_reporting_proxy = round((1 - (PMTCT_HEI_N / PMTCT_EID_N_all)) * 100, 1),
      viral_suppression = round((TX_PVLS_N / TX_PVLS_D) * 100, 1),
      viral_suppression_pregnant = round((TX_PVLS_N_pregnant / TX_PVLS_D_pregnant) * 100, 1),
      viral_suppression_breastfeeding = round((TX_PVLS_N_breastfeeding / TX_PVLS_D_breastfeeding) * 100, 1), 
      HEI_POS_on_ART = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1), 
      country = current_country
    )
  
  # Arrange the data to ensure it is in a logical order
  PVT_SITExIM_df <- PVT_SITExIM_df %>%
    arrange(country, funding_agency, fiscal_year, quarter)
  
  # Arrange the data to ensure it is in a logical order
  PVT_SITExIM_df_pp <- PVT_SITExIM_df_pp %>%
    arrange(prime_partner_name, funding_agency, fiscal_year, quarter)
  
  # Arrange the data to ensure it is in a logical order
  PVT_SITExIM_df_snu1 <- PVT_SITExIM_df_snu1 %>%
    arrange(snu1, funding_agency, fiscal_year, quarter)
  
  # note, here, I will need to dynamically save, such that the SITExIM_df dataframes (all three) are saved to the correct folders, with their country names
  
  # first, create a folder here with the correct country name 'C:/Users/georg/Desktop/databases/output/PVT/SITExIM_df/<insert country name for folder name>/' 
  
  base_name    <- basename(file_path)  
  country_name <- sub("Genie_SITE_IM_([^_]+)_.*", "\\1", base_name)
  # This should capture whatever appears after "Genie_SITE_IM_" and before the next underscore.
  
  # Now create the folder dynamically in:
  # "C:/Users/georg/Desktop/databases/output/PVT/SITExIM_df/<country>"
  output_dir <- file.path("C:/Users/georg/Desktop/databases/output/PVT/SITExIM_df", country_name)
  
  # If the directory doesn't exist yet, create it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  save_filename <- file.path(output_dir, "PVT_SITExIM_df.rda")
  save(PVT_SITExIM_df, file = save_filename)
  
  save_filename_pp <- file.path(output_dir, "PVT_SITExIM_df_pp.rda")
  save(PVT_SITExIM_df_pp, file = save_filename_pp)
  
  save_filename_snu1 <- file.path(output_dir, "PVT_SITExIM_df_snu1.rda")
  save(PVT_SITExIM_df_snu1, file = save_filename_snu1)
  
  # Optionally, print a message so you know it worked:
  message("Saved data for ", country_name, " to ", save_filename)

}


rm(list = ls())


#################### #################### #################### #################### #################### 
### STEP 2 ----------------------------------------------------------------------------------------------------
#################### #################### #################### #################### #################### 

# this step creates combined_dataframes from the individual country dataframes
# this create 3 master dataframes.  note  how each country has 3 dataframes, named PVT_SITExIM_df.rda, PVT_SITExIM_df_pp.rda, PVT_SITExIM_df_snu1.rda
# so, the three combined dataframes are:
#1. combined_PVT_SITExIM.rda, (all the PVT_SITExIM.rda from each country combined into 1 df); 
#2. combined_PVT_SITExIM_pp.rda, (all the PVT_SITExIM_pp.rda from each country combined into 1 df) ;
# 3 combined_PVT_SITExIM_snu1.rda, (all the PVT_SITExIM_snu1.rda from each country combined into 1 df) .



parent_dir  <- "C:/Users/georg/Desktop/databases/output/PVT/SITExIM_df/"
country_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)
combined_dir <- "C:/Users/georg/Desktop/databases/output/PVT/"



combined_PVT_SITExIM       <- data.frame()
combined_PVT_SITExIM_pp    <- data.frame()
combined_PVT_SITExIM_snu1  <- data.frame()


# LOOP THROUGH COUNTRY FOLDERS

for (c_dir in country_dirs) {
  
  # We'll construct the paths to each of the three .rda files
  file_pvt   <- file.path(c_dir, "PVT_SITExIM_df.rda")
  file_pp    <- file.path(c_dir, "PVT_SITExIM_df_pp.rda")
  file_snu1  <- file.path(c_dir, "PVT_SITExIM_df_snu1.rda")
  
  # Make sure each file exists in the country folder before trying to load it
  if (file.exists(file_pvt)) {
    load(file_pvt)                # loads PVT_SITExIM_df
    combined_PVT_SITExIM <- rbind(combined_PVT_SITExIM, PVT_SITExIM_df)
  }
  
  if (file.exists(file_pp)) {
    load(file_pp)                 # loads PVT_SITExIM_df_pp
    combined_PVT_SITExIM_pp <- rbind(combined_PVT_SITExIM_pp, PVT_SITExIM_df_pp)
  }
  
  if (file.exists(file_snu1)) {
    load(file_snu1)               # loads PVT_SITExIM_df_snu1
    combined_PVT_SITExIM_snu1 <- rbind(combined_PVT_SITExIM_snu1, PVT_SITExIM_df_snu1)
  }
}


# save the files 

save(combined_PVT_SITExIM, 
     file = file.path(combined_dir, "combined_PVT_SITExIM.rda"))
save(combined_PVT_SITExIM_pp, 
     file = file.path(combined_dir, "combined_PVT_SITExIM_pp.rda"))
save(combined_PVT_SITExIM_snu1, 
     file = file.path(combined_dir, "combined_PVT_SITExIM_snu1.rda"))



