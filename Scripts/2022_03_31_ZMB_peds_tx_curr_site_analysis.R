# PURPOSE: Peds vs. Adults TX CURR Site Analysis
# AUTHOR: K. Srikanth, T. Essam | SI
# LICENSE: MIT
# DATE: 2022-03-31
# NOTES: SI Ad hoc request from Mission

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(tidyverse)
library(gophr)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(gt)


# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

merdata <- glamr::si_path("path_msd")


# LOAD DATA ============================================================================  

msd <- read_msd(return_latest(folder = merdata, pattern = "Site_IM_FY20-22.*Zambia.zip")) 

# MUNGE ===============================================================================

#identify current period for filtering out future periods later
curr_year <- identifypd(msd, "year")
curr_qtr <- identifypd(msd, "quarter")

future_qtrs <- data.frame(x = c(1:4)) %>%
  dplyr::filter(x > curr_qtr) %>%
  dplyr::mutate(x = as.character(x)) %>%
  dplyr::pull()

df_tx <- msd %>% 
  clean_agency() %>% 
  filter(indicator == "TX_CURR",
         fiscal_year >= 2021,
         standardizeddisaggregate == "Age/Sex/HIVStatus",
        fundingagency %in% c("CDC", "USAID")
         ) %>%
  group_by(fundingagency, operatingunit,operatingunituid, orgunituid, snu1, snu1uid, psnu, psnuuid, sitename, mech_code, fiscal_year, trendscoarse) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = F)) %>% 
  ungroup() %>%
  reshape_msd() %>% #modified reshape_msd to not drop NA's (values_drop_na = FALSE)
  pivot_wider(names_from = trendscoarse, values_from = value) %>% 
  rename(TX_CURR_UNDER15 = `<15`,
         TX_CURR_OVER15 = `15+`) %>%
  mutate(reporting_peds = ifelse(TX_CURR_UNDER15 == 0 | is.na(TX_CURR_UNDER15), FALSE, TRUE),
         reporting_adults = ifelse(TX_CURR_OVER15 == 0 | is.na(TX_CURR_OVER15), FALSE, TRUE),
         peds_gap = ifelse(reporting_peds == FALSE & reporting_adults == TRUE, TRUE, FALSE),
         site_group = case_when(reporting_peds == FALSE & reporting_adults == TRUE ~ "Reporting only adult TX_CURR",
                                reporting_adults == FALSE & reporting_peds == TRUE ~ "Reporting only peds TX_CURR",
                                reporting_adults == TRUE & reporting_peds == TRUE ~ "Reporting both adult/peds TX_CURR",
                                reporting_adults == FALSE & reporting_peds == FALSE ~ "Reporting Neither")) 
  #count(reporting_peds, reporting_adults) %>% spread(reporting_peds, n)


df_tx <- df_tx %>%  
  rowwise() %>% 
  mutate(TX_CURR_volume = sum(c_across(TX_CURR_UNDER15:TX_CURR_OVER15), na.rm = T)) %>%
  group_by(orgunituid, mech_code) %>%
  mutate(tx_curr_ave = mean(TX_CURR_volume, na.rm = T)) %>% 
  ungroup() %>%
  mutate(
    site_size = ntile(tx_curr_ave, 3),
    site_size_label = case_when(
      site_size == 1 ~ "LOW VOLUME",
      site_size == 2 ~ "MEDIUM",
      site_size == 3 ~ 'HIGH',
      TRUE ~ NA_character_),
    site_size_label = fct_relevel(site_size_label, c("LOW VOLUME", "MEDIUM", "HIGH")),
    adults_to_peds = TX_CURR_OVER15/TX_CURR_UNDER15
  )

#export as csv for Tableau   
write_csv(df_tx, "Dataout/tx-curr-site-list4.csv")


#PLOT ----------------------------

#scatter
df_tx %>% 
  mutate(fill_color = ifelse(fundingagency == "USAID", denim, scooter_light)) %>% 
 # filter(TX_CURR_OVER15 > 20 & TX_CURR_UNDER15 > 20) %>% 
 # count(fundingagency)
  ggplot(aes(x = TX_CURR_OVER15, y = TX_CURR_UNDER15, color = fill_color)) +
  geom_point(alpha = 0.4) + 
  facet_wrap(~fundingagency, scales = "free_y") +
   scale_color_identity() + 
   si_style() +
  stat_summary_bin(fun = "mean", bins = 20, color = "orange",
                   size = 2, geom = "point") +
  scale_x_continuous(labels = label_number_si()) +
  scale_y_continuous(labels = label_number_si())



