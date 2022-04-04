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
  group_by(fundingagency, orgunituid, snu1, sitename, mech_name, fiscal_year, trendscoarse) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = F)) %>% 
  ungroup() %>%
  reshape_msd() %>% #modified reshape_msd to not drop NA's (values_drop_na = FALSE)
  pivot_wider(names_from = trendscoarse, values_from = value) %>% 
  rename(TX_CURR_UNDER15 = `<15`,
         TX_CURR_OVER15 = `15+`) %>%
  mutate(reporting_peds = ifelse(TX_CURR_UNDER15 == 0 | is.na(TX_CURR_UNDER15), FALSE, TRUE),
         reporting_adults = ifelse(TX_CURR_OVER15 == 0 | is.na(TX_CURR_OVER15), FALSE, TRUE))  
  #count(reporting_peds, reporting_adults) %>% spread(reporting_peds, n)


#export as csv for Tableau   
write_csv(df_tx, "Dataout/tx-curr-site-list.csv")


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



