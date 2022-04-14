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
source  <- source_info(return_latest(folder = merdata, pattern = "Site_IM_FY20-22.*Zambia.zip"))

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
         standardizeddisaggregate == "Age/Sex/HIVStatus",
        fundingagency %in% c("CDC", "USAID")
         ) %>%
  group_by(fundingagency, operatingunit,operatingunituid, orgunituid, 
           snu1, snu1uid, psnu, psnuuid, sitename, mech_code, fiscal_year, 
           trendscoarse) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T)) %>% 
  ungroup() %>%
  mutate(across(matches("qtr"), ~replace_na(.x, -99))) %>% 
  reshape_msd() %>% 
  mutate(value = ifelse(value == -99, NA_integer_, value)) %>% 
  filter(period %ni% c("FY22Q2", "FY22Q3", "FY22Q4")) %>% 
  mutate(fy = substr(period, 3, 4)) %>% 
  arrange(sitename, trendscoarse, period)

# Did a site report over/under 15s tx_curr for any periods in a given fy?
 df_tx_spell <- 
   df_tx %>% 
    group_by(sitename, fy, trendscoarse) %>% 
    mutate(na_count = sum(is.na(value))) %>% 
    ungroup()

 
# Let's plot reporting completeness by site starting at psnu level to keep it simple
# Let's break this down by psnu / agency
 psnu_tx_plot <- function(district = "Lusaka District", agency = "USAID") {
   
 df_tx_spell %>% 
   filter(fundingagency == agency, psnu == district) %>% 
   ggplot(aes(x = period, y = trendscoarse, fill = value)) +
   geom_tile(color = "white") +
   scale_fill_viridis_c(na.value = grey10k, trans = "log", 
                        breaks = c(1, 10, 50, 100, 250, 500, 1000, 2000),
                        labels = comma(c(1, 10, 50, 100, 250, 500, 1000, 2000), 1)) +
   geom_text(aes(label = comma(value, 1), color = ifelse(value < 75, "white", grey90k)), size = 6/.pt) +
   facet_wrap(~sitename+mech_code) +
   scale_color_identity() +
   scale_x_discrete(labels = c("FY20Q1", "", "Q3", "", "FY21Q1", "", "Q3", "", "FY22Q1")) +
   si_style(facet_space = 0.25) +
   si_legend_fill() +
   labs(x = NULL, y = NULL, title = glue::glue("{district}"), 
        subtitle = "Mising values in grey", 
        fill = "TX_CURR", 
        source = paste(source, "Site Level")) +
     theme(strip.text = element_text(size = 10))
   
    si_save(glue::glue("Images/Peds/{district}_{agency}_tx_curr_summary.png"), scale = 1.5)
        
 }
 
  #Where does USAID operate?
 psnu_usaid <- df_tx_spell %>% 
   filter(fundingagency == "USAID") %>% 
   distinct(psnu) %>% 
   pull()
 
 map(psnu_usaid, .f = ~psnu_tx_plot(.x, agency = "USAID"))
 
 # Create flags for Tableau
 
 
df_tx_tblow <- df_tx_spell %>% 
  pivot_wider(names_from = trendscoarse, values_from = value) %>% 
  rename(TX_CURR_UNDER15 = `<15`,
         TX_CURR_OVER15 = `15+`) %>%
  mutate(reporting_peds = ifelse(TX_CURR_UNDER15 == 0 | is.na(TX_CURR_UNDER15), FALSE, TRUE),
         reporting_adults = ifelse(TX_CURR_OVER15 == 0 | is.na(TX_CURR_OVER15), FALSE, TRUE),
         peds_gap = ifelse(reporting_peds == FALSE & reporting_adults == TRUE, TRUE, FALSE),
         site_group = case_when(reporting_peds == FALSE & reporting_adults == TRUE ~ "Reporting only adult TX_CURR",
                                reporting_adults == FALSE & reporting_peds == TRUE ~ "Reporting only peds TX_CURR",
                                reporting_adults == TRUE & reporting_peds == TRUE ~ "Reporting both adult/peds TX_CURR",
                                reporting_adults == FALSE & reporting_peds == FALSE ~ "Reporting Neither")
         ) 
  #count(reporting_peds, reporting_adults) %>% spread(reporting_peds, n)


df_tx_tblow <- df_tx_tblow %>%  
  rowwise() %>% 
  mutate(TX_CURR_volume = sum(c_across(TX_CURR_UNDER15:TX_CURR_OVER15), na.rm = T)) %>%
  group_by(orgunituid, mech_code) %>%
  mutate(tx_curr_ave = mean(TX_CURR_volume, na.rm = T)) %>% 
  ungroup() %>%
  mutate(site_size = case_when(
    tx_curr_ave < 500 ~ 1,
    between(tx_curr_ave, 500, 3000) ~ 2,
    tx_curr_ave > 3000 ~ 3),
    site_size_label = case_when(
      site_size == 1 ~ "LOW VOLUME",
      site_size == 2 ~ "MEDIUM",
      site_size == 3 ~ 'HIGH',
      TRUE ~ NA_character_),
    site_size_label = fct_relevel(site_size_label, c("LOW VOLUME", "MEDIUM", "HIGH")),
    adults_to_peds = TX_CURR_OVER15/TX_CURR_UNDER15
    )

# Merge in with factility location data
zmb_fac <- read_csv("../../../Downloads/Zambia - facilities_locations_2022-01-04.csv")

df_tx_geo <- df_tx_tblow %>% 
  left_join(., zmb_fac, by = c("orgunituid" = "id"))

#export as csv for Tableau   
write_csv(df_tx_geo, "Dataout/ZMB_tx_curr_peds_adults_geo.csv")
write_csv(df_tx_spell, "Dataout/ZMB_tx_curr_peds_long.csv")

df_tx_geo %>% count(site_group, period)

#PLOT ----------------------------

#scatter
df_tx_tblow %>% 
  mutate(fill_color = ifelse(fundingagency == "USAID", denim, scooter_light)) %>% 
 # filter(TX_CURR_OVER15 > 20 & TX_CURR_UNDER15 > 20) %>% 
 # count(fundingagency)
  ggplot(aes(x = TX_CURR_OVER15, y = TX_CURR_UNDER15, color = fill_color)) +
  geom_point(alpha = 0.4) + 
  facet_wrap(fundingagency~site_size_label, scales = "free") +
   scale_color_identity() + 
   si_style() +
  stat_summary_bin(fun = "mean", bins = 20, color = "orange",
                   size = 2, geom = "point") +
  scale_x_continuous(labels = label_number_si()) +
  scale_y_continuous(labels = label_number_si())



