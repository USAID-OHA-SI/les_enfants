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
  library(gt)
  library(ggbeeswarm)
  library(readxl)
  library(fuzzyjoin)
  library(ggnewscale)
  
  datain <- "Data"
  dataout <- "Dataout"
  graphics <- "Graphics"
  source <- "Source: FY20Q4 MSD"
  
  # indicators of focus
  indic_list <- c("TX_NET_NEW", "TX_NEW", "TX_CURR") 

# LOAD & MUNGE MSD --------------------------------------------------------
  
  # Read in MSD
    zmb_msd <- return_latest(si_path(), "Site_IM.*Zambia")
  
  # @tessam import/first time handled in 99_data_import (read_msd)
   msd <- read_msd(zmb_msd)
  
  # Subset to only select indicators
    msd <- msd %>% 
      filter(indicator %in% indic_list) %>%
      clean_agency()
  
  
  # Surge data to track priority sites
    surge <- read_excel(here(datain, "Pediatric Surge IP Mapping_12.05.2020.xls"), skip = 3) %>% 
      rename(peds_surge_ovc = `Priority Level for Pediatric Surge - OVC`) %>% 
      mutate(peds_surge_site = if_else(`Pediatric Surge Facilities` %in% c("x", "X"), 1, 0),
             peds_surge_ovc = if_else(is.na(peds_surge_ovc), "NONE", peds_surge_ovc))
    
  # Limit to just sites that were tagged (fuzzy join is messy)
    surge_peds <- 
      surge  %>% 
      filter(peds_surge_ovc != "NONE" | peds_surge_site == 1) %>% 
      select(`Health Facility`, District, Province, peds_surge_site, peds_surge_ovc) 
  
  # How many unique sites in the MSD?
    msd %>% 
      filter(fiscal_year == 2020) %>% 
      distinct(orgunituid) 
  
  # Join MSD to surge data, flag peds focus sites with colors    
      msd_surge <- 
        msd %>% 
        mutate(psnu = str_remove_all(psnu, " District"),
               snu1 = str_remove_all(snu1, " Province")) %>% 
        left_join(., surge_peds, 
                  by = c("sitename" = "Health Facility", 
                         "psnu" = "District", 
                         "snu1" = "Province")) %>% 
        mutate(peds_focus = if_else((!is.na(peds_surge_site) | !is.na(peds_surge_ovc)), 
                                    burnt_sienna, grey50k)) 

  # Export a site X IM data set filtered to under 15s for TX_CURR and TX_NET_NEW
    msd_surge %>% 
      filter(fiscal_year %in% c(2019, 2020), 
             indicator == "TX_NET_NEW", 
             trendscoarse == "<15", 
             !is.na(cumulative), 
             fundingagency == "USAID", 
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER-H"), "DISCOVER-H", mech_name)) %>%
      reshape_msd(clean = TRUE) %>% 
      filter(str_detect(period, "Q")) %>% 
      write_csv(here(dataout, "ZMB_treatment_2019_2020_site_by_IM.csv"))
    
# SITE SHIFTS? ------------------------------------------------------------
  
  # Limited site shifts in FY20 - request to look at data by site for FY20
  # May want to revisit this if we are batching this type of munging
    

# TRENDS FOR FY20 ------------------------------------------
 
   # What does partner performance look like over time?
     msd_surge %>%
        filter(
          fiscal_year %in% c(2020), 
          indicator == "TX_NET_NEW", 
          trendscoarse == "<15", 
          !is.na(cumulative),
          fundingagency == "USAID",
          standardizeddisaggregate == "Age/Sex/HIVStatus"
        ) %>%
        reshape_msd(clean = TRUE) %>%
        filter(!(period %in% c("FY20"))) %>%
        mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER-H"), "DISCOVER-H", mech_name)) %>%
        group_by(mech_name, period, mech_code, snu1) %>%
        summarise(tx_net_new = sum(val, na.rm = TRUE)) %>%
        ggplot(aes(factor(period), y = tx_net_new, group = mech_name, fill = mech_name)) +
        geom_col() +
        facet_grid(mech_name ~ snu1, scales = "free_y", switch = "y") +
        si_style_ygrid() +
        scale_fill_si(palette = "siei") +
        theme(
          strip.text.y.left = element_text(angle = 0),
          strip.placement = "outside",
          legend.position = "none"
        ) +
        labs(
          x = NULL, y = NULL,
          title = "TX_NET_NEW DECLINED SHARPLY FROM FY20Q1",
          subtitle = "Figures do not accont for potential site shifts in Q1",
          caption = source
        )
  
    si_save(here(graphics, "ZMB_TX_NET_NEW_by_mech"), scale = 1.5)
  
  # What does partner performance look like over time for under 15s by site? and totals?

    # Two sites that are jacking things up are with Discover numbers
    # orgunituid %in% c("EO0Vuhr2twp", "JFEY3Gu7mYm")
    
    #' @title PSNU achievement by quarter with sites overlaid
    #' 
    #' @param df MSD of site level data
    #' @param indicator Indicator name
    #' @param mech Name of mechanism over which you are plotting
    #' @param share_level Share levels: psnu, snu1 or null for country
    #' @return plot of indicator at psnu with sites overlaid as x-scatter
    
    site_bar_plots <- function(df, indicator, mech) {
      
      # For plotting sites and breaking out by surge status
      tmp <- df %>%
        filter(
          fiscal_year == 2020, 
          indicator == {{indicator}}, 
          trendscoarse == "<15", !is.na(cumulative),
          fundingagency == "USAID",
          standardizeddisaggregate == "Age/Sex/HIVStatus",
          !orgunituid %in% c("EO0Vuhr2twp", "JFEY3Gu7mYm", "aQx1JZLhMzw")
        ) %>%
        reshape_msd(clean = TRUE) %>%
        filter(period != "FY20") %>%
        mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER-H"), "DISCOVER-H", mech_name)) %>%
        group_by(orgunituid, period, mech_code, snu1, peds_focus, 
                 mech_name, peds_surge_site, peds_surge_ovc, psnu) %>%
        summarise(val = sum(val, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(psnusort = tidytext::reorder_within(psnu, val, snu1, sep = ":"))
    
    # For plotting the bar graphs at the psnu level an setting snu1 colors - 
    # setting them here for consistency across plots    
      tmp_mech_agg <-
        tmp %>%
        group_by(mech_name, period, mech_code, snu1, psnu) %>%
        summarise(mech_val = sum(val, na.rm = TRUE)) %>%
        ungroup() %>% 
        mutate(snu1_colors = factor(snu1, labels = si_palettes$siei))
        
      
    # Set jitter for sites within bars
      jitter <- position_jitter(width = 0.2, height = 0.1)  
      
    # Bar plot with two layers of points jittered on top to show sites contribution to bar length  
      ggplot() +
        geom_col(
          data = tmp_mech_agg %>%
            filter(mech_name == {{ mech }}) %>%
            mutate(psnusort = tidytext::reorder_within(psnu, mech_val, snu1, sep = ":")),
          aes(x = factor(period), y = mech_val, group = mech_name, fill = snu1_colors), alpha = 0.15,
        ) +
        scale_fill_identity() +
        facet_wrap(~paste0(snu1, ":", psnu ), scales = "free_y") +
        ggnewscale::new_scale_fill() +
        geom_point(
          data = tmp %>% filter(peds_focus == grey50k, mech_name == {{ mech }}),
          aes(x = factor(period), y = val, group = mech_name, fill = peds_focus, size = abs(val)),
          shape = 21, position = jitter, color = "white", alpha = 0.85
        ) +
        geom_point(
          data = tmp %>% filter(peds_focus == burnt_sienna, mech_name == {{ mech }}),
          aes(x = factor(period), y = val, group = mech_name, fill = peds_focus, size = abs(val)),
          shape = 21, position = jitter, color = "white", alpha = 0.85
        ) +
        si_style_ygrid() +
        scale_fill_identity() +
        theme(
          strip.text.y.left = element_text(angle = 0), strip.placement = "outside",
          legend.position = "none"
        ) +
        labs(
          x = NULL, y = NULL,
          title = paste0({{indicator}}, " TRENDS FOR UNDER 15s IMPLEMENTED BY ", {{ mech }}),
          caption = source,
          subtitle = "Bar length is overall district value for each quarter. Orange dots denote sites included in PEDS surge, grey dots all other sites."
        ) +
        scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
        scale_y_continuous(labels = scales::comma_format(accuracy = 1)) 
      
        si_save(here(graphics, paste0("ZMB_",{{indicator}}, "_UNDER_15s_trends_", {{mech}})), scale = 1.6)
    }
   
  # Retreive list of mechs to loop over  
    mech_list <-  
     msd_surge %>% 
     filter(fiscal_year == "2020", 
            indicator %in% c("TX_NET_NEW"), 
            fundingagency == "USAID") %>% 
     mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER-H"), "DISCOVER-H", mech_name)) %>%
     count(mech_name) %>% 
     pull(mech_name)
    
  # Batch across mechanisms and by treatment indicators
    map(mech_list, ~site_bar_plots(msd_surge, "TX_CURR", .x))
    map(mech_list, ~site_bar_plots(msd_surge, "TX_NET_NEW", .x))


  


