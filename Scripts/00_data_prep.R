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
  library(ggbeeswarm)

  graphics <- "Graphics"
  source <- "Source: FY20Q4 MSD"

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
  
<<<<<<< HEAD
=======
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
      pivot_longer(cols = starts_with(c("targ", "res", "qtr")), 
                   names_to = "metric", 
                   values_to = "val") 
    
    return(df_tx)
  }
>>>>>>> b9696ab89a589fb4697e53112a82d5739068400f

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
    select(`FY` = fiscal_year,
           `CDC sites` = CDC, 
           `USAID sites` =USAID, 
           `CDC peds sites` = CDC_peds, 
           `USAID peds sites` = USAID_peds) %>% 
    mutate(`USAID peds share` = (`USAID peds sites` / `USAID sites`), 
           `CDC peds share` = (`CDC peds sites` / `CDC sites`)) %>% 
    select(sort(current_vars())) %>% select(`FY`, everything()) %>% 
    filter(`FY` != 2021) %>% 
    gt() %>% 
    tab_options(table.font.names = "Source Sans Pro")  %>% 
    fmt_number(columns = c(3:4, 6:7), decimals = 0) %>% 
    fmt_percent(columns = c(2,5), decimals = 0) 
  
  %>% 
  gtsave(here(graphics, "ZMB_peds_share_tx_sites.png"))

  # FOCUS ON MECHS NOW
  # What does this look like by mech_code for only USAID?
  usaid_tx_count <- msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", !is.na(cumulative) | !is.na(targets), indicator == "TX_CURR", fundingagency == "USAID") %>% 
    distinct(orgunituid, mech_code, mech_name, fiscal_year, indicator, fundingagency) %>% 
    mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    count(fiscal_year, mech_name) %>% 
    spread(mech_name, n) %>% 
    select(-`Local Treatment Partner`) %>% 
    mutate(tag = "all")
  
  usaid_tx_count_peds <- msd %>% 
    filter(trendscoarse == "<15", !is.na(cumulative), indicator == "TX_CURR", fundingagency == "USAID") %>% 
    mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    mutate(tag = "peds") %>% 
    distinct(orgunituid, mech_code, mech_name, fiscal_year, indicator, fundingagency, tag) %>% 
    count(fiscal_year, mech_name, tag) %>% 
    spread(mech_name, n)
  
  rbind(usaid_tx_count, usaid_tx_count_peds) %>% 
    pivot_longer(cols = -c(fiscal_year, tag),
                 names_to = "mech", 
                 values_to = "site_count") %>% 
    pivot_wider(names_from = tag, values_from = site_count) %>% 
    filter(fiscal_year != 2021) %>% 
    ggplot(aes(factor(fiscal_year))) +
    geom_col(aes(y = all), fill = grey10k) +
    geom_col(aes(y = peds), fill = scooter) +
    geom_text(aes(label = peds, y = peds), vjust = 0, nudge_y = 1, color = scooter) +
    geom_text(aes(label = paste0("(", percent(peds/all, accuracy = 1), ")"), y = peds), vjust = 1, nudge_y = -3, color = "white") +
    geom_text(aes(label = all, y = all), vjust = 0, nudge_y = 1, color = grey80k) +
    facet_wrap(~mech) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 325)) +
    si_style_xline() +
    theme(axis.text.y = element_blank()) +
    labs(x = NULL, y = NULL,
         title = "THE SHARE OF USAID TREATMENT SITES SERVING PEDS (<15) HAS INCREASED ACROSS FISCAL YEARS AND TREATMENT PARTNERS",
         caption = source)
  
  si_save(here(graphics, "ZMB_share_tx_site_serving_peds"), scale = 1.25)
  
  
  left_join(usaid_tx_count, usaid_tx_count_peds)  %>%
    mutate(`DISCOVER-H_share` = (`DISCOVER-H_peds`/`DISCOVER-H`),
           EQUIP_share = (EQUIP_peds / EQUIP),
           SAFE_share = (SAFE_peds/SAFE) 
    ) %>% 
  select(sort(peek_vars())) %>% 
    select(fiscal_year, everything()) 
  
  # Main Takeaways
  # The share of sites that are reporting TX_CURR peds results has been increasing year over year
  # Next step is to look at the volume of results by partner


# PEDS VOLUME BY MECH_CODE  -----------------------------------------------
  tx_curr_mech_peds <- 
    msd %>% 
    filter(trendscoarse == "<15", !is.na(cumulative), indicator == "TX_CURR", fundingagency == "USAID", 
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    group_by(mech_code, mech_name, fiscal_year, indicator, trendsfine) %>% 
    summarise(TX_CURR_peds = sum(cumulative, na.rm = TRUE),
              targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(trendsfine) %>% 
    mutate(maxval = max(targets, na.rm = TRUE)) %>% 
    ungroup()
  
  # p <- tx_curr_mech_peds %>% 
  #   group_by(mech_name, trendsfine) %>% 
  #   summarise(
  #     tx_curr = spk_chr(
  #       TX_CURR_peds,
  #       type = "line")
  #   ) %>% 
  #   gt(rowname_col = "mech_name",
  #      groupname_col = "trends") %>%
  #   fmt_markdown(columns = vars(tx_curr, trendsfine)) 
  # 
  # p_html <- gt:::as.tags.gt_tbl(p)
  # p_html <- htmltools::attachDependencies(p_html, htmlwidgets::getDependency("sparkline"))
  # print(p_html, browse = interactive())
  
  # Background data frame to set axes across age bands
  tx_curr_mech_peds %>% 
    ggplot(aes(x = factor(fiscal_year), y = TX_CURR_peds, group = mech_name)) +
    geom_col(aes(y = targets), fill = grey10k) +
    geom_col(aes(fill = mech_name)) + 
    geom_point(aes(y = maxval), colour = "white") +
    geom_errorbar(data = . %>% filter(targets != 0), 
                  aes(ymin = targets, ymax = targets), colour = grey10k, size = 0.5, linetype = "dashed" ) +
    facet_wrap(paste0("Age band: ", trendsfine) ~ mech_name, scales = "free_y") +
    scale_fill_si(palette = "siei", reverse = TRUE) +
    scale_y_continuous(expand = c(0, Inf), limits = c(0, NaN))+
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = "NO USAID PARTNER MET THEIR FY20 TREATMENT TARGETS FOR THE <1 AGE BAND",
         subtitle = "Treatment ahievement depicted by filled bar height, targets shown in light gray (or with dotted line when surpassed)",
         caption = source)+
    theme(legend.position = "none")
  
  si_save(here(graphics, "ZMB_tx_performance_by_partner_age_band"), scale = 1.66)
  
  
  
  tx_curr_mech_under15s <- 
    msd %>% 
    # filter(!is.na(cumulative), indicator == "TX_CURR", fundingagency == "USAID", standardizeddisaggregate == "Total Numerator") %>% 
    filter(trendscoarse == "<15", !is.na(cumulative), indicator == "TX_CURR", fundingagency == "USAID", 
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
    group_by(mech_code, mech_name, fiscal_year, indicator) %>% 
    summarise(TX_CURR = sum(cumulative, na.rm = TRUE))
  
  # What does the composition of the mechs look like for under 15s? Is the mix of treament patients
  # changing across time for each partner?
   tx_curr_mech_under15s %>% 
     left_join(., tx_curr_mech_peds) %>% 
     mutate(share = (TX_CURR_peds/TX_CURR)) %>% 
    ggplot(aes(x = factor(fiscal_year), y = share, group = trendsfine, color = trendsfine)) + 
    geom_line() + 
    ggrepel::geom_text_repel(data = . %>% filter(fiscal_year != 2019), aes(label = comma(TX_CURR_peds, accuracy = 1))) +
    facet_wrap(~mech_name) +
    scale_y_continuous(labels = percent) +
    si_style_ygrid() +
    scale_color_si(palette = "siei")+
    labs(x = NULL, y = NULL,
         title = "THE AGE COMPOSITION OF TREATMENT HAS CHANGED LITTLE ACROSS FISCAL YEARS",
         caption = source) 
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
    

# WHAT DOES THE DISTRIBUTION LOOK LIKE ACROSS SITES / PARNTERS? -----------
    set.seed(20201215)  
   msd %>% 
      filter(trendscoarse == "<15", !is.na(cumulative), indicator == "TX_CURR", fundingagency == "USAID", 
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      mutate(mech_name = if_else(str_detect(mech_name, "DISCOVER"), "DISCOVER-H", mech_name)) %>% 
      group_by(orgunituid, mech_name, fiscal_year, trendsfine) %>% 
      summarise(tx_curr_peds = sum(cumulative, na.rm = TRUE),
                targets = sum(targets, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(target_achieved = if_else(tx_curr_peds > targets & targets !=0, 1, 0)) %>% 
      filter(fiscal_year == "2020", trendsfine != "<01") %>% 
      group_by(trendsfine) %>% 
      mutate(maxval = max(tx_curr_peds),
             average = mean(tx_curr_peds)
             ) %>% 
      ungroup()%>% 
      ggplot(aes(x = mech_name, y = tx_curr_peds, color = mech_name)) +
     geom_point(aes(y = maxval, colour = NA), ) +
     geom_hline(aes(yintercept = average), color = grey30k, linetype = "dashed") +
      geom_quasirandom(alpha = 0.5, method = "tukeyDense") +
      facet_wrap(~trendsfine, scales = "free_x") +
      si_style_ygrid() +
      scale_color_si(palette = "siei", reverse = TRUE) +
      labs(x = NULL, y = "TX_CURR Achieved",
           title = "IN FY20, TREATMENT SITES SERVED AN AVERAGE OF ABOUT 16 PATIENTS PER SITE",
           subtitle = "TX_CURR site level age band average depicted by dotted line.",
           caption = source)  +
      scale_y_continuous(trans = log_trans(),
                         breaks =c(1, 5, 10, 25, 50, 100, 300, 500))+
     theme(legend.position = "none") 
     
   si_save(here(graphics, "ZMB_beeswarm_tx_curr_sitelevel"), scale = 1.3)
    
    

    
      
# AGGREGATE TARGETS / RESULTS & CREATE SHARES -----------------------------
  
  msd %>% 
    sites_share() %>% View()
    