

############## Initialization and loading/shaping data ----------------------------------------------


library(tidyverse)
library(ggplot2)
library(stringr)
library(digest)
library(lme4)
library(broom.mixed)
library(lfe)


#Uganda_data = read.delim(file = 'C:/Users/pgeorge/Desktop/databases/input/DATIM_Genie/Genie-SITE_IM-Uganda-frozen-2024-12-13.txt', 
#                         sep = '\t' )

rm(list = ls())

list.files('C:/Users/pgeorge/Desktop/databases/input/DATIM_Genie/2024_Q4/')     
list.files("C:/Users/georg/Desktop/databases/input/DATIM_Genie/2024_Q4/")

# Directory containing .rda files
data_dir <- "C:/Users/georg/Desktop/databases/input/DATIM_Genie/2024_Q4/"

# List all .rda files
list_of_SITE_files <- list.files(data_dir, pattern = "\\.rda$", full.names = TRUE)

# Create empty lists to store results
mmd_site_list <- list()
mmd_country_list <- list()
vls_site_list <- list()
vls_country_list <- list()

for(file_path in list_of_SITE_files) {
  # Save current environment objects to identify new ones after load
  objs_before <- ls()
  
  # Load the file
  load(file_path)  # This loads all objects saved in the .rda
  
  # Identify newly loaded objects
  objs_after <- ls()
  new_objs <- setdiff(objs_after, objs_before)
  
  # Find the data frame object among new_objs
  df_candidates <- new_objs[sapply(new_objs, function(x) is.data.frame(get(x)))]
  
  if (length(df_candidates) != 1) {
    stop("Unable to uniquely identify a single data frame in the loaded file.")
  }
  
  # Assign the identified data frame to df_1
  df_1 <- get(df_candidates)
  
  # Remove original loaded objects to save memory (except df_1)
  rm(list = setdiff(new_objs, "df_1"))
  
  country_selected = df_1$country[1]
  
  print(country_selected)
  
  # pare down the dataframe so it's more manageable
  df_1 <- df_1 %>%
    filter(funding_agency == 'USAID') %>% 
    group_by(prime_partner_name, facilityuid) %>%
    mutate(unique_implementing_site_factor = as.factor(cur_group_id())) %>%
    ungroup() %>%
    mutate(unique_implementing_site_factor = paste(country_selected, unique_implementing_site_factor, sep = "_"), 
           indicator_ND = paste(indicator, numeratordenom, sep = "_")) %>% 
    select(country, snu1, snu2, facility, prime_partner_name, unique_implementing_site_factor, funding_agency, fiscal_year, ageasentered, qtr1, qtr2, qtr3, qtr4, sex, cumulative, targets, 
           target_age_2024, standardizeddisaggregate, otherdisaggregate, otherdisaggregate_sub, indicator_ND, indicator, indicatortype, categoryoptioncomboname, 
           modality, safe_for_net_new, safe_for_vlc)
  
  
  # Reshape the data to long format
  df_1 <- df_1 %>%
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
  
  ############## MMD data ----------------------------------------------
  
  
  MMD_df = df_1 %>% 
    filter(ageasentered == '<15', 
           indicator_ND == 'TX_CURR_N')
  
  table(MMD_df$otherdisaggregate, MMD_df$ageasentered, useNA = 'always')
  
  
  MMD_df_site <- MMD_df %>%
    # Group by fiscal year, quarter, and site
    group_by(fiscal_year, quarter_num, unique_implementing_site_factor) %>%
    # Summarize values for each disaggregate
    summarise(
      value_3_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - 3 to 5 months"], na.rm = TRUE),
      value_6_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - 6 or more months"], na.rm = TRUE),
      value_less_3_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - Less than 3 months"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculate the total denominator
    mutate(
      total_dispensing = value_3_months + value_6_months + value_less_3_months,
      percent_MMD_6months = ifelse(total_dispensing > 0, value_6_months / total_dispensing * 100, NA),
      percent_MMD_3months = ifelse(total_dispensing > 0, (value_3_months + value_6_months) / total_dispensing * 100, NA)
    ) %>% 
    mutate(
      country = country_selected
    )
  
  
  MMD_df_country <- MMD_df %>%
    # Group only by fiscal year and quarter
    group_by(fiscal_year, quarter_num) %>%
    # Summarize values for each disaggregate
    summarise(
      value_3_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - 3 to 5 months"], na.rm = TRUE),
      value_6_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - 6 or more months"], na.rm = TRUE),
      value_less_3_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - Less than 3 months"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculate total dispensing and MMD percentages
    mutate(
      total_dispensing = value_3_months + value_6_months + value_less_3_months,
      percent_MMD_6months = ifelse(total_dispensing > 0, value_6_months / total_dispensing * 100, NA),
      percent_MMD_3months = ifelse(total_dispensing > 0, (value_3_months + value_6_months) / total_dispensing * 100, NA)
    ) %>% 
    mutate(
      country = country_selected
    )
  
  
  
  
  ############### now for TX_PVLS, TX_CURR, etc ------------------------------------
  
  
  vls_df = df_1 %>% 
    filter(indicator_ND %in% c('TX_CURR_N', "TX_PVLS_D", "TX_PVLS_N", "TX_NET_NEW_N", "TX_NEW_N"), 
           ageasentered %in% c('<01', "01-04", "05-09", "10-14"))
  
  table(vls_df$indicator_ND, vls_df$ageasentered)
  
  
  
  vls_df_site <- vls_df %>%
    group_by(fiscal_year, quarter_num, unique_implementing_site_factor) %>%
    summarise(
      tx_curr = sum(value[indicator_ND == "TX_CURR_N"], na.rm = TRUE),
      tx_pvls_d = sum(value[indicator_ND == "TX_PVLS_D"], na.rm = TRUE),
      tx_pvls_n = sum(value[indicator_ND == "TX_PVLS_N"], na.rm = TRUE),
      tx_new = sum(value[indicator_ND == "TX_NEW_N"], na.rm = TRUE),
      tx_net_new = sum(value[indicator_ND == "TX_NET_NEW_N"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Arrange by time for lag calculations
    arrange(fiscal_year, quarter_num, unique_implementing_site_factor) %>%
    group_by(unique_implementing_site_factor) %>% 
    mutate(
      tx_curr_2QPrior = lag(tx_curr, 2)  # Get TX_CURR from 2 quarters prior
    ) %>%
    ungroup() %>%
    mutate(
      # Avoid division by zero or negative denominators with ifelse checks
      proxy_continuity = ifelse((tx_curr - tx_net_new + tx_new) > 0, 
                                tx_curr / (tx_curr - tx_net_new + tx_new), NA),
      vl_coverage = ifelse(!is.na(tx_curr_2QPrior) & tx_curr_2QPrior > 0,
                           tx_pvls_d / tx_curr_2QPrior, NA),
      vl_suppression = ifelse(tx_pvls_d > 0, tx_pvls_n / tx_pvls_d, NA),
      country = country_selected
    )
  
  
  vls_df_country <- vls_df %>%
    group_by(fiscal_year, quarter_num) %>%
    summarise(
      tx_curr = sum(value[indicator_ND == "TX_CURR_N"], na.rm = TRUE),
      tx_pvls_d = sum(value[indicator_ND == "TX_PVLS_D"], na.rm = TRUE),
      tx_pvls_n = sum(value[indicator_ND == "TX_PVLS_N"], na.rm = TRUE),
      tx_new = sum(value[indicator_ND == "TX_NEW_N"], na.rm = TRUE),
      tx_net_new = sum(value[indicator_ND == "TX_NET_NEW_N"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Arrange by time for lag calculations
    arrange(fiscal_year, quarter_num) %>%
    mutate(
      tx_curr_2QPrior = lag(tx_curr, 2)
    ) %>%
    mutate(
      proxy_continuity = ifelse((tx_curr - tx_net_new + tx_new) > 0,
                                tx_curr / (tx_curr - tx_net_new + tx_new), NA),
      vl_coverage = ifelse(!is.na(tx_curr_2QPrior) & tx_curr_2QPrior > 0,
                           tx_pvls_d / tx_curr_2QPrior, NA),
      vl_suppression = ifelse(tx_pvls_d > 0, tx_pvls_n / tx_pvls_d, NA),
      country = country_selected
    )
  
  
  mmd_site_list[[country_selected]] <- MMD_df_site
  mmd_country_list[[country_selected]] <- MMD_df_country
  vls_site_list[[country_selected]] <- vls_df_site
  vls_country_list[[country_selected]] <- vls_df_country    
}


# Combine MMD site-level data frames
mmd_site_combined <- bind_rows(mmd_site_list, .id = "country")

# Combine MMD country-level data frames
mmd_country_combined <- bind_rows(mmd_country_list, .id = "country")

# Combine VLS site-level data frames
vls_site_combined <- bind_rows(vls_site_list, .id = "country")

# Combine VLS country-level data frames
vls_country_combined <- bind_rows(vls_country_list, .id = "country")



# Combine MMD and VLS country-level data frames
country_combined <- full_join(
  mmd_country_combined, 
  vls_country_combined, 
  by = c("country", "fiscal_year", "quarter_num"),
  suffix = c("_MMD", "_VLS")
) %>% 
  select(country, fiscal_year, quarter_num, tx_curr, tx_pvls_d, tx_pvls_n, percent_MMD_3months, percent_MMD_6months, proxy_continuity, vl_coverage, vl_suppression)



table(country_combined$country)


# Combine MMD and VLS site-level data frames
site_combined <- full_join(
  mmd_site_combined, 
  vls_site_combined, 
  by = c("country", "fiscal_year", "quarter_num", "unique_implementing_site_factor"),
  suffix = c("_MMD", "_VLS")
) %>% 
  select(country, unique_implementing_site_factor, fiscal_year, quarter_num, tx_curr, tx_pvls_d, tx_pvls_n, percent_MMD_3months, percent_MMD_6months, proxy_continuity, vl_coverage, vl_suppression)

table(site_combined$country)

save(country_combined, file = 'C:/Users/georg/Desktop/databases/output/MMD/country_combined.rda')
save(site_combined, file = 'C:/Users/georg/Desktop/databases/output/MMD/site_combined.rda')



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### now for the data review -----------------------------------------------------------------------------


rm(list = ls())

load('C:/Users/georg/Desktop/databases/output/MMD/site_combined.rda')
load('C:/Users/georg/Desktop/databases/output/MMD/country_combined.rda')



table(site_combined$country)

selected_countries <- c(
  "Burundi", "Cote d'Ivoire", "Dominican Republic", "Democratic Republic of the Congo", "Eswatini", "Haiti",
  "Kenya", "Lesotho", "Malawi", "Mozambique", "South Sudan", "Tanzania",
  "Uganda", "Vietnam", "Zambia", "Zimbabwe")

west_africa_countries <- c("Benin", "Burkina Faso", "Togo", "Senegal", "Sierra Leone", "Ghana", "Mali", "Liberia")

site_combined = site_combined %>%
  filter(
    country %in% selected_countries | country %in% west_africa_countries
  ) 

country_combined = country_combined %>%
  filter(
    country %in% selected_countries | country %in% west_africa_countries
  ) 

checkvls = country_combined %>% filter(fiscal_year == 2024, 
                                       quarter_num == 4)

#------------------------------------------------------------
# 1. Basic Inspection & Summaries
#------------------------------------------------------------
# Overview of the dataset
glimpse(site_combined)

site_combined = site_combined %>% 
  mutate(vl_coverage = vl_coverage * 100, 
         vl_suppression = vl_suppression * 100)


n_distinct(site_combined$unique_implementing_site_factor)
n_distinct(site_combined$country)

site_combined_short = site_combined %>% 
  filter(tx_curr >= 1)

n_distinct(site_combined_short$unique_implementing_site_factor)
n_distinct(site_combined_short$country)


# Summary statistics of key variables
summary(site_combined_short[, c("percent_MMD_3months", "percent_MMD_6months", 
                          "vl_coverage", "vl_suppression", 
                          "tx_curr", "tx_pvls_d", "tx_pvls_n")])

summary(country_combined[, c("percent_MMD_3months", "percent_MMD_6months", 
                          "vl_coverage", "vl_suppression", 
                          "tx_curr", "tx_pvls_d", "tx_pvls_n")])

# Count number of unique sites and countries
length(unique(site_combined_short$unique_implementing_site_factor))
length(unique(site_combined_short$country))

# Simple frequency tables
table(site_combined_short$country)  # distribution by country
table(site_combined_short$fiscal_year, site_combined_short$quarter_num) # distribution over time


#------------------------------------------------------------
# 2. Change in Variables Over Time
#------------------------------------------------------------

# Function to summarize a variable over time (fiscal year and quarter)
# Function to summarize a variable over time (fiscal year and quarter)
summarize_over_time <- function(data, var_name) {
  data %>%
    group_by(fiscal_year) %>%
    summarise(
      mean_value = mean(.data[[var_name]], na.rm = TRUE),
      median_value = median(.data[[var_name]], na.rm = TRUE),
      p25_value = quantile(.data[[var_name]], 0.25, na.rm = TRUE),
      p75_value = quantile(.data[[var_name]], 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(fiscal_year)
}


# List of key variables to analyze
key_variables <- c("percent_MMD_3months", "percent_MMD_6months", 
                   "vl_coverage", "vl_suppression", 
                   "tx_curr")

# Loop through each variable and display its change over time
for (var in key_variables) {
  cat("Change in", var, "over time:\n")
  print(summarize_over_time(site_combined_short, var))
  cat("\n------------------------------\n")
}


# Loop through each variable and display its change over time
for (var in key_variables) {
  cat("Change in", var, "over time:\n")
  print(summarize_over_time(country_combined, var))
  cat("\n------------------------------\n")
}

recent_country = country_combined %>% 
  filter(fiscal_year == 2024, 
         quarter_num == 4)

#------------------------------------------------------------
# 2. Basic Associations (Unadjusted)
#------------------------------------------------------------
# Pearson correlation between MMD measures and VL suppression (unadjusted)
# (Note: This treats NA values as complete.obs, you may want a more rigorous approach)
cor(site_combined_short$percent_MMD_3months, site_combined_short$percent_MMD_6months, use = "complete.obs")
cor(country_combined$percent_MMD_6months, country_combined$percent_MMD_3months, use = "complete.obs")

# MMD/VLS cor
cor(site_combined_short$percent_MMD_3months, site_combined_short$vl_suppression, use = "complete.obs")

# MMD/VLC cor
cor(site_combined_short$percent_MMD_3months, site_combined_short$vl_coverage, use = "complete.obs")

# MMD/VLC cor
cor(site_combined_short$percent_MMD_3months, site_combined_short$proxy_continuity, use = "complete.obs")

# 6 month MMD
cor(site_combined_short$percent_MMD_6months, site_combined_short$vl_suppression, use = "complete.obs")
cor(site_combined_short$percent_MMD_6months, site_combined_short$vl_coverage, use = "complete.obs")
cor(site_combined_short$percent_MMD_6months, site_combined_short$proxy_continuity, use = "complete.obs")


# Simple grouped summaries: average VL suppression by quartiles of MMD_3months
site_combined_short %>%
  mutate(MMD3_quartile = ntile(percent_MMD_3months, 4)) %>%
  group_by(MMD3_quartile) %>%
  summarise(
    mean_vl_suppression = mean(vl_suppression, na.rm = TRUE),
    n = n()
  )


#------------------------------------------------------------
# 3. Basic Visualizations
#------------------------------------------------------------
# Histogram of percent MMD_3months


ggplot(site_combined_short %>% filter(fiscal_year == 2022), aes(x = percent_MMD_3months)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of % MMD (3-months)", x = "% MMD (3mo)")


ggplot(site_combined_short %>% filter(fiscal_year == 2022), aes(x = percent_MMD_3months)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of % MMD (3-months)", x = "% MMD (3mo)")


# Scatter plot of %MMD_3months vs. VL_suppression
ggplot(site_combined_short, aes(x = percent_MMD_3months, y = vl_suppression)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "VL Suppression vs. % MMD (3-months)", x = "% MMD (3mo)", y = "VL Suppression")


# Boxplot of VL suppression by country
ggplot(site_combined_short, aes(x = country, y = vl_suppression)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "VL Suppression by Country", x = "Country", y = "VL Suppression") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summarize the outcome variable by fiscal year and quarter
outcome_summary <- site_combined_short %>%
  group_by(fiscal_year, quarter_num) %>%
  summarise(mean_outcome = mean(vl_suppression, na.rm = TRUE), .groups = "drop")


# Prepare the data
site_combined_short_yearly <- site_combined_short %>%
  filter(fiscal_year %in% c(2022, 2023, 2024)) %>%
  mutate(fiscal_year = as.factor(fiscal_year)) # Ensure fiscal_year is a factor for faceting

n_distinct(site_combined_short_yearly$unique_implementing_site_factor)

# Calculate means for each year
site_combined_short_means <- site_combined_short_yearly %>%
  group_by(fiscal_year) %>%
  summarise(
    mean_MMD_3months = mean(percent_MMD_3months, na.rm = TRUE),
    mean_vl_suppression = mean(vl_suppression, na.rm = TRUE), 
    mean_vl_coverage = mean(vl_coverage, na.rm = TRUE), 
    median_MMD_3months = median(percent_MMD_3months, na.rm = TRUE),
    median_vl_suppression = median(vl_suppression, na.rm = TRUE), 
    median_vl_coverage = median(vl_coverage, na.rm = TRUE), 
    .groups = "drop"
  )



# Create the density plot faceted by year with median lines
ggplot(site_combined_short_yearly) +
  # Density plots
  geom_density(aes(x = percent_MMD_3months, color = "MMD (3+ Months)"), fill = "#2057a7", alpha = 0.4) +
  geom_density(aes(x = vl_suppression, color = "Viral suppression"), fill = "#e07653", alpha = 0.4) +
  
  # Add vertical lines for median values
  geom_vline(data = site_combined_short_means, aes(xintercept = median_MMD_3months, color = "MMD (3+ Months)"),
             linetype = "dashed", size = 1) +
  geom_vline(data = site_combined_short_means, aes(xintercept = median_vl_suppression, color = "Viral suppression"),
             linetype = "dashed", size = 1) +
  
  # Facet by year
  facet_wrap(~ fiscal_year, ncol = 1, scales = "free_y") +
  
  # Labels and color legend
  labs(
    title = "Density Plots of % MMD (3+ Months) and Viral Suppression by Year",    
    caption = "The blue density represents the distribution of MMD (3+ Months), 
    and the red density represents Viral suppression.\nDashed lines show the median value for each respective metric.
Viral suppression is a proxy metric, calculated at the site level.
Viral suppression = Number of viral load tests < 1000 cp/ml / Total number of viral load tests.",
    x = "Percentage",
    y = "Density",
    color = "Median of Metric"
  ) +
  scale_color_manual(values = c("MMD (3+ Months)" = "#2057a7", 
                                "Viral suppression" = "#e07653")) +
  
  coord_cartesian(xlim = c(0, 120)) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  
  # Theme adjustments
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 10)
  )



# Create the density plot faceted by year with median lines
ggplot(site_combined_short_yearly) +
  # Density plots
  geom_density(aes(x = percent_MMD_3months, color = "MMD (3+ Months)"), fill = "#2057a7", alpha = 0.4) +
  geom_density(aes(x = vl_coverage, color = "Viral coverage (%)"), fill = "#f2bc40", alpha = 0.4) +
  
  # Add vertical lines for median values
  geom_vline(data = site_combined_short_means, aes(xintercept = median_MMD_3months, color = "MMD (3+ Months)"),
             linetype = "dashed", size = 1) +
  geom_vline(data = site_combined_short_means, aes(xintercept = median_vl_coverage, color = "Viral coverage (%)"),
             linetype = "dashed", size = 1) +
  
  # Facet by year
  facet_wrap(~ fiscal_year, ncol = 1, scales = "free_y") +
  
  # Labels and color legend
  labs(
    title = "Density Plots of % MMD (3+ Months) and Viral Coverage by Year",    
    caption = "The blue density represents the distribution of MMD (3+ Months), 
    and the yellow density represents Viral coverage (%).\nDashed lines show the median value for each respective metric.
Viral coverage is a proxy metric, calculated at the site level. 
Viral coverage = Number of viral load tests / Number of patients on treatment 2 quarters prior.",
    x = "Percentage",
    y = "Density",
    color = "Median of Metric"
  ) +
  scale_color_manual(values = c("MMD (3+ Months)" = "#2057a7", 
                                "Viral coverage (%)" = "#f2bc40")) +
  
  coord_cartesian(xlim = c(0, 120)) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  
  # Theme adjustments
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 10)
  )




# Create histograms, faceted by year, with median lines
ggplot(site_combined_short_yearly) +
  # Histogram for MMD (3+ Months)
  geom_histogram(
    aes(x = percent_MMD_3months),
    binwidth = 10,                # Bin width of 1
    fill = "#2057a7",            # Blue fill
    color = "white",             # White borders around bars
    alpha = 0.4,
    position = "identity"
  ) +
  # Histogram for Viral Coverage (%)
  geom_histogram(
    aes(x = vl_coverage),
    binwidth = 10,                # Bin width of 1
    fill = "#f2bc40",            # Yellow fill
    color = "white",             
    alpha = 0.4,
    position = "identity"
  ) +
  
  # Add vertical lines for median values
  geom_vline(
    data = site_combined_short_means,
    aes(xintercept = median_MMD_3months, color = "MMD (3+ Months)"),
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    data = site_combined_short_means,
    aes(xintercept = median_vl_coverage, color = "Viral coverage (%)"),
    linetype = "dashed",
    size = 1
  ) +
  
  # Facet by year
  facet_wrap(~ fiscal_year, ncol = 1, scales = "free_y") +
  
  # Labels
  labs(
    title = "Histograms of % MMD (3+ Months) and Viral Coverage by Year",    
    caption = "The blue bars represent the distribution of MMD (3+ Months),
    and the yellow bars represent Viral coverage (%).
    Dashed vertical lines mark median values for each metric.
    Viral coverage is calculated at the site level as:
    Number of viral load tests / Number of patients on treatment 2 quarters prior.",
    x = "Percentage",
    y = "Count",
    color = "Median of Metric"
  ) +
  
  # Color scale for median lines
  scale_color_manual(
    values = c("MMD (3+ Months)" = "#2057a7",
               "Viral coverage (%)"      = "#f2bc40")
  ) +
  
  # Limit x-axis to 0???120 and place breaks every 10%
  coord_cartesian(xlim = c(0, 105)) +
  scale_x_continuous(breaks = seq(0, 105, by = 10)) +
  
  # Classic theme
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 10)
  )



# Create histograms, faceted by year, with median lines
ggplot(site_combined_short_yearly) +
  
  # Histogram for MMD (3+ Months)
  geom_histogram(
    aes(x = percent_MMD_3months, 
        fill = "MMD (3+ Months)"),  # <--- fill mapped to a label
    binwidth = 5,
    color = "white",
    alpha = 0.4,
    position = "identity"
  ) +
  
  # Histogram for Viral suppression
  geom_histogram(
    aes(x = vl_suppression, 
        fill = "Viral suppression"),    # <--- fill mapped to a label
    binwidth = 5,
    color = "white",
    alpha = 0.4,
    position = "identity"
  ) +
  
  # Add vertical lines for median MMD (solid)
  geom_vline(
    data = site_combined_short_means,
    aes(
      xintercept = median_MMD_3months,
      color = "MMD (3+ Months)",    # <--- color mapped to label
      linetype = "MMD (3+ Months)"  # <--- linetype mapped to label
    ),
    size = 1
  ) +
  
  # Add vertical lines for median VLS (dashed)
  geom_vline(
    data = site_combined_short_means,
    aes(
      xintercept = median_vl_suppression,
      color = "Viral suppression",      # <--- color mapped to label
      linetype = "Viral suppression"    # <--- linetype mapped to label
    ),
    size = 1
  ) +
  
  # Facet by year
  facet_wrap(~ fiscal_year, ncol = 1, scales = "free_y") +
  
  # Labels
  labs(
    title = "Facility-Level Distributions of Pediatric 3+ Month ART Dispensing and Viral Load Outcomes",
    subtitle = "Fiscal Year (FY) 2022 - 2024", 
    caption = "- Each panel shows histograms of facility-level 3+ month ART dispensing (3+MMD, blue bars) and viral load suppression (VLS, red bars) for 
children (<15 years), aggregated by fiscal year (FY2022–FY2024) across 8,383 USAID/PEPFAR-supported facilities in 22 countries. 
- The vertical lines mark the median value for each metric within each fiscal year. 
- The x-axis displays the facility-level reported measure for 3+ month ART dispensing or viral load suppression. 
- The y-axis indicates the number of facilities falling into each percentage bin (bin width = 5 percentage points).
- As shown, 3+MMD and VLS, including median values and by overall distribution, increase across the study period.",
    x = "Percentage",
    y = "Count",
    fill = "Metric",            # <--- Legend title for fills
    color = "Median of Metric", # <--- Legend title for color lines
    linetype = "Median of Metric"
  ) +
  
  # Color scale for the bars and lines
  scale_fill_manual(
    values = c(
      "MMD (3+ Months)" = "#2057a7",
      "Viral suppression"    = "#e07653"
    )
  ) +
  scale_color_manual(
    values = c(
      "MMD (3+ Months)" = "#2057a7",
      "Viral suppression"    = "#e07653"
    )
  ) +
  
  # Linetype scale for MMD (solid) vs. VLS (dashed)
  scale_linetype_manual(
    values = c(
      "MMD (3+ Months)" = "solid",
      "Viral suppression"    = "dashed"
    )
  ) +
  
  # Limit x-axis to 0–105 and place breaks every 10
  coord_cartesian(xlim = c(0, 105)) +
  scale_x_continuous(breaks = seq(0, 105, by = 10)) + 
  scale_y_continuous(limits = c(0, 12500)) +
  
  # Classic theme
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 10)
  )







# 1) Reshape the main dataset from wide to long
library(dplyr)
library(tidyr)
library(ggplot2)

site_combined_short_yearly_long <- site_combined_short_yearly %>%
  pivot_longer(
    cols = c(percent_MMD_3months, vl_coverage),
    names_to = "metric",
    values_to = "percentage"
  )

# 2) Reshape the dataset containing medians similarly
site_combined_short_means_long <- site_combined_short_means %>%
  pivot_longer(
    cols = c(median_MMD_3months, median_vl_coverage),
    names_to = "metric",
    values_to = "median_val"
  ) %>%
  mutate(
    # Match the 'metric' values to those in the long dataset
    metric = case_when(
      metric == "median_MMD_3months" ~ "percent_MMD_3months",
      metric == "median_vl_coverage" ~ "vl_coverage"
    )
  )

# 3) Define labels and colors for each metric
metric_labels <- c(
  "percent_MMD_3months" = "MMD (3+ Months)",
  "vl_coverage"         = "Viral coverage (%)"
)
metric_colors <- c(
  "percent_MMD_3months" = "#2057a7",  # Blue
  "vl_coverage"         = "#f2bc40"   # Yellow
)


# 4) Create the histograms with side-by-side bars and median lines
ggplot(site_combined_short_yearly_long, aes(x = percentage, fill = metric)) +
  geom_histogram(
    binwidth = 10,
    color = "white",
    alpha = 0.4,
    position = "dodge"      # Places bars side by side
  ) +
  # Median lines
  geom_vline(
    data = site_combined_short_means_long,
    aes(xintercept = median_val, color = metric),
    linetype = "dashed",
    size = 1
  ) +
  facet_wrap(~ fiscal_year, ncol = 1, scales = "free_y") +
  
  # Manual color scales for fill and line color
  scale_fill_manual(
    values = metric_colors,
    labels = metric_labels
  ) +
  scale_color_manual(
    values = metric_colors,
    labels = metric_labels
  ) +
  
  # Axis labels, title, and caption
  labs(
    title = "Side-by-Side Histograms of % MMD (3+ Months) and Viral Coverage by Year",
    caption = "Bars represent facility counts for each 1% increment of MMD (3+ Months) or Viral Coverage (%).\n
Dashed lines show median values per metric. Viral coverage = (Number of viral load tests) / (Number of patients on treatment 2 quarters prior).",
    x = "Percentage",
    y = "Count",
    fill = "Metric",
    color = "Metric"
  ) +
  
  # Limit x-axis to 0???120 with breaks every 10%
  coord_cartesian(xlim = c(0, 120)) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  
  # Theme adjustments
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0, size = 10)
  )








glimpse(country_combined)
### Comet tail plot, using 2022 Q4 as baseline, and 2024 Q4 as the endpoint, and viral suppression on the x axis, and MMD 3 month on the Y axis

# Filter data for 2022 Q4 and 2024 Q4
comet_data <- country_combined %>%
  filter(quarter_num == 4, fiscal_year %in% c(2022, 2024))


# Filter data for 2022 Q4 and 2024 Q4
comet_data <- country_combined %>%
  filter(quarter_num == 4, fiscal_year %in% c(2022, 2024)) %>%
  group_by(country) %>%
  summarise(
    vl_suppression_2022Q4 = vl_suppression[fiscal_year == 2022],
    percent_MMD_3months_2022Q4 = percent_MMD_3months[fiscal_year == 2022],
    vl_suppression_2024Q4 = vl_suppression[fiscal_year == 2024],
    percent_MMD_3months_2024Q4 = percent_MMD_3months[fiscal_year == 2024]
  ) %>%
  filter(!is.na(vl_suppression_2022Q4) & !is.na(vl_suppression_2024Q4)) # Remove NAs

# Plot comet tails
ggplot(comet_data) +
  # Add segments (tails) between start and end points
  geom_segment(aes(x = vl_suppression_2022Q4, y = percent_MMD_3months_2022Q4,
                   xend = vl_suppression_2024Q4, yend = percent_MMD_3months_2024Q4),
               arrow = arrow(length = unit(0.2, "cm")), color = "blue", size = 1) +
  # Add starting points (2022 Q4)
  geom_point(aes(x = vl_suppression_2022Q4, y = percent_MMD_3months_2022Q4),
             color = "red", size = 3, alpha = 0.7) +
  # Add ending points (2024 Q4)
  geom_point(aes(x = vl_suppression_2024Q4, y = percent_MMD_3months_2024Q4),
             color = "green", size = 3, alpha = 0.7) +
  # Add labels for countries
  geom_text(aes(x = vl_suppression_2024Q4, y = percent_MMD_3months_2024Q4,
                label = country), hjust = -0.1, vjust = 0.5, size = 3) +
  # Axis labels and title
  labs(
    title = "Comet Tail Plot: Change in VL Suppression and MMD 3 Months",
    subtitle = "Baseline: 2022 Q4 | Endpoint: 2024 Q4",
    x = "Viral Suppression",
    y = "MMD (3+ Months)"
  ) +
  theme_classic()


#------------------------------------------------------------
# 4. Data Preparation for Modeling
#------------------------------------------------------------

str(site_combined_short)

# Ensure factor variables are properly encoded
site_combined_short <- site_combined_short %>%
  mutate(
    country = as.factor(country),
    unique_implementing_site_factor = as.factor(unique_implementing_site_factor)
  )

# Create a continuous time variable
site_combined_short <- site_combined_short %>%
  mutate(time = (fiscal_year - 2022) * 4 + quarter_num)


#------------------------------------------------------------
# 5. Multivariable Mixed-Effects Model
#------------------------------------------------------------



# Define the outcomes of interest as strings
outcomes_of_interest <- c("vl_suppression", "vl_coverage", "proxy_continuity")


# Initialize empty data frames for results
re_results <- data.frame()
fe_results <- data.frame()

for (outcome in outcomes_of_interest) {
  
  # Construct formulas for random effects (RE) models with lmer
  # Using quarter_num as time variable (adjust as needed)
  form_re_3months <- as.formula(
    paste0(outcome, " ~ percent_MMD_3months + time + country + (1 | unique_implementing_site_factor)")
  )
  
  form_re_6months <- as.formula(
    paste0(outcome, " ~ percent_MMD_6months + time + country + (1 | unique_implementing_site_factor)")
  )
  
  # Fit RE models
  lmm_model_3months <- lmer(form_re_3months, data = site_combined_short)
  lmm_model_6months <- lmer(form_re_6months, data = site_combined_short)
  
  # Tidy up RE model results
  re_3m_tidy <- tidy(lmm_model_3months, effects = "fixed", conf.int = TRUE)
  re_3m_tidy$outcome <- outcome
  re_3m_tidy$model_type <- "RE"
  re_3m_tidy$mmd_type <- "3months"
  
  re_6m_tidy <- tidy(lmm_model_6months, effects = "fixed", conf.int = TRUE)
  re_6m_tidy$outcome <- outcome
  re_6m_tidy$model_type <- "RE"
  re_6m_tidy$mmd_type <- "6months"
  
  # Construct formulas for fixed effects (FE) models with felm
  # Here we include site and country as fixed effects through the | notation
  form_fe_3months <- as.formula(
    paste0(outcome, " ~ percent_MMD_3months + time | unique_implementing_site_factor + country")
  )
  
  form_fe_6months <- as.formula(
    paste0(outcome, " ~ percent_MMD_6months + time | unique_implementing_site_factor + country")
  )
  
  # Fit FE models
  fe_model_3months <- felm(form_fe_3months, data = site_combined_short)
  fe_model_6months <- felm(form_fe_6months, data = site_combined_short)
  
  # Tidy up FE model results
  fe_3m_tidy <- tidy(fe_model_3months, conf.int = TRUE)
  fe_3m_tidy$outcome <- outcome
  fe_3m_tidy$model_type <- "FE"
  fe_3m_tidy$mmd_type <- "3months"
  
  fe_6m_tidy <- tidy(fe_model_6months, conf.int = TRUE)
  fe_6m_tidy$outcome <- outcome
  fe_6m_tidy$model_type <- "FE"
  fe_6m_tidy$mmd_type <- "6months"
  
  # Append all results to the respective data frames
  re_results <- bind_rows(re_results, re_3m_tidy, re_6m_tidy)
  fe_results <- bind_rows(fe_results, fe_3m_tidy, fe_6m_tidy)
}

fe_results

summary(site_combined_short$vl_suppression)
summary(site_combined_short$vl_coverage)
summary(site_combined_short$percent_MMD_3months)

#------------------------------------------------------------
# 7. (Optional) Alternative Modeling Approaches
#------------------------------------------------------------
# For a more robust approach, consider transformations or different distributions,
# or use glmer() with a binomial family if you have numerator/denominator form.
# e.g., if you had binary outcomes or counts, you might do something like:
# glmer(cbind(tx_pvls_n, tx_pvls_d - tx_pvls_n) ~ percent_MMD_3months + ... + (1|site), family=binomial)
# This depends on the data structure available.

#------------------------------------------------------------
# Done
#------------------------------------------------------------




