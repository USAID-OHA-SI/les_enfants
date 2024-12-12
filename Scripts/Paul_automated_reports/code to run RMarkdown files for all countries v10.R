
# =============================================================================
# Automated Generation of Country-Specific RMarkdown Reports
# =============================================================================
#
# This script automates the creation of HTML reports for each country
# present in the 'metrics_95_peds_usaid.rda' dataset. It leverages
# RMarkdown's parameterized reports feature to customize each report.
#
# Author: Paul George
# Date: 12 Dec 2024
#
# =============================================================================

# -----------------------------#
# 1. Setup and Configuration   #
# -----------------------------#

# Set the working directory to the project's root directory
# Adjust the path as necessary
setwd("C:/Users/georg/OneDrive/R_projects/Exploring_MER/R code 95 plots")

# Clear the R environment to ensure a clean workspace
rm(list = ls())

# Load necessary libraries
# Install any missing packages before running the script
library(rmarkdown)   # For rendering RMarkdown documents
library(dplyr)        # For data manipulation
library(stringr)      # For string operations

# Define the base file paths
# Adjust these paths based on your directory structure
base_data_path <- "C:/Users/georg/Desktop/databases/output/Ninety95s_dfs/"
base_output_path <- "C:/Users/georg/OneDrive/R_projects/Exploring_MER/Reports/"

# -----------------------------#
# 2. Data Loading and Preparation #
# -----------------------------#

# Load the metrics data file
load('C:/Users/georg/Desktop/databases/output/Ninety95s_dfs/combined_95_metrics_targets_peds.rda')


combined_95_metrics_targets_peds <- combined_95_metrics_targets_peds %>% filter(!country %in% c("Angola", "Botswana", "Cameroon", "Ethiopia", "Namibia", "Rwanda"))

# Extract a list of unique countries from the dataset
country_list <- combined_95_metrics_targets_peds %>%
  distinct(country) %>%
  pull(country)

# Check if the country list is not empty
if (length(country_list) == 0) {
  stop("No countries found in the dataset.")
} else {
  message("Found ", length(country_list), " unique countries.")
}

# -----------------------------#
# 3. Report Generation Loop    #
# -----------------------------#

# Define additional parameters
fiscal_year <- 2024    # Set the fiscal year as needed
quarter <- 4         # Set the quarter as needed

# Path to the RMarkdown template
rmd_template <- "Country_95s_auto_v15.Rmd"  
getwd()


base_output_path = 'C:/Users/georg/Desktop/databases/output/Reports'

# Iterate through each country and render the report
for (country in country_list) {
  
  # Define the output file name, replacing spaces with underscores
  output_file <- paste0("Country_Report_", 
                        str_replace_all(country, " ", "_"), 
                        "_FY", fiscal_year, 
                        "_Q", quarter, 
                        ".html")
  
  # Define the full path for the output report
  output_path <- file.path(base_output_path, output_file)
  
  # Define the parameters for the current report
  params <- list(
    country = country,
    fiscal_year = fiscal_year,
    quarter = quarter
  )
  
  # Render the RMarkdown document with the specified parameters
  tryCatch({
    rmarkdown::render(
      input = rmd_template,
      output_file = output_path,
      params = params,
      envir = new.env(parent = globalenv())  # Use a clean environment for each render
    )
    message("Successfully generated report for: ", country)
  }, error = function(e) {
    warning("Failed to generate report for ", country, ": ", e$message)
  })
}


