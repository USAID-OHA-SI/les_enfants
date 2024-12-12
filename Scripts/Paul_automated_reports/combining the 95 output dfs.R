
###################### INTRODUCTION ---------------------------------------------------
# This code creates the dataframes to be used in the Pediatric automated country reports


# Steps for creating the automated pediatric country report:
#1. Download the country SITE x IM files
#2. Run creating_95_metrics_df.R file to create the summary dataframes 
#3. Run combining the 95s output dfs.R to combine the individual country dataframes into multiple, combined dataframes    ***** this code represents this step *****  
#4. Run the RMarkdown file to generate the report  
                  #a. Note, there is a separate R file that runs all the reports at once. 



rm(list = ls())

#################### Function to combine the dataframes  ---------------------------------------------------

# Define the function to create master dataframes
create_master_dataframes <- function(base_dir = "C:/Users/georg/Desktop/databases/output/Ninety95s_dfs") {
  
  # Validate base directory
  if (!dir.exists(base_dir)) {
    stop("The base directory does not exist: ", base_dir)
  }
  
  # List all country directories
  country_dirs <- list.dirs(path = base_dir, full.names = TRUE, recursive = FALSE)
  
  if (length(country_dirs) == 0) {
    stop("No country directories found in the base directory: ", base_dir)
  }
  
  message("Found ", length(country_dirs), " country directories.")
  
  # Initialize a vector to hold all unique .rda filenames
  all_rda_filenames <- c()
  
  # Iterate through each country directory to collect .rda filenames
  for (country_dir in country_dirs) {
    rda_files <- list.files(path = country_dir, pattern = "\\.rda$", full.names = FALSE)
    all_rda_filenames <- unique(c(all_rda_filenames, rda_files))
  }
  
  message("Identified ", length(all_rda_filenames), " unique .rda file patterns.")
  
  # Define master dataframe names by removing the ".rda" extension
  master_names <- sub("\\.rda$", "", all_rda_filenames)
  
  # Initialize a list to store combined dataframes
  master_dataframes <- list()
  
  # Iterate through each unique .rda filename pattern
  for (i in seq_along(all_rda_filenames)) {
    rda_filename <- all_rda_filenames[i]
    master_name <- master_names[i]
    
    message("Processing: ", rda_filename, " -> ", master_name)
    
    # Initialize a list to hold dataframes from each country
    country_dfs <- list()
    
    # Iterate through each country to load the specific .rda file
    for (country_dir in country_dirs) {
      country_name <- basename(country_dir)
      file_path <- file.path(country_dir, rda_filename)
      
      if (file.exists(file_path)) {
        # Load the .rda file into a temporary environment
        temp_env <- new.env()
        loaded_objects <- load(file_path, envir = temp_env)
        
        # Handle multiple objects
        for (obj in loaded_objects) {
          df <- temp_env[[obj]]
          
          # Verify that the loaded object is a data frame
          if (!is.data.frame(df)) {
            warning("The object '", obj, "' in file '", file_path, "' is not a data frame. Skipping.")
            next
          }
          
          # Add a column to indicate the country
          df <- df %>% mutate(country = country_name)
          
          # Append to the list with a unique key to prevent duplicates
          unique_key <- paste0(country_name, "_", obj)
          country_dfs[[unique_key]] <- df
        }
      } else {
        warning("File does not exist for country ", country_name, ": ", file_path)
      }
    }
    
    # Combine all country dataframes for this pattern
    if (length(country_dfs) > 0) {
      combined_df <- bind_rows(country_dfs) %>% distinct()
      
      # Assign to master list with the original RDA filename (without extension)
      master_dataframes[[master_name]] <- combined_df
      
      message("Combined ", length(country_dfs), " dataframes into ", master_name, ".")
    } else {
      warning("No data found for pattern ", rda_filename, ". Skipping.")
    }
  }
  
  # Define the path to save master dataframes (same as base_dir)
  master_save_dir <- base_dir
  
  # Iterate through each master dataframe to save them with unique names
  for (master_name in names(master_dataframes)) {
    
    # Define the save path
    save_path <- file.path(master_save_dir, paste0(master_name, ".rda"))
    
    # Create a temporary environment and assign the dataframe directly
    temp_save_env <- new.env()
    assign(master_name, master_dataframes[[master_name]], envir = temp_save_env)
    
    # Save the dataframe with its unique name
    save(list = master_name, envir = temp_save_env, file = save_path)
    
    message("Saved master dataframe: ", save_path)
  }
  
  message("All master dataframes have been successfully created and saved in ", master_save_dir)
}

# Execute the function
create_master_dataframes()


# Example of loading one of the saved files - check to be sure code ran correctly 
load('C:/Users/georg/Desktop/databases/output/Ninety95s_dfs/combined_95_metrics_targets_peds.rda')


