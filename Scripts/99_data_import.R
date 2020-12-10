# PROJECT: les_enfants
# AUTHORS: T Essam | A Chafetz | B kagniniwa
# PURPOSE: TX_NET_NEW Analysis focused on PEDS
# LICENSE: MIT
# DATE:    2020-12-10
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

  library(dplyr)
  library(googledrive)
  library(glamr)
  library(ICPIutilities)


# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  #GDrive location
    gdrive_path <- as_id("1ruCqI5kbS2MD9iHzwDggQF6d95sMz2XR")
  

# DOWNLOAD ----------------------------------------------------------------

  #get filename for downloading/saving
    msd_file <- drive_ls(gdrive_path, "Site_IM") %>%
      pull(name) 
    
  #download dataset from GDrive
    import_drivefile(gdrive_path, msd_file, si_path(), FALSE)

# IMPORT ------------------------------------------------------------------

  #save as rds
    read_msd(file.path(si_path(), msd_file), save_rds = TRUE, remove_txt = TRUE)
