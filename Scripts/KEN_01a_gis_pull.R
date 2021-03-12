  #Purpose: PUll Kenya levels, save admin0-admin2 polygons and generate lat/lons
  
  library(tidyverse)
  library(glamr)
  
  
  # GLOBALS -----------------------------------------------------------------
  # Load configs
  load_secrets()
  country <-"Kenya"
  
  # Function to pull level number
  get_spdf <- function(geo_level, cntry = "Kenya") {
    
    # Grab the level of geography from df_lvls table
    lvls <- df_lvls %>%
      dplyr::filter(operatingunit == {{cntry}}) %>%
      pull({{geo_level}})
    
    # Pull out the ids for each row corresponding to geometry
    lvl_id <- df_locs %>%
      dplyr::filter(level == lvls) %>%
      pull(id)
    
    # Filter the PEPFAR polygon shapefile and return result
    spdf <- 
      spdf_pepfar %>% 
      dplyr::filter(uid %in% lvl_id)
    
    return(spdf)
  }
  
  # Geodata
  file_shp <- 
    list.files(
      path = shpdata, 
      pattern = "VcPepfarPolygons.shp$",
      recursive = TRUE,
      full.names = TRUE)
  
  file_shp <- file_shp %>% 
    sort()  %>% 
    dplyr::last()  
  
  # Grab DATIM info
  
  # PEPFAR Geodata
  spdf_pepfar <- file_shp %>% sf::read_sf()
  
  # OUs
  df_ous <- glamr::identify_ouuids(datim_user(), datim_pwd())
  
  # Levels
  df_lvls <- glamr::identify_levels(datim_user(), datim_pwd()) 
  
  # Orgs & Printing out Zambia output for quick review
  df_locs <- gisr::extract_locations(
    country,
    level = NULL,
    add_geom = TRUE,
    username = NULL,
    password = NULL
  )
  df_lvls %>% dplyr::filter(countryname == "Kenya")
  
  # Return Administrative uids (ou, )
  spdf_county_ken <- get_spdf("prioritization", cntry = "Kenya")
  spdf_ou_ken <- get_spdf("country_lvl", cntry = "Kenya")
  
  # Get a terrain map for Zambia
  terr_map <- gisr::terrain_map(country, adm0 = spdf_ou_ken, terr = rasdata, mask = T)
  
  msd_geo <- 
    distinct(psnuuid, psnu, operatingunit, operatingunituid, snu1)
  
  spdf_county_ken <-  
    left_join(spdf_county_ken, msd_geo, by = c("uid" = "psnuuid")) %>% 
    glamr::clean_psnu()
  
  # Clean up workspace.
  remove(df_locs, df_lvls, df_ous, spdf_pepfar)  
  
  
  
