#' Function to clean occurrence points for a given species
#' 
#' @title clean_points
#' @param species_i Vector with scientific name of 1 or 2 species
#' @returns 
#' @examples
#' clean_points(species_i = "Rhinolophus affinis)
#' 
clean_points <- function(species_i){
  
  # for saving files later
  if(length(species_i) == 1){
    file_save_name <- species_i
  }else{
    file_save_name <- paste0(species_i[1], "_", 
                             strsplit(species_i[2], " ")[[1]][2])
  }
  
  # function for converting dms data to decimal degree
  # https://github.com/AMBarbosa/unpackaged/blob/master/dms2dec
  source("https://raw.githubusercontent.com/AMBarbosa/unpackaged/master/dms2dec", 
         encoding = "UTF-8")
  
  ## GBIF
  # perform cleaning to get higher quality point data
  occs_gbif <- read.csv("data_raw/bat_locations/gbif_bat_data.csv") %>% 
    dplyr::filter(species %in% species_i) %>% 
    dplyr::rename(latitude = decimalLatitude, longitude = decimalLongitude) %>% 
    mutate(source = "GBIF")
  
  ## literature records (scraped manually, see data_raw/bat_locations/literature_PDFs)
  occs_lit_dms <- read.csv("data_raw/bat_locations/literature_records_dms.csv",
                           encoding = "UTF-8") %>% 
    dplyr::filter(species %in% species_i) %>% 
    mutate(latitude = dms2dec(latitude),
           longitude = dms2dec(longitude)) 
  
  occs_lit_dd <- read.csv("data_raw/bat_locations/literature_records_dd.csv") %>% 
    dplyr::filter(species %in% species_i)
  
  occs_lit <- bind_rows(occs_lit_dms, occs_lit_dd)
  
  ## DarkCideS data
  occs_drkcds <- read.csv("data_raw/bat_locations/DarkCideS_v4_Dataset2.csv") %>% 
    dplyr::rename(species = Species.name, latitude = Lattitude, 
                  longitude = Longitude, source = Record.source) %>% 
    # fixing incorrect record manually
    mutate(latitude = case_when(Cave.site == "An Tinh Cave" ~ 22.183333,
                                TRUE ~ latitude),
           longitude = case_when(Cave.site == "An Tinh Cave" ~ 106.033333,
                                 TRUE ~ longitude)) %>% 
    # fixing incorrectly transcribed record
    mutate(latitude = case_when(Cave.site == "Birenda cave A" ~ 28.17888889,
                                TRUE ~ latitude),
           longitude = case_when(Cave.site == "Birenda cave A" ~ 83.99250000,
                                 TRUE ~ longitude)) %>% 
    dplyr::filter(species %in% species_i) %>% 
    dplyr::select(species, latitude, longitude, source)
  
  ## PREDICT
  occs_predict <- read.csv("data_raw/bat_locations/PREDICT_Animals_Sampled.csv") %>% 
    dplyr::filter(ScientificNameToLowestKnownRank %in% species_i) %>%
    dplyr::filter(AnimalClass == "free-ranging wild animal") %>% 
    dplyr::select(species = "ScientificNameToLowestKnownRank",
                  latitude = "SampleLocationLatitude", 
                  longitude = "SampleLocationLongitude") %>% 
    mutate(source = "PREDICT") 
  
  # combine all occurrence records
  occs_all <- bind_rows(occs_gbif, occs_lit, occs_drkcds, occs_predict) %>% 
    distinct(latitude, longitude, .keep_all = TRUE)
  
  print(paste("Number of occurrence points before cleaning:", nrow(occs_all)))
  
  occs_cleaned <- occs_all %>% 
    # filter records where provided coordinate uncertainty is > 10km
    # but a lot of records don't provide uncertainty, so still keep those
    dplyr::filter(coordinateUncertaintyInMeters < 10000 | 
                    is.na(coordinateUncertaintyInMeters)) %>% 
    # perform set of cleaning steps
    CoordinateCleaner::clean_coordinates(lon = "longitude",
                                         lat = "latitude",
                                         tests = c("capitals", 
                                                   "centroids",
                                                   "duplicates",
                                                   "equal",
                                                   "gbif",
                                                   "outliers",
                                                   #"seas",
                                                   "zeros"),
                                         capitals_rad = 5000,
                                         centroids_rad = 500,
                                         value = "clean")
  
  print(paste("Number of occurrence points after cleaning:", nrow(occs_cleaned)))
  
  # plot(occs_cleaned$latitude ~ occs_cleaned$longitude)
  
  # remove points that are NA for any predictor var
  env_stack_short <- terra::rast("data_processed/env_stack_short.grd")
  extracted <- terra::extract(x = env_stack_short,
                              y = occs_cleaned[, c("longitude", "latitude")])
  # add extracted raster values to point data
  occs_cleaned <- bind_cols(occs_cleaned, extracted) %>% 
    dplyr::select(species, latitude, longitude, source, bio1:perc_mxd_forests)
  # drop rows with NAs
  occs_noNA <- occs_cleaned[complete.cases(occs_cleaned), ]
  
  # clear the row names
  row.names(occs_noNA) <- NULL
  
  print(paste("Number of occurrence points removed due to having NA values for 1+ predictors:",
              nrow(extracted) - nrow(occs_noNA)))
  
  print(paste("Number of occurrence points before gridding:",
              nrow(occs_noNA)))
  
  # for the species we are treating as a complex
  if(length(species_i) == 2){
    occs_noNA$species <- file_save_name
  }
  
  occs_noNA <- occs_noNA %>% 
    dplyr::select(species, latitude, longitude, source)
  
  # save the cleaned data to disk as a .csv file:
  write.csv(occs_noNA, paste0("data_processed/bat_locations/occ_cleaned_", 
                          file_save_name, ".csv"), 
            row.names = FALSE)
  
  return(occs_noNA)
  
}
