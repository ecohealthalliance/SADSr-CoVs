#' Function to delineate the modeling region for a species
#' 
#' @title set_modeling_region
#' @param species_i Vector with scientific name of 1 or 2 species
#' @param load_points logical. if TRUE, load a previously cleaned and saved set of points
#'                              if FALSE, clean the points from scratch and save as a file
#' @param buff_dist distance in meters to set as a buffer between occurrence points
#' @param limit_by_countries logical. if TRUE, limit the modeling region to only specified countries
#' @param included_countries vector of country names (should be well-surveyed), to restrict the modeling region to
#' @returns 
#' @examples
#' set_modeling_region(species_i = "Rhinolophus affinis", load_points = FALSE, 
#' buff_dist = 300e3, limit_by_countries = TRUE, included_countries = c("China", 
#' "Hong Kong", "Laos", "Malaysia", "Thailand", "Vietnam"))
#' 
set_modeling_region <- function(species_i, load_points = FALSE, buff_dist, 
                                limit_by_countries = TRUE, included_countries){
  
  # for saving files later
  if(length(species_i) == 1){
    file_save_name <- species_i
  }else{
    file_save_name <- paste0(species_i[1], "_", 
                             strsplit(species_i[2], " ")[[1]][2])
  }
  
  if(load_points){
    # load previously cleaned points
    occurrences <- read.csv(paste0("data_processed/bat_locations/occ_cleaned_", 
                                   file_save_name, ".csv"))
  } else{
    source("r/clean_points.R")
    # go through the cleaning process from the start
    occurrences <- clean_points(species_i = species_i)
  }
  
  # load the predictor stack 
  env_stack <- terra::rast("data_processed/env_stack_short.grd")
  
  # load a map of countries
  countries <- geodata::world(path = "data_raw")
  
  # convert species occurrences table to a spatial object
  occ_points <- terra::vect(occurrences, geom = c("longitude", "latitude"), 
                            keepgeom = TRUE, crs = "EPSG:4326")
  
  # plot(occ_points, cex = 0.75)
  # plot(countries, add = TRUE)
  
  # select a modelling region using a buffer around presence points 
  pres_buff <- terra::buffer(occ_points, width = buff_dist)
  pres_buff <- terra::aggregate(pres_buff)
  
  # plot(pres_buff, col = "lightgrey", border = NA)
  # points(occurrences[ , c("longitude", "latitude")], pch = 20)
  # plot(countries, border = "darkgrey", add = TRUE)
  
  mod_region <- pres_buff
  
  if(limit_by_countries){
    
    # further limit modelling region to apparently well-surveyed countries:
    countries <- terra::disagg(countries)
    countries_with_points <- countries[occ_points, ]
    countries_with_points$NAME_0
    # plot(countries_with_points, border = "tan")
    # plot(occ_points, col = "red", cex = 0.5, add = TRUE)
    # text(countries_with_points, countries_with_points$NAME_0, halo = TRUE)
    # judge if some countries are visibly insufficiently surveyed (in this dataset)
    # select countries for modeling
    mod_region <- terra::crop(mod_region, 
                              subset(countries_with_points, 
                                     countries_with_points$NAME_0 %in% included_countries))
    
    # also crop out the dangling eastward parts
    ext_reg <- ext(mod_region)
    mod_region <- terra::crop(mod_region, ext(94, ext_reg[2], ext_reg[3], ext_reg[4]))
  }
  
  # plot the modelling region:
  # plot(pres_buff, col = "lightgrey", border = NA)
  # points(occurrences[ , c("longitude", "latitude")], pch = 20)
  # plot(countries, border = "darkgrey", add = TRUE)
  # plot(mod_region, border = "blue", lwd = 3, add = TRUE)
  
  # crop and the predictor stack by the chosen modeling region
  env_stack_cut <- terra::crop(env_stack, mod_region, mask = TRUE)
  
  # checking that everything overlaps correctly
  #plot(env_stack_cut)
  # names(env_stack_cut)
  # plot(env_stack_cut[[1]])
  # plot(countries, add = TRUE)
  # plot(mod_region, border = "blue", lwd = 2, add = TRUE)
  # plot(occ_points, pch = 20, cex = 0.8, add = TRUE)
  
  # inspect species data vs. size of variables' pixels:
  # R affinis
  # plot(env_stack_cut[[1]], xlim = c(101, 104), ylim = c(2, 6))
  # points(occ_points, cex = 0.5)
  # plot(env_stack_cut[[1]], xlim = c(110, 113), ylim = c(33, 35))
  # points(occ_points, cex = 0.5)
  
  # R pusillus
  # plot(env_stack_cut[[1]], xlim = c(110, 113), ylim = c(33, 35))
  # points(occ_points, cex = 0.5)
  
  # R sinicus/thomasi
  # plot(env_stack_cut[[1]], xlim = c(107, 112), ylim = c(27, 31))
  # points(occ_points, cex = 0.5)
  
  # R stheno
  # plot(env_stack_cut[[1]], xlim = c(98, 103), ylim = c(3, 8))
  # points(occ_points, cex = 0.5)
  
  # save the final cropped predictor layers
  terra::writeRaster(env_stack_cut, 
                     paste0("data_processed/env_stack_", file_save_name, ".tif"), 
                     overwrite = TRUE)
}
