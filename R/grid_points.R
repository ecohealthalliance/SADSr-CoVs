#' Function to make a dataframe of presence/absence data, 
#' gridded to the resolution of predictor rasters
#' 
#' @title grid_points
#' @param species_i Vector with scientific name of 1 or 2 species
#' @param mult.p multiplier factor for the number of absences relative to presences
#' @param seed seed for randomly choosing points 
#' @returns 
#' @examples
#' grid_points(species_i = "Rhinolophus affinis", mult.p = 50, seed = 42)
#' 
grid_points <- function(species_i, mult.p, seed = 42){
  
  # for loading and saving files
  if(length(species_i) == 1){
    file_save_name <- species_i
  }else{
    file_save_name <- paste0(species_i[1], "_", 
                             strsplit(species_i[2], " ")[[1]][2])
  }
  
  # load cleaned occurrence points
  occurrences <- read.csv(paste0("data_processed/bat_locations/occ_cleaned_", 
                                 file_save_name, ".csv"))
  
  # for plotting
  occ_points <- terra::vect(occurrences, 
                            geom = c("longitude", "latitude"), 
                            keepgeom = TRUE, 
                            crs = "EPSG:4326")
  
  # load species-specific predictor layers
  layers_cut <- terra::rast(paste0("data_processed/env_stack_", file_save_name, 
                                   ".tif"))
  
  # make df of species occurrence data gridded to resolution of predictor vars
  gridded_data <- fuzzySim::gridRecords(
    rst = layers_cut, 
    pres.coords = occurrences[ , c("longitude", "latitude")], 
    plot = F)

  # proportion of gridded records compared to all records
  nrow(subset(gridded_data, gridded_data$presence == 1)) / nrow(occurrences)
  
  head(gridded_data)
  
  # should be TRUE
  nrow(gridded_data) == sum(!is.na(values(layers_cut[[1]])))
  
  # plot the gridded records:
  # plot(layers_cut[[1]])
  # plot the absences (pixels without presence records):
  # points(subset(gridded_data, gridded_data$presence == 0, select = c("x", "y")), 
  #        col = "red", cex = 0.1)
  # # plot the presences (pixels with presence records):
  # points(subset(gridded_data, gridded_data$presence == 1, select = c("x", "y")), 
  #        col = "blue", cex = 0.6)
  
  # number of presences and absences
  table(gridded_data$presence)
  
  # to reduce large proportion of absences, 
  # randomly select mult.p * the number of presences as the number of absences
  gridded_short <- fuzzySim::selectAbsences(gridded_data, sp.cols = "presence", 
                                            coord.cols = c("x", "y"), 
                                            mult.p = mult.p, seed = seed, 
                                            plot = F)
  
  # plot again
  # plot(layers_cut[[1]])
  # points(subset(gridded_short, gridded_short$presence == 0, 
  #               select = c("x", "y")), col = "red", cex = 0.1)
  # points(subset(gridded_short, gridded_short$presence == 1, 
  #               select = c("x", "y")), col = "blue", cex = 0.6)
  
  # pick a small region with presences within which to look closer:
  # plot(occ_points)
  # plot(layers_cut[[1]], ext = c(99, 105, 1, 8)) # affinis
  # plot(layers_cut[[1]], ext = c(110, 115, 32, 36)) # pusillus
  # plot(layers_cut[[1]], ext = c(108, 111, 27, 31)) # sinicus/thomasi
  # plot(layers_cut[[1]], ext = c(98, 102, 3, 6)) # stheno
  # points(occ_points, cex = 0.5, col = "grey40")
  # points(gridded_short[gridded_short[ , "presence"] == 0, c("x", "y")], 
  #        col = "red", pch = 1, cex = 0.8)
  # points(gridded_short[gridded_short[ , "presence"] == 1, c("x", "y")], 
  #        col = "blue", pch = 20, cex = 0.8)
  # legend("bottomright", bg = "white", xpd = NA, bty = "n",
  #        legend = c(paste("N total =", nrow(gridded_short)),
  #                   paste("N pres =", sum(gridded_short$presence, na.rm = TRUE))))
  # title(species_i)
  
  # save the modelling dataframe to a .csv file on disk:
  write.csv(gridded_short, 
            paste0("data_processed/bat_locations/occ_gridded_short_", 
                   file_save_name, ".csv"), 
            row.names = FALSE)
}
