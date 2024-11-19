#' project optimal model to a new region
#' 
#' @title project_model
#' @param species_i Vector with scientific name of 1 or 2 species
#' @param threshold habitat suitability threshold from 0-1
#' @param thresh_10p logical. are you using 10 percentile threshold
#' @returns 
#' @examples
#' project_model(species_i = "Rhinolophus affinis", threshold = 0.75, thresh_10p = F)
#' 
project_model <- function(species_i, threshold, thresh_10p){
  
  # for loading and saving files
  if(length(species_i) == 1){
    file_save_name <- species_i
  }else{
    file_save_name <- paste0(species_i[1], "_", 
                             strsplit(species_i[2], " ")[[1]][2])
  }
  
  # define colourblind-friendly palette for maps:
  clrs <- hcl.colors(100, "viridis")
  
  # load cleaned occurrence records
  occs <- read.csv(paste0("data_processed/bat_locations/occ_cleaned_", 
                          file_save_name, ".csv"))
  
  # convert to a spatial vector map and plot it:
  occ_points <- terra::vect(occs, geom = c("longitude", "latitude"), 
                            crs = "EPSG:4326", keepgeom = TRUE)
  plot(occ_points, type = NULL)
  
  # add the countries map:
  countries <- geodata::world(path = "data_raw")
  plot(countries, add = TRUE, type = NULL)
  
  # load the uncut predictor stack 
  env_stack <- terra::rast("data_processed/env_stack_short.grd")
  # plot some layers to check they look OK:
  #plot(env_stack[[1:4]])
  
  # load cut predictor layers
  layers_cut <- terra::rast(paste0("data_processed/env_stack_", file_save_name, 
                                   ".tif"))
  # for outline plotting later
  mod_region <- terra::as.polygons(layers_cut * 0)
  
  # load the optimal model for the species
  best_mod <- readRDS(paste0("outputs/bestmod_", file_save_name, ".RDS"))
  
  # predict the model on the uncut predictor stack
  proj_full_continuous <- predict(env_stack, best_mod, type = "cloglog", 
                                  na.rm = TRUE)
  # save 
  writeRaster(proj_full_continuous, paste0("outputs/full_continuous_",
                                          file_save_name, ".tif"),
              overwrite = TRUE)
  
  # plot
  plot(proj_full_continuous, col = clrs, range = c(0, 1))
  plot(occ_points, add = T, col = "red", cex = 0.7)
  plot(countries, add = TRUE, type = NULL)
  # add modeling region to see preds vs obs in the extrapolation area
  plot(mod_region, lwd = 1, border = "red", add = TRUE)
  
  # restrict to areas with a chosen suitability value
  if(max(values(proj_full_continuous), na.rm = T) >= threshold){
    proj_full_binarized <- proj_full_continuous
    proj_full_binarized[proj_full_binarized >= threshold] <- 1
    proj_full_binarized[proj_full_binarized < threshold] <- NA
    
    plot(proj_full_binarized, col = clrs)
    plot(occ_points, add = T, col = "red", cex = 0.7)
    plot(countries, add = TRUE, type = NULL)
    # add modeling region to see predictions vs observations in the extrapolation area
    plot(mod_region, lwd = 1, border = "red", add = TRUE)
    
    if(thresh_10p){
      writeRaster(proj_full_binarized, paste0("outputs/full_binarized_",
                                              file_save_name, "_10p.tif"),
                  overwrite = TRUE)
    }else{
      writeRaster(proj_full_binarized, paste0("outputs/full_binarized_",
                                              file_save_name,  "_", threshold, 
                                              ".tif"),
                  overwrite = TRUE)
    }

  }else(print(paste0("No areas have >= ", threshold, 
                     " predicted habitat suitability")))
  
  
  # load IUCN range of the species as an extent to project the SDM to
  if(length(species_i) == 1){
    iucn_range <- terra::vect(paste0("data_raw/IUCN/", species_i, ".shp")) 
  }else{
    iucn_range_A <- terra::vect(paste0("data_raw/IUCN/", species_i[1], ".shp"))
    iucn_range_B <- terra::vect(paste0("data_raw/IUCN/", species_i[2], ".shp"))
    iucn_range <- rbind(iucn_range_A, iucn_range_B)
  }
  
  # now crop predictors by the IUCN range
  env_stack_iucn <- terra::crop(env_stack, iucn_range, mask = TRUE)
  
  # for outline plotting later
  iucn_region <- terra::as.polygons(env_stack_iucn * 0)
  
  proj_iucn_continuous <- predict(env_stack_iucn, best_mod, type = "cloglog", 
                                  na.rm = TRUE)
  
  writeRaster(proj_iucn_continuous, paste0("outputs/iucn_continuous_",
                                          file_save_name, ".tif"),
              overwrite = TRUE)
  
  # plot the continuous predictions
  png(paste0("outputs/figures/iucn_continuous_", file_save_name, ".png"), 
      width = 6.75, height = 6, units = "in", res = 600)
  plot(proj_iucn_continuous, col = clrs, range = c(0, 1))
  plot(iucn_region, type = NULL, border = "black", add = TRUE, lty = 2)
  plot(countries, add = T, type = NULL)
  plot(occ_points, add = T, col = "red", cex = 0.5)
  #plot(mod_region, lwd = 0.5, border = "red", add = TRUE)
  dev.off()
  
  # restrict to areas with a chosen suitability value
  if(max(values(proj_iucn_continuous), na.rm = T) >= threshold){
    proj_iucn_binarized <- proj_iucn_continuous
    proj_iucn_binarized[proj_iucn_binarized >= threshold] <- 1
    proj_iucn_binarized[proj_iucn_binarized < threshold] <- NA
    
    plot(proj_iucn_binarized, col = clrs)
    plot(occ_points, add = T, col = "red", cex = 0.7)
    plot(countries, add = TRUE, type = NULL)
    plot(mod_region, lwd = 1, border = "red", add = TRUE)
    
    if(thresh_10p){
      writeRaster(proj_iucn_binarized, paste0("outputs/iucn_binarized_",
                                              file_save_name, "_10p.tif"),
                  overwrite = TRUE)
    }else{
      writeRaster(proj_iucn_binarized, paste0("outputs/iucn_binarized_",
                                              file_save_name,  "_", threshold, 
                                              ".tif"),
                  overwrite = TRUE)
    }
    
    
  }else(print(paste0("No areas have >= ", threshold, 
                     " predicted habitat suitability")))
    
}


