#' combine binarized bat rasters
#' 
#' @title combine_binary
#' @param threshold habitat suitability threshold from 0-1 OR "10p"
#' @returns 
#' @examples
#' combine_binary(threshold = "10p")
#' 
combine_binary <- function(threshold){

  # get names of binarized habitat prediction rasters
  # ie. areas with > XX% habitat suitability, within the species IUCN range
  bat_ras_names <- list.files(path = "outputs/", 
                              pattern = paste0(threshold, ".tif"),
                              full.names = TRUE)
  bat_ras_names <- bat_ras_names[grep("iucn", bat_ras_names)]
  
  # import as a list of rasters
  bat_ras_list <- lapply(bat_ras_names, terra::rast)
  # combine using mosaic function
  bat_sprc <- terra::sprc(bat_ras_list)
  bat_mosaic <- terra::mosaic(bat_sprc)
  
  # save a plotted version
  png(paste0("outputs/figures/bat_mosaic_", threshold, ".png"), 
      width = 6.75, height = 6, units = "in", res = 600)
  plot(bat_mosaic)
  countries <- geodata::world(path = "data_raw")
  plot(countries, add = TRUE, type = NULL)  
  dev.off()
  
  # save 
  writeRaster(bat_mosaic, paste0("outputs/bat_mosaic_", threshold, ".tif"),
              overwrite = TRUE)
}

