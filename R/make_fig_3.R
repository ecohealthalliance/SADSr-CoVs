#' make figure showing areas of high pig and human density
#' in regions where bat habitat suitability is above a specified threshold
#' 
#' @title make_fig_3
#' @param threshold habitat suitability threshold from 0-1 OR "10p"
#' @returns 
#' @examples
#' make_fig_3(threshold = "10p")
#' 
make_fig_3 <- function(threshold){
  
  source("R/combine_binary.R")
  
  # mosaic individual species rasters at a given habitat suitability threshold
  combine_binary(threshold = threshold)
  
  bat_mosaic <- terra::rast(paste0("outputs/bat_mosaic_", threshold, ".tif"))
  
  # load country data for plotting
  countries <- geodata::world(path = "data_raw")
  
  # bat-pig overlap-------------------------------------------------------------
  
  # load pig count raster (number of pigs per pixel)
  pigs <- terra::rast("data_raw/GLW4/5_Pg_2015_Da.tif")
  # load area raster (to be able to calculate density)
  area <- terra::rast("data_raw/GLW4/8_Areakm.tif")
  
  # calculate density
  pig_dens <- pigs/area
  
  # crop for easier manipulation
  pig_dens_cropped <- terra::crop(pig_dens, ext(bat_mosaic))

  # resample the bat raster to the resolution of the pig density raster
  bats_resampled <- raster::resample(bat_mosaic, pig_dens_cropped, 
                                     method = "near")
  
  # by multiplying the rasters, get 0 for areas where bats aren't predicted
  # and get the pig density in areas where bats are predicted
  joint_pigs <- bats_resampled*pig_dens_cropped
  
  # what is average population density for pigs in this general region?
  mean(values(pig_dens_cropped), na.rm = T) # ~42
  
  joint_pigs[joint_pigs <= 40] <- NA
  
  # bin pig density into categories for improved visualization
  # right. logical. If TRUE, the intervals are closed on the right (and open on 
  # the left). If FALSE they are open at the right and closed at the left. 
  # "open" means that the extreme value is *not* included in the interval. Thus, 
  # right-closed and left open is (0,1] = {x | 0 < x <= 1}.
  joint_pigs_binned <- terra::classify(joint_pigs, 
                                       matrix(c(40, 150, 1,
                                                150, 250, 2, 
                                                250, 500, 3, 
                                                500, Inf, 4),
                                              ncol = 3, byrow = T),
                                       right = TRUE)
  
  # bat-human overlap-----------------------------------------------------------
  
  # global human population density
  pop_dens <- terra::rast("data_raw/gpw_v4/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif")
  
  # resample the population density raster to the resolution of the bat raster
  pop_dens_resampled <- raster::resample(pop_dens, bat_mosaic, method = "bilinear")
  
  # by multiplying the rasters, get 0 for areas where bats aren't predicted
  # and get the human density in areas where bats are predicted
  joint_humans <- bat_mosaic*pop_dens_resampled
  
  # what is average population density for humans in this region?
  mean(values(pop_dens_resampled), na.rm = T) # ~281
  
  joint_humans[joint_humans < 275] <- NA
  
  # bin human density into categories for improved visualization
  joint_humans_binned <- terra::classify(joint_humans, 
                                         matrix(c(275, 500, 1,
                                                  500, 1000, 2,
                                                  1000, 2000, 3,
                                                  2000, Inf, 4), 
                                                ncol = 3, byrow = T),
                                         right = TRUE)

  # make the figure-------------------------------------------------------------
  png(paste0("outputs/figures/Fig3_suitability_", threshold, ".png"), width = 6.75, 
      height = 12, units = "in", res = 600)
  
  # set color scheme
  myColors <- rev(brewer.pal(4, "RdYlBu"))
  
  par(mfrow = c(2, 1), mar = c(4.1, 4.1, 2.1, 0.1), oma = c(1, 2, 1, 1))
  
  # bat-pigs
  plot(joint_pigs_binned, col = myColors, xlab = "", ylab = "",  
       legend = FALSE)
  legend(x = 78, y = 3, legend = c("> 40 - 150", "> 150 - 250", "> 250 - 500", 
                                   "> 500"),
         fill = myColors, bty = "n", 
         title = expression(atop("Pig density", "(per sq. km)")))
  plot(countries, add = TRUE, col = "gray95", border = "gray70", fill = TRUE)
  mtext("A", side = 3, line = 1, cex = 1.5, adj = -0.15)
  mtext("Longitude", side = 1, line = 1.5)
  mtext("Latitude", side = 2, line = 1)
  plot(joint_pigs_binned, col = myColors, xlab = "", ylab = "",  
       legend = FALSE, add = TRUE)
  
  # bat-humans
  plot(joint_humans_binned, col = myColors, xlab = "", ylab = "", 
       legend = FALSE)
  legend(x = 78, y = 3, legend = c("> 275 - 500", "> 500 - 1000", 
                                   "> 1000 - 2000", "> 2000"),
         fill = myColors, bty = "n", 
         title = expression(atop("Human density", "(per sq. km)")))
  plot(countries, add = TRUE, col = "gray95", border = "gray70", fill = TRUE)
  mtext("B", side = 3, line = 1, cex = 1.5, adj = -0.15)
  mtext("Longitude", side = 1, line = 1.5)
  mtext("Latitude", side = 2, line = 1)
  plot(joint_humans_binned, col = myColors, xlab = "", ylab = "", 
       legend = FALSE, add = TRUE)
  
  dev.off()  

}

