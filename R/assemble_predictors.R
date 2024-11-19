#' assemble set of cropped, masked predictor variables
#' for more info about downloading data (especially the large files), 
#' see "raw_data/data_sources.R"
#' 
#' @title assemble_predictors
#' @returns 
#' @examples
#' assemble_predictors()
#' 
assemble_predictors <- function(){
  
  # load and (initially) crop files---------------------------------------------
  
  # encompasses Asia
  newExt <- terra::ext(c(70, 140, -15, 50))
  
  # bioclimatic vars
  envs <- geodata::worldclim_global(var = "bio", res = 2.5, path = "data_raw/", 
                                    version = "2.1")
  elev <- geodata::worldclim_global(var = "elev", res = 2.5, path = "data_raw/", 
                                    version = "2.1")
  # combine the bioclimatic and elevation data
  envs <- c(envs, elev)
  envs_cropped <- terra::crop(envs, newExt)
  rm(envs); rm(elev)
  
  # global human population density for 2020
  pop_dens <- terra::rast("data_raw/gpw_v4/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif")
  pop_dens_cropped <- terra::crop(pop_dens, newExt)
  rm(pop_dens)
  
  # MODIS 2020 land cover
  subdatasets <- terra::sds("data_raw/MCD12C1.A2020001.061.2022172062638.hdf")
  # percent cover of each IGBP LC class
  percent_lc <- subdatasets[3]
  rm(subdatasets)
  # keep only the water and forest layers--likely most relevant for bats
  percent_lc <- percent_lc[[1:6]]
  names(percent_lc) <- c("percent_water bodies", 
                         "percent_evergreen_needleleaf_forests", 
                         "percent_evergreen_broadleaf_forests",
                         "percent_deciduous_needleleaf_forests",
                         "percent_deciduous_broadleaf_forests",
                         "percent_mixed_forests")
  percent_lc_cropped <- terra::crop(percent_lc, newExt)
  rm(percent_lc)
  # change CRS to that of the other files
  percent_lc_cropped <- project(percent_lc_cropped, "EPSG:4326")
  
  # nighttime lights
  ntl <- terra::rast("data_raw/VNL_v21_npp_2020_global_vcmslcfg_c202205302300.median.dat.tif")
  ntl_cropped <- terra::crop(ntl, newExt)
  rm(ntl)
  
  # karst
  karst_shp <- terra::vect("data_raw/WHYMAP_WOKAM/shp/whymap_karst__v1_poly.shp")
  karst_cropped <- terra::crop(karst_shp, newExt)
  rm(karst_shp)
  # rasterize using percent land cover as a template
  # important to set background to a value other than NA
  karst_cropped <- terra::rasterize(karst_cropped, percent_lc_cropped[[1]], 
                                    background = 0)
  
  # resample predictor vars so they all have same extent and resolution---------
  # (match to land cover raster)
  
  # bioclim vars
  envs_resampled <- terra::resample(envs_cropped, percent_lc_cropped[[1]], 
                                    method = "bilinear", threads = TRUE)
  rm(envs_cropped)
  
  # nighttime lights
  ntl_resampled <- terra::resample(ntl_cropped, percent_lc_cropped[[1]], 
                                   method = "bilinear", threads = TRUE)
  # mask by bioclim raster
  ntl_resampled <- terra::mask(ntl_resampled, envs_resampled[[1]])
  rm(ntl_cropped)
  
  # population density
  pop_dens_resampled <- terra::resample(pop_dens_cropped, 
                                        percent_lc_cropped[[1]], 
                                        method = "bilinear")
  # mask by bioclim raster
  pop_dens_resampled <- terra::mask(pop_dens_resampled, envs_resampled[[1]])
  rm(pop_dens_cropped)
  
  # also mask the percent land cover rasters by bioclim raster
  percent_lc_cropped <- terra::mask(percent_lc_cropped, envs_resampled[[1]])
  
  # calculate distance to karst-------------------------------------------------
  
  # first mask
  karst_masked <- terra::mask(karst_cropped, envs_resampled[[1]])
  rm(karst_cropped)
  # now perform distance calculation only for areas marked with 0
  # terra::distance was way too slow, so using whitebox instead
  # input needs to be a path to a file, not the file itself
  terra::writeRaster(karst_masked, "data_processed/karst_masked.tif", 
                     overwrite = T)
  rm(karst_masked)
  
  input <- "data_processed/karst_masked.tif"
  output <- "data_processed/dist_to_karst.tif"
  # https://www.whiteboxgeo.com/manual/wbt_book/available_tools/gis_analysis_distance_tools.html#EuclideanDistance
  # https://whiteboxr.gishub.org/reference/wbt_euclidean_distance.html
  whitebox::wbt_euclidean_distance(input = input, output = output)
  
  dist_karst <- terra::rast(output)
  
  #plot(dist_karst, main = "Euclidean distance to karst")
  # "Distance in the output image is measured in the same units as the horizontal units of the input image."
  
  # stack all layers------------------------------------------------------------
  
  env_stack <- c(envs_resampled, ntl_resampled, pop_dens_resampled, dist_karst, 
                 percent_lc_cropped)
  
  rm(list = c("envs_resampled", "ntl_resampled", "pop_dens_resampled", 
              "dist_karst", "percent_lc_cropped"))
  
  # if any layer has an NA value where others don't, set the others to NA as well
  noNAs <- sum(env_stack, na.rm = F)
  env_stack <- mask(env_stack, noNAs)
  rm(noNAs)
  
  # save the stack
  terra::writeRaster(env_stack, "data_processed/env_stack.grd", overwrite = T)
  
  # test for collinearity among predictors--------------------------------------
  
  # gives a matrix of Pearson correlation coefficients between predictor pairs
  # can be a little slow
  # cor_matrix <- as.matrix(ENMTools::raster.cor.matrix(env_stack))
  # which(cor_matrix >0.7|cor_matrix < -0.7, arr.ind = TRUE)
  
  # can also visualize with corrplot
  # rownames(cor_matrix) <- c(paste0("bio", 1:19), "elev", "ntl", "pop_dens", 
  #                           "dist_karst", "perc_water", "perc_evgrn_ndlf_forests", 
  #                           "perc_evgrn_brdlf_forests", "perc_decid_ndlf_forests", 
  #                           "perc_decid_brdlf_forests", "perc_mxd_forests")
  # colnames(cor_matrix) <- c(paste0("bio", 1:19), "elev", "ntl", "pop_dens", 
  #                           "dist_karst", "perc_water", "perc_evgrn_ndlf_forests", 
  #                           "perc_evgrn_brdlf_forests", "perc_decid_ndlf_forests", 
  #                           "perc_decid_brdlf_forests", "perc_mxd_forests")
  # corrplot::corrplot(cor_matrix, method = "number")
  
  # subset predictors so none remain with Pearson correlation of > abs(0.7)
  # prioritizing those with more relevant biological meaning
  env_stack_short <- terra::subset(env_stack, subset = c(1, 12, 15, 21:29))
  rm(env_stack)
  
  # double check that no strong correlations still exist
  # cor_matrix2 <- as.matrix(ENMTools::raster.cor.matrix(env_stack_short))
  # which(cor_matrix2 >0.7|cor_matrix2 < -0.7, arr.ind = TRUE)
  
  # plot all the vars
  # plot(env_stack_short)
  
  # percent_deciduous_needleleaf_forests is 0 across nearly all the study range
  # so drop that layer too
  env_stack_short <- terra::subset(env_stack_short, subset = c(1:9, 11:12))
  
  names(env_stack_short) <- c("bio1", "bio12", "bio15", "ntl", "pop_dens",
                              "dist_karst", "perc_water_bodies", 
                              "perc_evgrn_ndlf_forests",
                              "perc_evgrn_brdlf_forests",
                              "perc_decid_brdlf_forests", "perc_mxd_forests")
  
  # save the stack
  terra::writeRaster(env_stack_short, "data_processed/env_stack_short.grd", 
                     overwrite = T)
  
}
