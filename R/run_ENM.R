#' model fitting
#' 
#' @title run_ENM
#' @param species_i Vector with scientific name of 1 or 2 species
#' @param block_size size of spatial blocks in meters
#' @param k number of folds for k-fold cross validation
#' @param seed random seed for spatial blocking
#' @param fc vector of feature classes to evaluate
#' @param rm vector of regularization multipliers to evaluate
#'           higher values impose a stronger penalty on model complexity
#' @returns 
#' @examples
#' run_ENM(species_i = "Rhinolophus affinis", block_size = 100e3, k = 5, 
#' seed = 84, fc = c("L", "LQ", "H", "LQH", "LQHP"), 
#' rm = seq(1, 5, by = 0.5), algorithm = "maxent.jar", parallel = TRUE)
#' 
run_ENM <- function(species_i, block_size, k, seed = 84, fc, rm, 
                    algorithm = "maxent.jar", parallel = F){
  
  # for loading and saving files
  if(length(species_i) == 1){
    file_save_name <- species_i
  }else{
    file_save_name <- paste0(species_i[1], "_", 
                             strsplit(species_i[2], " ")[[1]][2])
  }
  
  # load gridded points
  dat <- read.csv(paste0("data_processed/bat_locations/occ_gridded_short_", 
                         file_save_name, ".csv"))
  
  # load a map of countries
  countries <- geodata::world(path = "data_raw")
  
  # for plotting
  pres_centroids <- terra::vect(dat[dat$presence == 1, ], 
                                geom = c("x", "y"), 
                                crs = "epsg:4326")
  
  # for plotting
  dat_points <- terra::vect(dat, geom = c("x", "y"), crs = "epsg:4326")
  
  # load species-specific predictor layers
  layers_cut <- terra::rast(paste0("data_processed/env_stack_", file_save_name, 
                                   ".tif"))
  
  # define a colourblind-friendly palette for the maps:
  clrs <- hcl.colors(100, palette = "viridis")
  
  # set up spatial blocks for cross-validation----------------------------------
  blocks <- blockCV::cv_spatial(x = dat_points, column = "presence", 
                                r = layers_cut[[1]], size = block_size, 
                                hexagon = TRUE, k = k, selection = "random", 
                                seed = seed, plot = F, biomod2 = F)
  
  # add the fold ID to the data frame
  dat$fold <- blocks$folds_ids
  
  # total rows per fold:
  (nt <- table(dat$fold))
  # presence rows per fold
  (np <- table(subset(dat, presence == 1, select = "fold")))
  
  # add the fold ID also to the spatial data frame and plot it:
  dat_points$fold <- blocks$folds_ids
  # par(mfrow = c(1, 1))  # 1 plot per window
  # plot(dat_points, "fold", col = hcl.colors(5, "TealRose"), cex = 0.5, 
  #      main = file_save_name)
  # plot(countries, add = TRUE)
  # plot(terra::vect(blocks$blocks), border = "darkgrey", add = TRUE)
  # points(subset(dat_points, dat_points$presence == 1), cex = 0.7)
  # legend("bottomright", legend = paste(1:k, " ", nt, " ", np), xpd = NA,
  #        cex = 0.8, text.col = "blue")
  
  # build ENM-------------------------------------------------------------------
  
  # define occurrences
  occs <- dat %>% 
    filter(presence == 1) %>% 
    dplyr::select(x, y)
  
  # define background points
  bg <- dat %>% 
    filter(presence == 0) %>% 
    dplyr::select(x, y)
  
  # define folds for occurrences
  occs.grp <- dat %>% 
    filter(presence == 1) %>% 
    pull(fold)
  
  # define folds for background points
  bg.grp <- dat %>% 
      filter(presence == 0) %>% 
      pull(fold)
  
  # iterate model building over all chosen parameter settings
  # can take several min depending on number of points
  e <- ENMeval::ENMevaluate(occs = occs, envs = layers_cut, bg = bg, 
                            algorithm = algorithm, partitions = "user", 
                            user.grp = list(occs.grp = occs.grp, 
                                            bg.grp = bg.grp),
                            tune.args = list(fc = fc, rm = rm),
                            parallel = parallel)
  
  # save model output
  saveRDS(e, paste0("outputs/ENMevaluate_", file_save_name, ".RDS"))

}
