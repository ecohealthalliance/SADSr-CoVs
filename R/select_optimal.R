#' select optimal model from candidate models
#' 
#' @title select_optimal
#' @param species_i Vector with scientific name of 1 or 2 species
#' @param calculate_10p logical. should 10 percentile training threshold be calculated?
#' @returns 
#' @examples
#' select_optimal(species_i = "Rhinolophus affinis")
#' 
select_optimal <- function(species_i, calculate_10p = T){
  
  # for loading and saving files
  if(length(species_i) == 1){
    file_save_name <- species_i
  }else{
    file_save_name <- paste0(species_i[1], "_", 
                             strsplit(species_i[2], " ")[[1]][2])
  }
  
  # load candidate models
  e <- readRDS(paste0("outputs/ENMevaluate_", file_save_name, ".RDS"))
  
  # define a colourblind-friendly palette for the maps:
  clrs <- hcl.colors(100, palette = "viridis")

  # visualize results from tuned models
  # plot average omission rates (top panel) and validation AUC (bottom panel) 
  evalplot.stats(e, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm")
  # without error bars for easier visualization
  evalplot.stats(e, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", 
                 error.bars = FALSE)
  
  # here have feature class on the x axis instead of rm
  evalplot.stats(e, stats = c("or.mtp", "auc.val"), color = "rm", x.var = "fc", 
                 error.bars = FALSE)
  
  # look at various validation metrics
  # https://search.r-project.org/CRAN/refmans/ENMeval/html/ENMevaluation.html
  res <- eval.results(e)
  head(res) 
  
  # save all model evaluation results
  write_csv(res, paste0("outputs/evaluation_", file_save_name, ".csv"))

  # select optimal model from candidate models using: "a sequential method that 
  # uses cross-validation results by selecting models with the lowest average 
  # test omission rate, and to break ties, with the highest average validation AUC"
  # https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html#select
  opt.seq <- res %>% 
    filter(or.10p.avg == min(or.10p.avg)) %>% 
    filter(auc.val.avg == max(auc.val.avg))
  
  print(opt.seq)
  
  # look at variable importance for optimal model
  var_contributions <- e@variable.importance[[opt.seq$tune.args]] %>% 
    arrange(desc(permutation.importance))
  
  print(var_contributions)
  
  # save the variable contributions also
  write_csv(var_contributions, paste0("outputs/var_contributions_", 
                                      file_save_name, ".csv"))
  
  # view the marginal response curves
  dismo::response(eval.models(e)[[opt.seq$tune.args]])
  
  # plot model predictions for optimal model
  pred.seq <- eval.predictions(e)[[opt.seq$tune.args]]
  plot(pred.seq, col = clrs)
  # and overlay the actual presence points
  points(eval.occs(e), pch = 20, col ="red")
  
  # extract optimal model from ENMevaluation object using tune.args
  best_mod <- eval.models(e)[[opt.seq$tune.args]]
  
  # and save
  saveRDS(best_mod, paste0("outputs/bestmod_", file_save_name, ".RDS"))
  
  # calculate 10 percentile threshold-------------------------------------------
  if(calculate_10p){
    
    # https://github.com/jamiemkass/ENMeval/issues/53
    source("R/utilities.R") #https://github.com/jamiemkass/ENMeval/blob/master/R/utilities.R
    
    # extract predicted values for all presence points
    pred.vals <- terra::extract(pred.seq, e@occs[, 1:2])
    
    thresh <- calc.10p.trainThresh(pred.vals)
    
    print(paste0("10 percentile threshold is ", round(thresh, 2)))
    
    return(thresh)    
  }

  }