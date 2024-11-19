# code to perform species distribution modeling and create associated figures
# for Latinne et al. "Diversity and spillover risk of Swine Acute Diarrhea 
# Syndrome and related coronaviruses in China and Southeast Asia"

# source packages and functions-------------------------------------------------
source("packages.R")
source("R/assemble_predictors.R")
source("R/gbif_request.R") 
source("R/set_modeling_region.R")
source("R/grid_points.R")
source("R/run_ENM.R")
source("R/select_optimal.R")
source("R/project_model.R")
source("R/make_fig_3.R")
source("R/make_fig_4.R")

# preparatory steps (only need to be run once, not every time)------------------

# assemble set of predictor variables
assemble_predictors()

# download GBIF occurrence records
gbif_request()

# perform modeling steps for R. affinis-----------------------------------------

# create the study region for model training
set_modeling_region(species_i = "Rhinolophus affinis", load_points = FALSE,
                    buff_dist = 300e3, limit_by_countries = TRUE,
                    included_countries = c("China", "Hong Kong", "Laos", 
                                           "Malaysia", "Thailand", "Vietnam"))

# perform spatial thinning on occurrence points
grid_points(species_i = "Rhinolophus affinis", mult.p = 100, seed = 42)

# model tuning and cross validation (takes about 12 min with 12 cores)
run_ENM(species_i = "Rhinolophus affinis", block_size = 100e3, k = 5, seed = 84, 
        fc = c("L", "LQ", "H", "LQH"), rm = seq(1, 5, by = 0.5), 
        algorithm = "maxent.jar", parallel = TRUE)

# select the optimal model from the set of candidate models
# and save 10th percentile threshold for later binarization of model predictions
Ra_thresh <- select_optimal(species_i = "Rhinolophus affinis",
                            calculate_10p = T)

# make 0-1 predictions from optimal model and produce binary map of 
# suitable/unsuitable habitat based on a specified threshold
project_model(species_i = "Rhinolophus affinis", 
              threshold = round(Ra_thresh, 2), thresh_10p = T)

# perform modeling steps for R. pusillus----------------------------------------

set_modeling_region(species_i = "Rhinolophus pusillus", load_points = FALSE, 
                    buff_dist = 300e3, limit_by_countries = TRUE,
                    included_countries = c("China", "Hong Kong", "Laos", 
                                           "Vietnam"))

grid_points(species_i = "Rhinolophus pusillus", mult.p = 100, seed = 42)

# 6 min
run_ENM(species_i = "Rhinolophus pusillus", block_size = 100e3, k = 5, seed = 84, 
        fc = c("L", "LQ", "H", "LQH"), rm = seq(1, 5, by = 0.5), 
        algorithm = "maxent.jar", parallel = TRUE)

Rp_thresh <- select_optimal(species_i = "Rhinolophus pusillus")

project_model(species_i = "Rhinolophus pusillus", 
              threshold = round(Rp_thresh, 2), 
              thresh_10p = T)

# perform modeling steps for R. sinicus/thomasi---------------------------------

set_modeling_region(species_i = c("Rhinolophus sinicus", "Rhinolophus thomasi"), 
                    load_points = FALSE, buff_dist = 250e3, 
                    limit_by_countries = TRUE,
                    included_countries = c("China", "Hong Kong", "Laos", 
                                           "Vietnam"))

grid_points(species_i = c("Rhinolophus sinicus", "Rhinolophus thomasi"),
            mult.p = 100, seed = 42)

# 7 min
run_ENM(species_i = c("Rhinolophus sinicus", "Rhinolophus thomasi"), 
        block_size = 75e3, k = 5, seed = 84, 
        fc = c("L", "LQ", "H", "LQH"), rm = seq(1, 5, by = 0.5), 
        algorithm = "maxent.jar", parallel = TRUE)

Rst_thresh <- select_optimal(species_i = c("Rhinolophus sinicus", 
                                           "Rhinolophus thomasi"))

project_model(species_i = c("Rhinolophus sinicus", "Rhinolophus thomasi"),
              threshold = round(Rst_thresh, 2), thresh_10p = T)

# perform modeling steps for R. stheno------------------------------------------

set_modeling_region(species_i = "Rhinolophus stheno", load_points = FALSE, 
                    buff_dist = 250e3, 
                    included_countries = c("Malaysia", "Laos", "Vietnam"))

grid_points(species_i = "Rhinolophus stheno", mult.p = 100, seed = 42)

# not proceeding further due to small number of presences (33)

# make figures 3 and 4 ---------------------------------------------------------

make_fig_3(threshold = "10p")

make_fig_4(threshold = "10p")
