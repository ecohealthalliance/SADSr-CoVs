# SADSr-CoVs
Code to perform species distribution modeling for Latinne et al. "Diversity and spillover risk of Swine Acute Diarrhea Syndrome and related coronaviruses in China and Southeast Asia"

*Explanation of code organization:*
The `main_run.R` script performs the species distribution modeling described in 
the manuscript. It begins by loading packages (you will need to install them if 
you do not already have them) and sourcing functions in the R folder. The script 
next calls `assemble_predictors.R`, which assembles a set of predictor variables
(e.g. land cover, bioclimatic data). Some of the data files used in this 
function are very large and/or have restrictions around their use, so you need 
to download these on your own. Details on the original data sources can be found 
in `data_raw/data_sources.R`. The main script next calls `gbif_request.R`, which 
shows how a GBIF request was made to obtain occurrence records for the bat 
species of interest. These data can be also be downloaded 
[here](https://doi.org/10.15468/dl.5xmk3k). For each of the bat species, the 
same five functions are called to perform various steps of the species 
distribution modeling process: 1) `set_modeling_region.R` creates the geographic
study region for model training, 2) `grid_points.R` performs spatial thinning on 
occurrence points, 3) `run_ENM.R` performs model tuning and cross-validation, 4) 
`Ra_thresh.R` selects the optimal model from the set of candidate models and 
calculates a tenth percentile training presence threshold for later use, and 5) 
`project_model.R` produces a continuous estimate of habitat suitability from 
0-1, and produces a binary map of suitable vs unsuitable habitat based on the 
threshold calculated earlier. Finally, Figures 3 and 4 of the manuscript are 
generated with `make_fig_3.R` and `make_fig_4.R` respectively.