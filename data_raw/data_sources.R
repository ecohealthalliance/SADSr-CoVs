# data sources

# Bat occurrence records--------------------------------------------------------

# GBIF species records
# Request was made and downloaded 10/4/23: see "R/gbif_request.R"
# Can also be downloaded at https://doi.org/10.15468/dl.5xmk3k
# Saved as "data_raw/bat_locations/gbif_bat_data.csv"

# DarkCideS 1.0
# Version 4 posted on 2022-02-17, downloaded 10/4/23
# Download from https://figshare.com/articles/dataset/Metadata_for_DarkCideS_1_0_a_global_database_for_bats_in_karsts_and_caves/16413405?file=34091939
# Saved as: "data_raw/bat_locations/DarkCideS_v4_Dataset2.csv"

# PREDICT Animals Sampled
# Updated July 20, 2021, downloaded 10/4/23
# Download from https://data.usaid.gov/Global-Health-Security-in-Development-GHSD-/PREDICT-Animals-Sampled/m2kn-w8j5
# Saved as: "data_raw/bat_locations/PREDICT_Animals_Sampled.csv"

# Other records were sourced from published literature
# These papers are saved as PDFs within data_raw/bat_locations
# The transcribed locations are saved as:
# "data_raw/bat_locations/literature_records_dd.csv"
# "data_raw/bat_locations/literature_records_dms.csv"

# IUCN bat shapefiles-----------------------------------------------------------

# R. affinis
# Download from https://www.iucnredlist.org/species/19522/21982358
# Last assessed 06 August 2019, downloaded 17 October 2023
# Saved as: "data_raw/IUCN/Rhinolophus_affinis.shp"

# R. pusillus
# Download from https://www.iucnredlist.org/species/85707059/21994916
# Last assessed 26 July 2018, downloaded 17 October 2023
# Saved as: "data_raw/IUCN/Rhinolophus_pusillus.shp"

# R. sinicus
# Download from https://www.iucnredlist.org/species/41529/22005184
# Last assessed 15 Sept. 2018, downloaded 17 October 2023
# Saved as: "data_raw/IUCN/Rhinolophus_sinicus.shp"

# R. thomasi
# Download from https://www.iucnredlist.org/species/19573/21990671
# Last assessed 31 Aug. 2018, downloaded 17 October 2023
# Saved as "data_raw/IUCN/Rhinolophus_thomasi.shp"

# R. stheno
# Download from https://www.iucnredlist.org/species/84383122/21991664
# Last assessed 31 Aug. 2018, downloaded 17 October 2023
# Saved as "data_raw/IUCN/Rhinolophus_stheno.shp"


# WorldClim v 2.1 climate data (2.5 min)----------------------------------------

# See "R/assemble_predictors.R" to download automatically or
# Download from https://www.worldclim.org/data/worldclim21.html
# Saved in the format of: "data_raw/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif"

# EOG Nighttime Light, 2020-----------------------------------------------------

# LARGE FILE ~11 gb
# Download from https://eogdata.mines.edu/nighttime_light/annual/v21/2020/
# Saved as: "data_raw/VNL_v21_npp_2020_global_vcmslcfg_c202205302300.median.dat.tif"

# World Karst Aquifer Map (WOKAM)-----------------------------------------------

# Download from https://www.whymap.org/whymap/EN/Maps_Data/Wokam/wokam_node_en.html
# Multiple files in the zip but we used karst_v1
# Saved as "data_raw/WHYMAP_WOKAM/shp/whymap_karst__v1_poly.shp"
# (need to save all the other files like .cpg, .dbf)

# MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 0.05Deg CMG V061------------

# Info here https://lpdaac.usgs.gov/products/mcd12c1v061/
# Multiple data layers but we used Land_Cover_Type_1_Percent
# Download from https://e4ftl01.cr.usgs.gov/MOTA/MCD12C1.061/2020.01.01/
# Saved as "data_raw/MCD12C1.A2020001.061.2022172062638.hdf"

# Global pig distribution in 2015 (dasymetric product)--------------------------

# Gridded Livestock of the World
# Download from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CIVCPB
# Saved as: "data_raw/GLW4/5_Pg_2015_Da.tif"

# Pixel area (to be able to calculate density)
# Download from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CIVCPB
# Saved as: "data_raw/GLW4/8_Areakm.tif"

# Global human population data, 2020--------------------------------------------

# SEDAC/CIESIN Gridded Population of the World v4

# Population density 
# Download from https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11/data-download
# Saved as: "data_raw/gpw_v4/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"

# Population count
# Download from https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download
# Saved as: "data_raw/gpw_v4/gpw_v4_population_count_rev11_2020_2pt5_min.tif"

