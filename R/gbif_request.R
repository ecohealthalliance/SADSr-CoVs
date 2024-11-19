#' request and download GBIF occurrence data
#' 
#' @title gbif_request
#' @returns 
#' @examples
#' gbif_request()
#' 
gbif_request <- function(){
  
  # gbif username, password, and email are set in the .Renviron
  # need to have your own if you want to make a new data request
  # user <- Sys.getenv("GBIF_USER")
  # pwd <- Sys.getenv("GBIF_PWD")
  # email <- Sys.getenv("GBIF_EMAIL")
  
  # get species usage keys
  # hosts <- c("Rhinolophus affinis", "Rhinolophus pusillus", "Rhinolophus stheno",
  #            "Rhinolophus sinicus", "Rhinolophus thomasi")
  # 
  # gbif_taxon_keys <- taxize::get_gbifid_(sci = hosts, method = "backbone") %>%
  #   imap(~ .x %>% mutate(original_sciname = .y)) %>%
  #   bind_rows() %>%
  #   dplyr::filter(matchtype == "EXACT" & status == "ACCEPTED") %>%
  #   dplyr::pull(usagekey)
  
  # submit GBIF data download request
  # for this project, download was performed 10/4/23
  # rgbif::occ_download(
  #   pred_in("taxonKey", gbif_taxon_keys),
  #   pred("hasCoordinate", TRUE),
  #   pred("occurrenceStatus", "PRESENT"),
  #   pred_in("basisOfRecord", c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION",
  #                              "MATERIAL_SAMPLE", "OBSERVATION",
  #                              "PRESERVED_SPECIMEN")),
  #   pred_gte("year", 1973),
  #   format = "SIMPLE_CSV",
  #   user = user, pwd = pwd, email = email)
  
  # once the download request has completed, can download zip file
  gbif_bat_data_dl <- rgbif::occ_download_get("0002826-231002084531237",
                                              overwrite = T,
                                              path = "data_raw/bat_locations")
  # then unzip
  gbif_bat_data <- rgbif::occ_download_import(gbif_bat_data_dl,
                                              path = "data_raw/bat_locations")
  # then save
  write.csv(gbif_bat_data, "data_raw/bat_locations/gbif_bat_data.csv",
            row.names = FALSE)
  
}