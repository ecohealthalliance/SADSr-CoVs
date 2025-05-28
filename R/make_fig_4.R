#' make choropleth map to show the number of pigs and humans "at risk" by Chinese province
#' i.e. living in regions where bat habitat suitability is above a specified threshold
#' 
#' @title make_fig_4
#' @param threshold habitat suitability threshold from 0-1 OR "10p"
#' @returns 
#' @examples
#' make_fig_4(threshold = "10p")
#'
make_fig_4 <- function(threshold){
  
  # load map data---------------------------------------------------------------
  
  # get Chinese province data
  china <- geodata::gadm(country = "China", level = 1, path = "data_raw/")
  china_sf <- sf::st_as_sf(china)
  
  # get centers of provinces for labeling
  china_centroids <- terra::centroids(china)
  centroids_sf <- sf::st_as_sf(china_centroids)
  
  # Taiwan data
  taiwan <- geodata::gadm(country = "Taiwan", level = 0, path = "data_raw/")
  taiwan_sf <- sf::st_as_sf(taiwan)
  
  # bat distribution data
  bat_mosaic <- terra::rast(paste0("outputs/bat_mosaic_", threshold, ".tif"))
  
  # pigs------------------------------------------------------------------------
  
  # load pig count raster (number of pigs per pixel)
  pigs <- terra::rast("data_raw/GLW4/5_Pg_2015_Da.tif")
  
  # crop for easier manipulation
  pigs_cropped <- terra::crop(pigs, terra::ext(bat_mosaic))
  
  # resample the bat raster to the resolution of the pig raster
  bats_resampled <- raster::resample(bat_mosaic, pigs_cropped, method = "near")
  
  # mask the pig data by the bat data
  pigs_masked <- terra::mask(pigs_cropped, bats_resampled)
  
  # sum number of pigs within suitable bat habitat in each Chinese province
  pigs_by_province <- exactextractr::exact_extract(pigs_masked, china_sf, "sum")
  
  # add the number of pigs to the sf object
  china_sf$number_pigs <- pigs_by_province
  
  # change 0s to NAs
  china_sf <- china_sf %>%
    mutate(number_pigs = case_when(
      number_pigs == 0 ~ NA_real_,
      TRUE ~ number_pigs))
  
  sort(round(china_sf$number_pigs/1e6, 3))
  
  # only keep province names for provinces with pigs
  centroids_sf$prov_lab <- ""
  keepRows <- which(!is.na(china_sf$number_pigs), arr.ind = TRUE)
  centroids_sf$prov_lab[keepRows] <- centroids_sf$NAME_1[keepRows]
  
  # bin the pigs into categories for plotting
  china_sf <- china_sf %>% 
    dplyr::mutate(pigs_binned = case_when(
      number_pigs <= 5e6 ~ "<= 5",
      number_pigs > 5e6 & number_pigs <= 10e6 ~ "> 5 - 10",
      number_pigs > 10e6 & number_pigs <= 15e6 ~ "> 10 - 15",
      number_pigs > 15e6 & number_pigs <= 20e6 ~ "> 15 - 20",
      number_pigs > 20e6  ~ "> 20")) %>% 
    mutate_at(.vars = "pigs_binned", as.factor) %>% 
    dplyr::mutate(pigs_binned = fct_relevel(pigs_binned, "> 20", "> 15 - 20", 
                                            "> 10 - 15", "> 5 - 10", "<= 5"))
  
  taiwan_sf <- taiwan_sf %>%
    dplyr::mutate(pigs_binned = NA)
  
  # make the map
  # ggplot() +
  #   geom_sf(china_sf, mapping = aes(fill = pigs_binned), col = "gray20") +
  #   geom_sf(taiwan_sf, mapping = aes(fill = pigs_binned), col = "gray20") +
  #   scale_fill_viridis_d(name = "Pig population at risk\n(millions)", 
  #                        na.value = "gray70", direction = -1, 
  #                        begin = 0.15, end = 1) +
  #   labs(x = "Longitude", y = "Latitude") +
  #   geom_label_repel(data = centroids_sf, 
  #                    mapping = aes(label = prov_lab, geometry = geometry),
  #                    stat = "sf_coordinates",
  #                    size = 4, color = "black", alpha = 0.8,
  #                    segment.colour = "gray40", segment.size = 1, force = 5,
  #                    min.segment.length = 0, show.legend = FALSE) +
  #   theme_bw() +
  #   theme(legend.position = c(0.12, 0.8),
  #         legend.background = element_rect(fill = "transparent", color = NA),
  #         axis.text = element_text(color = "black")) -> f4a
  
  # new version of map 
  # plots the old map (to be able to use the same legend), 
  # then plots over it with a different map of China using the ggmapcn package
  ggplot() +
    geom_sf(china_sf, mapping = aes(fill = pigs_binned), col = "gray20") +
    geom_mapcn(fill = "gray70") +
    # fill is hard-coded because map data couldn't be accessed directly
    geom_mapcn(filter_attribute = "name_en", 
               filter = c("Zhejiang", "Hainan", "Hubei", "Guangxi", 
                          "Jiangsu", "Xizang", "Chongqing", "Fujian", 
                          "Guangdong", "Shanghai", "Jiangxi", "Yunnan", 
                          "Sichuan", "Anhui", "Hunan", "Shaanxi", 
                          "Guizhou"),
               fill = c("#2E6F8EFF", "#463480FF", "#1FA287FF", "#FDE725FF", 
                        "#463480FF", "#463480FF", "#1FA287FF", "#1FA287FF", 
                        "#FDE725FF", "#463480FF", "#72D056FF", "#1FA287FF",
                        "#2E6F8EFF", "#463480FF", "#FDE725FF", "#463480FF", 
                        "#1FA287FF")) +
    scale_fill_viridis_d(name = "Pig population at risk\n(millions)",
                         na.value = "gray70", direction = -1,
                         begin = 0.15, end = 1) +
    labs(x = "Longitude", y = "Latitude") +
    geom_label_repel(data = centroids_sf %>% 
                       dplyr::filter(!prov_lab == "Hong Kong"), 
                     mapping = aes(label = prov_lab, geometry = geometry),
                     stat = "sf_coordinates",
                     size = 4, color = "black", alpha = 0.8,
                     segment.colour = "gray40", segment.size = 1, force = 5,
                     min.segment.length = 0, show.legend = FALSE) +
    theme_bw() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.2, 0.2),
          legend.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(color = "black")) -> f4a
  
  # humans----------------------------------------------------------------------
  
  # load raster of human population count
  humans <- terra::rast("data_raw/gpw_v4/gpw_v4_population_count_rev11_2020_2pt5_min.tif")
  
  # crop for easier manipulation
  humans_cropped <- terra::crop(humans, ext(bat_mosaic))
  
  # resample the population count raster to the resolution of the bat raster
  humans_resampled <- raster::resample(humans_cropped, bat_mosaic, 
                                       method = "bilinear")
  
  # mask the human data by the bat data
  humans_masked <- terra::mask(humans_resampled, bat_mosaic)
  
  # calculate number of humans in each Chinese province
  humans_by_province <- exactextractr::exact_extract(humans_masked, china_sf, "sum")
  
  # add the number of humans to the sf object
  china_sf$number_humans <- humans_by_province
  
  # change 0s to NAs
  china_sf <- china_sf %>%
    mutate(number_humans = case_when(
      number_humans == 0 ~ NA_real_,
      TRUE ~ number_humans))
  
  sort(round(china_sf$number_humans/1e6, 3))
  
  # bin the pigs into categories for plotting
  china_sf <- china_sf %>% 
    dplyr::mutate(humans_binned = case_when(
      number_humans <= 10e6 ~ "<= 10",
      number_humans > 10e6 & number_humans <= 20e6 ~ "> 10 - 20",
      number_humans > 20e6 & number_humans <= 30e6 ~ "> 20 - 30",
      number_humans > 30e6 & number_humans <= 40e6 ~ "> 30 - 40",
      number_humans > 40e6  ~ "> 40")) %>% 
    mutate_at(.vars = "humans_binned", as.factor) %>% 
    dplyr::mutate(humans_binned = fct_relevel(humans_binned, "> 40", 
                                              "> 30 - 40", "> 20 - 30", 
                                              "> 10 - 20", "<= 10"))
  
  taiwan_sf <- taiwan_sf %>%
    dplyr::mutate(humans_binned = NA)
  
  # make the map
  # ggplot() +
  #   geom_sf(china_sf, mapping = aes(fill = humans_binned), col = "gray20") +
  #   geom_sf(taiwan_sf, mapping = aes(fill = humans_binned), col = "gray20") +
  #   scale_fill_viridis_d(name = "Human population at risk\n(millions)", 
  #                        na.value = "gray70", direction = -1, 
  #                        begin = 0.15, end = 1) +
  #   labs(x = "Longitude", y = "Latitude") +
  #   geom_label_repel(data = centroids_sf, 
  #                    mapping = aes(label = prov_lab, geometry = geometry),
  #                    stat = "sf_coordinates",
  #                    size = 4, color = "black", alpha = 0.8,
  #                    segment.colour = "gray40", segment.size = 1, force = 5,
  #                    min.segment.length = 0, show.legend = FALSE) +
  #   theme_bw() +
  #   theme(legend.position = c(0.13, 0.8),
  #         legend.background = element_rect(fill = "transparent", color = NA),
  #         axis.text = element_text(color = "black")) -> f4b
  
  # new version of map 
  # plots the old map (to be able to use the same legend), 
  # then plots over it with a different map of China using the ggmapcn package
  ggplot() +
    geom_sf(china_sf, mapping = aes(fill = humans_binned), col = "gray20") +
    geom_mapcn(fill = "gray70") +
    # fill is hard-coded because map data couldn't be accessed directly
    geom_mapcn(filter_attribute = "name_en", 
               filter = c("Zhejiang", "Hainan", "Hubei", "Guangxi", 
                          "Jiangsu", "Xizang", "Chongqing", "Fujian", 
                          "Guangdong", "Shanghai", "Jiangxi", "Yunnan", 
                          "Sichuan", "Anhui", "Hunan", "Shaanxi", 
                          "Guizhou"),
               fill = c("#FDE725FF", "#463480FF", "#2E6F8EFF", "#72D056FF", 
                        "#1FA287FF", "#463480FF", "#2E6F8EFF", "#1FA287FF", 
                        "#FDE725FF", "#2E6F8EFF", "#72D056FF", "#2E6F8EFF",
                        "#2E6F8EFF", "#463480FF", "#FDE725FF", "#463480FF", 
                        "#1FA287FF")) +
    scale_fill_viridis_d(name = "Human population at risk\n(millions)",
                         na.value = "gray70", direction = -1,
                         begin = 0.15, end = 1) +
    labs(x = "Longitude", y = "Latitude") +
    geom_label_repel(data = centroids_sf %>% 
                       dplyr::filter(!prov_lab == "Hong Kong"), 
                     mapping = aes(label = prov_lab, geometry = geometry),
                     stat = "sf_coordinates",
                     size = 4, color = "black", alpha = 0.8,
                     segment.colour = "gray40", segment.size = 1, force = 5,
                     min.segment.length = 0, show.legend = FALSE) +
    theme_bw() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.2, 0.2),
          legend.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(color = "black")) -> f4b
  
  # make combined plot----------------------------------------------------------
  fig4 <- f4a / f4b + 
    plot_annotation(tag_levels = 'A')
  
  ggsave(paste0("outputs/figures/Fig4_suitability_", threshold, ".png"), fig4, 
         device = 'png', height = 12, width = 8,
         units = "in", dpi = 600)
  
}

