## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## ----Connect to local EPD------------------------------------------------
library(EPDr)
epd.connection <- connect_to_epd(database = "epd", user = "epdr", password = "epdrpw")

## ----list_countries, R.options = list(max.print = 30)--------------------
list_countries(epd.connection)

## ----list_e, R.options = list(max.print = 30)----------------------------
e_ids <- list_e(epd.connection, country = c("Spain", "Portugal"))
e_ids <- e_ids$e_

## ----list_e 2, eval = FALSE----------------------------------------------
#  # e_ids <- list_e(epd.connection)
#  # e_ids <- e_ids$e_

## ----lapply(get_entity)--------------------------------------------------
epd_all <- lapply(e_ids, get_entity, epd.connection)
class(epd_all)
class(epd_all[[1]])

## ----lapply(entity_to_matrices)------------------------------------------
epd_all <- lapply(epd_all, entity_to_matrices)
class(epd_all[[1]])

## ----remove_restricted, R.options = list(max.print = 30)-----------------
length(epd_all)
sapply(epd_all, check_restriction)
epd_all <- remove_restricted(epd_all)
length(epd_all)

## ----remove_wo_ages, R.options = list(max.print = 30)--------------------
sapply(epd_all, check_default_chron)
epd_all <- remove_wo_ages(epd_all)
length(epd_all)

## ----lapply(extract_e)---------------------------------------------------
sapply(epd_all, extract_e)

## ----lapply(giesecke_default_chron)--------------------------------------
epd_all <- lapply(epd_all, giesecke_default_chron)

## ----lapply(filter_taxagroups)-------------------------------------------
epd_all <- lapply(epd_all, filter_taxagroups, c("HERB", "TRSH", "DWAR", "LIAN", "HEMI", "UPHE", "INUN"))

## ----lapply(taxa_to_accepted)--------------------------------------------
epd.taxonomy <- get_taxonomy_epd(epd.connection)
epd_all <- lapply(epd_all, taxa_to_acceptedtaxa, epd.taxonomy)

## ----unify_taxonomy------------------------------------------------------
epd_all <- unify_taxonomy(epd_all, epd.taxonomy)

## ----lapply(counts_to_percentage)----------------------------------------
epd_all <- lapply(epd_all, counts_to_percentage)

## ----lapply(interpolate_counts), R.options = list(max.print = 50)--------
epd_all <- lapply(epd_all, interpolate_counts, seq(0, 22000, by = 1000))
epd_all[[2]]@commdf@counts[, 1:7]

## ----standardize functions, eval = F-------------------------------------
#  
#  lapply(epd_all, table_by_taxa_age, c("Quercus"), c("1000", "2000"))
#  map_taxa_age(epd_all, "Pinus", "1000")
#  lapply(epd_all, blois_quality)
#  
#  Cedrus <- c("Cedrus", "Cedrus atlantica", "Cedrus cf. C. atlantica", "Cedrus-type", "cf. Cedrus")
#  
#  mapTaxaAge(percent.unr.ranges,  Cedrus, "20000-22000", pres_abse = T, pollen_thres = 0)(percent.unr.ranges,  Cedrus, "20000-22000", pres_abse = T, pollen_thres = 0)
#  mapTaxaAge(percent.unr.ranges, Cedrus, "5500-6500", pres_abse = F, legend_range = c(0,5))
#  mapTaxaAge(percent.unr.ranges,  Cedrus, "20000-22000", pres_abse = F, legend_range = c(0,5))
#  
#  Pinus <- c("Pinus", "Pinus pinaster", "Pinus pinea", "Pinus sylvestris", "Pinus-type", "Pinus sp.")
#  mapTaxaAge(percent.unr.ranges, Pinus, "5500-6500", pres_abse = T)
#  mapTaxaAge(percent.unr.ranges, Pinus, "20000-22000", pres_abse = T)
#  mapTaxaAge(percent.unr.ranges, Pinus, "5500-6500", pres_abse = F)
#  mapTaxaAge(percent.unr.ranges, Pinus, "20000-22000", pres_abse = F)
#  
#  
#  mapTaxaAge(percent.int.uni, "Cedrus", "0", pres_abse = F, zoom_coords = c(-20, 180, 30, 80), points_pch = 21,
#             points_colour = c("red", "red"), points_fill = c("red", "red"),
#             points_range_size = c(1, 1), map_title = "EPD sites",
#             legend_range = NULL, legend_title = NULL, napoints_size = 1, napoints_pch = 21,
#             napoints_colour = "red", napoints_fill = "red", countries_fill_colour = "grey80", countries_border_colour = "grey90")

