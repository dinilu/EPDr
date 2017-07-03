## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.width = 7)

## ----Connect to local EPD------------------------------------------------
library(EPDr)
epd.connection <- connect_to_epd(database = "epd",
                                 user = "epdr",
                                 password = "epdrpw")

## ----list_countries, R.options = list(max.print = 30)--------------------
list_countries(epd.connection)

## ----list_e, R.options = list(max.print = 30)----------------------------
e_ids <- list_e(epd.connection,
                country = c("Spain", "Portugal"))
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
vapply(epd_all, check_restriction, FUN.VALUE=logical(1))
epd_all <- remove_restricted(epd_all)
length(epd_all)

## ----remove_wo_ages, R.options = list(max.print = 30)--------------------
vapply(epd_all, check_default_chron, FUN.VALUE=logical(1))
epd_all <- remove_wo_ages(epd_all)
length(epd_all)

## ----lapply(extract_e)---------------------------------------------------
vapply(epd_all, extract_e, FUN.VALUE=numeric(1))

## ----lapply(giesecke_default_chron)--------------------------------------
epd_all <- lapply(epd_all, giesecke_default_chron)

## ----lapply(filter_taxagroups)-------------------------------------------
epd_all <- lapply(epd_all, filter_taxagroups,
                  c("HERB", "TRSH", "DWAR", "LIAN",
                    "HEMI", "UPHE", "INUN"))

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

## ----lapply(blois_quality), R.options = list(max.print = 10)-------------
epd_all[[1]]@agesdf@dataquality
epd_all <- lapply(epd_all, blois_quality)
epd_all[[1]]@agesdf@dataquality

## ----lapply(table_by_taxa_age), R.options = list(max.print = 20)---------
epd_tables <- lapply(epd_all,
                     table_by_taxa_age,
                     c("Quercus"), c("1000", "2000"))
epd_tables[[1]]

## ----lapply(table_by_taxa_age) 2, R.options = list(max.print = 20)-------
epd_table <- do.call(rbind, epd_tables)
epd_table

## ----map_taxa_age, R.options = list(max.print = 20)----------------------
map_taxa_age(epd_all, "Pinus", "1000")

## ----map_taxa_age 2, R.options = list(max.print = 70)--------------------
pinus <- c("Pinus", "Pinus diploxylon-type", "Pinus halepensis-type",
           "Pinus haploxylon-type", "Pinus nigra-type",
           "Pinus pinaster-type", "Pinus pinea-type")
map_taxa_age(epd_all, pinus, "1000")

## ----map_taxa_age 3, R.options = list(max.print = 20)--------------------
map_taxa_age(epd_all, pinus, "1000", pres_abse = T, pollen_thres = 0)

## ----map_taxa_age 4, R.options = list(max.print = 20)--------------------
map_taxa_age(epd_all, pinus, "1000", pres_abse = T, pollen_thres = 1)

