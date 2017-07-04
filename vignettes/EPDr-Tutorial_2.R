## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.width = 7)

## ----Connect to local EPD, eval = FALSE----------------------------------
#  epd.connection <- connect_to_epd(database = "epd",
#                                   user = "epdr",
#                                   password = "epdrpw")

## ----Connect to remote EPD, include = FALSE------------------------------
epd.connection <- connect_to_epd(database = "epd",
                                 user = "epdr",
                                 password = "epdrpw", 
                                 host = "rabbot19.uco.es")

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

