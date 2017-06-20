## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message=FALSE, warning=FALSE)

## ----Install from github, eval=F-----------------------------------------
#  library(devtools)
#  install_github("dinilu/EPDr", force=T)

## ----Install from CRAN, eval=F-------------------------------------------
#  install.packages("EPDr")

## ----Setting up EPD server, eval=F---------------------------------------
#  vignette("EPD-PostgreSQL", package="EPDr")

## ----Loading EPDr--------------------------------------------------------
library(EPDr)

## ----Connect to local EPD------------------------------------------------
epdConnection <- connect_to_epd(database="epd", user="epdr", password="epdrpw")

## ----Connect to remote EPD, eval=F---------------------------------------
#  epdConnection <- connect_to_epd(database="epd", user="epdr", password="epdrpw", host="http://remote.epd.server")

## ----Test the connection-------------------------------------------------
library(DBI)
dbListTables(epdConnection)

## ----listCountries and listTaxagroups, R.options=list(max.print=15)------
list_countries(epdConnection)
list_taxagroups(epdConnection)

## ----listRegions, R.options=list(max.print=40)---------------------------
list_regions(epdConnection)
list_regions(epdConnection, country="Spain")

## ----listTaxa, R.options=list(max.print=20)------------------------------
list_taxa(epdConnection)
list_taxa(epdConnection, group_id="HERB")

## ----listSites, R.options=list(max.print=30)-----------------------------
list_sites(epdConnection)
list_sites(epdConnection, country="Spain", region="Andalucia")
list_sites(epdConnection, coords=c(-4, 10, 36, 40))

## ----listE, R.options=list(max.print=50)---------------------------------
list_e(epdConnection)
list_e(epdConnection, site="Adange")
list_e(epdConnection, lastname="Tzedakis")

## ----listE multiple countries, R.options=list(max.print=60)--------------
list_e(epdConnection, country=c("Spain", "Portugal", "France", "Switzerland", "Austria", "Italy", "Malta", "Algeria", "Tunisia", "Morocco", "Atlantic ocean", "Mediterranean Sea"))

## ----listPubl, R.options=list(max.print=10)------------------------------
list_publ(epdConnection)
list_publ(epdConnection, e_=1)

## ----getEntity getSite and getRestriction, eval=F------------------------
#  get_entity(1, epdConnection)
#  get_site(1, epdConnection)
#  get_restriction(1, epdConnection)

## ---- eval=F-------------------------------------------------------------
#  get_c14(1, epdConnection)
#  get_events(1, epdConnection)
#  get_chronology(1, epdConnection)
#  get_datation(1, epdConnection)
#  
#  get_ages(1, epdConnection)
#  
#  get_psamples(1, epdConnection)
#  get_entity(1, epdConnection)
#  
#  get_taxonomy_epd(epdConnection)

## ----export functions, eval=F--------------------------------------------
#  # Export functions ----
#  c14.clam <- export_c14(c14)
#  
#  # Extract functions ----
#  
#  # Standardize functions ----
#  
#  # Tabulate functions ----
#  
#  # Plotting functions ----
#  
#  
#  # Section 1 - Recalibrate chronologies ------------------------------------------
#  
#  # Site with one chronologies with EXTRA data
#  core4Clam(1, epdConnection)
#  clam("1")
#  
#  # Site with two chronologies, one with EXTRA data
#  core4Clam(4, epdConnection)
#  clam("4")
#  
#  entity.list <- listE(epdConnection, country=c("Spain", "Portugal", "France", "Switzerland", "Austria", "Italy", "Malta", "Algeria", "Tunisia", "Morocco", "Atlantic ocean", "Mediterranean Sea"))
#  entity.list <- listE(epdConnection)$e_
#  counts.all <- lapply(entity.list, getAgedCounts, epdConnection)
#  
#  
#  counts.po <- lapply(counts.all, filterTaxaGroups, c("HERB", "TRSH", "DWAR", "LIAN", "HEMI", "UPHE"))
#  counts.gi <- lapply(counts.po, gieseckeDefaultChronology)
#  counts.un <- removeRestricted(counts.gi)
#  counts.wa <- removeWithoutAges(counts.un)
#  
#  percent.wa <- lapply(counts.wa, trans2Percentages)
#  percent.int <- lapply(percent.wa, interpolateCounts, seq(0, 22000, by=1000))
#  percent.ran <- lapply(percent.wa, intervalsCounts, seq(0, 21000, by=1000), seq(999, 21999, by=1000))
#  
#  epd.taxonomy <- getTaxonomyEPD(epdConnection)
#  
#  counts.wa.acc <- lapply(counts.wa, taxa2AcceptedTaxa, epd.taxonomy)
#  percent.wa.acc <- lapply(percent.wa, taxa2AcceptedTaxa, epd.taxonomy)
#  percent.int.acc <- lapply(percent.int, taxa2AcceptedTaxa, epd.taxonomy)
#  
#  # counts.wa.hig <- lapply(counts.wa, taxa2HigherTaxa, epd.taxonomy)
#  # percent.wa.hig <- lapply(percent.wa, taxa2HigherTaxa, epd.taxonomy)
#  # percent.ran.hig <- lapply(percent.ran, taxa2HigherTaxa, epd.taxonomy)
#  # percent.int.hig <- lapply(percent.int, taxa2HigherTaxa, epd.taxonomy)
#  
#  counts.wa.uni <- unifyTaxonomy(counts.wa.acc, epd.taxonomy)
#  percent.wa.uni <- unifyTaxonomy(percent.wa.acc, epd.taxonomy)
#  percent.int.uni <- unifyTaxonomy(percent.int.acc, epd.taxonomy)
#  
#  
#  # Journals:
#  #   Quartenary International
#  
#  Cedrus <- c("Cedrus", "Cedrus atlantica", "Cedrus cf. C. atlantica", "Cedrus-type", "cf. Cedrus")
#  
#  
#  
#  
#  
#  mapTaxaAge(percent.unr.ranges,  Cedrus, "20000-22000", pres_abse=T, pollen_thres=0)(percent.unr.ranges,  Cedrus, "20000-22000", pres_abse=T, pollen_thres=0)
#  mapTaxaAge(percent.unr.ranges, Cedrus, "5500-6500", pres_abse=F, legend_range=c(0,5))
#  mapTaxaAge(percent.unr.ranges,  Cedrus, "20000-22000", pres_abse=F, legend_range=c(0,5))
#  
#  Pinus <- c("Pinus", "Pinus pinaster", "Pinus pinea", "Pinus sylvestris", "Pinus-type", "Pinus sp.")
#  mapTaxaAge(percent.unr.ranges, Pinus, "5500-6500", pres_abse=T)
#  mapTaxaAge(percent.unr.ranges, Pinus, "20000-22000", pres_abse=T)
#  mapTaxaAge(percent.unr.ranges, Pinus, "5500-6500", pres_abse=F)
#  mapTaxaAge(percent.unr.ranges, Pinus, "20000-22000", pres_abse=F)
#  
#  
#  
#  
#  mapTaxaAge(percent.int.uni, "Cedrus", "0", pres_abse=F, zoom_coords=c(-20, 180, 30, 80), points_pch=21,
#             points_colour=c("red", "red"), points_fill=c("red", "red"),
#             points_range_size=c(1, 1), map_title="EPD sites",
#             legend_range=NULL, legend_title=NULL, napoints_size=1, napoints_pch=21,
#             napoints_colour="red", napoints_fill="red", countries_fill_colour="grey80", countries_border_colour="grey90")
#  
#  
#  
#  entity.list <- sapply(counts.wa.uni, extractE)
#  datation.co.wa.uni <- lapply(entity.list, getDatation, epdConnection)
#  
#  entity.list <- sapply(percent.wa.uni, extractE)
#  datation.pe.wa.uni <- lapply(entity.list, getDatation, epdConnection)
#  
#  entity.list <- sapply(percent.ran.uni, extractE)
#  datation.pe.ran.uni <- lapply(entity.list, getDatation, epdConnection)
#  
#  entity.list <- sapply(percent.int.uni, extractE)
#  datation.pe.int.uni <- lapply(entity.list, getDatation, epdConnection)
#  
#  
#  # Aquí me quedo con el problema de que hay algunos e_ que tienen NA en las fechas y casca a la hora de calcular el indice de calidad
#  for(ii in 1:length(entity.list)){
#    for(ii in 44){
#      counts.wa.uni.q <- mapply(qualityIndex, counts.wa.uni[ii], datation.co.wa.uni[ii])
#    }
#    percent.wa.uni.q <- mapply(qualityIndex, percent.wa.uni, datation.pe.wa.uni)
#    percent.ran.uni.q <- mapply(qualityIndex, percent.ran.uni, datation.pe.ran.uni)
#    percent.int.uni.q <- mapply(qualityIndex, percent.int.uni, datation.pe.int.uni)
#  
#  
#  

## ----Disconnecting from the EPD database, eval=F-------------------------
#  disconnectFromEPD(epdConnection)

