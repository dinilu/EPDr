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

## ----list_countries and list_taxagroups, R.options=list(max.print=15)----
list_countries(epdConnection)
list_taxagroups(epdConnection)

## ----list_regions, R.options=list(max.print=40)--------------------------
list_regions(epdConnection)
list_regions(epdConnection, country="Spain")

## ----list_taxa, R.options=list(max.print=20)-----------------------------
list_taxa(epdConnection)
list_taxa(epdConnection, group_id="HERB")

## ----list_sites, R.options=list(max.print=30)----------------------------
list_sites(epdConnection)
list_sites(epdConnection, country="Spain", region="Andalucia")
list_sites(epdConnection, coords=c(-4, 10, 36, 40))

## ----list_e, R.options=list(max.print=50)--------------------------------
list_e(epdConnection)
list_e(epdConnection, site="Adange")
list_e(epdConnection, lastname="Tzedakis")

## ----list_e multiple countries, R.options=list(max.print=60)-------------
list_e(epdConnection, country=c("Spain", "Portugal", "France", "Switzerland", "Austria", "Italy", "Malta", "Algeria", "Tunisia", "Morocco", "Atlantic ocean", "Mediterranean Sea"))

## ----list_publ, R.options=list(max.print=10)-----------------------------
list_publ(epdConnection)
list_publ(epdConnection, e_=1)

## ----get_ functions, R.options=list(max.print=10)------------------------
ent.1 <- get_ent(1, epdConnection)
class(ent.1)
slotNames(ent.1)
site.1 <- get_site(1, epdConnection)
slotNames(site.1)
chron.1 <- get_chron(1, epdConnection)
slotNames(chron.1)
geochron.1 <- get_geochron(1, epdConnection)
slotNames(geochron.1)
samples.1 <- get_samples(1, epdConnection)
slotNames(samples.1)
epd.1 <- get_entity(1, epdConnection)
class(epd.1)

## ----epd.entity structure, R.options=list(max.print=10)------------------
epd.1@e_
slot(epd.1, "e_")
epd.1@postbombzone
epd.1@numberofchron
epd.1@isingiesecke
epd.1@defaultchron
epd.1@entity
epd.1@site
epd.1@geochron
epd.1@chron
epd.1@samples

## ----check functions-----------------------------------------------------
check_restriction(epd.1)
check_default_chron(epd.1)

## ----export_c14 function, R.options=list(max.print=10)-------------------
export_c14("clam", epd.1@geochron@c14, epd.1@geochron@geochron)
export_c14("bacon", epd.1@geochron@c14, epd.1@geochron@geochron)
export_c14("clam", epd.1)

## ----export_agebasis function, R.options=list(max.print=10)--------------
export_agebasis("clam", epd.1@chron@agebasis)
export_agebasis("bacon", epd.1)

## ----export_events and export_depths functions, R.options=list(max.print=10)----
export_events("clam", epd.1@chron@synevent, epd.1@chron@event)
export_events("bacon", epd.1)
export_depths(epd.1@samples@psamples)
export_depths(epd.1)

## ----export_entity function, eval=F, R.options=list(max.print=10)--------
#  export_entity("clam", epd.1)
#  
#  ##  Chronology has coincident data with C14 data and, hence, the later will be used
#  ##  C14 data:
#  ##  lab_ID	C14_age	cal_age	error	reserv.	depth	thickn.
#  ##  KIGI-350	 910	NA	 20	NA	 83	 5	
#  ##  KIGI-349	2420	NA	200	NA	118	 5	
#  ##  KIGI-348	2900	NA	190	NA	140	10	
#  ##
#  ##  Chronology data:
#  ##  lab_ID	C14_age	cal_age	error	reserv.	depth	thickn.
#  ##  E1_CH1_S2	 910	NA	1	NA	 83	NA	
#  ##  E1_CH1_S3	2420	NA	1	NA	118	NA	
#  ##  E1_CH1_S4	2900	NA	1	NA	140	NA	
#  ##
#  ##  Chronology has additional no-C14 data.
#  ##  Chronology data:
#  ##  lab_ID	C14_age	cal_age	error	reserv.	depth	thickn.
#  ##  E1_CH1_S1	   0	NA	1	NA	  2	NA	
#  ##  E1_CH1_S5	4000	NA	1	NA	200	NA
#  ##
#  ##  Incorporate these data to the chronology? (Yes: TRUE then Intro, No: FALSE then Intro)TRUE
#  ##
#  ##        lab_ID C14_age cal_age error reservoir depth thickness
#  ##  11 E1_CH1_S1       0      NA     1        NA     2        NA
#  ##  1   KIGI-350     910      NA    20        NA    83         5
#  ##  2   KIGI-349    2420      NA   200        NA   118         5
#  ##  3   KIGI-348    2900      NA   190        NA   140        10
#  ##  5  E1_CH1_S5    4000      NA     1        NA   200        NA

## ----entity_to_matrices function, R.options=list(max.print=20)-----------
epd.1 <- entity_to_matrices(epd.1)
slotNames(epd.1)
epd.1@commdf@counts

## ----standardize functions, eval=F---------------------------------------
#  epd.1 <- filter_taxagroups(epd.1, c("DWAR", "HERB", "LIAN", "TRSH", "UPHE", "INUN"))
#  epd.1 <- taxa_to_acceptedtaxa(epd.1.int, get_taxonomy_epd(epd.connection))
#  epd.1 <- taxa_to_highertaxa(epd.1.int, get_taxonomy_epd(epd.connection))
#  
#  epd.1 <- filter_taxa(epd.1, c("Aesculus", "Aluns", "Betula", "Corpinus", "Salix"), get_taxonomy_epd(epd.connection))
#  epd.1 <- counts_to_percentages(epd.1)
#  epd.1 <- giesecke_default_chron(epd.1)
#  epd.1.int <- interpolate_counts(epd.1, c(1000, 2000, 3000), method="linear")
#  intervals_counts(epd.1, c(0,1000,2000,3000), c(999, 1999, 2999, 3999))
#  blois_quality(epd.1)
#  
#  # Tabulate functions ----
#  table_by_taxa_age(epd.1, as.character(c(1:10)), "Pinus")
#  
#  # Plotting functions ----
#  plot_diagram(epd.1)
#  
#  
#  entity.list <- list_e(epdConnection, country=c("Spain", "Portugal", "France", "Switzerland", "Austria", "Italy", "Malta", "Algeria", "Tunisia", "Morocco", "Atlantic ocean", "Mediterranean Sea"))
#  entity.list <- list_e(epdConnection)$e_
#  counts.all <- lapply(entity.list, get_entity, epdConnection)
#  
#  counts.po <- lapply(counts.all, filter_taxagroups, c("HERB", "TRSH", "DWAR", "LIAN", "HEMI", "UPHE"))
#  counts.gi <- lapply(counts.po, giesecke_default_chron)
#  counts.un <- remove_restricted(counts.gi)
#  counts.wa <- remove_wo_ages(counts.un)
#  
#  # Extract functions ----
#  extract_e(epd.1)
#  
#  
#  percent.wa <- lapply(counts.wa, counts_to_percentage)
#  percent.int <- lapply(percent.wa, interpolate_counts, seq(0, 22000, by=1000))
#  percent.ran <- lapply(percent.wa, intervals_counts, seq(0, 21000, by=1000), seq(999, 21999, by=1000))
#  
#  epd.taxonomy <- get_taxonomy_epd(epdConnection)
#  
#  counts.wa.acc <- lapply(counts.wa, taxa_to_acceptedtaxa, epd.taxonomy)
#  percent.wa.acc <- lapply(percent.wa, taxa_to_acceptedtaxa, epd.taxonomy)
#  percent.int.acc <- lapply(percent.int, taxa_to_acceptedtaxa, epd.taxonomy)
#  
#  # counts.wa.hig <- lapply(counts.wa, taxa_to_highertaxa, epd.taxonomy)
#  # percent.wa.hig <- lapply(percent.wa, taxa_to_highertaxa, epd.taxonomy)
#  # percent.ran.hig <- lapply(percent.ran, taxa_to_highertaxa, epd.taxonomy)
#  # percent.int.hig <- lapply(percent.int, taxa_to_highertaxa, epd.taxonomy)
#  
#  counts.wa.uni <- unify_taxonomy(counts.wa.acc, epd.taxonomy)
#  percent.wa.uni <- unify_taxonomy(percent.wa.acc, epd.taxonomy)
#  percent.int.uni <- unify_taxonomy(percent.int.acc, epd.taxonomy)
#  
#  
#  
#  
#  
#  e <- list_e(epd.connection)$e_
#  entity.list <- lapply(e[1:100], get_entity, epd.connection)
#  vapply(entity.list, check_default_chron, FUN.VALUE=logical(1))
#  vapply(entity.list, check_restriction, FUN.VALUE=logical(1))
#  
#  remove_restricted(entity.list)
#  remove_wo_ages(entity.list)
#  
#  lapply(entity.list, extract_e)
#  lapply(entity.list, giesecke_default_chron)
#  
#  lapply(entity.list, entity_to_matrices)
#  
#  lapply(entity.list, filter_taxa_groups, c("DWAR", "HERB", "LIAN", "TRSH", "UPHE", "INUN"))
#  
#  lapply(entity.list, filter_taxa, c("Pinus","Quercus","Urticaceae"), get_taxonomy_epd(epd.connection))
#  lapply(entity.list, taxa_to_acceptedtaxa, get_taxonomy_epd(epd.connection))
#  lapply(entity.list, taxa_to_highertaxa, get_taxonomy_epd(epd.connection))
#  lapply(entity.list, counts_to_percentages)
#  lapply(entity.list, interpolate_counts, c(1000, 2000, 3000))
#  lapply(entity.list, intervals_counts, c(1000, 2000, 3000), c(2000, 3000, 4000))
#  unify_taxonomy(entity.list, get_taxonomy_epd(epd.connection))
#  lapply(entity.list, table_by_taxa_age, c("Quercus"), c("1000", "2000"))
#  map_taxa_age(entity.list, "Pinus", "1000")
#  lapply(entity.list, blois_quality)
#  
#  
#  
#  # Journals:
#  #   Quartenary International
#  
#  Cedrus <- c("Cedrus", "Cedrus atlantica", "Cedrus cf. C. atlantica", "Cedrus-type", "cf. Cedrus")
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
#  mapTaxaAge(percent.int.uni, "Cedrus", "0", pres_abse=F, zoom_coords=c(-20, 180, 30, 80), points_pch=21,
#             points_colour=c("red", "red"), points_fill=c("red", "red"),
#             points_range_size=c(1, 1), map_title="EPD sites",
#             legend_range=NULL, legend_title=NULL, napoints_size=1, napoints_pch=21,
#             napoints_colour="red", napoints_fill="red", countries_fill_colour="grey80", countries_border_colour="grey90")
#  

## ----Disconnecting from the EPD database, eval=F-------------------------
#  disconnectFromEPD(epdConnection)

