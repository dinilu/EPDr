## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## ----Install from github, eval = F---------------------------------------
#  library(devtools)
#  install_github("dinilu/EPDr", force = T)

## ----Install from CRAN, eval = F-----------------------------------------
#  install.packages("EPDr")

## ----Setting up EPD server, eval = F-------------------------------------
#  vignette("EPD-PostgreSQL", package = "EPDr")

## ----Loading EPDr--------------------------------------------------------
library(EPDr)

## ----Connect to local EPD------------------------------------------------
epd.connection <- connect_to_epd(database = "epd", user = "epdr", password = "epdrpw")

## ----Connect to remote EPD, eval = F-------------------------------------
#  epd.connection <- connect_to_epd(database = "epd", user = "epdr", password = "epdrpw", host = "http://remote.epd.server")

## ----Test the connection-------------------------------------------------
library(DBI)
dbListTables(epd.connection)

## ----list_countries and list_taxagroups, R.options = list(max.print = 15)----
list_countries(epd.connection)
list_taxagroups(epd.connection)

## ----list_regions, R.options = list(max.print = 40)----------------------
list_regions(epd.connection)
list_regions(epd.connection, country = "Spain")

## ----list_taxa, R.options = list(max.print = 20)-------------------------
list_taxa(epd.connection)
list_taxa(epd.connection, group_id = "HERB")

## ----list_sites, R.options = list(max.print = 30)------------------------
list_sites(epd.connection)
list_sites(epd.connection, country = "Spain", region = "Andalucia")
list_sites(epd.connection, coords = c(-4, 10, 36, 40))

## ----list_e, R.options = list(max.print = 50)----------------------------
list_e(epd.connection)
list_e(epd.connection, site = "Adange")
list_e(epd.connection, lastname = "Tzedakis")

## ----list_e multiple countries, R.options = list(max.print = 60)---------
list_e(epd.connection, country = c("Spain", "Portugal", "France", "Switzerland", "Austria", "Italy", "Malta", "Algeria", "Tunisia", "Morocco", "Atlantic ocean", "Mediterranean Sea"))

## ----list_publ, R.options = list(max.print = 10)-------------------------
list_publ(epd.connection)
list_publ(epd.connection, e_ = 1)

## ----get_ent, R.options = list(max.print = 10)---------------------------
ent.1 <- get_ent(1, epd.connection)
class(ent.1)
slotNames(ent.1)

## ----get_site, R.options = list(max.print = 10)--------------------------
site.1 <- get_site(1, epd.connection)
class(site.1)
slotNames(site.1)

## ----get_geochron, R.options = list(max.print = 10)----------------------
geochron.1 <- get_geochron(1, epd.connection)
class(geochron.1)
slotNames(geochron.1)

## ----get_chron, R.options = list(max.print = 10)-------------------------
chron.1 <- get_chron(1, epd.connection)
class(chron.1)
slotNames(chron.1)

## ----get_sample, R.options = list(max.print = 10)------------------------
samples.1 <- get_samples(1, epd.connection)
class(samples.1)
slotNames(samples.1)

## ----get_entity, R.options = list(max.print = 10)------------------------
epd.1 <- get_entity(1, epd.connection)
class(epd.1)
slotNames(epd.1)

## ----epd.entity structure, R.options = list(max.print = 10)--------------
epd.1@e_
slot(epd.1, "e_")
epd.1@postbombzone
epd.1@numberofchron
epd.1@isingiesecke
epd.1@defaultchron
slotNames(epd.1@entity)
slotNames(epd.1@site)
slotNames(epd.1@geochron)
slotNames(epd.1@chron)
slotNames(epd.1@samples)

## ----check_--------------------------------------------------------------
check_restriction(epd.1)
check_default_chron(epd.1)

## ----export_c14, R.options = list(max.print = 25)------------------------
export_c14("clam", epd.1@geochron@c14, epd.1@geochron@geochron)
export_c14("bacon", epd.1@geochron@c14, epd.1@geochron@geochron)
export_c14("clam", epd.1)

## ----export_agebasis, R.options = list(max.print = 10)-------------------
export_agebasis("clam", epd.1@chron@agebasis)
export_agebasis("bacon", epd.1)

## ----export_events, R.options = list(max.print = 10)---------------------
export_events("clam", epd.1@chron@synevent, epd.1@chron@event)
export_events("bacon", epd.1)

## ----export_depths, R.options = list(max.print = 10)---------------------
export_depths(epd.1@samples@psamples)
export_depths(epd.1)

## ----export_entity function, eval = F, R.options = list(max.print = 10)----
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

## ----entity_to_matrices, R.options = list(max.print = 20)----------------
epd.1 <- entity_to_matrices(epd.1)
slotNames(epd.1)

## ----epd.entity.df structure, R.options = list(max.print = 20)-----------
epd.1@countstype
epd.1@countsprocessing
epd.1@taxatype
epd.1@taxaprocessing

## ----samplesdf structure, R.options = list(max.print = 20)---------------
slotNames(epd.1@samplesdf)

## ----agesdf structure, R.options = list(max.print = 20)------------------
slotNames(epd.1@agesdf)

## ----commdf structure, R.options = list(max.print = 20)------------------
slotNames(epd.1@commdf)

## ----nopodf structure, R.options = list(max.print = 20)------------------
slotNames(epd.1@nopodf)

## ----get_taxonomy_epd, R.options = list(max.print = 60)------------------
epd.taxonomy <- get_taxonomy_epd(epd.connection)
epd.taxonomy

## ----taxa_to_acceptedtaxa, R.options = list(max.print = 50)--------------
epd.1@commdf@taxanames
epd.1 <- taxa_to_acceptedtaxa(epd.1, epd.taxonomy)
epd.1@commdf@taxanames

## ----taxa_to_highertaxa, R.options = list(max.print = 50)----------------
epd.1@commdf@taxanames
epd.1.ht <- taxa_to_highertaxa(epd.1, epd.taxonomy)
epd.1.ht@commdf@taxanames

## ----taxa_to_highertaxa @counts, R.options = list(max.print = 300)-------
rowSums(epd.1@commdf@counts)
rowSums(epd.1.ht@commdf@counts)

## ----filter_taxagroups, R.options = list(max.print = 40)-----------------
epd.1 <- filter_taxagroups(epd.1, c("DWAR", "HERB", "LIAN", "TRSH", "UPHE", "INUN"))
epd.1@commdf@taxanames
rowSums(epd.1@commdf@counts)

## ----filter_taxa, R.options = list(max.print = 40)-----------------------
epd.1.ft <- filter_taxa(epd.1, c("Alnus", "Artemisia", "Betula", "Carpinus betulus", "Corylus"), epd.taxonomy)
head(epd.1.ft@commdf@counts)

## ----filter_taxa misspelling, R.options = list(max.print = 40)-----------
epd.1.ft <- filter_taxa(epd.1, c("Aluns", "Artemisia", "Betula", "Carpinus betulus", "Carylus"), epd.taxonomy)
head(epd.1.ft@commdf@counts)

## ----counts_to_percentage, R.options = list(max.print = 40)--------------
epd.1@countstype
epd.1 <- counts_to_percentage(epd.1)
epd.1@countstype
head(epd.1@commdf@counts)

## ----counts_to_percentage check, R.options = list(max.print = 40)--------
rowSums(epd.1@commdf@counts)

## ----giesecke_default_chron----------------------------------------------
epd.1@defaultchron
epd.1@numberofchron
epd.1@isingiesecke
epd.1 <- giesecke_default_chron(epd.1)
epd.1@defaultchron

## ----interpolate_counts--------------------------------------------------
epd.1.int <- interpolate_counts(epd.1, c(0, 1000, 2000, 3000, 4000, 5000), method = "linear")

## ----interpolate_counts check @commdf@counts, R.options = list(max.print = 50)----
epd.1@commdf@counts[, 1:5]
epd.1.int@commdf@counts[, 1:5]

## ----interpolate_counts check @countsprocessing--------------------------
epd.1@countsprocessing
epd.1.int@countsprocessing

## ----interpolate_counts check @agesdf, R.options = list(max.print = 18)----
epd.1@agesdf
epd.1.int@agesdf

## ----interpolate_counts check @samplesdf, R.options = list(max.print = 20)----
epd.1@samplesdf
epd.1.int@samplesdf

## ----intervals_counts----------------------------------------------------
epd.1.ran <- intervals_counts(epd.1, c(0,1000,2000,3000, 4000, 5000), c(999, 1999, 2999, 3999, 4999, 5999))
epd.1.ran@commdf@counts[, 1:5]

## ----intervals_counts check @countsprocessing----------------------------
epd.1.ran@countsprocessing

## ----intervals_counts check @agesdf, R.options = list(max.print = 18)----
epd.1.ran@agesdf

## ----intervals_counts check @samplesdf, R.options = list(max.print = 20)----
epd.1.ran@samplesdf

## ----blois_quality, R.options = list(max.print = 20)---------------------
epd.1 <- blois_quality(epd.1)
epd.1.int <- blois_quality(epd.1.int)
epd.1@agesdf@dataquality
epd.1.int@agesdf@dataquality

## ----taxa_by_taxa_age, R.options = list(max.print = 20)------------------
table_by_taxa_age(epd.1, c("Pinus", "Quercus"), as.character(c(1:10)))
table_by_taxa_age(epd.1.int, "Quercus", c("1000", "2000", "3000"))

## ----plot_diagram, fig.width = 7, fig.height = 9, fig.align = "center"----
plot_diagram(epd.1)
plot_diagram(epd.1.int)

## ----disconnect_from_epd-------------------------------------------------
disconnect_from_epd(epd.connection)

