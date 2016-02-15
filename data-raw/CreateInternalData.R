#' Calibration tables for 14C and postbomb calibrations
#'
#' Internal dataset with calibration tables for use in the calibration functions
#'
#' @format Tables with ...
#' \describe{
#'   \item{Column1}{Describir}
#'   \item{Column2}{Describir}
#'   \item{Column3}{Describir}
#'   ...
#' }
#' @source \url{http://chrono.qub.ac.uk/blaauw/clam.html}

# Loading all the data from the original CLAM data
IntCal13.14C <- read.table("data-raw/IntCal13.14C")
Marine13.14C <- read.table("data-raw/Marine13.14C")
SHCal13.14C <- read.table("data-raw/SHCal13.14C")
postbomb_NH1.14C <- read.table("data-raw/postbomb_NH1.14C")
postbomb_NH2.14C <- read.table("data-raw/postbomb_NH2.14C")
postbomb_NH3.14C <- read.table("data-raw/postbomb_NH3.14C")
postbomb_SH1_2.14C <- read.table("data-raw/postbomb_SH1-2.14C")
postbomb_SH3.14C <- read.table("data-raw/postbomb_SH3.14C")

#' Postbomb zones of the world
#'
#' Internal shapefile with postbomb zones of the world. This is mean to extract the data from the site points to correctly feed CLAM
#'
#' @format Shapefile
#' \describe{
#'   \item{Column1}{Describir}
#'   ...
#' }
#' @source \url{http://calib.qub.ac.uk/CALIBomb/}
#' @source \url{https://journals.uair.arizona.edu/index.php/radiocarbon/article/view/16177/pdf}
#' @source \reference{RADIOCARBON 55(4): 2059–2072. 2013. ATMOSPHERIC RADIOCARBON FOR THE PERIOD 1950–2010. Quan Hua, Mike Barbetti, Andrzej Z Rakowski}

library(maptools)

postbomb.map <- readShapePoly("data-raw/PostBombZones")

# Saving the data
devtools::use_data(IntCal13.14C, Marine13.14C, SHCal13.14C, postbomb_NH1.14C, postbomb_NH2.14C, postbomb_NH3.14C, postbomb_SH1_2.14C, postbomb_SH3.14C, postbomb.map, internal=TRUE)

