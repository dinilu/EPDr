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

# Saving the data
devtools::use_data(IntCal13.14C, Marine13.14C, SHCal13.14C, postbomb_NH1.14C, postbomb_NH2.14C, postbomb_NH3.14C, postbomb_SH1_2.14C, postbomb_SH3.14C, internal=TRUE)
