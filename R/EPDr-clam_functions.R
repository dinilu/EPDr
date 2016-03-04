#' CLAM calibration with automatic postbomb zone selection
#'
#'  Recalibrate using CLAM but automagically capturing the coordinates of the core to get the right postbomb zone
#'  to be used in the calibration. Similar to the \code{\link[EPDr:clam]{clam}} function, \code{epdrCLAM} read calibration files
#'  from the following folder structure: \code{Cores/code_number}. 
#'
#' @param core_number Integer indicating the number of the core (entity) to be calibrated with CLAM
#' @param connection The connection to the EPD to get the geographical position of the core
#'
#' @return The same set of 
#' 
#' @export
#'
#' @examples
#' #TBW
epdrClam <- function(core_number, connection){
    sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", core_number, ";", sep="")
    site_num <- as.character(dbGetQuery(connection, sqlQuery))
    
    sqlQuery <-paste("SELECT * FROM siteloc WHERE site_=", site_num, ";", sep="")
    siteloc <- dbGetQuery(connection, sqlQuery)
    
    coord <- siteloc[, c("londd", "latdd")]     
    
    pb_zone <- over(SpatialPoints(coord), postbomb.map)
    if(pb_zone$Zone == "NH1") pb <- 1
    if(pb_zone$Zone == "NH2") pb <- 2
    if(pb_zone$Zone == "NH3") pb <- 3
    if(pb_zone$Zone == "SH12") pb <- 4
    if(pb_zone$Zone == "SH3") pb <- 5
    
    clam(as.character(core_number), postbomb=pb)
}
