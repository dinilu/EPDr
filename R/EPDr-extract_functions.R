#' Extract Entity number from EPDr objects
#' 
#' This function look into objects from the different EPDr classes (i.e.,  \code{\link[EPDr:counts]{counts}},
#' \code{\link[EPDr:agedcounts]{agedcounts}}, \code{\link[EPDr:ages]{ages}}, \code{\link[EPDr:datation]{datation}},
#' and \code{\link[EPDr:chronology]{chronology}}), returning the entity number of the objects
#'
#' @param object Any object of classes \code{\link[EPDr:counts]{counts}},
#' \code{\link[EPDr:agedcounts]{agedcounts}}, \code{\link[EPDr:ages]{ages}}, \code{\link[EPDr:datation]{datation}},
#' or \code{\link[EPDr:chronology]{chronology}}.
#'
#' @return numeric. Value of the entity number.
#' 
#' @export
#'
#' @examples
#' ##NOT TO RUN
#' # Not run
#' # epd.connection <- connectToEPD()
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # epd.connection
#' 
#' # To list all the tables in the database we have connected with
#' # data <- getDatation(1, epd.connection)
#' # extractE(data)
#' # [1] 1
#' # data <- getDatation(45, epd.connection)
#' # extractE(data)
#' # [1] 45
#' # disconnectFromEPD(connection=epd.connection)
#' 
extractE <- function(object){
    if(!class(object) %in% c("counts", "agedcounts", "ages", "datation", "chronology")){
        stop("'object' has to be a list of 'counts', 'agedcounts', 'ages', 'datation', or 'chronology' objects.\n
             See ?getCounts, ?getAgedCounts, ?getAges, getDatation, ?getChronology functions.")
    }
    e_ <- object@e_
    return(e_)
}