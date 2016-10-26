#' Title TBW
#'
#' @param object  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
extract_e <- function(object){
    if(!class(object) %in% c("counts", "agedcounts", "ages", "datation", "chronology")){
        stop("'object' has to be a list of 'counts', 'agedcounts', 'ages', 'datation', or 'chronology' objects.\n
             See ?getCounts, ?getAgedCounts, ?getAges, getDatation, ?getChronology functions.")
    }
    e_ <- object@e_
    return(e_)
}