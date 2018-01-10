
# extract_e -----------------------------------------------

#' Extract Entity number from EPDr objects
#' 
#' This function looks into EPDr objects (i.e., \code{\link[EPDr]{epd.entity-class}}
#' or \code{\link[EPDr]{epd.entity.df-class}} classes), returning the entity 
#' number of the objects.
#'
#' @param x epd.entity Any object of classes \code{\link[EPDr]{epd.entity-class}} or
#' \code{\link[EPDr]{epd.entity.df-class}}.
#'
#' @return numeric. Value of the entity number.
#' @examples
#' \dontrun{
#' epd.connection <- connectToEPD()
#' # To list all the tables in the database we have connected with
#' epd.1 <- get_entity(1, epd.connection)
#' extract_e(epd.1)
#' [1] 1
#' epd.45 <- get_entity(45, epd.connection)
#' extract_e(epd.45)
#' [1] 45
#' }
#' @rdname extract_e-method
#' @exportMethod extract_e
setGeneric("extract_e", function(x){
  standardGeneric("extract_e")
})

#' @rdname extract_e-method
#' @aliases extract_e-method
setMethod("extract_e", signature(x = "epd.entity"), function(x){
  e_ <- x@e_
  return(e_)
}
)
