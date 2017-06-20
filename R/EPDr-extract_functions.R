
# extract_e -----------------------------------------------

#' Extract Entity number from EPDr objects
#' 
#' This function look into objects from the different EPDr classes (i.e.,
#' \code{\link[EPDr:epd.entity]{epd.entity}} or \code{\link[EPDr:epd.entity.df]{epd.entity.df}},
#' returning the entity number of the objects
#'
#' @param x Any object of classes \code{\link[EPDr:epd.entity]{epd.entity}} or
#' \code{\link[EPDr:epd.entity.df]{epd.entity.df}}.
#'
#' @return numeric. Value of the entity number.
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
#' # extract_e(data)
#' # [1] 1
#' # data <- getDatation(45, epd.connection)
#' # extract_e(data)
#' # [1] 45
#' # disconnectFromEPD(connection=epd.connection)
#' 
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
