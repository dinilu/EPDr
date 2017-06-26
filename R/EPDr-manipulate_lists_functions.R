
# remove_restricted --------------------------------------------------------

#' Remove restricted data from a list of objects
#'
#' This function is designed to work with list of EPDr objects
#' (\code{\link[EPDr]{epd.entity.df}}). The function parses all the elements
#' on the list and remove those with restriction on their use. See
#' \code{\link[EPDr]{check_restriction}}.
#'
#' @param list List of EPDr objects (\code{\link[EPDr]{epd.entity.df}}, or
#' \code{\link[EPDr]{epd.entity}}).
#'
#' @return List of EPDr objects.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' e.list <- list_e(epd.connection, country=c("Spain"))
#' e.list <- e.list$e_
#' epd.spain <- lapply(e.list, get_entity, epd.connection)
#' epd.spain.un <- remove_restricted(epd.spain)
#' length(epd.spain)
#' length(epd.spain.un)
#' }
remove_restricted <- function(list){
  index <- which(!vapply(list, check_restriction, FUN.VALUE = logical(1)))
  list <- list[index]
  return(list)
}



# remove_wo_ages -------------------------------------------------------

#' Remove data without ages from a list of objects
#' 
#' This function is designed to work with list of EPDr objects
#' (\code{\link[EPDr]{epd.entity.df}}, or
#' \code{\link[EPDr]{epd.entity}}).
#' The function parses all the elements on the list and remove those without a default
#' chronology (see \code{\link[EPDr]{check_default_chron}}).
#'
#' @param list List of EPDr objects (\code{\link[EPDr]{epd.entity.df}},
#' or \code{\link[EPDr]{epd.entity}})
#'
#' @return List of EPDr objects.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                user="epdr", password="epdrpw")
#' e.list <- listE(epd.connection, country=c("Spain"))
#' e.list <- e.list$e_
#' spain.agedcounts <- lapply(e.list, getAgedCounts, epd.connection)
#' spain.agedcounts.wo <- remove_wo_ages(spain.agedcounts)
#' length(spain.agedcounts)
#' length(spain.agedcounts.wo)
#' }
remove_wo_ages <- function(list){
  index <- which(vapply(list, check_default_chron, FUN.VALUE = logical(1)))
  list <- list[index]
  return(list)
}



# unify_taxonomy -----------------------------------------------------------

#' Unify taxonomy of counts in multiple objects
#' 
#' This function compares the taxa registered in a list of
#' \code{\link[EPDr]{epd.entity.df}} objects and expand their taxonomy (using
#' \code{\link[EPDr]{filter_taxa}}) to make them match. When neccesary the
#' function add empty columns (with \code{NA} values).
#'
#' @param list List of \code{\link[EPDr]{epd.entity.df}} objects to be modified.
#' @param epd.taxonomy Data frame with the taxonomy from the EPD as from the
#' \code{\link[EPDr]{get_taxonomy_epd}} function.
#'
#' @return list of \code{\link[EPDr]{epd.entity.df}} objects with taxonomy
#' modified.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                user="epdr", password="epdrpw")
#' e.list <- listE(epd.connection, country=c("Spain"))
#' e.list <- e.list$e_
#' spain.agedcounts <- lapply(e.list, getAgedCounts, epd.connection)
#' spain.agedcounts.ut <- unify_taxonomy(spain.agedcounts, get_taxonomy_epd(epd.connection))
#' colnames(spain.agedcounts[[1]]@counts@counts)
#' colnames(spain.agedcounts.ut[[1]]@counts@counts)
#' }
unify_taxonomy <- function(list, epd.taxonomy){
  if (!all(lapply(list, class) %in% "epd.entity.df")){
    stop("'list' has to be a list of 'epd.entity.df' objects.
         See ?entityToMatrices function.")
  }
  taxa_list <- lapply(list, function(x){
    x@commdf@taxanames
    })
  taxa_list <- sort(unique(unlist(taxa_list)))
  list <- lapply(list, filter_taxa, taxa_list, epd.taxonomy)
  return(list)
}
