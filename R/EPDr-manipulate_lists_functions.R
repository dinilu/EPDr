
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
#' spain <- lapply(e.list, get_entity, epd.connection)
#' spain.wo <- remove_wo_ages(spain)
#' length(spain.agedcounts)
#' length(spain.agedcounts.wo)
#' }
remove_wo_ages <- function(list){
  index <- which(vapply(list, check_default_chron, FUN.VALUE = logical(1)))
  list <- list[index]
  return(list)
}


# remove_wo_counts -------------------------------------------------------

#' Remove data without counts from a list of objects
#' 
#' This function is designed to work with list of
#' \code{\link[EPDr]{epd.entity.df}} objects.
#' The function parses all the elements on the list and remove those without 
#' counts data for any taxa.
#'
#' @param list List of \code{\link[EPDr]{epd.entity.df}} objects.
#'
#' @return List of \code{\link[EPDr]{epd.entity.df}} objects, which is a 
#' subset of 'list' with counts data.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                user="epdr", password="epdrpw")
#' e.list <- listE(epd.connection, country=c("France"))
#' e.list <- e.list$e_
#' france <- lapply(e.list, get_entity, epd.connection)
#' france.wo <- remove_wo_counts(france)
#' length(france)
#' length(france.wo)
#' }
remove_wo_counts <- function(list){
  index <- which(vapply(list, check_counts, FUN.VALUE = logical(1)))
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
#' @param na_value numeric Number indicating the value to be used for
#' taxa not previously present in the entities.
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
#' spain <- lapply(e.list, get_entity, epd.connection)
#' spain.ut <- unify_taxonomy(spain, get_taxonomy_epd(epd.connection))
#' colnames(spain[[1]]@commdf@counts)
#' colnames(spain.ut[[1]]@commdf@counts)
#' }
unify_taxonomy <- function(list, epd.taxonomy, na_value = 0){
  if (!all(lapply(list, class) %in% "epd.entity.df")){
    stop("'list' has to be a list of 'epd.entity.df' objects.
         See ?entityToMatrices function.")
  }
  taxa_list <- lapply(list, function(x){
    x@commdf@taxanames
    })
  taxa_list <- sort(unique(unlist(taxa_list)))
  list <- lapply(list, filter_taxa, taxa_list, epd.taxonomy, na_value)
  return(list)
}
