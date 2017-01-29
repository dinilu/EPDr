#' Tabulate counts by taxa and age
#' 
#' This function tabulate data from the \code{@@counts} slot in EPDr objects
#' (\code{\link[EPDr:agedcounts]{agedcounts}} or \code{\link[EPDr:counts]{counts}} to
#' summarize information for particular taxa at particular age or time intervals (samples).
#' This function is useful to reshape the data to be plotted and mapped by \code{link[ggplot]{ggplot}}. It was written to be used by \code{\link[EPDr:mapTaxaAge]{mapTaxaAge}} function.
#' 
#' @param agedcounts \code{\link[EPDr:agedcounts]{agedcounts}} object where to extract the 
#' data from.
#' @param sample_label Character vector indicating the ages or time intervals to be included
#' in the table.
#' @param taxa Character vector indicating the taxa to be included in the table. Several
#' taxa can be specified but the function returns data for only one taxa by summing all
#' counts, and the taxa name specified in the output is the first one in \code{taxa}.
#'
#' @return Data frame with five columns:
#' \itemize{
#' \item{"e_"}{Entity identification number.}
#' \item{"londd"}{Longitude of the site in decimal degrees.}
#' \item{"latdd"}{Latitude of the site in decimal degrees.}
#' \item{"count"}{The count of that taxon in that particular sample (age or time interval).}
#' \item{"sample_label"}{The sample (age or time interval) at which the particulates
#' were counted.}
#' \item{"taxa_label"}{The taxa that has been counted.}
#' }
#' 
#' @export
#'
#' @examples
#' # TBW
tableByTaxaAge <- function(agedcounts, sample_label, taxa){
  if(!all(taxa %in% agedcounts@counts@taxa_names)){
    stop("taxa has to be a valid taxon in agedcounts@counts@counts and agedcounts@counts@taxa_names")
  }
  if(!all(sample_label %in% agedcounts@counts@sample_label)){
    stop("sample_label has to be valid sample labels in agedcounts@counts@counts and agedcounts@counts@sample_label")
  }
  e_ <- agedcounts@e_
  londd <- agedcounts@site$londd
  latdd <- agedcounts@site$latdd
  count <- agedcounts@counts@counts[which(agedcounts@counts@sample_label %in% sample_label), which(agedcounts@counts@taxa_names %in% taxa)]
  if(class(count) == "data.frame"){
    count <- apply(count, MARGIN=1, FUN=sum)
  }
  if(length(count) == 0){count <- NA}
  output <- data.frame(e_, londd, latdd, count, sample_label, taxa[[1]])
  colnames(output) <- c("e_", "londd", "latdd", "count", "sample_label", "taxa")
  return(output)
}

