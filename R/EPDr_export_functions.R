#' Title TBW
#'
#' @param agedcount TBW
#' @param sample_label TBW
#' @param taxa TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
table_by_taxaAge <- function(agedcount, sample_label, taxa){
    e_ <- agedcount@e_
    londd <- agedcount@site$londd
    latdd <- agedcount@site$latdd
    count <- agedcount@counts@counts[which(agedcount@counts@sample_label %in% sample_label), which(agedcount@counts@taxa_names %in% taxa)]
    if(class(count) == "data.frame"){
        count <- apply(count, MARGIN=1, FUN=sum)
    }
    if(length(count) == 0){count <- NA}
    output <- data.frame(e_, londd, latdd, count, sample_label, taxa[[1]])
    colnames(output) <- c("e_", "londd", "latdd", "count", "sample_label", "taxa")
    return(output)
}

