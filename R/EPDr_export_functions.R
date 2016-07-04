#' Title
#'
#' @param agedcount 
#' @param sample_label 
#' @param taxa 
#'
#' @return
#' @export
#'
#' @examples
table_by_taxaAge <- function(agedcount, sample_label, taxa){
    core_number <- agedcount@core_number
    londd <- agedcount@site$londd
    latdd <- agedcount@site$latdd
    count <- agedcount@counts@counts[which(agedcount@counts@sample_label %in% sample_label), which(agedcount@counts@taxa_names %in% taxa)]
    if(class(count) == "data.frame"){
        count <- apply(count, MARGIN=1, FUN=sum)
    }
    if(length(count) == 0){count <- NA}
    output <- data.frame(core_number, londd, latdd, count, sample_label, taxa[[1]])
    colnames(output) <- c("core_number", "londd", "latdd", "count", "sample_label", "taxa")
    return(output)
}

