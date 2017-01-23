#' Title TBW
#'
#' @param counts  TBW
#' @param taxa_groups  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
filterTaxaGroups <- function(counts, taxa_groups){

    if(!class(counts) %in% c("counts", "agedcounts")){
        stop("Counts has to be a 'count' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
    }
    if(class(counts) == "agedcounts"){
        counts.tmp <- counts@counts
    }else{
        counts.tmp <- counts
    }
    
    addedparticles <- c("Eucalyptus (added)",
                        "Eucalyptus (counted)",
                        "Exotics (counted)",
                        "Lycopodium (added)",
                        "Lycopodium (counted)",
                        "Lycopodium (counted) for charcoal",
                        "Lycopodium (counted) for Pediastrum",
                        "Marker (added)",
                        "Marker (counted)",
                        "Microspheres (counted)",
                        "Microspheres suspension (added)",
                        "Microsphere suspension (volume added, ml)",
                        "Number of counted lines",
                        "Number of spike added",
                        "Tablets (added)",
                        "Volume of spike added")
                        
    index1 <- which(counts.tmp@taxa_groupid %in% taxa_groups)
    index2 <- which(counts.tmp@taxa_names %in% addedparticles)
    index <- unique(c(index1, index2))
    
    counts.tmp@taxa_names <- counts.tmp@taxa_names[index]
    counts.tmp@taxa_ <- counts.tmp@taxa_[index]
    counts.tmp@taxa_accepted <- counts.tmp@taxa_accepted[index]
    counts.tmp@taxa_mhvar <- counts.tmp@taxa_mhvar[index]
    counts.tmp@taxa_groupid<- counts.tmp@taxa_groupid[index]
    counts.tmp@counts <- counts.tmp@counts[,index]
    if(class(counts) == "agedcounts"){
        counts@counts <- counts.tmp
    }else{
        counts <- counts.tmp
    }
    return(counts)
}




#' Title TBW
#'
#' @param counts  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
trans2Percentages <- function(counts){

    if(!class(counts) %in% c("counts", "agedcounts")){
        stop("Counts has to be a 'count' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
    }
    if(class(counts) == "agedcounts"){
        counts.tmp <- counts@counts@counts
    }else{
        counts.tmp <- counts@counts
    }

    counts_type <- factor("Percentages", levels=c("Counts", "Percentages"))
    totals <- rowSums(counts.tmp)
    counts.tmp <- (counts.tmp / totals) * 100
    
    if(class(counts) == "agedcounts"){
        counts@counts@counts <- counts.tmp
        counts@counts@counts_type <- counts_type
    }else{
        counts@counts <- counts.tmp
        counts@counts_type <- counts_type
    }
    return(counts)
}
    





#' Title TBW
#'
#' @param ages  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
gieseckeDefaultChronology <- function(ages){

    if(!class(ages) %in% c("agedcounts", "ages")){
        stop("ages has to be of class ages or agedcounts (see ?getAges or ?getAgedCounts)")
    }
    if(class(ages) == "agedcounts"){
        if(ages@ages@giesecke == TRUE){
            ages@ages@default_chronology <- -9999
        }
    }else{
        if(ages@giesecke == TRUE){
            ages@default_chronology <- -9999
        }
    }
    return(ages)
}



#' Title TBW
#'
#' @param agedcount  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' #  TBW
checkRestriction <- function(agedcount){
    if(agedcount@restriction$usestatus == "R"){
        return(TRUE)
    }else{
        return(FALSE)
    }
}



#' Title TBW
#'
#' @param agedcounts  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
removeRestricted <- function(agedcounts){
    index <- which(!sapply(agedcounts, checkRestriction))
    agedcounts <- agedcounts[index]
    return(agedcounts)
}


#' Title TBW
#'
#' @param agedcount  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
checkDefaultChronology <- function(agedcount){
    if(length(agedcount@ages@default_chronology) == 0){
        return(FALSE)
    }else{
        if(agedcount@ages@default_chronology == 0){
            return(FALSE)    
        }else{
            return(TRUE)    
        }
    }
}


#' Title TBW
#'
#' @param agedcounts  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
removeWithoutAges <- function(agedcounts){
    index <- which(sapply(agedcounts, checkDefaultChronology))
    agedcounts <- agedcounts[index]
    return(agedcounts)
}





#' Interpolate counts to specific time periods
#'
#' It use data (sample ages and sample counts) from a /code{agedcount} object to estimate by linear interpolation the counts at
#' specific time periods defined by the user. This can be used to estimate counts for the same time periods for multiple entities 
#' or cores in the database, standardizing them for integrative analysis.
#'
#' @param agedcount An /code{agedcount} object as returned by the /link[EPDr:getAgedCounts]{/code{getAgedCounts}} function.
#' @param time Vector with time periods, in the same system (i.e., cal BP) than "ages" in agedcount, in which counts have to be 
#' estimated.
#' @param chronology Number specifying the chronology from which ages should be used to calculate the interpolations. If none is
#' provide the function uses the default chronology from the EPD database.
#'
#' @return The function return a /code{agedcount} object, similar to agedcount in which ages and counts has been modified to the
#' time periods specified in time and the counts estimated for these periods. Accordingly, /code{default_chronology} is also modified
#' to 1, so subsequent analysis will automatically pick up the first (and only) column with the new time periods.
#' 
#' @export
#'
#' @examples
#' #connEPD <- connectToEPD(host="localhost", database="epd_ddbb", user="epdr",
#' # password="epdrpw")
#' #t <- c(seq(0, 21000, by=500))
#' #ac.1 <- getAgedCounts(1, connEPD)
#' #interpolateCounts(ac.1, t)
#' 
#' #ac.2 <- getAgedCounts(2, connEPD)
#' #interpolateCounts(ac.2, t)
#' 
#' #ac.3 <- getAgedCounts(3, connEPD)
#' #interpolateCounts(ac.3, t)
#' #interpolateCounts(ac.3, t, 1)
#' #interpolateCounts(ac.3, t, 2)
interpolateCounts <- function(agedcount, time, chronology=NULL){
#     agedcount <- getAgedCounts(100, connEPD)
#     time <- c(seq(0, 21000, by=500))
#     chronology <- NULL
     
    if(is.null(chronology)){
        chronology <- agedcount@ages@default_chronology
    }
    
    if(chronology == -9999){
        chronology <- "giesecke"
    }
    
    sample.ages <- agedcount@ages@depth_ages[,as.character(chronology)]
    sample.depthcm <- agedcount@ages@depthcm
    sample.id <- agedcount@counts@sample_
    sample.counts <- agedcount@counts@counts
    
    # remove data from depths whithout ages associated
    index1 <- which(!is.na(sample.ages))
    index2 <- 1:length(sample.id)
    index <- intersect(index2, index1) 
    
    sample.ages <- sample.ages[index]
    sample.depthcm <- sample.depthcm[index]
    sample.id <- sample.id[index]
    sample.counts <- sample.counts[index,]
    
    ## set the time bounds
    min.sample.age <- min(sample.ages, na.rm=T)
    max.sample.age <- max(sample.ages, na.rm=T)
    min.time <- time[which(time >= min.sample.age)][1]
    max.time <- time[which(time <= max.sample.age)][length(which(time <= max.sample.age))]
    interp.ages <- time[which(time >= min.time & time <= max.time)]
    
    ## interpolate the relative datas to the nearest 1000 years & create the final site data frame
    interp.counts <- as.data.frame(matrix(nrow=length(interp.ages), ncol=ncol(sample.counts)))
    interp.depthcm <- numeric(0)
    if(length(interp.ages) != 0){
        
        interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
        if(length(interp.ages) == 1){
            interp.counts <- as.data.frame(t(interp.counts))
        }else{
            interp.counts <- as.data.frame(interp.counts)
        }
        interp.depthcm <- stats::approx(sample.ages, sample.depthcm, xout=interp.ages)$y
    }
    colnames(interp.counts) <- colnames(sample.counts)
    
    # create output object with NA and fill with interpolated values when appropriate
    output.counts <- as.data.frame(matrix(nrow=length(time), ncol=ncol(sample.counts)))
    colnames(output.counts) <- colnames(sample.counts)
    output.depthcm <- rep(NA, length(time))
    output.ages <- time
    index <- which(output.ages %in% interp.ages)
    
    output.counts[index,] <- interp.counts
    output.depthcm[index] <- interp.depthcm
    
    # create final object with new values for different slots
    interp.agedcount <- agedcount
    
    interp.agedcount@counts@counts_processing <- factor("Interpolated", levels=c("Samples", "Interpolated", "Ranged means"))
    interp.agedcount@counts@sample_ <- seq(20001, length.out=length(output.ages))
    interp.agedcount@counts@sample_label <- as.character(output.ages)
    interp.agedcount@counts@default_ages <- output.ages
    interp.agedcount@counts@depthcm <- output.depthcm
    interp.agedcount@counts@counts <- output.counts
    
    return(interp.agedcount)    
}



#' Title TBW
#'
#' @param agedcount  TBW
#' @param tmin  TBW
#' @param tmax  TBW
#' @param labels TBW
#' @param chronology TBW 
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
intervalsCounts <- function(agedcount, tmin, tmax, labels=NULL, chronology=NULL){
#     agedcount <- ac.1
#     tmin <- seq(0, 21000, by=1000)
#     tmax <- seq(1000, 22000, by=1000)
#     tmin <- c(0, 1000)
#     tmax <- c(1000, 2000)
#     labels <- NULL
#     chronology <- NULL

    if(!class(agedcount) == "agedcounts"){
        stop("Agedcount has to be an 'agedcounts' object. See ?getAgedCounts")
    }
    if(length(tmin) != length(tmax)){
        stop("length(tmin) != length(tmax). Please, specify two vectors of the same length")
    }
    
    if(is.null(labels)){
        labels <- paste(tmin, "-", tmax, sep="")
    }
    
    if(is.null(chronology)){
        chronology <- agedcount@ages@default_chronology
    }
    if(chronology == -9999){
        chronology <- "giesecke"
    }

    sample.ages <- agedcount@ages@depth_ages[,as.character(chronology)]
    sample.depthcm <- agedcount@counts@depthcm
    sample.id <- agedcount@counts@sample_
    sample.counts <- agedcount@counts@counts
       
    index1 <- which(!is.na(sample.ages))
    index2 <- 1:length(sample.id)
    index <- intersect(index2, index1) 
    
    sample.ages <- sample.ages[index]
    sample.depthcm <- sample.depthcm[index]
    sample.id <- sample.id[index]
    sample.counts <- sample.counts[index,]

    .is.between <- function(x, a, b) {
        x >= a & x <= b
    }    
    
    index <- mapply(function(a, b, x){.is.between(x, a, b)}, tmin, tmax, MoreArgs=list(sample.ages))
    if(is.vector(index)){
        intervalid <- which(index)
        index <- 1
    }else{
        intervalid <- unlist(apply(index, MARGIN=1, FUN=which))
        index <- unlist(apply(index, MARGIN=2, FUN=which))
    }
    
    range.agedcount <- agedcount
    
    output.depthcm <- rep(NA, length(labels))
    output.ages <- rowMeans(cbind(tmin, tmax), na.rm=TRUE)
    output.counts <- as.data.frame(matrix(NA, nrow=length(labels), ncol=ncol(sample.counts)))
    colnames(output.counts) <- colnames(sample.counts)

    if(length(index) == 0 | !all(index %in% range.agedcount@counts@sample_)){
    }else{
        range.counts <- sample.counts[index,]
        range.depthcm <- sample.depthcm[index]
        range.ages <- sample.ages[index]

        range.means <- apply(range.counts, MARGIN=2, FUN=function(x, y, z){stats::aggregate(x~y, FUN=z)}, intervalid, mean)
        range.means <- reshape2::dcast(reshape2::melt(range.means, id.vars="y"), y ~ L1)
        range.means <- range.means[,-which(colnames(range.means) == "y")]
        
        range.depth.means <- stats::aggregate(range.depthcm ~ intervalid, FUN=mean)
        range.depth.means <- range.depth.means$range.depthcm
        
        range.ages.means <- stats::aggregate(range.ages ~ intervalid, FUN=mean)
        range.ages.means <- range.ages.means$range.ages
    
        output.counts[unique(intervalid),] <- range.means
        output.depthcm[unique(intervalid)] <- range.depth.means
        output.ages[unique(intervalid)] <- range.ages.means
    }

    range.agedcount@counts@counts_processing <- factor("Ranged means", levels=c("Samples","Interpolated", "Ranged means"))
    range.agedcount@counts@sample_ <- 10000 + 1:length(labels)
    range.agedcount@counts@sample_label <- labels
    
    range.agedcount@counts@default_ages <- output.ages
    range.agedcount@counts@depthcm <- as.numeric(output.depthcm)
    range.agedcount@counts@counts <- output.counts
    
    return(range.agedcount)    
}



#' Title TBW
#'
#' @param count  TBW
#' @param epd_taxonomy  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
taxa2AcceptedTaxa <- function(count, epd_taxonomy){
    if(!class(count) %in% c("counts", "agedcounts")){
        stop("Counts has to be a 'count' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
    }
    if(class(count) == "agedcounts"){
        count.tmp <- count@counts
    }else{
        count.tmp <- count
    }
    
    taxa_names <- count.tmp@taxa_names
    taxa_ <- count.tmp@taxa_
    taxa_acc <- count.tmp@taxa_accepted
    
    new_taxa_type <- factor("Accepted", levels=c("Samples", "Accepted", "Higher"))

    new_taxa_names <- epd_taxonomy$varname[match(taxa_acc, epd_taxonomy$var_)]

    new_counts <- count.tmp@counts
    colnames(new_counts) <- new_taxa_names
    
    if(nrow(new_counts) == 0){
        new_counts <- new_counts[,1:length(new_taxa_names)]
        colnames(new_counts) <- sort(unique(new_taxa_names))
    }else{
        new_counts$sample_ <- 1:nrow(new_counts)
        new_counts <- reshape2::melt(new_counts, id.vars=c("sample_"))
        new_counts <- reshape2::dcast(new_counts, sample_ ~ variable, fun.aggregate=sum, value.var="value")[,-1]
    }

    new_taxa_names <- colnames(new_counts)
    new_taxa_ <- epd_taxonomy$var_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_acc <- epd_taxonomy$accvar_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_mhvar <- epd_taxonomy$mhvar_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_groupid <- epd_taxonomy$groupid[match(new_taxa_names, epd_taxonomy$varname)]
        
    count.tmp@counts <- new_counts 
    count.tmp@taxa_type <- new_taxa_type
    count.tmp@taxa_names <- new_taxa_names
    count.tmp@taxa_ <- new_taxa_
    count.tmp@taxa_accepted <- new_taxa_acc
    count.tmp@taxa_mhvar <- new_taxa_mhvar
    count.tmp@taxa_groupid <- new_taxa_groupid

    if(class(count) == "agedcounts"){
        count@counts <- count.tmp
    }else{
        count <- count.tmp
    }
    return(count)
}
    

#' Title TBW
#'
#' @param count  TBW
#' @param taxa_list  TBW
#' @param epd_taxonomy  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
expandTaxonomy <- function(count, taxa_list, epd_taxonomy){
    if(!class(count) %in% c("counts", "agedcounts")){
        stop("Counts has to be a 'counts' or 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
    }
    if(class(count) == "agedcounts"){
        count.tmp <- count@counts
    }else{
        count.tmp <- count
    }
    
    site_taxa <- count.tmp@taxa_names
    diff_names <- setdiff(taxa_list, site_taxa)

    if(nrow(count.tmp@counts) == 0){
        new_counts <- data.frame(matrix(ncol=length(taxa_list), nrow=0))
        colnames(new_counts) <- taxa_list
    }else{
        new_counts <- count.tmp@counts
        new_counts[,diff_names] <- NA
    }

    new_counts <- new_counts[,taxa_list]
    
    new_taxa_type <- factor("Expanded", levels=c("Samples", "Accepted", "Higher"))
    
    new_taxa_names <- colnames(new_counts)
    new_taxa_ <- epd_taxonomy$var_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_acc <- epd_taxonomy$accvar_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_mhvar <- epd_taxonomy$mhvar_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_groupid <- epd_taxonomy$groupid[match(new_taxa_names, epd_taxonomy$varname)]
    
    count.tmp@counts <- new_counts 
    count.tmp@taxa_type <- new_taxa_type
    count.tmp@taxa_names <- new_taxa_names
    count.tmp@taxa_ <- new_taxa_
    count.tmp@taxa_accepted <- new_taxa_acc
    count.tmp@taxa_mhvar <- new_taxa_mhvar
    count.tmp@taxa_groupid <- new_taxa_groupid
    
    if(class(count) == "agedcounts"){
        count@counts <- count.tmp
    }else{
        count <- count.tmp
    }
    return(count)
}


#' Title TBW
#'
#' @param counts  TBW
#' @param epd_taxonomy  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
unifyTaxonomy <- function(counts, epd_taxonomy){
    if(!class(counts[[1]]) %in% c("counts", "agedcounts")){
        stop("Counts has to be a list of 'counts' or 'agedcounts' objects. See ?getCounts and ?getAgedCounts functions.")
    }
    if(class(counts[[1]]) == "agedcounts"){
        counts.tmp <- lapply(counts, function(x){x@counts})
    }else{
        counts.tmp <- counts
    }
    
    taxa_list <- lapply(counts.tmp, function(x){x@taxa_names})
    taxa_list <- sort(unique(unlist(taxa_list)))
    
    counts.tmp <- lapply(counts.tmp, expandTaxonomy, taxa_list, epd_taxonomy)
    
    if(class(counts[[1]]) == "agedcounts"){
        counts <- mapply(function(x, y){x@counts <- y; return(x)}, counts, counts.tmp)
    }else{
        counts <- counts.tmp
    }
    return(counts)
}








#' Title TBW
#'
#' @param count  TBW
#' @param epd_taxonomy  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
taxa2HigherTaxa <- function(count, epd_taxonomy){

    if(!class(count) %in% c("counts", "agedcounts")){
        stop("Counts has to be a 'count' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
    }
    if(class(count) == "agedcounts"){
        count.tmp <- count@counts
    }else{
        count.tmp <- count
    }
    
    taxa_names <- count.tmp@taxa_names
    taxa_ <- count.tmp@taxa_
    taxa_mhvar <- count.tmp@taxa_mhvar
    
    taxa_mhvar[which(is.na(taxa_mhvar))] <- taxa_[which(is.na(taxa_mhvar))]
    
    new_taxa_type <- factor("Higher", levels=c("Samples", "Accepted", "Higher"))
    
    new_taxa_names <- epd_taxonomy$varname[match(taxa_mhvar, epd_taxonomy$var_)]
    
    new_counts <- count.tmp@counts
    colnames(new_counts) <- new_taxa_names
    
    if(nrow(new_counts) == 0){
        new_counts <- new_counts[,1:length(new_taxa_names)]
        colnames(new_counts) <- sort(unique(new_taxa_names))
    }else{
        new_counts$sample_ <- 1:nrow(new_counts)
        new_counts <- reshape2::melt(new_counts, id.vars=c("sample_"))
        new_counts <- reshape2::dcast(new_counts, sample_ ~ variable, fun.aggregate=sum, value.var="value")[,-1]
    }
    
    new_taxa_names <- colnames(new_counts)
    new_taxa_ <- epd_taxonomy$var_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_acc <- epd_taxonomy$accvar_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_mhvar <- epd_taxonomy$mhvar_[match(new_taxa_names, epd_taxonomy$varname)]
    new_taxa_groupid <- epd_taxonomy$groupid[match(new_taxa_names, epd_taxonomy$varname)]
    
    count.tmp@counts <- new_counts 
    count.tmp@taxa_type <- new_taxa_type
    count.tmp@taxa_names <- new_taxa_names
    count.tmp@taxa_ <- new_taxa_
    count.tmp@taxa_accepted <- new_taxa_acc
    count.tmp@taxa_mhvar <- new_taxa_mhvar
    count.tmp@taxa_groupid <- new_taxa_groupid
    
    if(class(count) == "agedcounts"){
        count@counts <- count.tmp
    }else{
        count <- count.tmp
    }
    return(count)
}


#' Title TBW
#' 
#' From the Ecography 2013 paper, Appendix 3: For each site at a particular 1 kyr
#' time period, site data-quality was calculated as the mean normalized distance
#' of the nearest pollen sample and the nearest chronological control.  We
#' calculated the distance in years of the nearest pollen sample and the nearest
#' chronological control to each 1 kyr time period.  We eliminated sites where
#' the nearest pollen sample was over 2000 years away or the nearest
#' chronological control was over 5000 years away.  For the remaining sites in
#' each 1 kyr period, we created a summary measure of site data-quality by 
#' rescaling the two distances in years to a 0 - 1 scale and calculating the
#' mean.  For example, if the nearest sample to the 1 kyr BP time period at a
#' given site was at 1.050 kyr BP and the nearest chronological control was at
#' 1.100 kyr BP, the raw distances would be 50 years and 100 years, respectively.
#' These equate to scaled values of 0.975 (i.e., 1 - 50/2000) and 0.98 (i.e., 1 -
#' 100/5000) for sample and chronological quality, respectively, with a mean
#' data-quality for this site at the 1 kyr BP time period of 0.9775.
#' 
#' @param agedcount TBW
#' @param datation TBW
#' @param max_sample_dist TBW
#' @param max_c14_dist TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
qualityIndex <- function(agedcount, datation, max_sample_dist=2000, max_c14_dist=5000){
# 
#     agedcount <- counts.wa.uni[[44]]
#     datation <- datation.co.wa.uni[[44]]
#     max_sample_dist <- 2000
#     max_c14_dist <- 5000
    
    if(!class(agedcount) %in% c("agedcounts")){
        stop("Agedcount has to be an 'agedcounts' object. See ?getAgedCounts.")
    }
    if(!class(datation) %in% c("datation")){
        stop("Datation has to be a 'datation' object. See ?getDatation.")
    }
    
    ages <- agedcount@ages
    counts <- agedcount@counts
    
    e_ <- ages@e_
    sample.ages <- ages@depth_ages

    data.ages <- counts@default_ages
    
    nchron <- datation@chronology@number_of_chronologies
    agebasis <- datation@chronology@agebasis
    
    .mindiff <- function(x, y){
        sorted.y <- sort(y)
        myfun <- stats::stepfun(sorted.y, 0:length(y))
        indices <- pmin(pmax(1, myfun(x)), length(sorted.y) - 1)
        mindist <- pmin(abs(x - sorted.y[indices]), abs(x - sorted.y[indices + 1]))
        return(mindist)
    }

    .c14Diff <- function(ii, x, y, max_diff){
        y.ii <- y[y$chron_ == ii, "age"]
        diff <- .mindiff(x, y.ii)
        if(!is.null(max_diff)){
            diff <- pmin(diff, max_diff)
        }
        return(diff)
    }

    .sampleDiff <- function(ii, x, y, max_diff){
        y.ii <- y[,as.character(ii)]
        diff <- .mindiff(x, y.ii)
        if(!is.null(max_diff)){
            diff <- pmin(diff, max_diff)
        }
        return(diff)
    }

    c14.dist <- sapply(1:nchron, .c14Diff, data.ages, agebasis, max_c14_dist)
    sample.dist <- sapply(1:nchron, .sampleDiff, data.ages, sample.ages, max_sample_dist)
    
    c14.dist <-  1 - (c14.dist/max_c14_dist)
    sample.dist <- 1 - (sample.dist/max_sample_dist)
    
    data.quality <- as.data.frame((c14.dist + sample.dist) / 2)
    colnames(data.quality) <- 1:nchron
    
    agedcount@ages@data_quality <- data.quality
    
    return(agedcount)
}



