#' Title
#'
#' @param counts 
#' @param taxa_groups 
#'
#' @return
#' @export
#'
#' @examples
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




#' Title
#'
#' @param counts 
#'
#' @return
#' @export
#'
#' @examples
trans2Percentages <- function(counts){
    if(!class(counts) %in% c("counts", "agedcounts")){
        stop("Counts has to be a 'count' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
    }
    if(class(counts) == "agedcounts"){
        counts.tmp <- counts@counts@counts
    }else{
        counts.tmp <- counts@counts
    }

    data_type <- factor("Percentages", levels=c("Counts", "Percentages"))
    totals <- rowSums(counts.tmp)
    counts.tmp <- (counts.tmp / totals) * 100
    
    if(class(counts) == "agedcounts"){
        counts@counts@counts <- counts.tmp
        counts@counts@data_type <- data_type
    }else{
        counts@counts <- counts.tmp
        counts@data_type <- data_type
    }
    return(counts)
}
    





#' Title
#'
#' @param ages 
#'
#' @return
#' @export
#'
#' @examples
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
    ## extract ages and counts from the agedcount object
    if(is.null(chronology)){
        chronology <- agedcount@ages@default_chronology
    }
    
    if(chronology == -9999){
        chronology <- "giesecke"
    }
    
    sample.ages <- agedcount@ages@depth_ages[,as.character(chronology)]
    sample.depthcm <- agedcount@ages@depthcm
    sample.counts <- agedcount@counts@counts
    
    ## set the time bounds
    min.time <- time[which(time >= min(sample.ages))[1]]
    max.time <- time[which(time <= max(sample.ages, na.rm=T))][length(which(time <= max(sample.ages, na.rm=T)))]
    interp.ages <- time[which(time >= min.time & time <= max.time)]
    
    ## interpolate the relative datas to the nearest 1000 years & create the final site data frame
    interp.counts <- matrix(nrow=length(interp.ages), ncol=ncol(sample.counts))
    interp.counts <- as.data.frame(interp.counts)
    interp.counts[,1] <- interp.ages
    colnames(interp.counts) <- colnames(sample.counts)
    
    interp.counts <- as.data.frame(apply(sample.counts, MARGIN=2, FUN=function(x, y, z){approx(y, x, xout=z)$y}, sample.ages, interp.ages))
    interp.depthcm <- as.data.frame(apply(sample.depthcm, MARGIN=2, FUN=function(x, y, z){approx(y, x, xout=z)$y}, sample.ages, interp.ages))
    
    interp.agedcount <- agedcount
    
    interp.agedcount@ages@sample_ <- 20000:(20000+length(interp.ages))
    interp.agedcount@ages@sample_label <- as.character(interp.ages)
    interp.agedcount@ages@depthcm <- interp.depthcm
    interp.agedcount@ages@depths <- data.frame(e_=NA, sample_=NA, depthcm=NA, thickness=NA, analyst=NA, analydate=NA, notes=NA, lab_ID=NA)[,-1]
    interp.agedcount@ages@depth_ages <- as.data.frame(interp.ages)
    
    interp.agedcount@counts@data_processing <- factor("Interpolated", levels=c("Samples", "Interpolated", "Ranged means"))
    interp.agedcount@counts@sample_ <- 20000:(20001+length(interp.ages))
    interp.agedcount@counts@sample_label <- as.character(interp.ages)
    interp.agedcount@counts@counts <- interp.counts

    return(interp.agedcount)    
}


#' Title
#'
#' @param agedcount 
#'
#' @return
#' @export
#'
#' @examples
checkRestriction <- function(agedcount){
    if(agedcount@restriction$usestatus == "R"){
        return(TRUE)
    }else{
        return(FALSE)
    }
}



#' Title
#'
#' @param agedcounts 
#'
#' @return
#' @export
#'
#' @examples
removeRestricted <- function(agedcounts){
    index <- which(!sapply(agedcounts, checkRestriction))
    agedcounts <- agedcounts[index]
    return(agedcounts)
}

#' Title
#'
#' @param agedcount 
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param agedcounts 
#'
#' @return
#' @export
#'
#' @examples
removeWithoutAges <- function(agedcounts){
    index <- which(sapply(agedcounts, checkDefaultChronology))
    agedcounts <- agedcounts[index]
    return(agedcounts)
}


#' Title
#'
#' @param agedcount 
#' @param tmin 
#' @param tmax 
#' @param chronology 
#'
#' @return
#' @export
#'
#' @examples
intervalsCounts <- function(agedcount, tmin, tmax, labels=NULL, chronology=NULL){
    # agedcount <- counts.unr.wages.gie[[129]]
    # tmin <- c(5500, 20000)
    # tmax <- c(6500, 22000)
    # labels <- NULL
    # chronology <- NULL
    
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
    sample.counts <- agedcount@counts@counts
        
    .is.between <- function(x, a, b) {
        x >= a & x <= b
    }    
    index <- which(sapply(sample.ages, function(x, a, b){any(.is.between(x, a, b))}, tmin, tmax))
    intervalid <- unlist(sapply(sample.ages, function(x, a, b){which(.is.between(x, a, b))}, tmin, tmax))

    range.agedcount <- agedcount
    
    if(length(index) == 0 | !all(index %in% range.agedcount@counts@sample_)){
        range.agedcount@ages@sample_ <- numeric(0)
        range.agedcount@ages@sample_label <- character(0)
        range.agedcount@ages@depthcm <- numeric(0)
        range.agedcount@ages@depths <- range.agedcount@ages@depths[-c(1:nrow(range.agedcount@ages@depths)),]
        range.agedcount@ages@depth_ages <- data.frame()
        
        range.agedcount@counts@data_processing <- factor("Ranged means", levels=c("Samples", "Interpolated", "Ranged means"))
        range.agedcount@counts@sample_ <- numeric(0)
        range.agedcount@counts@sample_label <- character(0)
        range.agedcount@counts@counts <- range.agedcount@counts@counts[-c(1:nrow(range.agedcount@counts@counts)),]
    }else{
        range.counts <- agedcount@counts@counts[index,]

        range.means <- apply(range.counts, MARGIN=2, FUN=function(x, y, z){aggregate(x~y, FUN=z)}, intervalid, mean)
        range.means <- dcast(melt(range.means, id.vars="y"), y ~ L1)
        range.means <- range.means[,-which(colnames(range.means) == "y")]
        
        range.agedcount@ages@sample_ <- range.agedcount@ages@sample_[index]
        range.agedcount@ages@sample_label <- range.agedcount@ages@sample_label[index]
        range.agedcount@ages@depthcm <- range.agedcount@ages@depthcm[index]
        range.agedcount@ages@depths <- range.agedcount@ages@depths[index,]
        range.agedcount@ages@depth_ages <- as.data.frame(range.agedcount@ages@depth_ages)
        
        range.agedcount@counts@data_processing <- factor("Ranged means", levels=c("Samples", "Interpolated", "Ranged means"))
        range.agedcount@counts@sample_ <- 10000 + as.numeric(unique(intervalid))
        range.agedcount@counts@sample_label <- labels[unique(intervalid)]
        range.agedcount@counts@counts <- range.means
    }

    return(range.agedcount)    
}

standardizeTaxonomy <- function(counts){}
    
