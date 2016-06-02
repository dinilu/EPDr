filterTaxagroups <- function(counts){}

trans2Percentages <- function(counts){}
    
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
    sample.ages <- agedcount@ages@depth_ages[,chronology]
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

    interp.agedcount <- agedcount
    
    interp.agedcount@ages@default_chronology <- 1
    interp.agedcount@ages@sample_ <- 1:length(interp.ages)
    interp.agedcount@ages@depth_ages <- as.data.frame(interp.ages)
    
    interp.agedcount@counts@sample_ <- 1:length(interp.ages)
    interp.agedcount@counts@counts <- interp.counts
    
    return(interp.agedcount)    
}

standardizeTaxonomy <- function(counts){}
    
