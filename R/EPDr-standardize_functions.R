#' Filter counts by taxa groups
#'
#' This function removes taxa from the slot @@counts in \code{\link[EPDr:counts]{counts}} or
#' \code{\link[EPDr:agedcounts]{agedcounts}} objects based on specified groups of taxa.
#' For example, the user can select to work only with pollen from trees and shrubs (TRSH)
#' or algae (ALGA). The function automatically remove counts from any particle added by 
#' the entity analyst to calculate particle concentrations in the samples 
#' (e.g., Lycopodium (added)).
#'
#' @param counts \code{\link[EPDr:counts]{counts}} or
#' \code{\link[EPDr:agedcounts]{agedcounts}} objects.
#' @param taxa_groups Character vector indicating taxa groups to be selected
#'
#' @return The function returns a modified version of the \code{counts} object with
#' information on @counts only for taxa belonging to the specified taxa groups.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcount <- getAgedCounts(1, epd.connection)
#' # agedcount.pollen <- filterTaxaGroups(agedcount, "TRSH")
#' # str(agedcount@counts@counts)
#' # str(agedcount.pollen@counts@counts)
#' 
filterTaxaGroups <- function(counts, taxa_groups){
  
  if(!class(counts) %in% c("counts", "agedcounts")){
    stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
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



#' Calculate counts percentages
#'
#' This function transforms counts in the slot @@counts in \code{\link[EPDr:counts]{counts}}
#' or \code{\link[EPDr:agedcounts]{agedcounts}} objects to percentages relative to 
#' the total amount of particles in each sample (row).
#'
#' @param counts \code{\link[EPDr:counts]{counts}} or
#' \code{\link[EPDr:agedcounts]{agedcounts}} objects.
#'
#' @return The function returns a modified version of the \code{counts} object, in which
#' values in slot @@counts represent percentages instead of raw counts.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcount <- getAgedCounts(1, epd.connection)
#' # agedcount.pollen <- filterTaxaGroups(agedcount, c("DWAR", "HERB", "LIAN",
#' # "TRSH", "UPHE", "INUN")) ## All pollen taxa groups
#' # agedcount.percent <- trans2Percentages(agedcount.pollen)
#' # head(agedcount.pollen@counts@counts)
#' # head(agedcount.percent@counts@counts)
#' 
trans2Percentages <- function(counts){
  
  if(!class(counts) %in% c("counts", "agedcounts")){
    stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
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



#' Make Giesecke the default chronology
#' 
#' This function makes chronologies from Giesecke the default to be used in
#' \code{\link[EPDr:ages]{ages}} and \code{\link[EPDr:agedcounts]{agedcounts}} 
#' objects if they are available for that particular entity.
#'
#' @param ages \code{\link[EPDr:ages]{ages}} and \code{\link[EPDr:agedcounts]{agedcounts}} 
#' object.
#'
#' @return The function returns a modified version of the \code{ages} in which the
#' slot @@default_chronology is changed to \code{-9999} if they recalculated ages for this
#' entity. A numeric code is used (\code{-9999}) because of the object definition is
#' numeric. This values is used in other functions to grab the right ages from Giesecke.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcount <- getAgedCounts(1, epd.connection)
#' # agedcount@ages@default_chronology
#' # agedcount <- gieseckeDefaultChronology(agedcount)
#' # agedcount@ages@default_chronology
#' 
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



#' Check restrictions on EPDr objects
#' 
#' This function check EPDr objects (\code{\link[EPDr:ages]{ages}},
#' \code{\link[EPDr:counts]{counts}}, \code{\link[EPDr:agedcounts]{agedcounts}}, 
#' \code{\link[EPDr:chronology]{chronology}}, and \code{\link[EPDr:datation]{datation}}), 
#' and return a logical value indicating whether the data are restricted (TRUE) or
#' unrestricted (FALSE).
#'
#' @param object Any EPDr object (\code{\link[EPDr:ages]{ages}},
#' \code{\link[EPDr:counts]{counts}}, \code{\link[EPDr:agedcounts]{agedcounts}}, 
#' \code{\link[EPDr:chronology]{chronology}}, and \code{\link[EPDr:datation]{datation}}).
#'
#' @return logical value indicating whether the data are restricted (TRUE) or
#' unrestricted (FALSE).
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcount <- getAgedCounts(1, epd.connection)
#' # checkRestriction(agedcount)
#' # agedcount <- getAgedCounts(1046, epd.connection)
#' # checkRestriction(agedcount)
#' 
checkRestriction <- function(object){
  if(object@restriction$usestatus == "R"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}



#' Remove restricted data from a list of objects
#'
#' This function is designed to work with list of EPDr objects
#' (\code{\link[EPDr:ages]{ages}}, \code{\link[EPDr:counts]{counts}},
#' \code{\link[EPDr:agedcounts]{agedcounts}}, \code{\link[EPDr:chronology]{chronology}},
#' and \code{\link[EPDr:datation]{datation}}). The function parses all the elements 
#' on the list and remove those with restriction on their use.
#'
#' @param objects  List of EPDr objects (\code{\link[EPDr:ages]{ages}},
#' \code{\link[EPDr:counts]{counts}}, \code{\link[EPDr:agedcounts]{agedcounts}}, 
#' \code{\link[EPDr:chronology]{chronology}}, and \code{\link[EPDr:datation]{datation}})
#'
#' @return List of EPDr objects.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # e.list <- listE(epd.connection, country=c("Spain"))
#' # e.list <- e.list$e_
#' # spain.agedcounts <- lapply(e.list, getAgedCounts, epd.connection)
#' # spain.agedcounts.un <- removeRestricted(spain.agedcounts)
#' # length(spain.agedcounts)
#' # length(spain.agedcounts.un)
#' 
removeRestricted <- function(objects){
  index <- which(!sapply(objects, checkRestriction))
  objects <- objects[index]
  return(objects)
}


#' Check default chronology on EPDr objects
#' 
#' The function check EPDr objects with slot @@default_chronology 
#' (\code{\link[EPDr:ages]{ages}}, \code{\link[EPDr:agedcounts]{agedcounts}},
#' \code{\link[EPDr:chronology]{chronology}}, and \code{\link[EPDr:datation]{datation}})
#' to see if there are a default chronology specified.
#'
#' @param object Any EPDr object but \code{\link[EPDr:counts]{counts}}.
#'
#' @return Logical value indicating whether the object has a default chronology (TRUE)
#' or not (FALSE).
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                                user="epdr", password="epdrpw")
#' # e.list <- listE(epd.connection, country=c("Spain"))
#' # e.list <- e.list$e_
#' # spain.agedcounts <- lapply(e.list, getAgedCounts, epd.connection)
#' # spain.agedcounts.wo <- sapply(spain.agedcounts, checkDefaultChronology)
#' # spain.agedcounts.wo
#' # spain.agedcounts[[1]]@@ages@@default_chronology
#' # spain.agedcounts[[16]]@@ages@@default_chronology
#' 
checkDefaultChronology <- function(object){
  
  if(class(object) == "agedcounts") object <- object@ages
  if(class(object) == "datation") object <- object@chronology
  if(length(object@default_chronology) == 0){
    return(FALSE)
  }else{
    if(object@default_chronology == 0){
      return(FALSE)    
    }else{
      return(TRUE)    
    }
  }
}


#' Remove data without ages from a list of objects
#' 
#' This function is designed to work with list of EPDr objects
#' (\code{\link[EPDr:ages]{ages}}, \code{\link[EPDr:agedcounts]{agedcounts}},
#' \code{\link[EPDr:chronology]{chronology}}, and \code{\link[EPDr:datation]{datation}}),
#' but not (\code{\link[EPDr:counts]{counts}}). The function parses
#' all the elements on the list and remove those without a default chronology.
#'
#' @param objects  List of EPDr objects (\code{\link[EPDr:ages]{ages}},
#' \code{\link[EPDr:agedcounts]{agedcounts}}, \code{\link[EPDr:chronology]{chronology}},
#' and \code{\link[EPDr:datation]{datation}})
#'
#' @return List of EPDr objects.
#' 
#' @export
#' 
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                                user="epdr", password="epdrpw")
#' # e.list <- listE(epd.connection, country=c("Spain"))
#' # e.list <- e.list$e_
#' # spain.agedcounts <- lapply(e.list, getAgedCounts, epd.connection)
#' # spain.agedcounts.wo <- removeWithoutAges(spain.agedcounts)
#' # length(spain.agedcounts)
#' # length(spain.agedcounts.wo)
#' 
removeWithoutAges <- function(objects){
  index <- which(sapply(objects, checkDefaultChronology))
  objects <- objects[index]
  return(objects)
}



#' Interpolate counts to specific time periods
#'
#' This function uses data (sample ages and sample counts) from an
#' \code{\link[EPDr:agedcounts]{agedcounts}} object to estimate by linear interpolation
#' the counts at specific time periods defined by the user. This can be used to
#' estimate counts for the same time periods for multiple entities 
#' or cores in the database, standardizing them for integrative analysis.
#' 
#' Data for time periods in \code{time} but not recorded in the entity are fill with 
#' \code{NA}. This is convenient if analysis are carried out with multiple entities.
#'
#' @param agedcounts An \code{\link[EPDr:agedcounts]{agedcounts}} object as returned by
#' the \code{\link[EPDr:getAgedCounts]{getAgedCounts}} function.
#' @param time Vector with time periods, in the same system (i.e., cal BP) than
#' "ages" in agedcounts, in which counts have to be estimated.
#' @param chronology Number specifying the chronology from which ages should be
#' used to calculate the interpolations. If none is
#' provided the function uses the default chronology from the object (see
#' \code{\link[EPDr:gieseckeDefaultChronology]{gieseckeDefaultChronology}}).
#' @param method interpolation method, should be an unambiguous abbreviation of
#' either linear, loess, sspline or aspline. See details.
#' @param rep_negt logical to indicate whether or not to replace negative values with zero in the interpolated data.
#' @param span span for loess, default=0.25.
#' @param df degress of freedome for smoothing spline, default is the lower of
#' 20 or 0.7 * number of samples.
#' @param ...	additional arguments to loess, smooth.spline and aspline.
#' 
#' @details  Interpolation can be done using linear interpolation between data points
#' in the original series (default) using \code{\link[stats:approx]{approx}}, using
#' a fitted \code{\link[stats:loess]{loess}} locally weighted regression, or by 
#' \code{\link[stats:smooth.spline]{smooth.spline}}. The latter two methods will
#' also smooth the dataand additional arguments may be passed to these functions
#' to control the amount of smoothing.
#'
#' @return The function returns an \code{\link[EPDr:agedcounts]{agedcounts}} object,
#' similar to \code{agedcounts} in which ages and counts has been modified to the
#' time periods specified in time and the counts estimated for these periods.
#' Accordingly, \code{default_chronology} is also modified to 1, so subsequent analysis
#' will automatically pick up the first (and only) column with the new time periods.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # t <- c(seq(0, 21000, by=500))
#' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' # agedcounts.1.int <- interpolateCounts(agedcounts.1, t)
#' #
#' # agedcounts.3 <- getAgedCounts(3, connEPD)
#' # agedcounts.3.int <- interpolateCounts(agedcounts.3, t)
#' # agedcounts.3.int <- interpolateCounts(agedcounts.3, t, 1)
#' # agedcounts.3.int <- interpolateCounts(agedcounts.3, t, 2)
#' 
interpolateCounts <- function(agedcounts, time, chronology=NULL,
    method=c("linear", "loess", "sspline"), rep_negt=TRUE, span=0.25,
    df=min(20, nrow(agedcounts@counts@counts) * 0.7), ...){
  #     agedcounts <- getAgedCounts(100, connEPD)
  #     time <- c(seq(0, 21000, by=500))
  #     chronology <- NULL
  
  if(is.null(chronology)){
    chronology <- agedcounts@ages@default_chronology
  }
  
  if(chronology == -9999){
    chronology <- "giesecke"
  }
  
  sample.ages <- agedcounts@ages@depth_ages[,as.character(chronology)]
  sample.depthcm <- agedcounts@ages@depthcm
  sample.id <- agedcounts@counts@sample_
  sample.counts <- agedcounts@counts@counts
  
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
    
      method <- match.arg(method)
      if (is.null(method)) 
        stop("Interpolation method not recognised")
      if (method == "linear") {
        lin.f <- function(y, x, xout) {
          stats::approx(x, y, xout)$y
        }
        interp.counts <- apply(sample.counts, MARGIN=2, lin.f, x=sample.ages, xout=interp.ages, ...)
        interp.depthcm <- lin.f(sample.depthcm, sample.ages, xout=interp.ages)
       # res <- apply(y, 2, lin.f, x1 = x, xout = xout, ...)
       # interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
      }
      else if (method == "loess") {
        lo.f <- function(y, x, xout, span, ...) {
          fit <- stats::loess(y ~ x, span=span, ...)
          stats::predict(fit, newdata=data.frame(x=xout))
        }
        interp.counts <- apply(sample.counts, MARGIN=2, lo.f, x=sample.ages, xout=interp.ages, span=span, ...)
        interp.depthcm <- lo.f(sample.depthcm, sample.ages, xout=interp.ages, span=span, ...)
        # res <- apply(y, 2, lo.f, x1 = x, xout = xout, span = span, ...)
        # interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
      }
      else if (method == "sspline") {
        ss.f <- function(y, x, xout, df, ...) {
          fit <- stats::smooth.spline(y ~ x, df=df, ...)
          stats::predict(fit, x=data.frame(x=xout))$y[, 1]
        }
        interp.counts <- apply(sample.counts, MARGIN=2, ss.f, x=sample.ages, xout=interp.ages)
        interp.depthcm <- ss.f(sample.depthcm, sample.ages, xout=interp.ages, df=df, ...)
        # res <- apply(y, 2, ss.f, x1 = x, xout = xout, df = df, ...)
        # interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
      }
      if (rep_negt) {
        interp.counts[interp.counts < 0] <- 0
      }
      
#    interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
    
    if(length(interp.ages) == 1){
      interp.counts <- as.data.frame(t(interp.counts))
    }else{
      interp.counts <- as.data.frame(interp.counts)
    }
    # interp.depthcm <- stats::approx(sample.ages, sample.depthcm, xout=interp.ages)$y
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
  interp.agedcounts <- agedcounts
  
  interp.agedcounts@counts@counts_processing <- factor("Interpolated", levels=c("Samples", "Interpolated", "Ranged means"))
  interp.agedcounts@counts@sample_ <- seq(20001, length.out=length(output.ages))
  interp.agedcounts@counts@sample_label <- as.character(output.ages)
  interp.agedcounts@counts@default_ages <- output.ages
  interp.agedcounts@counts@depthcm <- output.depthcm
  interp.agedcounts@counts@counts <- output.counts
  
  return(interp.agedcounts)    
}



#' Mean counts for specific time intervals
#'
#' This function uses data (sample ages and sample counts) from an
#' \code{\link[EPDr:agedcounts]{agedcounts}} object to calculate mean counts
#' for samples within specific time intervals defined by the user. This can be
#' used to estimate mean counts for the same time intervals for multiple entities 
#' or cores in the database, standardizing them for integrative analysis.
#' 
#' Time intervals without sample (data) in the entity are fill with 
#' \code{NA}. This is convenient if analysis are carried out with multiple entities.
#'
#' @param agedcounts An \code{\link[EPDr:agedcounts]{agedcounts}} object as returned by
#' the \code{\link[EPDr:getAgedCounts]{getAgedCounts}} function.
#' @param tmin Numeric vector indicating the lower limits (in years cal. BP) for the 
#' time intervals.
#' @param tmax Numeric vector indicating the upper limits (in years cal. BP) for the
#' time intervals
#' @param labels Character vector with labels for each time intervals, if none are 
#' provided the functions generate them with the following format \code{tmin}-\code{tmax}.
#' @param chronology Number specifying the chronology from which ages should be
#' used to calculate the interpolations. If none is provided the function uses
#' the default chronology from the object (see
#' \code{\link[EPDr:gieseckeDefaultChronology]{gieseckeDefaultChronology}}).
#'
#' @return The function returns a \code{\link[EPDr:agedcounts]{agedcounts}} object, similar
#' to \code{agedcounts} in which ages and counts has been modified to the
#' time intervarls specified and the counts estimated for these periods.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' # agedcounts.1.int <- intervalsCounts(agedcounts.1, tmin=seq(0, 21000, by=1000),
#' # tmax=seq(999, 21999, by=1000))
#' #
#' # agedcounts.3 <- getAgedCounts(3, connEPD)
#' # agedcounts.3.int <- intervalsCounts(agedcounts.3, tmin=seq(0, 21000, by=1000),
#' # tmax=seq(999, 21999, by=1000))
#' # agedcounts.3.int <- intervalsCounts(agedcounts.3, tmin=seq(0, 21000, by=1000),
#' # tmax=seq(999, 21999, by=1000), 2)
#' 
intervalsCounts <- function(agedcounts, tmin, tmax, labels=NULL, chronology=NULL){
  if(!class(agedcounts) == "agedcounts"){
    stop("Agedcount has to be an 'agedcounts' object. See ?getAgedCounts")
  }
  if(length(tmin) != length(tmax)){
    stop("length(tmin) != length(tmax). Please, specify two vectors of the same length")
  }
  
  if(is.null(labels)){
    labels <- paste(tmin, "-", tmax, sep="")
  }
  
  if(is.null(chronology)){
    chronology <- agedcounts@ages@default_chronology
  }
  if(chronology == -9999){
    chronology <- "giesecke"
  }
  
  sample.ages <- agedcounts@ages@depth_ages[,as.character(chronology)]
  sample.depthcm <- agedcounts@counts@depthcm
  sample.id <- agedcounts@counts@sample_
  sample.counts <- agedcounts@counts@counts
  
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
  
  range.agedcounts <- agedcounts
  
  output.depthcm <- rep(NA, length(labels))
  output.ages <- rowMeans(cbind(tmin, tmax), na.rm=TRUE)
  output.counts <- as.data.frame(matrix(NA, nrow=length(labels), ncol=ncol(sample.counts)))
  colnames(output.counts) <- colnames(sample.counts)
  
  if(length(index) == 0 | !all(index %in% range.agedcounts@counts@sample_)){
  }else{
    range.counts <- sample.counts[index,]
    range.depthcm <- sample.depthcm[index]
    range.ages <- sample.ages[index]
    
    range.means <- apply(range.counts, MARGIN=2, FUN=function(x, y, z){stats::aggregate(x, by=list(y=y), FUN=z)}, intervalid, mean)
    range.means <- reshape2::dcast(reshape2::melt(range.means, id.vars="y"), y ~ L1)
    range.means <- range.means[,-which(colnames(range.means) == "y")]
    
    range.depth.means <- stats::aggregate(range.depthcm, by=list(y=intervalid), FUN=mean)
    range.depth.means <- range.depth.means$x
    
    range.ages.means <- stats::aggregate(range.ages, by=list(y=intervalid), FUN=mean)
    range.ages.means <- range.ages.means$x
    
    output.counts[unique(intervalid),] <- range.means
    output.depthcm[unique(intervalid)] <- range.depth.means
    output.ages[unique(intervalid)] <- range.ages.means
  }
  
  range.agedcounts@counts@counts_processing <- factor("Ranged means", levels=c("Samples","Interpolated", "Ranged means"))
  range.agedcounts@counts@sample_ <- 10000 + 1:length(labels)
  range.agedcounts@counts@sample_label <- labels
  
  range.agedcounts@counts@default_ages <- output.ages
  range.agedcounts@counts@depthcm <- as.numeric(output.depthcm)
  range.agedcounts@counts@counts <- output.counts
  
  return(range.agedcounts)    
}



#' Change taxa to accepted taxa names
#' 
#' This function modifies the taxa names in the \code{@@counts} slot of EPDr objects
#' (\code{\link[EPDr:agedcounts]{agedcounts}} and \code{\link[EPDr:counts]{counts}}).
#' More specifically this function compares the taxa name with the taxonomy of the EPD
#' to use the accepted names. If these changes result in duplicated columns of the same
#' taxa their values are unified by summing them.
#'
#' @param counts \code{\link[EPDr:agedcounts]{agedcounts}} or
#' \code{\link[EPDr:counts]{counts}} objects with a \code{@@counts} slot.
#' @param epd_taxonomy Data frame with the taxonomy from the EPD as from the
#' \code{\link[EPDr:getTaxonomyEPD]{getTaxonomyEPD}} function.
#'
#' @return \code{\link[EPDr:agedcounts]{agedcounts}} or \code{\link[EPDr:counts]{counts}}
#' object with new taxa names.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' # agedcounts.1@counts@taxa_names
#' # colnames(agedcounts.1@counts@counts)
#' # agedcounts.1.acc <- taxa2AcceptedTaxa(agedcounts.1, getTaxonomyEPD(epd.connection))
#' # agedcounts.1.acc@counts@taxa_names
#' # colnames(agedcounts.1.acc@counts@counts)
#' 
taxa2AcceptedTaxa <- function(counts, epd_taxonomy){
  if(!class(counts) %in% c("counts", "agedcounts")){
    stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
  }
  if(class(counts) == "agedcounts"){
    counts.tmp <- counts@counts
  }else{
    counts.tmp <- counts
  }
  
  taxa_names <- counts.tmp@taxa_names
  taxa_ <- counts.tmp@taxa_
  taxa_acc <- counts.tmp@taxa_accepted
  
  new_taxa_type <- factor("Accepted", levels=c("Samples", "Accepted", "Higher"))
  
  new_taxa_names <- epd_taxonomy$varname[match(taxa_acc, epd_taxonomy$var_)]
  
  new_counts <- counts.tmp@counts
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
  
  counts.tmp@counts <- new_counts 
  counts.tmp@taxa_type <- new_taxa_type
  counts.tmp@taxa_names <- new_taxa_names
  counts.tmp@taxa_ <- new_taxa_
  counts.tmp@taxa_accepted <- new_taxa_acc
  counts.tmp@taxa_mhvar <- new_taxa_mhvar
  counts.tmp@taxa_groupid <- new_taxa_groupid
  
  if(class(counts) == "agedcounts"){
    counts@counts <- counts.tmp
  }else{
    counts <- counts.tmp
  }
  return(counts)
}


#' Expand EPDr objects with new taxa
#' 
#' This functions modifies EPDr objects with \code{@@counts} slot
#' (\code{\link[EPDr:agedcounts]{agedcounts}} or \code{\link[EPDr:counts]{counts}})
#' to filter taxa columns to match \code{taxa_list}. The function add empty columns
#' (\code{NA}) if a new taxa is defined int \code{taxa_list} or remove columns for the 
#' taxa not included in \code{taxa_list}. The function may look useless for a single 
#' entity but it is useful when standardizing data from multiple entities.
#'
#' @param counts \code{\link[EPDr:agedcounts]{agedcounts}} or
#' \code{\link[EPDr:counts]{counts}} objects to be modified.
#' @param taxa_list Character vector indicating the new taxa in the \code{@@counts} slot. 
#' @param epd_taxonomy Data frame with the taxonomy from the EPD as from the
#' \code{\link[EPDr:getTaxonomyEPD]{getTaxonomyEPD}} function.
#'
#' @return \code{\link[EPDr:agedcounts]{agedcounts}} or
#' \code{\link[EPDr:counts]{counts}} objects with the modified \code{@@counts} slot.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' # agedcounts.1@counts@taxa_names
#' # colnames(agedcounts.1@counts@counts)
#' # agedcounts.1.ft <- filterTaxa(agedcounts.1,
#' # c(agedcounts.1@counts@taxa_names, "prueba"), getTaxonomyEPD(epd.connection))
#' # colnames(agedcounts.1.ft@counts@counts)
#' # agedcounts.1.ft@counts@taxa_names
#' # head(agedcounts.1.ft@counts@counts)
#' 
filterTaxa <- function(counts, taxa_list, epd_taxonomy){
  if(!class(counts) %in% c("counts", "agedcounts")){
    stop("Counts has to be a 'counts' or 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
  }
  if(class(counts) == "agedcounts"){
    counts.tmp <- counts@counts
  }else{
    counts.tmp <- counts
  }
  
  site_taxa <- counts.tmp@taxa_names
  diff_names <- setdiff(taxa_list, site_taxa)
  
  if(nrow(counts.tmp@counts) == 0){
    new_counts <- data.frame(matrix(ncol=length(taxa_list), nrow=0))
    colnames(new_counts) <- taxa_list
  }else{
    new_counts <- counts.tmp@counts
    new_counts[,diff_names] <- NA
  }
  
  new_counts <- new_counts[,taxa_list]
  
  new_taxa_type <- factor("Expanded", levels=c("Samples", "Accepted", "Higher"))
  
  new_taxa_names <- colnames(new_counts)
  new_taxa_ <- epd_taxonomy$var_[match(new_taxa_names, epd_taxonomy$varname)]
  new_taxa_acc <- epd_taxonomy$accvar_[match(new_taxa_names, epd_taxonomy$varname)]
  new_taxa_mhvar <- epd_taxonomy$mhvar_[match(new_taxa_names, epd_taxonomy$varname)]
  new_taxa_groupid <- epd_taxonomy$groupid[match(new_taxa_names, epd_taxonomy$varname)]
  
  counts.tmp@counts <- new_counts 
  counts.tmp@taxa_type <- new_taxa_type
  counts.tmp@taxa_names <- new_taxa_names
  counts.tmp@taxa_ <- new_taxa_
  counts.tmp@taxa_accepted <- new_taxa_acc
  counts.tmp@taxa_mhvar <- new_taxa_mhvar
  counts.tmp@taxa_groupid <- new_taxa_groupid
  
  if(class(counts) == "agedcounts"){
    counts@counts <- counts.tmp
  }else{
    counts <- counts.tmp
  }
  return(counts)
}


#' Unify taxonomy of counts in multiple objects
#' 
#' This function compares the taxa list registered in multiple EPDr objects with
#' a \code{@@counts} slot (\code{\link[EPDr:agedcounts]{agedcounts}} or
#' \code{\link[EPDr:counts]{counts}}) and expand their taxonomy (using
#' \code{\link[EPDr:filterTaxa]{filterTaxa}}) to make them match. When neccesary the
#' function add empty columns (with \code{NA} values).
#'
#' @param counts \code{\link[EPDr:agedcounts]{agedcounts}} or
#' \code{\link[EPDr:counts]{counts}} objects to be modified.
#' @param epd_taxonomy Data frame with the taxonomy from the EPD as from the
#' \code{\link[EPDr:getTaxonomyEPD]{getTaxonomyEPD}} function.
#'
#' @return \code{\link[EPDr:agedcounts]{agedcounts}} or
#' \code{\link[EPDr:counts]{counts}} objects with the modified \code{@@counts} slot.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                                user="epdr", password="epdrpw")
#' # e.list <- listE(epd.connection, country=c("Spain"))
#' # e.list <- e.list$e_
#' # spain.agedcounts <- lapply(e.list, getAgedCounts, epd.connection)
#' # spain.agedcounts.ut <- unifyTaxonomy(spain.agedcounts, getTaxonomyEPD(epd.connection))
#' # colnames(spain.agedcounts[[1]]@counts@counts)
#' # colnames(spain.agedcounts.ut[[1]]@counts@counts)
#' 
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
  
  counts.tmp <- lapply(counts.tmp, filterTaxa, taxa_list, epd_taxonomy)
  
  if(class(counts[[1]]) == "agedcounts"){
    counts <- mapply(function(x, y){x@counts <- y; return(x)}, counts, counts.tmp)
  }else{
    counts <- counts.tmp
  }
  return(counts)
}



#' Change taxa to higher taxa level
#' 
#' This function modifies the taxa names in the \code{@@counts} slot of EPDr objects
#' (\code{\link[EPDr:agedcounts]{agedcounts}} and \code{\link[EPDr:counts]{counts}}).
#' More specifically this function compares the taxa name with the taxonomy of the EPD
#' to use the higher taxa names. If these changes result in duplicated columns of the same
#' taxa their values are unified by summing them.
#'
#' @param counts \code{\link[EPDr:agedcounts]{agedcounts}} or
#' \code{\link[EPDr:counts]{counts}} objects with a \code{@@counts} slot.
#' @param epd_taxonomy Data frame with the taxonomy from the EPD as from the
#' \code{\link[EPDr:getTaxonomyEPD]{getTaxonomyEPD}} function.
#'
#' @return \code{\link[EPDr:agedcounts]{agedcounts}} or \code{\link[EPDr:counts]{counts}}
#' object with new taxa names.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' # agedcounts.1@counts@taxa_names
#' # colnames(agedcounts.1@counts@counts)
#' # agedcounts.1.hn <- taxa2HigherTaxa(agedcounts.1, getTaxonomyEPD(epd.connection))
#' # agedcounts.1.hn@counts@taxa_names
#' # colnames(agedcounts.1.hn@counts@counts)
#' 
taxa2HigherTaxa <- function(counts, epd_taxonomy){
  
  if(!class(counts) %in% c("counts", "agedcounts")){
    stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
  }
  if(class(counts) == "agedcounts"){
    counts.tmp <- counts@counts
  }else{
    counts.tmp <- counts
  }
  
  taxa_names <- counts.tmp@taxa_names
  taxa_ <- counts.tmp@taxa_
  taxa_mhvar <- counts.tmp@taxa_mhvar
  
  taxa_mhvar[which(is.na(taxa_mhvar))] <- taxa_[which(is.na(taxa_mhvar))]
  
  new_taxa_type <- factor("Higher", levels=c("Samples", "Accepted", "Higher"))
  
  new_taxa_names <- epd_taxonomy$varname[match(taxa_mhvar, epd_taxonomy$var_)]
  
  new_counts <- counts.tmp@counts
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
  
  counts.tmp@counts <- new_counts 
  counts.tmp@taxa_type <- new_taxa_type
  counts.tmp@taxa_names <- new_taxa_names
  counts.tmp@taxa_ <- new_taxa_
  counts.tmp@taxa_accepted <- new_taxa_acc
  counts.tmp@taxa_mhvar <- new_taxa_mhvar
  counts.tmp@taxa_groupid <- new_taxa_groupid
  
  if(class(counts) == "agedcounts"){
    counts@counts <- counts.tmp
  }else{
    counts <- counts.tmp
  }
  return(counts)
}


#' Blois quality index for palynological samples
#' 
#' This function apply the quality index described in Blois et al. (2013).
#' 
#' From the Ecography 2013 paper, Appendix 3: "For each site at a particular 1 kyr
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
#' data-quality for this site at the 1 kyr BP time period of 0.9775."
#' 
#' To replicate the calculation the function allows to specify different maximum
#' distances as parameters of the function.
#' 
#' @param agedcounts \code{\link[EPDr:agedcounts]{agedcounts}} object.
#' @param datation \code{\link[EPDr:datation]{datation}} object for the same entity
#' in \code{agedcounts}. This is mandatory to calculate distances between samples and 
#' control points.
#' @param max_sample_dist Maximum numeric distance to be considered to the
#' palynological samples for interpolated or ranged data.
#' @param max_c14_dist Maximum numeric distance to be considered to the (C14) control
#' points.
#'
#' @return \code{\link[EPDr:agedcounts]{agedcounts}} object with no empty
#' \code{@@ages@@data_quality} slot.
#' 
#' @references Blois, J.L, J.W. Williams, M.C. Fitzpatrick, S. Ferrier, S.D. Veloz, F. He, Z. Liu, G. Manion, and Bette Otto-Bliesner (2013). Modeling the Climatic Drivers of Spatial Patterns in Vegetation Composition since the Last Glacial Maximum. Ecography 36(4): 460-473. doi:10.1111/j.1600-0587.2012.07852.x.
#' 
#' @export
#'
#' @examples
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' # user="epdr", password="epdrpw")
#' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' # datation.1 <- getDatation(1, epd.connection)
#' # agedcounts.1.qi <- bloisQualityIndex(agedcounts.1, datation.1)
#' # agedcounts.1.qi@ages@data_quality
#' # 
#' # agedcounts.1.ran <- intervalsCounts(agedcounts.1, tmin=seq(0, 21000, by=1000),
#' # tmax=seq(999, 21999, by=1000))
#' # agedcounts.1.ran.qi <- bloisQualityIndex(agedcounts.1.ran, datation.1)
#' # agedcounts.1.ran.qi@ages@data_quality
#' # 
#' # t <- c(seq(0, 21000, by=500))
#' # agedcounts.1.int <- interpolateCounts(agedcounts.1, t)
#' # agedcounts.1.int.qi <- bloisQualityIndex(agedcounts.1.int, datation.1)
#' # agedcounts.1.int.qi@ages@data_quality
bloisQualityIndex <- function(agedcounts, datation, max_sample_dist=2000, max_c14_dist=5000){
      # agedcounts <- counts.wa.uni[[44]]
      # datation <- datation.co.wa.uni[[44]]
      # max_sample_dist <- 2000
      # max_c14_dist <- 5000
  
  if(!class(agedcounts) %in% c("agedcounts")){
    stop("Agedcounts has to be an 'agedcounts' object. See ?getAgedCounts.")
  }
  if(!class(datation) %in% c("datation")){
    stop("Datation has to be a 'datation' object. See ?getDatation.")
  }
  
  ages <- agedcounts@ages
  counts <- agedcounts@counts
  
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
  
  agedcounts@ages@data_quality <- data.quality
  
  return(agedcounts)
}



