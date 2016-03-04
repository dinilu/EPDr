# EPDr R-package developed by Diego Nieto-Lugilde (dnietolugilde@gmail.com) reusing code from:
# clam, R code for classical (non-Bayesian) age-depth modelling, this version 2.2
# Maarten Blaauw <maarten.blaauw@qub.ac.uk>
# see accompanying manual.html and Blaauw 2010 (Quaternary Geochronology 5: 512-518)

#' Calibrate C14 ages and calculate age-depth models
#'
#' Calibrate C14 ages and calculate age-depth models to calculate ages at C14 samples and companing files
#'
#' @author Maarten Blaauw \email{kbroman@@biostat.wisc.edu}, adapted by Diego Nieto-Lugilde \email{dnietolugilde@gmail.com}
#' @references \url{http://chrono.qub.ac.uk/blaauw/clam.html}
#'
#' @keywords clam c14 calibration age
#'
#' @param name The name of a folder with the data as required by clam2.2 as character (e.g. "Example").
#' @param type Method type for interpolation: 1 linear interpolation between neighbouring levels ("int", "inter", "interp"); 2 linear or higher polynomial regression ("reg", "regr", "poly" or "polyn", default linear); 3 cubic spline ("spl", "spline"); 4 smooth spline ("sm", "smooth", default smoothing 0.3); 5 locally weighted spline ("loess", "lowess", default smoothing 0.75, cannot extrapolate). Default is 1.
#' @param smooth degree of smoothing. Gives polynomial degree for model type 2. Default are: 1 (linear) for type=2, 0.3 for type=4, 0.75 for type=5.
#' @param prob Confidence interval (between 0 and 1). Default is 0.95
#' @param its Amount of iterations. Default is 1000.
#' @param wghts Weights as: 0 no weighting; 1 weighted to calibrated probabilities of sampled calendar years; 2 weighted to (inverse squared) errors of the dates. Default is 1.
#' @param cc Calibration curve for C14 dates (1, 2 or 3). Alternatively also 4 and 5, but then cc4 and cc5 curves has to be supplied into the working directory. Default is 1
#' @param cc1 Calibration curve for northern hemisphere terrestrial C14 dates. This calibration curve is included in the package and no need to be in the working directory. IntCal13.14C
#' @param cc2 Calibration curve for marine C14 dates. This calibration curve is included in the package and no need to be in the working directory. Marine13.14C
#' @param cc3 Calibration curve for southern hemisphere C14 dates. This calibration curve is included in the package and no need to be in the working directory. SHCal13.14C
#' @param cc4 Calibration curve for mixed terrestrial/marine C14 dates. These two calibration curves are included in the package and no need to be in the working directory. mixed.14C
#' @param cc5 Calibration curve for southern hemisphere C14 dates beyond 11 kcal BP. The new calibration curves extend until 50 kcal BP and, hence, this is not needed any more. However the option is keeped in the package for back compatibility. gluedHemispheres.14C
#' @param postbomb Postbomb curve to be used for negative C14 ages. 0 = none. Default is 0.
#' @param pb1 Postbomb curve for northern hemisphere region 1 postbomb C14 dates: postbomb_NH1.14C
#' @param pb2 Postbomb curve for northern hemisphere region 2 postbomb C14 dates: postbomb_NH2.14C
#' @param pb3 Postbomb curve for northern hemisphere region 3 postbomb C14 dates: postbomb_NH3.14C
#' @param pb4 Postbomb curve for southern hemisphere postbomb region 3 C14 dates: postbomb_SH1-2.14C
#' @param pb5 Postbomb curve for southern hemisphere postbomb regions 1-2 C14 dates: postbomb_SH3.14C
#' @param outliers The number of any dates to be considered outlying, e.g. c(5,6) for the fifth and sixth date counting from the top of a core.
#' @param ignore The number of any dates that should be ignored, e.g., c(5,6) for the fifth and sixth date counting from the top of a core.
#' @param youngest The age beyond which dates should be truncated
#' @param extradates Depths of any additional dates with their files of ages and probabilities.
#' @param slump Upper and lower depths of sections of abrupt accumulation that should be excised, e.g., c(600, 550, 120, 100) for two sections of 600-550 and 120-100cm depth.
#' @param est ????:	1 weighted averages of age-depth model derived ages; 2 midpoints of age-depth model derived ages; 3 midpoints of calibrated ranges; 4 weighted means of calibrated ranges; 5 medians of calibrated distributions; 6 maximum densities of calibrated distributions; 7 midpoints of entire calibrated distributions (with probabilities beyond threshold). Default is 1.
#' @param calibt Logical, FALSE by default. Provide two parameters such as c(3,4).
#' @param mixed.effect Set to TRUE to activate mixed-effect modelling. Default is FALSE
#' @param dmin Minimum depth of age-depth model (e.g., extrapolate)
#' @param dmax Maximum depth of age-depth model (e.g., extrapolate)
#' @param every Resolution at which (ages for) depths are calculated. Default is 1.
#' @param yrmin Minimum of calendar axis of age-depth plot (calculate automatically by default).
#' @param yrmax Maximum of calendar axis of age-depth plot (calculated automatically by default).
#' @param yrsteps Temporal resolution at which calibrated ages are calculated (in calendar years). Default is 1.
#' @param pbsteps Temporal resolution at which postbomb C14 ages are calibrated (in calendar years). Default is 0.01.
#' @param hpdsteps Temporal resolution at which highest posterior density ranges are calibrated (in calendar years). Default is 1.
#' @param BCAD Logical indicating whether to use BC/AD instead of cal BP. Default is FALSE.
#' @param decimals Amount of decimals for rounding. Default is 0.
#' @param accrate Give accumulation rates in yr/cm (0) or cm/yr (1). Default is 0.
#' @param ageofdepth Calculate age estimates of a specific depth.
#' @param depth Depth units. Default is "cm".
#' @param depthseq Sequence of depths for which age estimates are to be calculated (default: from dmin to dmax with steps of size every).
#' @param depths.file Logical indicating whether to use a file with depths for depthseq. Default is FALSE.
#' @param thickness Thickness of the dated samples. Default is 1.
#' @param hiatus Depths of any hiatuses, e.g., c(500, 300). Each sub-section must have at least 2 dates (4 for smoothing spline; does not work with loess as it cannot extrapolate).
#' @param remove.reverse Proportion of age-models with reversals that can be removed before prompting a warning. Set at FALSE to avoid removing models with reversals. Default is 0.5.
#' @param times Half-range of calibration curve used to calibrate dates (multiplication factor for the dates' errors). Default is 5.
#' @param sep Separator between the fields of the plain text file containing the dating information. Default is ",".
#' @param ext Extension of the file containing the dating information. Default is ".csv".
#' @param runname Text to add to the corename for specific runs, e.g., "MyCore_Test1".
#' @param storedat Logical indicating whether to store the dates and age-model within R after a clam run. Default is FALSE.
#' @param threshold Below which value should probabilities be excluded from calculations. Default is 1e-6.
#' @param proxies Logical, default is FALSE. Set to TRUE to plot proxies against age after the run.
#' @param revaxes Logical, default is FALSE. Set to TRUE to plot ages on the vertical axis and depth on the horizontal axis.
#' @param revd Logical, default is TRUE. Plot depth axis in reverse.
#' @param revyr Logical, default is TRUE. Plot age axis in reverse.
#' @param calhght Heights of the calibrated distributions in the age-depth plot. Default is 1.
#' @param maxhght Maximum height of age probability distributions. Default is 0.1.
#' @param mirror Logical, default is TRUE. Plot the age distributions in "mirror" style (above and below depth).
#' @param plotrange Logical, default is TRUE. Plot the confidence ranges of the age-model.
#' @param bty Default is "1".
#' @param mar Vector of 4 elements (c(3.5, 3, 2, 1)). Plot margins (amount of white space along edges of axes 1-4).
#' @param mgp Axis text margins (where should titles, labels and tick marks be plotted).
#' @param plotpdf Logical, default is TRUE. Produce a pdf file of the age-depth plot
#' @param plotpng Logical, default is TRUE. Produce a png file of the age-depth plot
#' @param greyscale Produce a grey-scale representation of all age-models (number gives resolution, e.g., 500 bins; will cancel plotting of the confidence intervals). Default is "none".
#' @param yrlab Alternative names can be provided. Defaults are "cal BP" or "BC/AD", depending on BCAD value.
#' @param dlab Alternative names can be provided. Default is "depth (cm)".
#' @param calcol Colour of the calibrated distributions in the age-depth plot. Default is rgb(0,0.5,0.5,0.5) (transparent blue).
#' @param C14col Colour of the calibrated ranges of the dates. Default is "blue".
#' @param outcol Colour of outlying dates. Default is "red".
#' @param outlsize Size of symbols outlying dates. Default is 1.
#' @param bestcol Colour of the "best" age-depth model (based on chosen value for est). Default is "black".
#' @param rangecol Colour of plotted confidence ranges. Default is rgb(0,0,0,0.3) (transparent grey).
#' @param slumpcol Colour of slump. Default is grey(0.75).
#' @param plotname Logical indicating whether to print the core name on the graph. Default is TRUE.
#' @param ash Logical indicating whether to plot all distributions at the same height. Default is FALSE.
#'
#' @return This function does not return any value or R object, instead it creates a folder structure into the working directory. First it creates a folder called "Cores" in the working directory. In this folder it creates a new folder with the number specified in parameter "name". Inside this folder it creates several .txt and .csv files with the required information to
#' @export
#'
#' @examples
#' # Not run:
#' # clam("Example")

# start modification by DNL
# clam <- function(name="Example", type=1, smooth=c(), prob=0.95, its=1000, wghts=1, cc=1, cc1="IntCal13.14C", cc2="Marine13.14C", cc3="SHCal13.14C", cc4="mixed.14C", cc5="gluedHemispheres.14C", postbomb=FALSE, pb1="postbomb_NH1.14C", pb2="postbomb_NH2.14C", pb3="postbomb_NH3.14C", pb4="postbomb_SH1-2.14C", pb5="postbomb_SH3.14C", outliers=c(), ignore=c(), youngest=c(), extradates=c(), slump=c(), est=1, calibt=FALSE, mixed.effect=FALSE, dmin=c(), dmax=c(), every=1, yrmin=c(), yrmax=c(), yrsteps=1, pbsteps=0.01, hpdsteps=1, BCAD=FALSE, decimals=0, accrate=0, ageofdepth=c(), depth="cm", depthseq=c(), depths.file=FALSE, thickness=1, hiatus=c(), remove.reverse=0.5, times=5, sep=",", ext=".csv", runname=c(), storedat=FALSE, threshold=1e-6, proxies=FALSE, revaxes=FALSE, revd=TRUE, revyr=TRUE, calhght=0.3, maxhght=0.01, mirror=TRUE, plotrange=TRUE, bty="l", mar=c(3.5,3,2,1), mgp=c(2,1,0), plotpdf=TRUE, plotpng=TRUE, greyscale=c(), yrlab=c(), dlab=c(), calcol=rgb(0,0.5,0.5,0.5), C14col=rgb(0,0,1,0.5), outcol="red", outlsize=1, bestcol="black", rangecol=rgb(0,0,0,0.3), slumpcol=grey(0.75), plotname=TRUE, ash=FALSE)
clam <- function(name="Example", type=1, smooth=c(), prob=0.95, its=1000, wghts=1, cc=1, cc1="IntCal13.14C", cc2="Marine13.14C", cc3="SHCal13.14C", cc4="mixed.14C", cc5="gluedHemispheres.14C", postbomb=FALSE, pb1="postbomb_NH1.14C", pb2="postbomb_NH2.14C", pb3="postbomb_NH3.14C", pb4="postbomb_SH1_2.14C", pb5="postbomb_SH3.14C", outliers=c(), ignore=c(), youngest=c(), extradates=c(), slump=c(), est=1, calibt=FALSE, mixed.effect=FALSE, dmin=c(), dmax=c(), every=1, yrmin=c(), yrmax=c(), yrsteps=1, pbsteps=0.01, hpdsteps=1, BCAD=FALSE, decimals=0, accrate=0, ageofdepth=c(), depth="cm", depthseq=c(), depths.file=FALSE, thickness=1, hiatus=c(), remove.reverse=0.5, times=5, sep=",", ext=".csv", runname=c(), storedat=FALSE, threshold=1e-6, proxies=FALSE, revaxes=FALSE, revd=TRUE, revyr=TRUE, calhght=0.3, maxhght=0.01, mirror=TRUE, plotrange=TRUE, bty="l", mar=c(3.5,3,2,1), mgp=c(2,1,0), plotpdf=TRUE, plotpng=TRUE, greyscale=c(), yrlab=c(), dlab=c(), calcol=rgb(0,0.5,0.5,0.5), C14col=rgb(0,0,1,0.5), outcol="red", outlsize=1, bestcol="black", rangecol=rgb(0,0,0,0.3), slumpcol=grey(0.75), plotname=TRUE, ash=FALSE){
#     .clam(name, type, smooth, prob, its, wghts, cc, cc1, cc2, cc3, cc4, cc5, postbomb, pb1, pb2, pb3, pb4, pb5, outliers, ignore, youngest, extradates, slump, est, calibt, mixed.effect, dmin, dmax, every, yrmin, yrmax, yrsteps, pbsteps, hpdsteps, BCAD, decimals, accrate, ageofdepth, depth, depthseq, depths.file, thickness, hiatus, remove.reverse, times, sep, ext, runname, storedat, threshold, proxies, revaxes, revd, revyr, calhght, maxhght, mirror, plotrange, bty, mar, mgp, plotpdf, plotpng, greyscale, yrlab, dlab, calcol, C14col, outcol, outlsize, bestcol, rangecol, slumpcol, plotname, ash)
# }
# # end modification by DNL
# 
# 
# .clam <- function(name, type, smooth, prob, its, wghts, cc, cc1, cc2, cc3, cc4, cc5, postbomb, pb1, pb2, pb3, pb4, pb5, outliers, ignore, youngest, extradates, slump, est, calibt, mixed.effect, dmin, dmax, every, yrmin, yrmax, yrsteps, pbsteps, hpdsteps, BCAD, decimals, accrate, ageofdepth, depth, depthseq, depths.file, thickness, hiatus, remove.reverse, times, sep, ext, runname, storedat, threshold, proxies, revaxes, revd, revyr, calhght, maxhght, mirror, plotrange, bty, mar, mgp, plotpdf, plotpng, greyscale, yrlab, dlab, calcol, C14col, outcol, outlsize, bestcol, rangecol, slumpcol, plotname, ash)
# {
    # warn and stop if abnormal settings are provided
    # start modification by DNL
    if(type > 5 || type < 1 || prob < 0 || prob > 1 || its < 100 || wghts < 0 || wghts > 1 || est < 1 || est > 7 || yrsteps <= 0 || hpdsteps <= 0 || every <= 0 || decimals < 0 || accrate < 0 || accrate > 1 || thickness < 0 || times < 1 || calhght < 0 || (type==5 && length(hiatus)>0)){
        stop("clam cannot run with these settings! Please check the manual.\n", call.=FALSE)
    }
    dets <- suppressWarnings(read.csv(paste("Cores/", name, "/", name, ext, sep=""), sep=sep))
    d <- dets[,6]
    if(min(diff(d)) < 0){
        warning("Depths not in ascending order (top ones should come first).\n")
    }
    # avoid confusing warning when using sample for first time in session
    tmp <- suppressWarnings(sample(1:1e3, 1, prob=rep(.001,1e3), replace=TRUE))

    # avoid Windows/Mac habit of silently adding .txt extension to plain text files
    win <- list.files(paste("Cores/", name, sep=""), pattern=".csv.txt")
    if(length(win) > 0)
    {
        cat("Removing unnecessary .txt extension from .csv file", win[1], "\n")
        file.rename(paste("Cores/", name, "/", name, ".csv.txt", sep=""),
                    paste("Cores/", name, "/", name, ".csv", sep=""))
    }

    # set the calibration curve

    # start modification by DNL
    #
    # if(cc==1) calcurve <- read.table(cc1) else
    #    if(cc==2) calcurve <- read.table(cc2) else
    #        if(cc==3) calcurve <- read.table(cc3) else
    #            if(cc==4) calcurve <- read.table(cc4) else
    #                if(cc==5) calcurve <- read.table(cc5) else
    #                    stop("I do not understand which calibration curve you mean, check the manual", call.=FALSE)
    #if(cc==1) ccname <- cc1 else
    #    if(cc==2) ccname <- cc2 else
    #        if(cc==3) ccname <- cc3 else
    #            if(cc==4) ccname <- cc4 else
    #                if(cc==5) ccname <- cc5

    # Check if mixed and glue calibration curves exist in the working environment

    if(cc==4 & !exists(cc4)) stop("Calibration curve does not exist. Please check the Clam manual and provide a valid calibration curve or generate one with function mix.curves()")
    if(cc==5 & !exists(cc5)) stop("Calibration curve does not exist. Please check the Clam manual and provide a valid calibration curve or generate one with function glue.curves()")

    if(cc==1){ calcurve <- get(cc1); ccname <- cc1 }else{
        if(cc==2){ calcurve <- get(cc2); ccname <- cc2 }else{
            if(cc==3){ calcurve <- get(cc3); ccname <- cc3 }else{
                if(cc==4){ calcurve <- get(cc4); ccname <- cc4 }else{
                    if(cc==5){ calcurve <- get(cc5); ccname <- cc5 }else{
                        stop("I do not understand which calibration curve you mean, check the manual", call.=FALSE)
                    }
                }
            }
        }
    }
    #     end modification by DNL

    # negative C14 ages and postbomb curve
    # pb <- 0
    pbnames <- c(pb1, pb2, pb3, pb4, pb5)
    cdat <- dets[,2]
    if(length(cdat[!is.na(cdat)]) > 0)
        if(min(cdat[!is.na(cdat)]) < 0)
            if(postbomb==FALSE)
                warning("Negative 14C ages, should I use a postbomb curve?\n") else
                {
                    if(postbomb>5)
                        stop("I do not understand which postbomb curve you mean, check the manual", call.=FALSE)
                    yrsteps <- min(pbsteps, yrsteps)

                    # start modification by DNL
                    # pb <- read.table(pbnames[postbomb])
                    pb <- get(pbnames[postbomb])
                    # end modification by DNL

                    pb.x <- seq(min(pb[,1]), max(pb[,1]), by=yrsteps)
                    pb.y <- approx(pb[,1], pb[,2], pb.x)$y
                    pb.sd <- approx(pb[,1], pb[,3], pb.x)$y
                    calcurve <- cbind(c(pb.x, calcurve[,1]), c(pb.y, calcurve[,2]), c(pb.sd, calcurve[,3]))
                }

    # work in BC/AD if needed, and prepare for calculations in f14C
    if(BCAD)
    {
        theta <- 1950-calcurve[,1]
        border <- max(which(theta > 0))
        theta <- c(theta[1:(border-1)], theta[border]:theta[border+2], theta[(border+3):length(theta)])
        mu <- approx(1950-calcurve[,1], calcurve[,2], theta)$y
        sigma <- approx(1950-calcurve[,1], calcurve[,3], theta)$y
        theta[theta <=0] <- theta[theta <=0]-1
        calcurve <- cbind(theta, mu, sigma)
    } else theta <- calcurve[,1]
    if(length(yrlab)==0) yrlab <- ifelse(BCAD, "cal BC/AD", "cal BP")
    f.mu <- exp(-calcurve[,2]/8033)
    f.sigma <- exp(-(calcurve[,2]-calcurve[,3])/8033) - f.mu


    # prepare for slumps and hiatuses
    cat(paste("Core name:", name))
    if(length(greyscale) > 0) storedat <- TRUE
    if(length(slump) > 0)
    {
        if(length(slump) %% 2 == 1)
            stop("slumps need both upper and lower depths. Please check the manual", call.=FALSE)
        slump <- matrix(sort(slump), ncol=2, byrow=TRUE)
        if(length(dmax)==0)
            dmax <- max(dets[,6])
        if(length(extradates) > 0)
            dmax <- max(dmax, extradates)
        for(i in 1:nrow(slump))
        {
            d[d > min(slump[i,])] <- d[d > min(slump[i,])] - (max(slump[i,]) - min(slump[i,]))
            dmax <- dmax - (max(slump[i,])-min(slump[i,]))
        }
        if(length(hiatus) > 0)
            for(i in 1:nrow(slump))
            {
                below.slump <- which(hiatus > max(slump[i,]))
                above.slump <- which(hiatus < min(slump[i,]))
                hiatus[below.slump] <- hiatus[below.slump] - (max(slump[i,])-min(slump[i,]))
                hiatus <- hiatus[c(above.slump, below.slump)]
            }
    }

    # read in the data
    dat <- .read.clam(name, ext, hpdsteps, yrsteps, prob, times, sep, BCAD, storedat, ignore, thickness, youngest, slump, threshold, theta, f.mu, f.sigma, calibt, extradates, calcurve, pb)
    cat("\n Calibrating dates... ")


    # calculate the depths to be used, based on the ranges and resolution
    if(length(dmin)==0) dmin <- floor(min(dat$depth))
    if(length(dmax)==0) dmax <- ceiling(max(dat$depth))
    if(depths.file)
        if(file.exists(dd <- paste("Cores/", name, "/", name, "_depths.txt", sep="")))
        {
            if(length(depthseq) == 0)
                depthseq <- seq(dmin, dmax, by=every)
            depthseq <- sort(unique(c(depthseq, suppressWarnings(read.table(dd))[,1])))
            dmin <- min(depthseq)#, read.table(dd)[,1])
            dmax <- max(depthseq)#, read.table(dd)[,1])
        } else
            stop(paste("\nCannot find file ", dat$name, "_depths.txt!\n", sep=""), call.=FALSE)
    if(length(depthseq) == 0)
        depthseq <- seq(dmin, dmax, by=every)
    if(proxies)
    {
        storedat <- TRUE
        if(file.exists(dd <- paste("Cores/", name, "/", name, "_proxies.csv", sep="")))
            dat$proxies <- suppressWarnings(read.csv(dd, sep=sep)) else
                stop(paste("\nCannot find file ", dat$name, " _proxies.csv!\n", sep=""), call.=FALSE)
        dmin <- min(depthseq, dat$proxies[,1])
        dmax <- max(depthseq, dat$proxies[,1])
        depthseq <- sort(unique(c(depthseq, dat$proxies[,1])))
    }

    if(length(ageofdepth) > 0)
        depthseq <- sort(unique(c(ageofdepth, depthseq)))

    # decide which models and point estimates should be used
    if(any(type==c(1, "int", "inter", "interp"))) type <- 1 else
        if(any(type==c(2, "reg", "regr", "poly", "polyn"))) type <- 2 else
            if(any(type==c(3, "spline", "spl"))) type <- 3 else
                if(any(type==c(4, "smooth", "sm"))) type <- 4 else
                    if(any(type==c(5, "loess", "lowess"))) type <- 5
    if(est==1 || est==2) Est <- dat$mid1 else # 1 dummy, calculated later
        if(est==3) Est <- dat$mid1 else
            if(est==4) Est <- dat$wmn else
                if(est==5) Est <- dat$med else
                    if(est==6) Est <- dat$mode else
                        if(est==7) Est <- dat$mid2

    # remove outliers from the age-depth modelling
    if(length(outliers) > 0)
    {
        depths <- dat$depth[-outliers]
        errors <- dat$error[-outliers]
        calibs <- dat$calib[-outliers]
        Est <- Est[-outliers]
    } else
    {
        depths <- dat$depth
        errors <- dat$error
        calibs <- dat$calib
    }

    # age-depth modelling with curves through sampled age estimates
    # in sections if one or more hiatuses are present
    if(length(hiatus) > 0)
    {
        allrange <- c(0,0,0,0)
        hiatusseq <- sort(c(range(depthseq), hiatus))
        for(i in 2:length(hiatusseq))
        {
            cat(paste("\n section ", i-1, ",", sep=""))
            section <- depthseq[min(which(depthseq >= hiatusseq[i-1])) : max(which(depthseq <= hiatusseq[i]))]
            if(i>2) section <- section[-1]
            sel <- min(which(depths >= min(section))):max(which(depths <= max(section)))
            if(mixed.effect)
                if(length(outliers) > 0)
                    smp <- .mixed.effect(its, depths, dat$cal[-outliers], dat$cage[-outliers], errors, calibs, Est, theta, f.mu, f.sigma, yrsteps, calibt) else
                        smp <- .mixed.effect(its, depths, dat$cal, dat$cage, errors, calibs, est, theta, f.mu, f.sigma, yrsteps, calibt) else
                            smp <- .smpl(its, depths[sel], calibs[sel], Est[sel])
            calrange <- .model.clam(type, smooth, its, wghts, depths[sel], errors[sel], section, prob, est, dat, smp, greyscale, remove.reverse, storedat, ageofdepth, BCAD)
            allrange <- rbind(allrange, calrange)
        }
        calrange <- allrange[2:nrow(allrange),]
    } else
    {
        if(mixed.effect)
            if(length(outliers) > 0)
                smp <- .mixed.effect(its, depths, dat$cal[-outliers], dat$cage[-outliers], errors, calibs, est, theta, f.mu, f.sigma, yrsteps, calibt) else
                    smp <- .mixed.effect(its, depths, dat$cal, dat$cage, errors, calibs, Est, theta, f.mu, f.sigma, yrsteps, calibt) else
                        smp <- .smpl(its, depths, calibs, Est)
                    calrange <- .model.clam(type, smooth, its, wghts, depths, errors, depthseq, prob, est, dat, smp, greyscale, remove.reverse, storedat, ageofdepth, BCAD)
    }
    dat$model <- approx(calrange[,1], (calrange[,2]+calrange[,3])/2, dat$depth)$y

    if(est==2) calrange[,4] <- (calrange[,2]+calrange[,3])/2
    if(!BCAD && any(diff(calrange[,4]) < 0) || BCAD && any(diff(calrange[,4]) > 0))
        reversal <- TRUE else reversal <- FALSE
    gfit <- round(.gfit(theta, f.mu, f.sigma, dat, calrange, outliers), 2)

    # re-correct the depths if slumps were applied
    if(length(slump) > 0)
    {
        dat <- .read.clam(name, ext, hpdsteps, yrsteps, prob, times, sep, BCAD, storedat, ignore, thickness, youngest, slump=c(), threshold, theta, f.mu, f.sigma, calibt, extradates, calcurve, bp) # read in the original dates again
        calrange <- calrange[which(calrange[,1] <= dmax),]
        d <- calrange[,1]
        for(i in 1:nrow(slump))
        {
            d[d > min(slump[i,])] <- d[d > min(slump[i,])] + (max(slump[i,]) - min(slump[i,]))
            dmax <- dmax + (max(slump[i,]) - min(slump[i,]))
            calrange[,1] <- d
            hiatus[hiatus > min(slump[i,])] <- hiatus[hiatus > min(slump[i,])] + (max(slump[i,]) - min(slump[i,]))
        }
    }

    # produce the age-depth plot, and a pdf copy if desired
    if(length(yrmin)==0) yrmin <- min(dat$mid1, calrange[,2])
    if(length(yrmax)==0) yrmax <- max(dat$mid1, calrange[,3])
    if(length(ageofdepth > 0)) layout(matrix(c(1,2,1,3), nrow=2), heights=c(.7,.3))
    .ageplot(yrmin, yrmax, dmin, dmax, revaxes, revd, revyr, yrlab, dlab, hiatus, depthseq, outliers, plotrange, BCAD, greyscale, if(length(greyscale)>0) chron else c(), C14col, outcol, outlsize, bestcol, rangecol, dat, calrange, depth, calhght, maxhght, mirror, calcol, slump, slumpcol, plotname, name, bty, mar, mgp, ash)

    # write files providing calibrated dates, age-model and settings
    colnames(calrange) <- c("Depth", paste("min.", 100*prob, "%range", sep=""), paste("max.", 100*prob, "%range", sep=""), "point")
    .write.clam(dat, runname, calrange, name, prob, type, remove.reverse, smooth, wghts, its, outliers, ignore, est, BCAD, yrsteps, every, decimals, accrate, depth, depthseq, hiatus, gfit, reversal, plotpdf, plotpng, yrmin, yrmax, dmin, dmax, dlab, yrlab, plotrange, greyscale, if(length(greyscale)>0) chron else c(), C14col, outcol, outlsize, bestcol, rangecol, calhght, maxhght, mirror, calcol, slump, slumpcol, revaxes, revyr, revd, calibt, youngest, extradates, plotname, calcurve, ccname, postbomb, pbnames, depths.file, bty, mar, mgp, ash)
    closeAllConnections()

    if(storedat)
    {
        calrange <<- calrange
        dat <<- dat
        smp <<- smp
    }

    # plot the age distribution of a provided depth
    if(length(ageofdepth) > 0)
    {
        if(revaxes)
            abline(v=ageofdepth, lty=2) else
                abline(h=ageofdepth, lty=2)
        xlim <- range(.ageofdepth)
        if(!BCAD) xlim <- xlim[2:1]
        hst <- density(.ageofdepth, n=max(1, max(xlim)-min(xlim)))
        yr <- seq(min(xlim), max(xlim), by=yrsteps)
        hst <- cbind(c(yr, max(xlim), min(xlim)), c(approx(hst$x, hst$y, yr)$y, 0, 0))
        plot(hst, type="n", main="", xlim=xlim, xlab=yrlab, ylab="")
        polygon(hst, col="grey")
        legend("topleft", paste(ageofdepth, depth), bty="n")
        layout(matrix(1))
        rng <- round(calrange[max(which(calrange[,1] <= ageofdepth)),])
        cat("\n  Age range of ", ageofdepth, " ", depth, ": ", rng[3], " to ", rng[2], ifelse(BCAD, " cal BC/AD", " cal BP"), " (", rng[3]-rng[2], " yr, ", prob, " % range)",  sep="")
    }

    # report the confidence ranges, the goodness-of-fit, and whether any age-reversals occurred
    rng <- round(calrange[,3]-calrange[,2])
    cat("\n  ", name, "'s ", 100*prob, "% confidence ranges span from ", min(rng), " to ", max(rng), " yr (average ", round(mean(rng)), " yr)", sep="")
    cat("\n  Fit (-log, lower is better):", gfit, "\n")
    if(reversal) cat("  Age reversals occurred. Try other model?\n")
}






#' Title TBW
#'
#' @param ratio TBW
#' @param cc1 TBW 
#' @param cc2 TBW 
#' @param offset TBW 
#'
#' @return TBW
#' 
#' @export
#'
#' @examples #TBW
# if two curves need to be 'mixed' to calibrate, e.g. for dates of mixed terrestrial and marine carbon sources
#
# start modificaction by DNL
# mix.curves <- function(ratio=.5, cc1="IntCal13.14C", cc2="Marine13.14C", name="mixed.14C", offset=c(0,0))
mix.curves <- function(ratio=.5, cc1="IntCal13.14C", cc2="Marine13.14C", offset=c(0,0)){
# end modification by DNL
    
    # start modification by DNL
    # cc1 <- read.table(cc1)
    # cc2 <- read.table(cc2)
    cc1 <- get(cc1)
    cc2 <- get(cc2)
    # end modification by DNL

    cc2.mu <- approx(cc2[,1], cc2[,2], cc1[,1], rule=2)$y + offset[1] # interpolate cc2 to the calendar years of cc1
    cc2.error <- approx(cc2[,1], cc2[,3], cc1[,1], rule=2)$y
    cc2.error <- sqrt(cc2.error^2 + offset[2]^2)
    mu <- ratio * cc1[,2] + (1-ratio) * cc2.mu
    error <- ratio * cc1[,3] + (1-ratio) * cc2.error

    # start modification by DNL
    # write.table(cbind(cc1[,1], mu, error), name, row.names=FALSE, col.names=FALSE, sep="\t")
    mixed.14C <- cbind(cc1[,1], mu, error)
    return(mixed.14C)
    # end modification by DNL
}





#' Title #TBW
#'
#' @param nh  #TBW
#' @param sh  #TBW
#' @param offset  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples #TBW
# 'glue' two curves together, e.g., for calibrating southern hemisphere dates older than SHCal04
#
# start modification by DNL
#
#glue.curves <- function(nh="IntCal09.14C", sh="SHCal04.14C", offset=c(56, 24), name="gluedHemispheres.14C")
glue.curves <- function(nh="IntCal09.14C", sh="SHCal04.14C", offset=c(56, 24)){
# end modification by DNL
    nh <- nh[nh[,1] > max(sh[,1]),] # only use years beyond SHCal04
    nh[,2] <- nh[,2] + offset[1] # nh to sh, means
    nh[,3] <- sqrt(nh[,3]^2 + offset[2]^2) # errors, squared summed

    # start modification by DNL
    # write.table(rbind(sh, nh), "gluedHemispheres.14C", sep="\t", row.names=F, col.names=F)
    glued.14C <- rbind(sh, nh)
    return(glued.14C)
    # end modification by DNL
}





#' Title #TBW
#'
#' @param mn  #TBW
#' @param sdev  #TBW
#' @param ratio  #TBW
#' @param decimals  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# calculate C14 ages from pmC values
pMC.age <- function(mn, sdev, ratio=100, decimals=0){
    y <- -8033*log(mn/ratio)
    sdev <- y - -8033*log((mn+sdev)/ratio)
    round(c(y, sdev), decimals)
}





#' Title #TBW
#'
#' @param mn  #TBW
#' @param sdev  #TBW
#' @param ratio  #TBW
#' @param decimals  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# calculate pMC values from C14 ages
age.pMC <- function(mn, sdev, ratio=100, decimals=3){
    y <- exp(-mn/8033)
    sdev <- y - exp(-(mn+sdev)/8033)
    signif(ratio*c(y, sdev), decimals)
}






#' Title #TBW
#'
#' @param t.a  #TBW
#' @param t.b  #TBW
#' @param f.cage  #TBW
#' @param f.error  #TBW
#' @param theta  #TBW
#' @param f.mu  #TBW
#' @param f.sigma  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# See Christen and Perez 2009, Radiocarbon 51:1047-1059. Instead of assuming the standard Gaussian model (default in clam), a student t distribution can be used with two parameters. Christen and Perez 2009 suggest t.a = 3 and t.b = 4; this can be put as clam( calibt=c(3,4) )
.calibt <- function(t.a, t.b, f.cage, f.error, theta, f.mu, f.sigma){
    (t.b + ((f.cage-f.mu)^2) / (2*(f.sigma^2 + f.error^2))) ^ (-1*(t.a+0.5))
}
    



#' Title #TBW
#'
#' @param y  #TBW
#' @param error  #TBW
#' @param t.a  #TBW
#' @param t.b  #TBW
#' @param cc  #TBW
#' @param postbomb #TBW 
#' @param cc1  #TBW
#' @param cc2  #TBW
#' @param cc3  #TBW
#' @param cc4  #TBW
#' @param Cutoff  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# should really be done in F14C
student.t <- function(y=2450, error=50, t.a=3, t.b=4, cc=1, postbomb=c(), cc1="IntCal13", cc2="Marine13", cc3="SHCal13", cc4="ConstCal", Cutoff=1e-5){
    if(cc==0)
    {
        cc <- seq(y-10*error, y+10*error, length=1e3)
        cc <- cbind(cc, cc, rep(0, length(cc)))
    } else
    {
        # start modification by DNL
        # if(cc1=="IntCal13") cc1 <- read.table("IntCal13.14C") else
        #     cc1 <- read.csv(cc1)[,1:3]
        # if(cc2=="Marine13") cc2 <- read.table("Marine13.14C") else
        #     cc2 <- read.csv(cc2)[,1:3]
        # if(cc3=="SHCal13") cc3 <- read.table("SHCal13.14C") else
        #     cc3 <- read.table(cc3)[,1:3]
        # if(cc4 != "ConstCal") cc4 <- read.table(cc4)[,1:3]
        if(cc1=="IntCal13") cc1 <- get("IntCal13.14C") else
            cc1 <- read.csv(cc1)[,1:3]
        if(cc2=="Marine13") cc2 <- get("Marine13.14C") else
            cc2 <- read.csv(cc2)[,1:3]
        if(cc3=="SHCal13") cc3 <- get("SHCal13.14C") else
            cc3 <- read.table(cc3)[,1:3]
        if(cc4 != "ConstCal") cc4 <- get(cc4)[,1:3]

        # end modification by DNL

        if(cc==1) cc <- cc1 else if(cc==2) cc <- cc2 else if(cc==3) cc <- cc3 else cc <- cc4
    }

    if(y < 0)
        if(length(postbomb) > 0)
        {
            # start modification by DNL
            # if(postbomb==1) bomb <- read.table("postbomb_NH1.14C")[,1:3] else
            #     if(postbomb==2) bomb <- read.table("postbomb_NH2.14C")[,1:3] else
            #         if(postbomb==3) bomb <- read.table("postbomb_NH3.14C")[,1:3] else
            #             if(postbomb==4) bomb <- read.table("postbomb_SH1-2.14C")[,1:3] else
            #                 if(postbomb==5) bomb <- read.table("postbomb_SH3.14C")[,1:3] else
            if(postbomb==1) bomb <- get("postbomb_NH1.14C") else
                if(postbomb==2) bomb <- get("postbomb_NH2.14C") else
                    if(postbomb==3) bomb <- get("postbomb_NH3.14C") else
                        if(postbomb==4) bomb <- get("postbomb_SH1_2.14C") else
                            if(postbomb==5) bomb <- get("postbomb_SH3.14C") else                      # end modification by DNL

                                stop("Cannot find postbomb curve #", postbomb, " (use values of 1 to 5 only)")
                            bomb.x <- seq(max(bomb[,1]), min(bomb[,1]), by=-.1) # interpolate
                            bomb.y <- approx(bomb[,1], bomb[,2], bomb.x)$y
                            bomb.z <- approx(bomb[,1], bomb[,3], bomb.x)$y
                            bomb <- cbind(bomb.x, bomb.y, bomb.z, deparse.level=0)

                            # start modification by DNL
                            # if(info$postbomb < 4)
                            if(postbomb < 4){
                            # end modification by DNL
                                cc1 <- rbind(bomb, cc1, deparse.level=0)
                            }else{
                                cc3 <- rbind(bomb, cc3, deparse.level=0)
                            }
        }

    norm.cal <- dnorm(cc[,2], y, sqrt(cc[,3]^2+error^2))
    t.cal <- (t.b + ((y-cc[,2])^2) / (2*(cc[,3]^2 + error^2))) ^ (-1*(t.a+0.5))

    norm.cal <- cbind(cc[,1], norm.cal/sum(norm.cal))
    acc <- which(norm.cal[,2] >= Cutoff)
    if(length(acc) > 1) norm.cal <- norm.cal[acc,]

    t.cal <- cbind(cc[,1], t.cal/sum(t.cal))
    acc <- which(t.cal[,2] >= Cutoff)
    if(length(acc) > 1) t.cal <- t.cal[acc,]
    t.cal <- cbind(c(min(t.cal[,1]), t.cal[,1], max(t.cal[,1])), c(0, t.cal[,2], 0))

    plot(norm.cal, type="l", xlab="cal BP", xlim=range(c(t.cal[,1], norm.cal[,1]))[2:1], ylab="", ylim=c(0, max(t.cal[,2], norm.cal[,2])), col=2, lwd=1.5)
    polygon(t.cal, col=rgb(0,0,0,.25), border=rgb(0,0,0,.5))
    legend("topright", "Gaussian", text.col=2, bty="n")
    legend("topright", paste("\nstudent-t (a=", t.a, ", b=", t.b, ")", sep=""), bty="n", text.col=grey(.4))
}




#' Title #TBW
#'
#' @param f.cage  #TBW
#' @param f.error  #TBW
#' @param theta  #TBW
#' @param f.mu  #TBW
#' @param f.sigma  #TBW
#' @param yrsteps  #TBW
#' @param threshold  #TBW
#' @param calibt  #TBW
#' @param BCAD  #TBW
#' @param normalise  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# find the calibrated distributions of 14C dates
.caldist <- function(f.cage, f.error, theta, f.mu, f.sigma, yrsteps, threshold, calibt, BCAD, normalise=FALSE){
    if(f.cage > 1)
    {
        if(f.cage > 1) yrsteps <- min(yrsteps, .1)
        pb <- theta[which(f.mu > 1)]
        if(length(pb)==0)
            stop("help, something exploded with a postbomb date")
        x <- approx(theta, f.mu, seq(min(pb), max(pb), by=yrsteps))
        xsd <- approx(theta, f.sigma, x$x)$y
        theta <- c(x$x, theta[which(f.mu <= 0)])
        f.mu <- c(x$y, f.mu[which(f.mu <= 0)])
        f.sigma <- c(xsd, f.sigma[which(f.mu <= 0)])
        threshold <- 0
    }

    # calibrate; find how far f.cage (measurement) is from f.mu (calibration curve)
    if(length(calibt) < 2)
        cal <- cbind(theta, dnorm(f.mu, f.cage, sqrt(f.error^2+f.sigma^2))) else
            cal <- cbind(theta, .calibt(calibt[1], calibt[2], f.cage, f.error, theta, f.mu, f.sigma))

        # interpolate and normalise calibrated distribution to 1
        cal <- cal[min(which(cal[,2] > 0)):max(which(cal[,2] > 0)),] # remove unnecessary data
        cal <- approx(cal[,1], cal[,2], seq(min(cal[,1]), max(cal[,1]), by=yrsteps))
        cal <- cbind(cal$x, cal$y/sum(cal$y))
        if(BCAD && (0 %in% cal[,1]))
            cal <- cal[-which(cal[,1]==0),] # 0 BC/AD does not exist
        # only report those normalised calibrated probabilities beyond a threshold
        cal[cal[,2] > threshold,]
}




#' Title #TBW
#'
#' @param dat  #TBW
#' @param prob  #TBW
#' @param hpdsteps  #TBW
#' @param yrsteps  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# find the highest posterior density (hpd) of the calibrated distribution
.hpd <- function(dat, prob, hpdsteps, yrsteps){
    # interpolate and rank the ages according to their calibrated distribution probabilities
    dat <- approx(dat[,1], dat[,2], seq(min(dat[,1]), max(dat[,1]), by=yrsteps))
    o <- order(dat$y, decreasing=TRUE)
    dat <- cbind(dat$x[o], dat$y[o]/sum(dat$y))

    # only retain those ages with cumulative normalised probabilities within required percentage
    dat <- dat[which(cumsum(dat[,2]) <= prob),]
    dat <- dat[order(dat[,1]),]

    # identify any individual ranges within the hpd range and calculate their probability
    dif <- which(diff(dat[,1]) > hpdsteps)
    if(length(dif)==0)
        hpds <- cbind(min(dat[,1]), max(dat[,1]), 100*prob) else
        {
            dif <- c(dat[1,1], sort(c(dat[dif,1], dat[dif+1,1])), dat[nrow(dat),1])
            dif <- matrix(dif, ncol=2, byrow=TRUE)
            probs <- c()
            for(i in 1:nrow(dif))
                probs[i] <- round(100*sum(dat[which(dat[,1]==dif[i,1]):which(dat[,1]==dif[i,2]),2]), 1)
            hpds <- cbind(dif, probs)
        }
    hpds
}




#' Title #TBW
#'
#' @param type  #TBW
#' @param smooth  #TBW
#' @param its  #TBW
#' @param wghts  #TBW
#' @param depths  #TBW
#' @param errors  #TBW
#' @param depthseq  #TBW
#' @param prob  #TBW
#' @param est  #TBW
#' @param dat  #TBW
#' @param smp  #TBW
#' @param greyscale  #TBW
#' @param remove.reverse  #TBW
#' @param storedat  #TBW
#' @param ageofdepth  #TBW
#' @param BCAD  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# calculate the age-depth model and its uncertainty
.model.clam <- function(type, smooth, its, wghts, depths, errors, depthseq, prob, est, dat, smp, greyscale, remove.reverse, storedat, ageofdepth, BCAD){
    # warn for extrapolation, refuse to do so for loess
    if(min(depthseq) < min(dat$depth) || max(depthseq) > max(dat$depth))
        if(type==5)
            stop(" cannot extrapolate using loess! Change settings.\n ", call.=FALSE) else
                warning(" extrapolating beyond dated levels, dangerous!\n ")

    # choose model: interpolation, (polynomial) regression, spline, smooth spline or loess
    chron <- array(0, dim=c(length(depthseq), its))
    if(type==1) chron <- .interp(depthseq, depths, its, chron, smp) else
        if(type==2) chron <- .poly(depthseq, smooth, wghts, errors, depths, its, chron, smp) else
            if(type==3) chron <- .spline(depthseq, smooth, depths, its, chron, smp) else
                if(type==4) chron <- .smooth(depthseq, smooth, wghts, errors, depths, its, chron, smp) else
                    if(type==5) chron <- .loess(depthseq, smooth, wghts, errors, depths,  its, chron, smp)

    # test against age reversals
    warp <- c()
    if(remove.reverse!=FALSE)
        for(i in 1:ncol(chron))
            if(!BCAD && min(diff(chron[,i])) <= 0 || BCAD && max(diff(chron[,i])) >= 0)
                warp <- c(warp, i)
    if(length(warp) > 0)
        if(length(warp) > remove.reverse*its)
            warning(" !!! Too many models with age reversals!!!\n") else
            {
                cat("\n Removing", length(warp), "models with age reversals,", its-length(warp), "models left...")
                chron <- chron[,-warp]
                smp <- smp[,-warp,]
            }

    if(length(ageofdepth) > 0)
        if(ageofdepth %in% depthseq)
            .ageofdepth <<- chron[which(depthseq==ageofdepth),]

    if(storedat)
    {
        chron <<- chron
        smp <<- smp
    }

    # find uncertainty ranges of calendar age for each depth of the core
    calrange <- array(0, dim=c(nrow(chron), 2))
    wm <- c()
    for(i in 1:nrow(chron))
    {
        x <- chron[i,2:ncol(chron)]
        qp <- (1-prob)/2
        calrange[i,] <- quantile(x, c(qp, 1-qp))
        if(est==1) wm[i] <- weighted.mean(x)
    }
    if(est==1)
        cbind(depthseq, cbind(calrange, wm)) else
            cbind(depthseq, cbind(calrange, chron[,1]))
}



#' Title #TBW
#'
#' @param its  #TBW
#' @param depths  #TBW
#' @param calibs  #TBW
#' @param Est  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# sample point age estimates from the calibrated distributions ('its' times)
# the probability of a year being sampled is proportional to its calibrated probability
.smpl <- function(its, depths, calibs, Est){
    smp <- array(1, dim=c(length(depths), 1+its, 2))
    smp[,1,1] <- Est
    for(i in 1:length(calibs))
        smp[i,(1:its)+1,] <-
        calibs[[i]][sample(1:length(calibs[[i]][,1]), its, prob=calibs[[i]][,2], TRUE),]
    smp
}




#' Title #TBW
#'
#' @param its  #TBW
#' @param depths  #TBW
#' @param cals  #TBW
#' @param cages  #TBW
#' @param errors  #TBW
#' @param calibs  #TBW
#' @param Est  #TBW
#' @param theta  #TBW
#' @param f.mu  #TBW
#' @param f.sigma  #TBW
#' @param yrsteps  #TBW
#' @param calibt  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# akin to Heegaard et al.'s mixed effect modelling, but using calibrated dates
.mixed.effect <- function(its, depths, cals, cages, errors, calibs, Est, theta, f.mu, f.sigma, yrsteps, calibt){
    cat("\n Mixed effect modelling, this will take some time")
    smp <- array(1, dim=c(length(depths), 1+its, 2))
    smp[,1,1] <- Est
    for(i in 1:length(cals))
        if(!is.na(cals[i]))
            if(length(calibt)==0)
            {
                x <- rnorm(its, cals[i], errors[i])
                smp[i,(1:its)+1,] <- c(x, dnorm(x, cals[i], errors[i]))
            } else
            {
                x <- (cals[i]-10*errors[i]) : (cals[i]+10*errors[i])
                x <- cbind(x, .calibt(calibt[1], calibt[2], cals[i], errors[i], x, x, 0))
                o <- order(x[,2], decreasing=TRUE)
                x <- cbind(x[o,1], cumsum(x[o,2])/sum(x[,2]))
                sampled.x <- max(which(x[,2] <= runif(1, 0, max(x[,2]))))
                smp[i,(1:its)+1,] <- x[sampled.x,]
            } else
                for(j in 1:its)
                {
                    if(j/(its/3) == round(j/(its/3))) cat(".")
                    yr <- rnorm(1, cages[i], errors[i])
                    f.yr <- exp(-yr/8033)
                    f.error <- f.yr - exp(-(yr+errors[i])/8033)
                    yr <- cbind(theta, dnorm(f.mu, f.yr, sqrt(f.error^2+f.sigma^2)))
                    yr <- yr[yr[,2]>0,]
                    yr <- approx(yr[,1], yr[,2], seq(min(yr[,1]), max(yr[,1]), by=yrsteps))
                    smp.yr <- sample(length(yr$x), 1, prob=yr$y)
                    smp[i,j+1,] <- c(yr$x[smp.yr], yr$y[smp.yr])
                }
    smp
}




#' Title #TBW
#'
#' @param depthseq  #TBW
#' @param depths  #TBW
#' @param its  #TBW
#' @param chron  #TBW
#' @param smp  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# interpolate linearly between the data (default)
.interp <- function(depthseq, depths, its, chron, smp){
    cat(" Interpolating, sampling")
    for(i in 1:its)
    {
        temp <- approx(depths, smp[,i,1], depthseq, ties=mean)$y

        # allow for extrapolation... dangerous!
        if(min(depthseq) < min(depths))
        {
            minus <- which(depthseq < min(depths))
            slope <- diff(temp)[max(minus)+1]/diff(depthseq)[max(minus)+1]
            temp[minus] <- temp[max(minus)+1] + slope * (depthseq[minus] - min(depths))
        }
        if(max(depthseq) > max(depths))
        {
            maxim <- which(depthseq > max(depths))
            slope <- diff(temp)[min(maxim)-2]/diff(depthseq)[min(maxim)-2]
            temp[maxim] <- temp[min(maxim)-1] + slope * (depthseq[maxim] - max(depths))
        }
        chron[,i] <- temp
        if(i/(its/5) == round(i/(its/5))) cat(".")
    }
    chron
}



#' Title #TBW
#'
#' @param depthseq  #TBW
#' @param smooth  #TBW
#' @param wghts  #TBW
#' @param errors  #TBW
#' @param depths  #TBW
#' @param its  #TBW
#' @param chron  #TBW
#' @param smp  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# polynomial regressions of certain order through the data (default linear, y=ax+b)
.poly <- function(depthseq, smooth, wghts, errors, depths, its, chron, smp){
    if(length(smooth)==0)
        cat(" Using linear regression, sampling") else
            cat(paste(" Using polynomial regression (degree ", smooth, "), sampling", sep=""))
    if(wghts==0) w <- c() else w <- 1/errors^2
    for(i in 1:its)
    {
        if(wghts==1) w <- smp[,i,2]
        chron[,i] <- predict(lm(smp[,i,1] ~ poly(depths, max(1, smooth)), weights=w), data.frame(depths=depthseq))
        if(i/(its/5) == round(i/(its/5))) cat(".")
    }
    chron
}



#' Title #TBW
#'
#' @param depthseq  #TBW
#' @param smooth  #TBW
#' @param depths  #TBW
#' @param its  #TBW
#' @param chron  #TBW
#' @param smp  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# fit cubic spline interpolations through the data
.spline <- function(depthseq, smooth, depths, its, chron, smp){
    if(length(smooth) < 1) smooth <- .3
    cat(paste(" Using cubic spline sampling", sep=""))
    for(i in 1:its)
    {
        chron[,i] <- spline(depths, smp[,i,1], xout=depthseq)$y
        if(i/(its/5) == round(i/(its/5))) cat(".")
    }
    chron
}


#' Title #TBW
#'
#' @param depthseq  #TBW
#' @param smooth  #TBW
#' @param wghts  #TBW
#' @param errors  #TBW
#' @param depths  #TBW
#' @param its  #TBW
#' @param chron  #TBW
#' @param smp  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# fit cubic smoothed splines through the data, with smoothing factor
.smooth <- function(depthseq, smooth, wghts, errors, depths, its, chron, smp){
    if(length(smooth) < 1) smooth <- .3
    cat(paste(" Using smoothing spline (smoothing ", smooth, "), sampling", sep=""))
    if(wghts==0) w <- c() else w <- 1/errors^2
    for(i in 1:its)
    {
        if(wghts==1) w <- smp[,i,2]
        chron[,i] <- predict(smooth.spline(depths, smp[,i,1], w=w, spar=smooth), depthseq)$y
        if(i/(its/5) == round(i/(its/5))) cat(".")
    }
    chron
}



#' Title #TBW
#'
#' @param depthseq  #TBW
#' @param smooth  #TBW
#' @param wghts  #TBW
#' @param errors  #TBW
#' @param depths  #TBW
#' @param its  #TBW
#' @param chron  #TBW
#' @param smp  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# fit locally weighted (1/errors^2) splines through the data, with smoothing factor
.loess <- function(depthseq, smooth, wghts, errors, depths, its, chron, smp){
    if(length(smooth) < 1) smooth <- .75
    cat(paste(" Using loess (smoothing ", smooth, "), sampling", sep=""))
    if(wghts==0) w <- c() else w <- 1/errors^2
    for(i in 1:its)
    {
        if(wghts==1) w <- smp[,i,2]
        chron[,i] <- predict(loess(smp[,i,1] ~ depths, weights=w, span=smooth), depthseq)
        if(i/(its/5) == round(i/(its/5))) cat(".")
    }
    chron
}



#' Title #TBW
#'
#' @param name  #TBW
#' @param ext  #TBW
#' @param hpdsteps  #TBW
#' @param yrsteps  #TBW
#' @param prob  #TBW
#' @param times  #TBW
#' @param sep  #TBW
#' @param BCAD  #TBW
#' @param storedat  #TBW
#' @param ignore  #TBW
#' @param thickness  #TBW
#' @param youngest  #TBW
#' @param slump  #TBW
#' @param threshold  #TBW
#' @param theta  #TBW
#' @param f.mu  #TBW
#' @param f.sigma  #TBW
#' @param calibt  #TBW
#' @param extradates  #TBW
#' @param calcurve  #TBW
#' @param postbomb  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# read the data and perform first calculations incl. calibrations
.read.clam <- function(name, ext, hpdsteps, yrsteps, prob, times, sep, BCAD, storedat, ignore, thickness, youngest, slump, threshold, theta, f.mu, f.sigma, calibt, extradates, calcurve, postbomb){
    # read the file with the dating information
    dat <- list(coredir=paste("Cores/", name, "/", sep=""), name=name)
    if(!file.exists(paste("Cores/", name, sep="")))
        stop(paste("Cannot find a folder within Cores/ named ", name, ". Have you saved it in the right place and with the right name? Please check the manual\n\n", sep=""), call.=FALSE)
    if(!file.exists(paste(dat$coredir, name, ext, sep="")))
        stop(paste("Cannot find file ", name, ".csv in folder Cores/", name, ". Have you saved it in the right place and named it correctly? Please check the manual\n\n", sep=""), call.=FALSE)
    dets <- suppressWarnings(read.table(paste(dat$coredir, name, ext, sep=""), comment.char="", header=TRUE, sep=sep, na.strings = c("#N/A!", "NA", "@NA")))

    # ignore dates if required, add thickness column if it was left out
    if(length(ignore) > 0)
    {
        dat$ignore <- as.character(dets[ignore,1])
        dets <- dets[-ignore,]
    }
    if(ncol(dets) < 7)
        dets <- cbind(dets, thickness) else
            dets[is.na(dets[,7]),7] <- thickness

    # should slumps be taken into account?
    if(length(slump) > 0)
    {
        d.adapt <- dets[,6]
        d.lost <- c()
        for(i in 1:nrow(slump))
        {
            below.slump <- which(dets[,6] > max(slump[i,]))
            above.slump <- which(dets[,6] < min(slump[i,]))
            d.lost <- c(d.lost, which(!(1:nrow(dets) %in% c(above.slump, below.slump))))
            d.adapt[below.slump] <- d.adapt[below.slump] - (max(slump[i,])-min(slump[i,]))
        }
        dets[,6] <- d.adapt
        if(length(d.lost) > 0)
            dets <- dets[-d.lost,]
    }
    # check for common errors
    dets <- dets[,1:7]
    x <- 0
    for(i in 2:7) if(is.factor(dets[,i])) x <- 1
    if(x==1) stop(paste("\n Some value fields in ", name, ".csv contain letters, please adapt", sep=""), call.=FALSE)
    if(length(dets[is.na(dets[,2]),2])+length(dets[is.na(dets[,3]),3]) != nrow(dets))
        stop(paste("\n Remove duplicate entries within the C14 and calendar fields in ", name, ".csv", sep=""), call.=FALSE)
    if(min(dets[,4]) <= 0)
        stop(paste("\n Errors of dates should be larger than zero. Please adapt ", name, ".csv", sep=""), call.=FALSE)
    dat$ID <- as.character(dets[,1])

    # correct for any reservoir effect
    dets[is.na(dets[,5]),5] <- 0
    dat$cage <- dets[,2] - dets[,5]
    dat$error <- dets[,4]

    # work in F14C for calibration
    dat$f.cage <- exp(-dat$cage/8033)
    dat$f.error <- dat$f.cage - exp(-(dat$cage+dat$error)/8033)

    # check if any 14C dates are (entirely or partly) beyond the calibration curve
    outside <- which(!is.na(dat$cage))
    rangecc <- c(min(calcurve[,2]-calcurve[,3]),max(calcurve[,2]+calcurve[,3]))
    outside <- outside[c(which(dat$cage[outside]-times*dat$error[outside] < rangecc[1]), which(dat$cage[outside]+times*dat$error[outside] > rangecc[2]))]
    if(length(outside) > 0)
    {
        truncate <- 0
        for(i in 1:length(outside)) # check if date lies only partly beyond the curve limits
            if((dat$cage[outside[i]]-times*dat$error[outside[i]] < rangecc[1] &&
                dat$cage[outside[i]]+times*dat$error[outside[i]] > rangecc[1]) ||
               (dat$cage[outside[i]]-times*dat$error[outside[i]] < rangecc[2] &&
                dat$cage[outside[i]]+times*dat$error[outside[i]] > rangecc[2]))
                truncate <- truncate + 1
            if(truncate > 0)
                warning("Dates spanning beyond the calibration curve will be truncated! ")

            # remove dates which lie entirely outside the limits of the calibration curve
            outside <- outside[c(which(dat$cage[outside]+qnorm(1-(1-prob)/2)*dat$error[outside] < rangecc[1]), which(dat$cage[outside]-qnorm(1-(1-prob)/2)*dat$error[outside] > rangecc[2]))]
            if(length(outside) > 0)
            {
                warning("Dates older than the calibration curve will be ignored! ")
                dets <- dets[-outside,]
                dat$cage <- dat$cage[-outside]
                dat$error <- dat$error[-outside]
                dat$f.cage <- dat$f.cage[-outside]
                dat$f.error <- dat$f.error[-outside]
                dat$outside <- dat$ID[outside]
                dat$ID <- dat$ID[-outside]
            }
    }

    # fill the 'dat' list with additional information
    dat$cal <- c(dets[,3], extradates)
    dat$res <- c(dets[,5], extradates)
    dat$depth <- c(dets[,6], extradates)
    dat$thick <- c(dets[,7], rep(thickness, length(extradates)))
    dat$BCAD <- BCAD

    # find distribution (calibrated if 14C) and point estimates for each date
    for(i in 1:length(dat$depth))
    {
        if(length(extradates) > 0 && i > nrow(dets))
        {
            tmp <- read.table(paste(dat$coredir, name, "_", extradates[i-nrow(dets)], ".txt", sep=""))
            calib <- cbind(tmp[,1], tmp[,2]/sum(tmp[,2]))
        } else
            if(is.na(dat$cage[[i]]))
            {
                age <- dat$cal[[i]]
                error <- dat$error[[i]]
                ageseq <- seq(age-(times*error), age+(times*error), by=yrsteps)
                calib <- cbind(ageseq, dnorm(ageseq, age, error))
            } else
                calib <- .caldist(dat$f.cage[[i]], dat$f.error[[i]], theta, f.mu, f.sigma, yrsteps, threshold, calibt, BCAD)
            if(length(youngest) > 0) # truncate ages younger than a limit
            {
                if(BCAD) calib <- calib[which(calib[,1] <= youngest),] else
                    calib <- calib[which(calib[,1] >= youngest),]
                if(length(calib) == 0)
                    if(BCAD)
                        calib <- cbind(seq(youngest-(3*yrsteps), youngest+yrsteps, length=5), c(0:3,0)/3) else
                            calib <- cbind(seq(youngest-yrsteps, youngest+(3*yrsteps), length=5), c(0,3:0)/3)
            }
            dat$calib[[i]] <- calib
            dat$hpd[[i]] <- .hpd(calib, prob=prob, hpdsteps=hpdsteps, yrsteps=yrsteps)
            dat$mid1[[i]] <- (dat$hpd[[i]][1] + dat$hpd[[i]][2*nrow(dat$hpd[[i]])])/2
            yrs <- calib[,1]
            dat$mid2[[i]] <- mean(c(max(yrs), min(yrs)))
            dat$wmn[[i]] <- weighted.mean(calib[,1], 1/calib[,2])
            dat$med[[i]] <- calib[max(which(cumsum(calib[,2]) <= .5)),1]
            dat$mode[[i]] <- calib[which(calib[,2] == max(calib[,2])),1][1]
    }

    if(storedat) dets <<- dets
    dat
}


#' Title #TBW
#'
#' @param theta  #TBW
#' @param f.mu  #TBW
#' @param f.sigma  #TBW
#' @param dat  #TBW
#' @param calrange  #TBW
#' @param outliers  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# calculate goodness-of-fit (small number, so calculate its -log)
.gfit <- function(theta, f.mu, f.sigma, dat, calrange, outliers){
    gfit <- c()
    if(length(outliers) > 0)
    {
        dat$cage <- dat$cage[-outliers]
        dat$error <- dat$error[-outliers]
        dat$cal <- dat$cal[-outliers]
        dat$model <- dat$model[-outliers]
    }
    gfit <- pnorm(dat$cal, dat$model, dat$error^2)
    if(length(c14 <- which(!is.na(dat$cage))) > 0) # if there are radiocarbon dates
    {
        gfit.c <- approx(theta, f.mu, dat$model[c14])$y # C14 age at cc of modelled cal date
        f.cage <- exp(-dat$cage[c14]/8033)
        f.error <- exp(-(dat$cage[c14]-dat$error[c14])/8033) - f.cage
        gfit.var <- f.error^2 + approx(theta, f.sigma, dat$model[c14])$y^2
        gfit[c14] <- pnorm(f.cage, gfit.c, sqrt(gfit.var)) # deviation between measured and cc ages
    }
    dat$gfit <- -sum(log(gfit[!is.na(gfit)]))
}



#' Title #TBW
#'
#' @param dat  #TBW
#' @param runname  #TBW
#' @param calrange  #TBW
#' @param name  #TBW
#' @param prob  #TBW
#' @param type  #TBW
#' @param remove.reverse  #TBW
#' @param smooth  #TBW
#' @param wghts  #TBW
#' @param its  #TBW
#' @param outliers  #TBW
#' @param ignore  #TBW
#' @param est  #TBW
#' @param BCAD  #TBW
#' @param yrsteps  #TBW
#' @param every  #TBW
#' @param decimals  #TBW
#' @param accrate  #TBW
#' @param depth  #TBW
#' @param depthseq  #TBW
#' @param hiatus  #TBW
#' @param gfit  #TBW
#' @param reversal  #TBW
#' @param plotpdf  #TBW
#' @param plotpng  #TBW
#' @param yrmin  #TBW
#' @param yrmax  #TBW
#' @param dmin  #TBW
#' @param dmax  #TBW
#' @param yrlab  #TBW
#' @param dlab  #TBW
#' @param plotrange  #TBW
#' @param greyscale  #TBW
#' @param chron  #TBW
#' @param C14col  #TBW
#' @param outcol  #TBW
#' @param outlsize  #TBW
#' @param bestcol  #TBW
#' @param rangecol  #TBW
#' @param calhght  #TBW
#' @param maxhght  #TBW
#' @param mirror  #TBW
#' @param calcol  #TBW
#' @param slump  #TBW
#' @param slumpcol  #TBW
#' @param revaxes  #TBW
#' @param revyr  #TBW
#' @param revd  #TBW
#' @param calibt  #TBW
#' @param youngest  #TBW
#' @param extradates  #TBW
#' @param plotname  #TBW
#' @param calcurve  #TBW
#' @param ccname  #TBW
#' @param postbomb  #TBW
#' @param pbnames  #TBW
#' @param depths.file  #TBW
#' @param bty  #TBW
#' @param mar  #TBW
#' @param mgp  #TBW
#' @param ash  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# write files of the age-depth model, calibrated ranges, and settings
.write.clam <- function(dat, runname, calrange, name, prob, type, remove.reverse, smooth, wghts, its, outliers, ignore, est, BCAD, yrsteps, every, decimals, accrate, depth, depthseq, hiatus, gfit, reversal, plotpdf, plotpng, yrmin, yrmax, dmin, dmax, yrlab, dlab, plotrange, greyscale, chron, C14col, outcol, outlsize, bestcol, rangecol, calhght, maxhght, mirror, calcol, slump, slumpcol, revaxes, revyr, revd, calibt, youngest, extradates, plotname, calcurve, ccname, postbomb, pbnames, depths.file, bty, mar, mgp, ash){
    # age-depth model; age estimates, accumulation rates and ranges for every analysed depth
    runnames <- c("_interpolated", "_polyn_regr", "_cubic_spline", "_smooth_spline", "_loess")
    calrange <- cbind(calrange, round(c(diff(calrange[,4])/diff(calrange[,1]), NA), decimals+2))
    if(accrate==1) calrange[,5] <- 1/calrange[,5]
    calrange[,2:4] <- round(calrange[,2:4], decimals)
    ifelse(length(runname)==0, runname <- runnames[type], runname)
    if(depths.file && file.exists(dd <- paste("Cores/", name, "/", name, "_depths.txt", sep="")))
    {
        dd <- read.table(dd)[,1]
        this <- c()
        for(i in 1:length(dd))
            this[i] <- which(calrange[,1]==dd[i])[1] # find where the relevant ages are
        write.table(calrange[this,], paste(dat$coredir, name, runname, "_ages.txt", sep=""), row.names=FALSE, col.names=c("depth", paste("min", 100*prob, "%", sep=""), paste("max", 100*prob, "%", sep=""), "best", "acc.rate"), quote=FALSE, sep="\t")
    } else
        write.table(calrange, paste(dat$coredir, name, runname, "_ages.txt", sep=""), row.names=FALSE, col.names=c("depth", paste("min", 100*prob, "%", sep=""), paste("max", 100*prob, "%", sep=""), "best", "accrate"), quote=FALSE, sep="\t")

    # calibrated ranges of all dates
    hpd.file <- file(paste(dat$coredir, name, "_calibrated.txt", sep=""), "w")
    cat(paste("Calibrated age ranges at ", 100*prob, "% confidence intervals\n", sep=""), file=hpd.file)
    for(i in 1:length(dat$depth))
    {
        cat(paste("\n\nDepth: ", dat$depth[[i]], "\nyrmin\tyrmax\tprobability\n"), file=hpd.file)
        hpds <- dat$hpd[[i]]
        for(j in 1:nrow(hpds))
        {
            for(k in 1:3) cat(hpds[j,k], "\t", file=hpd.file)
            cat("\n", file=hpd.file)
        }
    }
    close(hpd.file)

    # relevant settings and results
    set.file <- file(paste(dat$coredir, name, runnames[type], "_settings.txt", sep=""), "w")
    cat(paste("Settings (square brackets give names of the constants)\n\n",
              "Calibration curve: ", ccname,
              if(postbomb!=FALSE) paste(",", pbnames[postbomb], "for postbomb dates"),
              "\nAge-depth model: ",
              if(type==1) "linear interpolation between dated levels [type=1]" else
                  if(type==2) ifelse(length(smooth)==0, "linear regression [type=2, smooth=c()]",
                                     paste("polynomial regression [type=2] of order", smooth, "[smooth]")) else
                                         if(type==3) "cubic spline [type=3]" else
                                             if(type==4) paste("smooth spline [type=4] with spar =", ifelse(length(smooth)<1, 0.3, smooth), "[smooth]") else
                                                 if(type==5) paste("locally weighted spline [type=5] with span =", ifelse(length(smooth)<1, 0.75, smooth), "[smooth]"),
              if(wghts==1) "\nWeighted by the calibrated probabilities [wghts=1]",
              if(wghts==2) "\nWeighted by the errors (1/sdev^2) [wghts=2]",
              "\nCalculations at ", 100*prob, "% confidence ranges [prob=", prob, "]",
              "\nAmount of iterations: ", its, " [its]",
              "\nCalendar age point estimates for depths based on ",
              if(est==1) "weighted average of all age-depth curves [est=1]" else
                  if(est==2) "midpoints of the hpd ranges of the age-depth curves [est=2]" else
                      if(est==3) "midpoints of the hpd ranges of the dated levels [est=3]" else
                          if(est==4) "weighted means of the dated levels [est=4]" else
                              if(est==5) "medians of the dated levels [est=5]" else
                                  if(est==6) "modes/maxima/intercepts of the dated levels [est=6]",
              "\nCalendar scale used: ", if(BCAD) "cal BC/AD" else "cal BP",
              " [BCAD=", BCAD, "] at a resolution of ", yrsteps, " yr [yrsteps]",
              "\nAges were calculated every ", every, " [every] ", depth,
              " [depth], from ", min(depthseq), " [dmin] to ", max(depthseq), " [dmax] ", depth, sep=""), file=set.file)
    if(length(youngest) > 0) cat("\n\nDates with ages younger than", youngest, ifelse(BCAD, "BC/AD", "cal BP"), "were truncated", file=set.file)
    if(length(calibt)> 1) cat("\n\nInstead of assuming the standard Gaussian model, a student t distribution was used with t.a =", calibt[1], "and t.b =", calibt[2], "(see Christen and Perez 2009, Radiocarbon 51:1047-1059)", file=set.file)
    if(length(slump) == 2) cat("\n\nA slump was excised between", max(slump), "and", min(slump), depth, file=set.file)
    if(length(slump) > 2)
    {
        cat("\n\nSlumps were excised from ", file=set.file)
        sl <- array(sort(slump), dim=c(2, length(slump)/2))
        for(i in 1:ncol(sl))
            cat(sl[1,i], "to", sl[2,i], depth, if(i<ncol(sl)) "and ", file=set.file)
    }
    if(length(outliers) > 0)
    {
        cat("\n\nDates assumed outlying [outliers]: ", file=set.file)
        for(i in outliers) cat(i, " (", dat$ID[i], ") ", sep="", file=set.file)
    }
    if(length(ignore) > 0)
    {
        cat("\n\nDates ignored [ignore]: ", file=set.file)
        for(i in 1:length(ignore)) cat(ignore[i], " (", dat$ignore[i], ") ", sep="", file=set.file)
    }
    if(length(dat$outside) > 0)
    {
        cat("\n\nDates outside calibration curve and ignored: ", file=set.file)
        for(i in 1:length(dat$outside)) cat(dat$outside[i], " ", sep="", file=set.file)
    }
    cat(paste(
        if(length(hiatus) > 0)
            paste("\nA hiatus was inferred at", hiatus, depth, "[hiatus]"),
        "\n\nGoodness-of-fit (-log, lower is better): ", gfit,
        if(reversal) "\nSome age-depth reversals occurred"),
        if(remove.reverse) "\nAny models with age-depth reversals were removed",
        "\n\nProduced ", date(), sep="", file=set.file)
    close(set.file)

    if(plotpdf)
    {
        pdf(file=paste(dat$coredir, name, runname, ".pdf", sep=""))
        .ageplot(yrmin, yrmax, dmin, dmax, revaxes, revd, revyr, dlab, yrlab, hiatus, depthseq, outliers, plotrange, BCAD, greyscale, if(length(greyscale)>0) chron else c(), C14col, outcol, outlsize, bestcol, rangecol, dat, calrange, depth, calhght, maxhght, mirror, calcol, slump, slumpcol, plotname, name, bty, mar, mgp, ash)
        dev.off()
    }
    if(plotpng)
    {
        png(file=paste(dat$coredir, name, runname, ".png", sep=""))
        .ageplot(yrmin, yrmax, dmin, dmax, revaxes, revd, revyr, dlab, yrlab, hiatus, depthseq, outliers, plotrange, BCAD, greyscale, if(length(greyscale)>0) chron else c(), C14col, outcol, outlsize, bestcol, rangecol, dat, calrange, depth, calhght, maxhght, mirror, calcol, slump, slumpcol, plotname, name, bty, mar, mgp, ash)
        dev.off()
    }
}



#' Title #TBW
#'
#' @param yrmin  #TBW
#' @param yrmax  #TBW
#' @param dmin  #TBW
#' @param dmax  #TBW
#' @param revaxes  #TBW
#' @param revd  #TBW
#' @param revyr  #TBW
#' @param yrlab  #TBW
#' @param dlab  #TBW
#' @param hiatus  #TBW
#' @param depthseq  #TBW
#' @param outliers  #TBW
#' @param plotrange  #TBW
#' @param BCAD  #TBW
#' @param greyscale  #TBW
#' @param chron  #TBW
#' @param C14col  #TBW
#' @param outcol  #TBW
#' @param outlsize  #TBW
#' @param bestcol  #TBW
#' @param rangecol  #TBW
#' @param dat  #TBW
#' @param calrange  #TBW
#' @param depth  #TBW
#' @param calhght  #TBW
#' @param maxhght  #TBW
#' @param mirror  #TBW
#' @param calcol  #TBW
#' @param slump  #TBW
#' @param slumpcol  #TBW
#' @param plotname  #TBW
#' @param name  #TBW
#' @param bty  #TBW
#' @param mar  #TBW
#' @param mgp  #TBW
#' @param ash  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
.ageplot <- function(yrmin, yrmax, dmin, dmax, revaxes, revd, revyr, yrlab, dlab, hiatus, depthseq, outliers, plotrange, BCAD, greyscale, chron, C14col, outcol, outlsize, bestcol, rangecol, dat, calrange, depth, calhght, maxhght, mirror, calcol, slump, slumpcol, plotname, name, bty="l", mar, mgp, ash=FALSE){
    # set up initial parameters
    if(length(dlab)==0) dlab <- paste("Depth (", depth, ")", sep="")
    ifelse(BCAD || !revyr, yr.lim <- c(yrmin, yrmax), yr.lim <- c(yrmax, yrmin))
    if(revd) d.lim <- c(dmax, dmin) else d.lim <- c(dmin, dmax)

    par(xaxt="s", xaxs="r", yaxt="s", yaxs="r", bty=bty, mar=mar, mgp=mgp, font=2)
    if(revaxes) plot(0, type="n", ylim=yr.lim, xlim=d.lim, xlab=dlab, ylab=yrlab) else
        plot(0, type="n", xlim=yr.lim, ylim=d.lim, xlab=yrlab, ylab=dlab)
    if(plotname) legend("topleft", name, bty="n")

    # draw histograms of all age-depth models. Off by default, time-consuming!
    if(length(greyscale)==1)
    {
        plotrange <- FALSE
        depgr=seq(dmin, dmax, length=greyscale)
        for(i in 2:greyscale)
        {
            temp <- density(chron[max(which(calrange[,1]<=depgr[i])),2:ncol(chron)], n=greyscale)
            if(revaxes)
                image(c(depgr[i-1], depgr[i]), temp$x, matrix(temp$y), col=grey(1-(0:100)/100), add=TRUE) else
                    image(temp$x, c(depgr[i-1], depgr[i]), matrix(temp$y), col=grey(1-(0:100)/100), add=TRUE)
        }
    }

    # draw the age-depth models, per section if hiatuses were inferred
    if(length(hiatus) > 0)
    {
        if(length(slump) == 0)
            hiatusseq <- sort(c(range(depthseq), hiatus)) else
                hiatusseq <- sort(c(range(depthseq, depthseq+sum(slump[,2]-slump[,1])), hiatus))
            for(i in 2:length(hiatusseq))
            {
                sec <- calrange[min(which(calrange[,1] > hiatusseq[i-1])):max(which(calrange[,1] < hiatusseq[i])),]
                pol <- cbind(c(sec[,2], rev(sec[,3])), c(sec[,1], rev(sec[,1])))
                if(plotrange)
                    if(revaxes)
                        polygon(pol[,2], pol[,1], col=rangecol, border=rangecol) else
                            polygon(pol, col=rangecol, border=rangecol)
                if(revaxes)
                    lines(sec[,1], sec[,4], lwd=2, col=bestcol) else
                        lines(sec[,4], sec[,1], lwd=2, col=bestcol)
                if(revaxes)
                    abline(v=hiatus, col="grey", lty="dashed") else
                        abline(h=hiatus, col="grey", lty="dashed")
            }
    } else
    {
        pol <- cbind(c(calrange[,2], rev(calrange[,3])), c(calrange[,1], rev(calrange[,1])))
        if(plotrange)
            if(revaxes)
                polygon(pol[,2], pol[,1], col=rangecol, border=rangecol) else
                    polygon(pol, col=rangecol, border=rangecol)
        if(revaxes)
            lines(calrange[,1], calrange[,4], lwd=2, col=bestcol) else
                lines(calrange[,4], calrange[,1], lwd=2, col=bestcol)
    }

    # draw slumps if these were given
    if(length(slump) > 0)
        for(i in 1:nrow(slump))
            if(revaxes)
                rect(min(slump[i,]), min(yr.lim)-1e4, max(slump[i,]), max(yr.lim)+1e4, col=slumpcol, border=slumpcol) else
                    rect(min(yr.lim)-1e4, min(slump[i,]), max(yr.lim)+1e4, max(slump[i,]), col=slumpcol, border=slumpcol)

    # draw the calibrated distributions of the dates
    top <- 1
    for(i in 1:length(dat$depth))
        top <- min(top, max(dat$calib[[i]][,2])) # find the lowest peak

    if(calhght > 0)
        for(i in 1:length(dat$depth))
        {
            if(is.na(dat$cal[[i]])) col <- C14col else col <- calcol
            pol <- dat$calib[[i]] # already normalised to 1
            if(ash) pol[,2] <- pol[,2]/max(pol[,2])/1e3 # draw all same height
            pol[pol[,2] > maxhght,2] <- maxhght
            pol[,2] <- calhght*(dmax-dmin)*pol[,2]/(top*100)
            pol <- cbind(c(pol[,1], rev(pol[,1])),
                         c(dat$depth[[i]]-pol[,2], dat$depth[[i]]+mirror*rev(pol[,2])))
            if(revaxes) polygon(pol[,2], pol[,1], col=col, border=col) else
                polygon(pol, col=col, border=col)
        }

    # draw the calibrated ranges of the dates
    for(i in 1:length(dat$depth))
    {
        if(is.na(dat$cal[[i]])) col <- C14col else col <- calcol
        for(j in 1:nrow(dat$hpd[[i]]))
            if(revaxes)
                rect(dat$depth[i]-dat$thick[i]/2, dat$hpd[[i]][j,1], dat$depth[i]+dat$thick[i]/2, dat$hpd[[i]][j,2], lwd=1, lend=2, col=col, border=NA) else
                    rect(dat$hpd[[i]][j,1], dat$depth[i]-dat$thick[i]/2, dat$hpd[[i]][j,2], dat$depth[i]+dat$thick[i]/2, lwd=1, lend=2, col=col, border=NA)
    }
    if(length(outliers)>0) # any outliers?
    {
        for(i in outliers)
            for(j in 1:nrow(dat$hpd[[i]]))
                if(revaxes)
                    rect(dat$depth[i]-dat$thick[i]/2, dat$hpd[[i]][j,1], dat$depth[i]+dat$thick[i]/2, dat$hpd[[i]][j,2], col=outcol, border=outcol, lwd=1, lend=2) else
                        rect(dat$hpd[[i]][j,1], dat$depth[i]-dat$thick[i]/2, dat$hpd[[i]][j,2], dat$depth[i]+dat$thick[i]/2, col=outcol, border=outcol, lwd=1, lend=2)
        if(revaxes)
            points(dat$depth[outliers], dat$mid1[outliers], cex=outlsize, pch=4, col=outcol) else
                points(dat$mid1[outliers], dat$depth[outliers], cex=outlsize, pch=4, col=outcol)
    }
}


#' Title #TBW
#'
#' @param depth  #TBW
#' @param yrcm  #TBW
#' @param prob  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
## calculates *for each iteration* the slope of a straight curve between depths above and below the desired point. Requires sufficiently dense density of depths, e.g. yrsteps=1
# to calculate accumulation rates at a depth. Before running this, run your core in clam and store the data, so, provide the option storedat=TRUE
accrate.depth <- function(depth, yrcm=TRUE, prob=.95){
    if(depth <= min(calrange[,1]) || depth >= max(calrange))
        stop("Accumulation rates cannot be calculated for the top or bottom of the core. Please check the manual", call.=FALSE)
    d <- max(which(calrange[,1] <= depth))
    if(yrcm)
        accrate <- (chron[d+1,]-chron[d-1,]) / (calrange[d+1,1]-calrange[d-1,1]) else
            accrate <- (calrange[d+1,1]-calrange[d-1,1]) / (chron[d+1,]-chron[d-1,])
    acc <- density(accrate)
    plot(acc, main="", xlab=if(yrcm) "yr/cm" else "cm/yr")
    abline(h=0)
    o <- order(acc$y, decreasing=TRUE)
    acc <- cbind(acc$x[o], cumsum(acc$y[o])/sum(acc$y))
    acc <- range(acc[acc[,2] <= prob,1])
    rect(acc[1], 0, acc[2], -999, col=grey(.5), border=grey(.5))
    cat(100*prob, "% ranges: ", acc[1], " to ", acc[2], if(yrcm) " yr/cm\n" else " cm/yr\n", sep="")
}


#' Title #TBW
#'
#' @param age  #TBW
#' @param yrcm  #TBW
#' @param prob  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
## calculates *for each iteration* the slope of a straight curve between depths above and below the desired point. Requires sufficiently dense density of depths, e.g. steps=1
# to calculate accumulation rate at an age. Before doing this, run your core in clam and store the data, so, provide the option storedat=TRUE
accrate.age <- function(age, yrcm=TRUE, prob=.95){
    accrate <- c()
    for(i in 1:ncol(chron))
    {
        a <- max(which(chron[,i] <= age))
        if(yrcm)
            accrate <- c(accrate, (chron[a+1,i]-chron[a-1,i]) / (calrange[a+1,1]-calrange[a-1,1])) else
                accrate <- c( accrate, (calrange[a+1,1]-calrange[a-1,1]) / (chron[a+1,i]-chron[a-1,i]))
    }
    acc <- density(accrate)
    plot(acc, main="", xlab=if(yrcm) "yr/cm" else "cm/yr")
    abline(h=0)
    o <- order(acc$y, decreasing=TRUE)
    acc <- cbind(acc$x[o], cumsum(acc$y[o])/sum(acc$y))
    acc <- range(acc[acc[,2] <= prob,1])
    rect(acc[1], 0, acc[2], -999, col=grey(.5), border=grey(.5))
    cat(100*prob, "% ranges: ", acc[1], " to ", acc[2], if(yrcm) " yr/cm\n" else " cm/yr\n", sep="")
}



#' Title #TBW
#'
#' @param prox  #TBW
#' @param errors  #TBW
#' @param proxcol  #TBW
#' @param revyr  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# only works after doing a clam run with proxies=TRUE
plot.proxies <- function(prox, errors=TRUE, proxcol=grey(0.5), revyr=TRUE){
    prx <- dat$proxies
    if(length(prox)>1) layout(matrix(1:length(prox), ncol=1))
    for(j in 1:length(prox))
    {
        pr <- prx[which(!is.na(prx[,prox+1])),]
        ages <- array(0, dim=c(nrow(pr),3))
        for(i in 1:nrow(pr))
            ages[i,] <- calrange[which(calrange[,1]==pr[i,1]),c(2,3,4)]
        xlim <- range(ages)
        if(!dat$BCAD) xlim <- rev(xlim)
        if(revyr) xlim <- rev(xlim)
        plot(ages[,3], pr[,prox+1], type="n", xlim=xlim, xlab=ifelse(dat$BCAD, "cal BC/AD", "cal BP"), ylab=names(pr)[prox+1])
        if(errors)
            for(i in 2:nrow(pr))
                polygon(c(ages[(i-1):i,1], ages[i:(i-1),2]), c(pr[c((i-1):i, i:(i-1)),prox+1]), col=proxcol, border=proxcol)
        lines(ages[,3], pr[,prox+1])
    }
    layout(1)
}




#' Title #TBW
#'
#' @param cage  #TBW
#' @param error  #TBW
#' @param reservoir  #TBW
#' @param prob  #TBW
#' @param cc  #TBW
#' @param cc1  #TBW
#' @param cc2  #TBW
#' @param cc3  #TBW
#' @param cc4  #TBW
#' @param cc5  #TBW
#' @param postbomb  #TBW
#' @param pb1  #TBW
#' @param pb2  #TBW
#' @param pb3  #TBW
#' @param pb4  #TBW
#' @param pb5  #TBW
#' @param yrsteps  #TBW
#' @param pbsteps  #TBW
#' @param hpdsteps  #TBW
#' @param calibt  #TBW
#' @param yrmin  #TBW
#' @param yrmax  #TBW
#' @param minC14  #TBW
#' @param maxC14  #TBW
#' @param times  #TBW
#' @param calheight  #TBW
#' @param expand  #TBW
#' @param threshold  #TBW
#' @param storedat  #TBW
#' @param graph  #TBW
#' @param xlab  #TBW
#' @param ylab  #TBW
#' @param BCAD  #TBW
#' @param mar  #TBW
#' @param mgp  #TBW
#' @param bty  #TBW
#' @param title  #TBW
#' @param date.col  #TBW
#' @param cc.col  #TBW
#' @param dist.col  #TBW
#' @param sd.col  #TBW
#'
#' @return #TBW
#' 
#' @export
#'
#' @examples
#'  #TBW
# to calibrate individual 14C dates
calibrate <- function(cage=2450, error=50, reservoir=0, prob=0.95, cc=1, cc1="IntCal13.14C", cc2="Marine13.14C", cc3="SHCal13.14C", cc4="mixed.14C", cc5="gluedHemispheres.14C", postbomb=FALSE, pb1="postbomb_NH1.14C", pb2="postbomb_NH2.14C", pb3="postbomb_NH3.14C", pb4="postbomb_SH1_2.14C", pb5="postbomb_SH3.14C", yrsteps=1, pbsteps=0.01, hpdsteps=1, calibt=FALSE, yrmin=c(), yrmax=c(), minC14=c(), maxC14=c(), times=5, calheight=0.3, expand=0.1, threshold=1e-6, storedat=FALSE, graph=TRUE, xlab=c(), ylab=c(), BCAD=FALSE, mar=c(3.5,3,2,1), mgp=c(2,1,0), bty="l", title=c(), date.col="red", cc.col=rgb(0,.5,0,0.7), dist.col=rgb(0,0,0,0.3), sd.col=rgb(0,0,0,0.5)){
# start modification by DNL
#calibrate <- function(cage=2450, error=50, reservoir=0, prob=0.95, cc=1, cc1="IntCal13.14C", cc2="Marine13.14C", cc3="SHCal13.14C", cc4="mixed.14C", cc5="gluedHemispheres.14C", postbomb=FALSE, pb1="postbomb_NH1.14C", pb2="postbomb_NH2.14C", pb3="postbomb_NH3.14C", pb4="postbomb_SH1-2.14C", pb5="postbomb_SH3.14C", yrsteps=1, pbsteps=0.01, hpdsteps=1, calibt=FALSE, yrmin=c(), yrmax=c(), minC14=c(), maxC14=c(), times=5, calheight=0.3, expand=0.1, threshold=1e-6, storedat=FALSE, graph=TRUE, xlab=c(), ylab=c(), BCAD=FALSE, mar=c(3.5,3,2,1), mgp=c(2,1,0), bty="l", title=c(), date.col="red", cc.col=rgb(0,.5,0,0.7), dist.col=rgb(0,0,0,0.3), sd.col=rgb(0,0,0,0.5))
#     .calibrate(cage, error, reservoir, prob, cc, cc1, cc2, cc3, cc4, cc5, postbomb, pb1, pb2, pb3, pb4, pb5, yrsteps, pbsteps, hpdsteps, calibt, yrmin, yrmax, minC14, maxC14, times, calheight, expand, threshold, storedat, graph, xlab, ylab, BCAD, mar, mgp, bty, title, date.col, cc.col, dist.col, sd.col)
#     }
# 
# # end modification by DNL
# 
# .calibrate <- function(cage, error, reservoir, prob, cc, cc1, cc2, cc3, cc4, cc5, postbomb, pb1, pb2, pb3, pb4, pb5, yrsteps, pbsteps, hpdsteps, calibt, yrmin, yrmax, minC14, maxC14, times, calheight, expand, threshold, storedat, graph, xlab, ylab, BCAD, mar, mgp, bty, title, date.col, cc.col, dist.col, sd.col)
# {
    # set calibration curve

    # start modification by DNL
    # if(cc==1) calcurve <- read.table(cc1) else
    #     if(cc==2) calcurve <- read.table(cc2) else
    #       if(cc==3) calcurve <- read.table(cc3) else
    #           if(cc==4) calcurve <- read.table(cc4) else
    #               if(cc==5) calcurve <- read.table(cc5) else
    if(cc==1) calcurve <- get(cc1) else
        if(cc==2) calcurve <- get(cc2) else
            if(cc==3) calcurve <- get(cc3) else
                if(cc==4) calcurve <- get(cc4) else
                    if(cc==5) calcurve <- get(cc5) else
    # end modification by DNL

                        stop("I do not understand which calibration curve you mean, check the manual", call.=FALSE)

                    # include postbomb curve if required
                    if(cage < 0)
                    {
                        pb <- 0
                        if(postbomb==FALSE)
                            stop("\n  Negative 14C age, should I use a postbomb curve?\n", call.=FALSE)
                        if(postbomb==1) pb <- pb1 else
                            if(postbomb==2) pb <- pb2 else
                                if(postbomb==3) pb <- pb3 else
                                    if(postbomb==4) pb <- pb4 else
                                        if(postbomb==5) pb <- pb5 else
                                            stop("I do not understand which postbomb curve you mean, check the manual", call.=FALSE)
                                        yrsteps <- min(pbsteps, yrsteps)
                                        if(length(pb) > 0)
                                        {
                                            # start modification by DNL
                                            # pb <- read.table(pb)
                                            pb <- get(pb)
                                            # end modification by DNL

                                            pb.x <- seq(min(pb[,1]), max(pb[,1]), by=yrsteps)
                                            pb.y <- approx(pb[,1], pb[,2], pb.x)$y
                                            pb.sd <- approx(pb[,1], pb[,3], pb.x)$y
                                            calcurve <- cbind(c(pb.x, calcurve[,1]), c(pb.y, calcurve[,2]), c(pb.sd, calcurve[,3]))
                                        }
                                        cat("  postbomb date, interpolating to every", pbsteps, "yr.")
                    }

                    # check whether date lies partly or entirely beyond the calibration curve
                    if(length(reservoir) == 2) # assuming that first value is mean offset, second is error
                    {
                        error <- sqrt(error^2 + reservoir[2]^2)
                        reservoir <- reservoir[1]
                    }
                    border <- 0
                    if(cage-reservoir-error < min(calcurve[,2]+calcurve[,3]))
                        if(cage-reservoir+error > min(calcurve[,2]-calcurve[,3]))
                            border <- 1 else border <- 2
                    if(cage-reservoir+error > max(calcurve[,2]-calcurve[,3]))
                        if(cage-reservoir-error < max(calcurve[,2]+calcurve[,3]))
                            border <- 1 else border <- 2
                    if(border==1)
                        warning("\nDate falls partly beyond calibration curve and will be truncated!")
                    if(border==2)
                        stop("\nCannot calibrate dates beyond calibration curve!\n\n")

                    # work in BC/AD if needed, and prepare for calculations in f14C
                    if(BCAD)
                    {
                        theta <- 1950-calcurve[,1]
                        ad <- max(which(theta > 0)) # one side of the border between AD and BC
                        theta <- c(theta[1:(ad-1)], theta[ad]:theta[ad+2], theta[(ad+3):length(theta)])
                        mu <- approx(1950-calcurve[,1], calcurve[,2], theta)$y
                        sigma <- approx(1950-calcurve[,1], calcurve[,3], theta)$y
                        theta[theta <=0] <- theta[theta <=0]-1
                        calcurve <- cbind(theta, mu, sigma)
                    } else theta <- calcurve[,1]
                    f.mu <- exp(-calcurve[,2]/8033)
                    f.sigma <- exp(-(calcurve[,2]-calcurve[,3])/8033) - f.mu
                    f.cage <- exp(-(cage-reservoir)/8033)
                    f.error <- f.cage - exp(-(cage-reservoir+error)/8033)

                    # calibrate the date and report its highest posterior density (hpd) range
                    if(length(xlab)==0) xlab <- ifelse(BCAD, "cal BC/AD", "cal BP")
                    calib <- .caldist(f.cage, f.error, theta, f.mu, f.sigma, yrsteps, threshold, calibt, BCAD)
                    hpd <- .hpd(calib, prob, hpdsteps, yrsteps)
                    colnames(hpd) <- c("yrmin", "yrmax", "prob")
                    dat <- list(calib=calib, hpd=hpd)
                    if(storedat) dat <<- dat
                    cat("\nmin\tmax\tprob\n")
                    for(i in 1:nrow(hpd))
                    {
                        for(j in 1:3) cat(hpd[i,j], "\t")
                        cat("\n")
                    }
                    cat("\n")

                    # produce a graph of the calibrated distribution (default)
                    if(graph)
                    {
                        ifelse(BCAD,
                               xrange <- 1950+c((1+expand)*(min(calib[,1])-1950), (1-expand)*(max(calib[,1])-1950)),
                               xrange <- c((1+expand)*max(calib[,1]), (1-expand)*min(calib[,1])))
                        if(length(yrmin) > 0) xrange[2] <- yrmin
                        if(length(yrmax) > 0) xrange[1] <- yrmax
                        ifelse(BCAD,
                               cc <- calcurve[max(which(theta >= min(xrange))):min(which(theta <= max(xrange))),],
                               cc <- calcurve[min(which(theta >= min(xrange))):max(which(theta <= max(xrange))),])

                        # first plot the calibrated distribution, and its hpd ranges
                        par(mar=mar, mgp=mgp, bty=bty, xaxs="i", xaxt="s", yaxt="n", yaxs="i", new=FALSE)
                        pol <- cbind(c(calib[,1], rev(calib[,1])), c(calib[,2]/max(calib[,2]), rep(0, length=nrow(calib))))
                        plot(0, type="n", xlim=xrange, ylim=c(0,1/calheight), xlab="", ylab="")
                        polygon(pol, col=dist.col, border=NA)
                        for(i in 1:nrow(hpd))
                        {
                            if(hpd[i,1]==hpd[i,2])
                            {
                                probs <- calib[which(calib[,1]==hpd[i,1]),]
                                lines(rep(probs[1], 2), c(0, probs[2]/max(calib[,2])), col=grey(.5))
                            } else
                            {
                                probs <- calib[max(which(calib[,1]<=hpd[i,1])):max(which(calib[,1]<=hpd[i,2])),]
                                pol <- cbind(c(probs[,1], rev(probs[,1])), c(probs[,2]/max(calib[,2]), rep(0, length=nrow(probs))))
                                polygon(pol, col=sd.col, border=NA)
                            }
                        }
                        lines(calib[,1], calib[,2]/max(calib[,2]))
                        abline(h=0)

                        # now draw the 14C distribution (normal distribution, on vertical axis)
                        par(new=TRUE, yaxt="s", yaxs="r", xaxt="n")
                        if(length(cc)==3) cc <- cbind(cc[1], cc[2], cc[3])
                        if(reservoir!=0)
                            main <- substitute(cage-res %+-% er, list(cage=cage, er=error, res=reservoir)) else
                                main <- substitute(cage %+-% er, list(cage=cage, er=error))
                        if(length(title)>0) main <- title
                        if(length(minC14)==0)
                            minC14 <- min(cc[,2]-qnorm(1-(1-prob)/2)*cc[,3], cage-reservoir-qnorm(1-(1-prob)/2)*error)
                        if(length(maxC14)==0)
                            maxC14 <- max(cc[,2]+qnorm(1-(1-prob)/2)*cc[,3], cage-reservoir+qnorm(1-(1-prob)/2)*error)
                        if(length(ylab)==0)
                            ylab <- expression(paste(""^14, "C BP"))
                        plot(0, type="n", xlim=xrange, ylim=c(minC14, maxC14), xlab=xlab, ylab=ylab, main=main)
                        if(length(calibt) > 0)
                            times <- 5*times
                        yage <- (cage-reservoir-times*error):(cage-reservoir+times*error) # must not be on F14C for plot
                        if(length(calibt) < 2)
                            xage <- dnorm(exp(-yage/8033), f.cage, f.error) else
                                xage <- (calibt[2] + ((f.cage-exp(-yage/8033))^2) / (2*(f.error^2))) ^ -(calibt[1]+0.5)
                        xage.plot <- xrange[1]-((xrange[1]-xrange[2])*calheight)*xage/max(xage)
                        pol <- cbind(c(xage.plot, rep(xrange[1], length(xage))), c(yage, rev(yage)))
                        polygon(pol, col=dist.col, border="black")

                        # draw the highest posterior density (hpd) range of the 14C date
                        xage[which(cumsum(xage)/sum(xage) > 1 - (1-prob)/2)] <- 0
                        xage[which(cumsum(xage)/sum(xage) < (1-prob)/2)] <- 0
                        xage <- xrange[1]-((xrange[1]-xrange[2])*calheight)*(xage/max(xage))
                        pol <- cbind(c(xage, rep(xrange[1], length=length(xage))), c(yage, rev(yage)))
                        polygon(pol, col=sd.col, border=FALSE)

                        # plot the mid and error of the 14C date
                        points(xrange[1]-.01*(xrange[1]-xrange[2]), cage, pch=19, col=date.col)
                        lines(rep(xrange[1]-.01*(xrange[1]-xrange[2]), 2), c(cage-error, cage+error), lwd=2, col=date.col)

                        # now draw the calibration curve
                        pol <- cbind(c(theta, rev(theta)),
                                     c(calcurve[,2]-qnorm(1-(1-prob)/2)*calcurve[,3], rev(calcurve[,2]+qnorm(1-(1-prob)/2)*calcurve[,3])))
                        polygon(pol, border=cc.col, col=cc.col)
                    }
}

