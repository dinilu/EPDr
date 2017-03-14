#' Reshape C14 data to CLAM or BACON format
#' 
#' This function takes C14 data, as those extracted from \code{\link[EPDr:getC14]{getC14}}, to fit into a new table that comply with CLAM or BACON format.
#'
#' @param C14 Data frame with C14 data as those extracted from \code{\link[EPDr:getC14]{getC14}}. 
#' @param format Character string indicating whether to export to "clam" or to "bacon" format.
#'
#' @return Data frame with C14 data in CLAM or BACON format.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # c14 <- getC14(400, epd.connection)
#' # c14.clam <- exportC14(c14, "clam")
#' # c14.bacon <- exportC14(c14, "bacon")
#' 
exportC14 <- function(C14, format=c("clam","bacon")) {
    if(nrow(C14) == 0){
        stop("Table without c14 data. Not performing conversion, useless for CLAM.")
    }
    if(!format %in% c("clam", "bacon")){
        stop("Incorrect output format. format has to be 'clam' or 'bacon'.")
    }
    output <- data.frame(lab_ID=C14$labnumber, C14_age=C14$agebp, error=C14$agesdup, depth=C14$depthcm, thickness=C14$thickness)
    output$cal_age <- NA        
    output$reservoir <- NA
    if(format == "clam"){
        output <- output[,c("lab_ID", "C14_age", "cal_age", "error", "reservoir", "depth", "thickness")]
    }
    if(format == "bacon"){
        output <- output[, c("lab_ID", "C14_age", "error", "depth")]
        colnames(output) <- c("labID", "age", "error", "depth")
    }
    return(output)
}



#' Reshape no-C14 data to CLAM or BACON format
#' 
#' This function takes no-C14 data, that can be extracted from a chronology object, to fit into a new table that
#' comply with CLAM or BACON format.
#'
#' @param agebasis Data frame with no-C14 data as those in a chronology list returned by
#' \code{\link[EPDr:getChronology]{getChronology}}.
#' @param format Character string indicating whether to export to "clam" or to "bacon" format.
#'
#' @return Data frame with no-C14 data in CLAM or BACON format. This data frame can be easily combined with C14
#' data from \code{\link[EPDr:exportC14]{exportC14}} using \code{rbind}.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @export
#'
#' @examples
#' #library(EPDr)
#' #epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' #c14 <- getC14(400, epd.connection)
#' #c14.clam <- exportC14(c14, "clam")
#' #c14.bacon <- exportC14(c14, "bacon")
#' #chron <- getChronology(400, epd.connection)
#' #noc14.clam <- exportAgebasis(chron@agebasis, "clam")
#' #noc14.bacon <- exportAgebasis(chron@agebasis, "bacon")
#' #all.clam <- rbind(c14.clam, noc14.clam)
#' #all.bacon <- rbind(c14.bacon, noc14.bacon)
#' 
exportAgebasis <- function(agebasis, format=c("clam", "bacon")){
    if(nrow(agebasis) == 0){
        stop("Table without data. Not performing conversion, useless for CLAM.")
    }
    if(!format %in% c("clam", "bacon")){
        stop("Incorrect output format. format has to be 'clam' or 'bacon'.")
    }
    output <- data.frame(lab_ID=paste("EPDr_", agebasis$e_, "_CH", agebasis$sample_, sep=""), C14_age=agebasis$age, error=agebasis$ageup - agebasis$age, depth=agebasis$depthcm, thickness=agebasis$thickness)
    output$cal_age <- NA
    output$error[which(is.na(output$error) | output$error == 0)] <- 1
    output$reservoir <- NA
    if(format == "clam"){
        output <- output[,c("lab_ID", "C14_age", "cal_age", "error", "reservoir", "depth", "thickness")]
    }
    if(format == "bacon"){
        output <- output[,c("lab_ID", "C14_age", "error", "depth")]
        colnames(output) <- c("labID", "age", "error", "depth")
    }
    return(output)
}


#' Reshape events data to CLAM or BACON format
#' 
#' This function takes event data to fit into a new table that comply with CLAM or BACON format.
#'
#' @param event Data frame with event data for a particular core (entity) in the EPD DDBB.
#' @param format Character string indicating whether to export to "clam" or to "bacon" format.
#'
#' @return Data frame with event data in CLAM or BACON format.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @export
#'
#' @examples
#' #epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' #event <- getEvents(51, epd.connection)
#' #event.clam <- exportEvents(event, format="clam")
#' #event.bacon <- exportEvents(event, format="bacon")
#' 
exportEvents <- function(event, format=c("clam", "bacon")){
    if(nrow(event) == 0){
        stop("Table without dated events. Not performing conversion, useless for CLAM.")
    }
    if(!format %in% c("clam", "bacon")){
        stop("Incorrect output format. format has to be 'clam' or 'bacon'.")
    }
    output <- data.frame(lab_ID=paste("EPDr_", event$e_, "_EV", event$event_, sep=""), C14_age=event$agebp, error=event$ageuncertup, depth=event$depthcm, thickness=event$thickness)
    output$error[which(is.na(output$error) | output$error == 0)] <- 1
    output$cal_age <- NA
    output$reservoir <- NA
    if(format == "clam"){
        output <- output[,c("lab_ID", "C14_age", "cal_age", "error", "reservoir", "depth", "thickness")]
    }
    if(format == "bacon"){
        output <- output[, c("lab_ID", "C14_age", "error", "depth")]
        colnames(output) <- c("labID", "age", "error", "depth")
    }
        return(output)
}


#' Reshape depths data to CLAM or BACON format
#'
#' This function takes depths data, as returned by \code{\link[EPDr:getPSamples]{getPSamples}}, to comply with CLAM or BACON format.
#'
#' @param depths Data frame with at least a column called depthcm.
#'
#' @return Vector with depths in ascending order.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @export
#'
#' @examples
#' #epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' #depths.1 <- getPSamples(1, epd.connection)
#' #exportDepths(depths.1)
#' #disconnectFromEPD(connection=epd.connection)
exportDepths <- function(depths){
    output <- depths[order(depths$depthcm),]
    output <- output$depthcm
    return(output)
}


#' Reshape datation data to CLAM or BACON format
#' 
#' This function takes datation data, as returned by \code{\link[EPDr:getDatation]{getDatation}} and reshape it to comply with CLAM or BACON format.
#' 
#' The function has an interactive implementation. If some of the parameters \code{include_chron_not_in_c14}, \code{include_C14_not_in_chron}, \code{use_c14_conf_age}, and/or \code{use_c14_conf_depth} are not specified, the function will check the data and ask the user how it should proceed regarding some conflicts in the data between C14 data and chronologies.
#'
#' @param datation \code{\link[EPDr:datation]{datation}} object from which all information will be extracted to compose the clam or bacon file.
#' @param format Character string indicating whether to export to "clam" or to "bacon" format.
#' @param chronology_number Number indicating the chronology from which to extract the agebasis for the "clam" or "bacon" file. If unspecified, the function use the default chronology according to the EPD for that particular entity.
#' @param include_chron_not_in_c14 Logical value indicating whether the function should include in the result agebasis present in the specified (or the default) chronology (TRUE) but not in the C14 table, or should not include them (FALSE).
#' @param include_c14_not_in_chron Logical value indicating whether the function should include in the result agebasis present in the C14 data but not present in the specified (or default) chronology (TRUE), or should not include them (FALSE).
#' @param use_c14_conf_age Logical value indicating whether the function should use data from C14 table when there are conflicting ages between the C14 table or the chronology (TRUE), or it should take data from the chronology instead (FALSE).
#' @param use_c14_conf_depth Logical value indicating whether the function should use data from C14 table when there are conflicting depths between the C14 table or the chronology (TRUE), or it should take data from the chronology instead (FALSE).
#' @param include_depths Logical value indicating whether depths of pollen samples should be exported too. The default value is TRUE. This is helpful because running CLAM or BACON with depths calculate the calibrated ages for those sample in the same step.
#' @param include_events Logical value indicating whether events information should be included (TRUE), or not (FALSE).
#'
#' @return Data frame with specific format for "CLAM" or "BACON" age-depth modelling softwares.
#' CLAM format has 7 columns: lab_ID, C14_age, cal_age, error, reservoir, depth, and thickness. lab_ID is the code of the radiocarbon (C14) samples. C14_age is the radiocarbon (C14) dates. error is the error estimated in the radiocarbon (C14) datation of the samples. reservoir is to specify if the samples have marine reservoir effects. depth is the depth in cm of the radiocarbon samples. thickness is the tickness of the radiocarbon samples.
#' BACON format has 4 columns: labID, age, error, and depth. labID is the code of the radiocarbon (C14) samples. age is the radiocarbon (C14) date. error is the error estimated in the radiocarbon (C14) datation for the samples. depth is the depth in cm of the radiocarbon samples.
#' 
#' The function also produce the files required by CLAM or BACON in the working directory according to this folder structure: ~/{format}/Cores/{entity_number}/
#' The function creates a file .csv ({entity_number}.csv) with the radiocarbon data and two .txt ({entity_number}_depths.txt and {entity_number}_depthsID.txt). The last one is not used by CLAM or BACON but it might be useful to follow track of samples depths.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @export
#'
#' @examples
#' # TBW
exportDatation <- function(datation, format="clam", chronology_number=NA, include_chron_not_in_c14=NA,
                              include_c14_not_in_chron=NA, use_c14_conf_age=NA, use_c14_conf_depth=NA, include_depths=T,
                              include_events=NULL){
  # Define internal functions
    .printData <- function(data, format){
        if(format == "clam"){
            cat(c("lab_ID", "C14_age", "cal_age", "error", "reserv.", "depth", "thickn.\n"), sep="\t")
        }
        if(format == "bacon"){
            cat(c("labID", "age", "error", "depth\n"))
        }
        apply(data, "\n", MARGIN=1, FUN=cat, sep="\t")
    }
    .printEvents <- function(event){
        cat(c("event_", "e_", "depthcm", "thickn.", "event", "name", "agebp", "ageup", "agelo", "publ"), sep="\t", fill=T)
        apply(event, "\n",  MARGIN=1, FUN=cat, sep="\t")
    }
    
    # Check for right class objects
    if(class(datation)  != "datation"){
        stop("Invalid datation object. datation has to be a datation object. See ?getDatation")
    }
    if(!format %in% c("clam", "bacon")){
        stop("Incorrect output format. format has to be 'clam' or 'bacon'.")
    }
    
    # Check if datation object has C14 and chronology information
    if(nrow(datation@c14) == 0){
        stop("Datation object for a core without c14 data. Not performing conversion, useless for CLAM or BACON.")
    }
    
    # Get sub-objects from datation object
    e_ <- datation@e_
    chronology <- datation@chronology
    c14 <- datation@c14
    events <- datation@events
    depths <- datation@depths
    
    # Default chronology if no defined
    if(is.na(chronology_number)){
        chronology_number <- chronology@default_chronology
    }else{
        if(!chronology_number %in% 1:chronology@number_of_chronologies){stop("The chronology does not exist.")}
    }
    
    # Get agebasis for the chronology and convert for clam or bacon
    agebasis <- chronology@agebasis
    agebasis <- agebasis[agebasis$chron_ == chronology_number, ]
    
    chron <- exportAgebasis(agebasis, format)
    c14 <- exportC14(c14, format)
    
    # Check for data in c14 and the chronology and for conflicting data
    if(format == "clam"){
        c14_in_chron <- which(c14$C14_age %in% chron$C14_age & c14$depth %in% chron$depth)
        c14_not_in_chron <- which(!c14$C14_age %in% chron$C14_age & !c14$depth %in% chron$depth) 
        c14_conf_age <- which(!c14$C14_age %in% chron$C14_age & c14$depth %in% chron$depth)
        c14_conf_depth <- which(c14$C14_age %in% chron$C14_age & !c14$depth %in% chron$depth)
        
        chron_in_c14 <- which(chron$C14_age %in% c14$C14_age & chron$depth %in% c14$depth)
        chron_not_in_c14 <- which(!chron$C14_age %in% c14$C14_age & !chron$depth %in% c14$depth)
        chron_conf_age <- which(!chron$C14_age %in% c14$C14_age & chron$depth %in% c14$depth)
        chron_conf_depth <- which(chron$C14_age %in% c14$C14_age & !chron$depth %in% c14$depth)
    }
    if(format == "bacon"){
        c14_in_chron <- which(c14$age %in% chron$age & c14$depth %in% chron$depth)
        c14_not_in_chron <- which(!c14$age %in% chron$age & !c14$depth %in% chron$depth) 
        c14_conf_age <- which(!c14$age %in% chron$age & c14$depth %in% chron$depth)
        c14_conf_depth <- which(c14$age %in% chron$age & !c14$depth %in% chron$depth)
        
        chron_in_c14 <- which(chron$age %in% c14$age & chron$depth %in% c14$depth)
        chron_not_in_c14 <- which(!chron$age %in% c14$age & !chron$depth %in% c14$depth)
        chron_conf_age <- which(!chron$age %in% c14$age & chron$depth %in% c14$depth)
        chron_conf_depth <- which(chron$age %in% c14$age & !chron$depth %in% c14$depth)
    }
    
    # Check for information on the tables and interactively ask for data use if there are conflicts
    if(length(c14_in_chron) > 0){
        cat("\n")
        cat("Chronology has coincident data with C14 data and, hence, the later will be used\n")
        cat("C14 data:\n")
        .printData(c14[c14_in_chron,], format)
        cat("\n")
        cat("Chronology data:\n")
        .printData(chron[chron_in_c14,], format)
    }
    if(length(chron_not_in_c14) > 0){
        cat("\n")
        cat("Chronology has additional no-C14 data.\n")
        cat("Chronology data:\n")
        .printData(chron[chron_not_in_c14,], format)
        while(is.na(include_chron_not_in_c14) || !is.logical(include_chron_not_in_c14)){
            include_chron_not_in_c14 <- as.logical(readline("Incorporate these data to the chronology? (Yes: T then Intro, No: F then Intro)"))
            if(is.na(include_chron_not_in_c14) || !is.logical(include_chron_not_in_c14)){
                warning("Sorry! Invalid value.", call.=F, immediate.=T)
            }
        }
    }else{include_chron_not_in_c14 <- FALSE}
    if(length(c14_not_in_chron) > 0){
        cat("\n")
        cat("There are additional C14 data not included in the chronology.\n")
        cat("C14 data:\n")
        .printData(c14[c14_not_in_chron,], format)
        while(is.na(include_c14_not_in_chron) || !is.logical(include_c14_not_in_chron)){
            include_c14_not_in_chron <- as.logical(readline("Incorporate these data to the chronology? (Yes: T then Intro, No: F then Intro)"))
            if(is.na(include_c14_not_in_chron) || !is.logical(include_c14_not_in_chron)){
                warning("Sorry! Invalid value.", call.=F, immediate.=T)
            }
        }
    }else{include_c14_not_in_chron <- FALSE}
    if(length(c14_conf_age) > 0){
        cat("\n")
        cat("There are age conflicts between c14 data and the chronology.\n")
        cat("C14 data:\n")
        .printData(c14[c14_conf_age,], format)
        cat("Chronology data:\n")
        .printData(chron[chron_conf_age,], format)
        while(is.na(use_c14_conf_age) || !is.logical(use_c14_conf_age)){
            use_c14_conf_age <- as.logical(readline("Use ages from the C14 table? If not, ages from the chronology will be used. (Yes: T then Intro, No: F then Intro)"))
            if(is.na(use_c14_conf_age) || !is.logical(use_c14_conf_age)){
                warning("Sorry! Invalid value.", call.=F, immediate.=T)
            }
        }
    }else{use_c14_conf_age <- FALSE}
    if(length(c14_conf_depth) > 0){
        cat("There are age conflicts between c14 data and the chronology.\n")
        cat("C14 data:\n")
        .printData(c14[c14_conf_depth,], format)
        cat("\n")
        cat("Chronology data:\n")
        .printData(chron[chron_conf_depth,], format)
        while(is.na(use_c14_conf_depth) || !is.logical(use_c14_conf_depth)){
            use_c14_conf_depth <- as.logical(readline("Use depths from the C14 table? If not, depths from the chronology will be used. (Yes: T then Intro, No: F then Intro)"))
            if(is.na(use_c14_conf_depth) || !is.logical(use_c14_conf_depth)){
                warning("Sorry! Invalid value.", call.=F, immediate.=T)
            }
        }
    }else{use_c14_conf_depth <- FALSE}
    
    # Combine chron and c14 accordingly to specified data use
    output <- chron[-c(1:nrow(chron)),]
    output <- rbind(output, c14[c14_in_chron,])
    
    if(include_chron_not_in_c14){
        output <- rbind(output, chron[chron_not_in_c14,])
    }
    if(include_c14_not_in_chron){
        output <- rbind(output, c14[c14_not_in_chron,])
    }
    if(use_c14_conf_age){
        output <- rbind(output, c14[c14_conf_age,])
    }else{
        output <- rbind(output, chron[chron_conf_age,])
    }
    if(use_c14_conf_depth){
        output <- rbind(output, c14[c14_conf_depth,])
    }else{
        output <- rbind(output, chron[chron_conf_depth,])
    }

    # Check for events in the datation object
    if(nrow(events) > 0){
        warning("There are dated events for this core (entity).", immediate.=T, call.=F)
        cat("\n")
        cat("Events data:\n")
        .printEvents(events)
        include_events <- readline("Include events information in the files? (Yes: T then Intro, No: F then Intro)")
        try(include_events <- eval(parse(text=include_events)))
        while(!(is.logical(include_events) || is.numeric(include_events)) || is.na(include_events) || is.null(include_events)){
            warning("Sorry! Invalid value.", call.=F, immediate.=T)
            include_events <- readline("Include events information in the files? (Yes: T then Intro, No: F then Intro)")
            try(include_events <- eval(parse(text=include_events)))
        }
    }else{include_events <- FALSE}
    if((is.logical(include_events) && include_events == TRUE) || is.numeric(include_events)){
        if(is.numeric(include_events)){
            events <- events[include_events,]
        }
        events.export <- exportEvents(events, format)
        output <- rbind(output, events.export)
    }
    
    
    # Create directory to save files for CLAM
    if(!dir.exists(paste(format, "/Cores/", e_, sep=""))){
        dir.create(paste(format, "/Cores/", e_, sep=""), recursive=TRUE)
    }
    
    # Order dataframe by depths and write to the directory
    output <- output[order(output$depth),]    
    utils::write.csv(output, file=paste(format, "/Cores/", e_, "/", e_, ".csv", sep=""), na="", row.names=FALSE)
    
    # Extract depth columns for pollen counts and create depths.txt files.
    if(exists("include_depths")){
        depths.export <- exportDepths(depths)
        utils::write.table(depths.export, file=paste(format, "/Cores/", e_, "/", e_, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        utils::write.table(depths, file=paste(format, "/Cores/", e_, "/", e_, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE, sep=",")
    }
    return(output)
}
