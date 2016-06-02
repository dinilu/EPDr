#' Reshape C14 data to CLAM format
#' 
#' This function takes C14 data, as those extracted \code{\link[EPDr:getC14]{getC14}}, to fit into a new table that comply with
#' CLAM format.
#'
#' @param C14 Data frame with C14 data as those extracted from \code{\link[EPDr:getC14]{getC14}}. 
#'
#' @return Data frame with C14 data in CLAM format.
#' 
#' @export
#'
#' @examples
#' #library(EPDr)
#' #epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#' #                               user="epdr", password="epdrpw")
#' #c14 <- getC14(400, epd.connection)
#' #c14.clam <- c14_epd2clam(c14)
c14_epd2clam <- function(C14) {
    if(nrow(C14) == 0){
        stop("Table without c14 data. Not performing conversion, useless for CLAM.")
    }
    output <- data.frame(lab_ID=C14$labnumber, C14_age=C14$agebp)
    output$cal_age <- NA        
    output$error <- C14$agesdup
    output$reservoir <- NA
    output$depth <- C14$depthcm
    output$thickness <- C14$thickness
    output <- output[,c("lab_ID", "C14_age", "cal_age", "error", "reservoir", "depth", "thickness")]
    return(output)
}




#' Reshape no-C14 data to CLAM format
#' 
#' This function takes no-C14 data, that can be extracted from a chronology object, to fit into a new table that comply with
#' CLAM format.
#'
#' @param agebasis Data frame with no-C14 data as those in a chronology list returned by \code{\link[EPDr:getChronology]{getChronology}}. 
#'
#' @return Data frame with no-C14 data in CLAM format. This data frame can be easily combined with C14 data from \code{\link[EPDr:getChronology]{c14_epd2clam}} using \code{rbind}.
#' 
#' @export
#'
#' @examples
#' #library(EPDr)
#' #epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#' #                               user="epdr", password="epdrpw")
#' #c14 <- getC14(400, epd.connection)
#' #c14.clam <- c14_epd2clam(c14)
#' #chron <- getChronology(400, epd.connection)
#' #noc14.clam <- agebasis_epd2clam(chron@agebasis)
#' #all.clam <- rbind(c14.clam, noc14.clam)
agebasis_epd2clam <- function(agebasis){
    if(nrow(agebasis) == 0){
        stop("Table without data. Not performing conversion, useless for CLAM.")
    }
    output <- data.frame(lab_ID=paste("EPDr_", agebasis$e_, "_CH", agebasis$sample_, sep=""), C14_age=agebasis$age, error=agebasis$ageup - agebasis$age, depth=agebasis$depthcm, thickness=agebasis$thickness)
    output$cal_age <- NA
    output$error[which(is.na(output$error | output$error == 0))] <- 1
    output$reservoir <- NA
    output <- output[,c("lab_ID", "C14_age", "cal_age", "error", "reservoir", "depth", "thickness")]
    return(output)
}





#' Reshape events data to CLAM format
#' 
#' This function takes event data to fit into a new table that comply with CLAM format.
#'
#' @param event Data frame with event data for a particular core (entity) in the EPD DDBB.
#'
#' @return Data frame with event data in CLAM format.
#' 
#' @export
#'
#' @examples
#' #epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#' #                               user="epdr", password="epdrpw")
#' #synevent <- dbGetQuery(epd.connection, "SELECT * FROM synevent WHERE e_ = 51;")
#' #event <- dbGetQuery(epd.connection, paste("SELECT * FROM event WHERE event_ = ",
#' #                    synevent$event_, ";", sep=""))
#' #event <- merge(synevent, event, by="event_")
#' #event.clam <- events_epd2clam(event)
events_epd2clam <- function(event){
    if(nrow(event) == 0){
        stop("Table without dated events. Not performing conversion, useless for CLAM.")
    }
    output <- data.frame(lab_ID=paste("EPDr_", event$e_, "_EV", event$event_, sep=""), C14_age=event$agebp, depth=event$depthcm, thickness=event$thickness)
    output$cal_age <- NA
    output$error <- 1
    output$reservoir <- NA
    output <- output[,c("lab_ID", "C14_age", "cal_age", "error", "reservoir", "depth", "thickness")]
    return(output)
}





#' Reshape depths data to CLAM format
#'
#' This function takes depths data, as returned by \code{\link[EPDr:getDepths]{getDepths}}, to comply with CLAM format.
#'
#' @param depths Data frame with at least a column called depthcm.
#'
#' @return Vector with depths in ascending order.
#' 
#' @export
#'
#' @examples
#' #epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#' #                                 password="epdrpw", host="diegonl.ugr.es")
#' #depths.1 <- getDepths(1, epd.connection)
#' #depths_epd2clam(depths.1)
#' #disconnectFromEPD(connection=epd.connection)
depths_epd2clam <- function(depths){
    output <- depths[order(depths$depthcm),]
    output <- output$depthcm
    return(output)
}



#' Title TBW
#'
#' @param x TBW
#' @param chronology_number TBW
#' @param include_chron_not_in_c14 TBW
#' @param include_c14_not_in_chron TBW
#' @param use_c14_conf_age TBW
#' @param use_c14_conf_depth TBW
#' @param include_depths TBW
#' @param include_events TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
datation_epd2clam <- function(x, chronology_number=NA, include_chron_not_in_c14=NA, include_c14_not_in_chron=NA, 
                              use_c14_conf_age=NA, use_c14_conf_depth=NA, include_depths=T, include_events=NULL){
    # x <- prueba
    # chronology_number=NA
    # include_chron_not_in_c14=NA
    # include_c14_not_in_chron=NA
    # use_c14_conf_age=NA
    # use_c14_conf_depth=NA
    # include_depths=T
    
    # Define internal functions
    .printCLAM <- function(data){
        cat(c("lab_ID", "C14_age", "cal_age", "error", "reserv.", "depth", "thickn.\n"), sep="\t")
        apply(data, "\n", MARGIN=1, FUN=cat, sep="\t")
    }
    .printEvents <- function(event){
        cat(c("event_", "e_", "depthcm", "thickn.", "event", "name", "agebp", "ageup", "agelo", "publ"), sep="\t", fill=T)
        apply(event, "\n",  MARGIN=1, FUN=cat, sep="\t")
    }
    
    # Check for right class object
    if(class(x)  != "datation"){
        stop("Invalid x object. x has to be a datation object. See ?getDatation")
    }

    # Check if datation object has C14 and chronology information
    if(nrow(x@c14) == 0){
        stop("Datation object for a core without c14 data. Not performing conversion, useless for CLAM.")
    }
    
    # Get sub-objects from datation object
    core_number <- x@core_number
    chronology <- x@chronology
    c14 <- x@c14
    events <- x@events
    depths <- x@depths
    
    # Default chronology if no defined
    if(is.na(chronology_number)){
        chronology_number <- chronology@default_chronology
    }else{
        if(!chronology_number %in% 1:chronology@number_of_chronologies){stop("The chronology does not exist.")}
    }
    
    # Get agebasis for the chronology and convert for clam
    agebasis <- chronology@agebasis
    agebasis <- subset(agebasis, chron_ == chronology_number)
    
    chron <- agebasis_epd2clam(agebasis)
    c14 <- c14_epd2clam(c14)
    
    # Check for data in c14 and the chronology and for conflicting data
    c14_in_chron <- which(c14$C14_age %in% chron$C14_age & c14$depth %in% chron$depth)
    c14_not_in_chron <- which(!c14$C14_age %in% chron$C14_age & !c14$depth %in% chron$depth) 
    c14_conf_age <- which(!c14$C14_age %in% chron$C14_age & c14$depth %in% chron$depth)
    c14_conf_depth <- which(c14$C14_age %in% chron$C14_age & !c14$depth %in% chron$depth)
    
    chron_in_c14 <- which(chron$C14_age %in% c14$C14_age & chron$depth %in% c14$depth)
    chron_not_in_c14 <- which(!chron$C14_age %in% c14$C14_age & !chron$depth %in% c14$depth)
    chron_conf_age <- which(!chron$C14_age %in% c14$C14_age & chron$depth %in% c14$depth)
    chron_conf_depth <- which(chron$C14_age %in% c14$C14_age & !chron$depth %in% c14$depth)
    
    # Check for information on the tables and interactively ask for data use if there are conflicts
    if(length(c14_in_chron) > 0){
        cat("\n")
        cat("Chronology has coincident data with C14 data and, hence, the later will be used\n")
        cat("C14 data:\n")
        .printCLAM(c14[c14_in_chron,])
        cat("\n")
        cat("Chronology data:\n")
        .printCLAM(chron[chron_in_c14,])
    }
    if(length(chron_not_in_c14) > 0){
        cat("\n")
        cat("Chronology has additional no-C14 data.\n")
        cat("Chronology data:\n")
        .printCLAM(chron[chron_not_in_c14,])
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
        .printCLAM(c14[c14_not_in_chron,])
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
        .printCLAM(c14[c14_conf_age,])
        cat("Chronology data:\n")
        .printCLAM(chron[chron_conf_age,])
        while(is.na(use_c14_conf_age) || !is.logical(use_c14_conf_age)){
            use_c14_conf_age <- as.logical(readline("Use ages from the C14 table? If not ages from the chronology will be used. (Yes: T then Intro, No: F then Intro)"))
            if(is.na(use_c14_conf_age) || !is.logical(use_c14_conf_age)){
                warning("Sorry! Invalid value.", call.=F, immediate.=T)
            }
        }
    }else{use_c14_conf_age <- FALSE}
    if(length(c14_conf_depth) > 0){
        cat("There are age conflicts between c14 data and the chronology.\n")
        cat("C14 data:\n")
        .printCLAM(c14[c14_conf_depth,])
        cat("\n")
        cat("Chronology data:\n")
        .printCLAM(chron[chron_conf_depth,])
        while(is.na(use_c14_conf_depth) || !is.logical(use_c14_conf_depth)){
            use_c14_conf_depth <- as.logical(readline("Use depths from the C14 table? If not depths from the chronology will be used. (Yes: T then Intro, No: F then Intro)"))
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
    }else{include_events <- FALSE}
    if(is.null(include_events)){
        include_events <- readline("Include events information in the files? (Yes: T then Intro, No: F then Intro)")
        try(include_events <- eval(parse(text=include_events)))
        while(!(is.logical(include_events) || is.numeric(include_events)) || is.na(include_events) || is.null(include_events)){
            warning("Sorry! Invalid value.", call.=F, immediate.=T)
            include_events <- readline("Include events information in the files? (Yes: T then Intro, No: F then Intro)")
            try(include_events <- eval(parse(text=include_events)))
        }
    }
    if((is.logical(include_events) && include_events == TRUE) || is.numeric(include_events)){
        if(is.numeric(include_events)){
            events <- events[include_events,]
        }
        events.clam <- events_epd2clam(events)
        output <- rbind(output, events.clam)
    }
    
    
    # Create directory to save files for CLAM
    if(!dir.exists(paste("Cores/", core_number, sep=""))){
        dir.create(paste("Cores/", core_number, sep=""), recursive=TRUE)
    }
    
    # Order dataframe by depths and write to the directory
    output <- output[order(output$depth),]    
    write.csv(output, file=paste("Cores/", core_number, "/", core_number, ".csv", sep=""), na="", row.names=FALSE)
    
    # Extract depth columns for pollen counts and create depths.txt files.
    if(exists("include_depths")){
        depths.clam <- depths_epd2clam(depths)
        write.table(depths.clam, file=paste("Cores/", core_number, "/", core_number, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        write.table(depths, file=paste("Cores/", core_number, "/", core_number, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE, sep=",")
    }
    return(output)
}


