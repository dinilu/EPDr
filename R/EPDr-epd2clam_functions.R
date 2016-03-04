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
#' library(EPDr)
#' epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#'                                user="epdr", password="epdrpw")
#' c14 <- getC14(400, epd.connection)
#' c14.clam <- c14_epd2clam(c14)
c14_epd2clam <- function(C14) {
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
#' library(EPDr)
#' epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#'                                user="epdr", password="epdrpw")
#' c14 <- getC14(400, epd.connection)
#' c14.clam <- c14_epd2clam(c14)
#' chron <- getChronology(400, epd.connection)
#' noc14.clam <- agebasis_epd2clam(chron@agebasis)
#' all.clam <- rbind(c14.clam, noc14.clam)
agebasis_epd2clam <- function(agebasis){
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
#' epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#'                                user="epdr", password="epdrpw")
#' synevent <- dbGetQuery(epd.connection, "SELECT * FROM synevent WHERE e_ = 51;")
#' event <- dbGetQuery(epd.connection, paste("SELECT * FROM event WHERE event_ = ",
#'                     synevent$event_, ";", sep=""))
#' event <- merge(synevent, event, by="event_")
#' event.clam <- events_epd2clam(event)
events_epd2clam <- function(event){
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
#' epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#'                                  password="epdrpw", host="diegonl.ugr.es")
#' depths.1 <- getDepths(1, epd.connection)
#' depths_epd2clam(depths.1)
#' disconnectFromEPD(connection=epd.connection)
depths_epd2clam <- function(depths){
    output <- depths[order(depths$depthcm),]
    output <- output$depthcm
    return(output)
}



#
#
# #' Reshape a chronology object for CLAM
# #'
# #' @param chronology Chronology object as returned by \code{\link[EPDr:getChronology]{getChronology}}.
# #' @param c14 Data frame with C14 data as returned by \code{\link[EPDr:getC14]{getC14}}.
# #' @param chronology_number Integer indicating the number of the chronology to be used. By default it uses the default chronology as in the EPD DDBB.
# #'
# #' @return Data frame with C14 and noC14 data ready for CLAM
# #' 
# #' @export
# #'
# #' @examples
# #' library(EPDr)
# #' epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
# #'                                user="epdr", password="epdrpw")
# #' c14 <- getC14(4, epd.connection)
# #' chron <- getChronology(4, epd.connection)
# #' #chron.clam <- chron_epd2clam(chron, c14)
# chron_epd2clam <- function(chronology, c14, chronology_number=NA){
#     if(is.na(chronology_number)){
#         chronology_number <- chronology$default_chronology
#     }else{
#         if(!chronology_number %in% 1:chronology$number_of_chronologies){stop("The chronology does not exist.")}
#     }
#     output <- core4clam(chronology, c14, chronology_number, T, F, F, F)
#     return(output)
# }
# 

#' TBW
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
        warning("There are events data for this core (entity).", immediate.=T, call.=F)
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
    if(exists("use_dephts")){
        depths.clam <- depths_epd2clam(depths)
        write.table(depths.clam, file=paste("Cores/", core_number, "/", core_number, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        write.table(depths, file=paste("Cores/", core_number, "/", core_number, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE, sep=",")
    }
    return(output)
}




# Mirar en la web http://www.europeanpollendatabase.net el c?digo del testigo que nos interesa, (usar ese c?digo en core_number)
#' Title
#'
#' @param core_number TBW
#' @param connection TBW
#' @param get_dephts TBW
#'
#' @return TBW
#' @export
#'  
#' @examples
#' # TBW
core4Clam_old <- function(core_number, connection, get_dephts=TRUE){
    # Just for testing. Do no uncomment
    # core_number <- "1"
    # connection <- connEPD
    # get_dephts <- TRUE
    
    
    # Define internal functions
    .printC14 <- function(data){
        cat(c("lab_ID", "C14_age", "cal_age", "error", "reserv.", "depth", "thickn.\n"), sep="\t")
        apply(data, "\n", MARGIN=1, FUN=cat, sep="\t")
    }
    
    .printNoC14 <- function(chron_object, chron_=unique(chron_object$no_C14$chron_)){
        cat("\n")
        cat("NOTES:")
        cat(chron_object$chron$notes[which(chron_object$no_C14$chron_ %in% chron_)], fill=T)
        cat(c("e_", "chron_", "sample_", "depthcm", "thickn.", "age", "ageup", "agelo", "rcode"), sep="\t", fill=T)
        apply(chron_object$no_C14[which(chron_object$no_C14$chron_ %in% chron_),], "\n",  MARGIN=1, FUN=cat, sep="\t")
    }
    
    .printEvents <- function(event){
        cat("\n")
        cat(c("event_", "e_", "depthcm", "thickn.", "event", "name", "agebp", "ageup", "agelo", "publ"), sep="\t", fill=T)
        apply(event, "\n",  MARGIN=1, FUN=cat, sep="\t")
    }
    

    # Get C14 data for a specific core
    c14geochron <- getC14(core_number, connection)
    
    # Extract C14 data in CLAM format and plot them
    clam.table <- c14_epd2clam(c14geochron)
    cat("THESE ARE THE C14 DATA FOR CORE:", core_number, "\n")
    .printC14(clam.table)
    
    # Extract existing chronologies for this particular core in the DDBB
    cat("\nCOMPARING WITH PREVIOUS CHRONOLOGIES IN THE EPD DDBB:\n")
    chrons <- getChronology(core_number, connection)
    
    # Check for information on the chronology object and interactively ask for data use if default is not specified
    if(chrons$number_of_chronologies == 1){
        cat("  Core", core_number, "has", chrons$number_of_chronologies,"chronology...\n")
        cat("    with this NO C14 data:\n")
        .printNoC14(chrons)
        include.extradata <- as.logical(readline("Include extra data in the calibration files? (Yes: T then Intro, No: F then Intro)"))
        while(!exists("include.extradata") | is.na(include.extradata) | !is.logical(include.extradata)){
            warning("Sorry! Invalid value.", call.=F, immediate.=T)
            include.extradata <- as.logical(readline("Include extra data in the calibration files? (Yes: T then Intro, No: F then Intro)"))
        }
    }else{
        cat("  Core", core_number, "has", chrons$number_of_chronologies, "chronologies...\n")
        cat("  Default is chronology", chrons$default_chronology, "\n")
        
        lapply(1:chrons$number_of_chronologies, function(x, y){.printNoC14(y, x)}, chrons)        
        
        include.extradata <- as.logical(readline("Include extra data in the calibration files? (Yes: T then Intro, No: F then Intro)"))
        while(!exists("include.extradata") | is.na(include.extradata) | !is.logical(include.extradata)){
            warning("Sorry! Invalid value.", call.=F, immediate.=T)
            include.extradata <- as.logical(readline("Include extra data in the calibration files? (Yes: T then Intro, No: F then Intro)"))
        }
        
        if(include.extradata){
            use.all.chron <- as.logical(readline("Use extra info from ALL chronologies? (Yes: T then Intro, No: F then Intro)"))
            while(!exists("use.all.chron") | is.na(use.all.chron) | !is.logical(use.all.chron)){
                warning("Sorry! Invalid value.", call.=F, immediate.=T)
                use.all.chron <- as.logical(readline("Use extra info from ALL chronologies? (Yes: T then Intro, No: F then Intro)"))
            }
            
            if(exists("use.all.chron") & !use.all.chron){
                which.chron <- readline("Which chronology/ies do you want to use info from? (Provide a number or a sequence of number separated by comma without spaces. For example:1,3,4)")
                which.chron <- as.numeric(unlist(strsplit(which.chron, ",")))
                while(!exists("which.chron") | any(is.na(which.chron))){
                    warning("Sorry! Invalid value.", call.=F, immediate.=T)
                    which.chron <- readline("Which chronology/ies do you want to use info from? (Provide a number or a sequence of number separated by comma without spaces. For example:1,3,4)")
                    which.chron <- unlist(strsplit(which.chron, ","))
                }
            }
        }
    }
    
    # Add additional data (NO C14 and EVENTS) according to specified arguments
    if(chrons$number_of_chronologies == 1){
        if(include.extradata){
            noC14.CLAM <- agebasis_epd2clam(chrons$no_C14)
            clam.table <- rbind(clam.table, noC14.CLAM)
        }
    }else{
        if(include.extradata){
            if(use.all.chron){
                which.chron <- 1:chrons$number_of_chronologies
            }
            noC14.data <- subset(chrons$no_C14, chrons$no_C14$chron_ %in% which.chron)
            noC14.CLAM <- agebasis_epd2clam(noC14.data)
            clam.table <- rbind(clam.table, noC14.CLAM)
        }
    }

    events.table <- getEvents(core_number, connection)
    
    if(is.na(event.table)){
        cat("This core has no events in the chronology.")
    }else{
        warning("This core has events in the chronology.", immediate.=T, call.=F)
        .printEvents(event.table)
        include.events <- as.logical(readline("Include events information in the files? (Yes: T then Intro, No: F then Intro)"))
        while(!exists("include.events") | is.na(include.events) | !is.logical(include.events)){
            warning("Sorry! Invalid value.", call.=F, immediate.=T)
            include.events <- as.logical(readline("Do you want to include events information in the files? (Yes: T then Intro, No: F then Intro)"))
        }
        if(include.events){
            event.CLAM <- events_epd2clam(event.table)
            clam.table <- rbind(clam.table, event.CLAM)
        }
    }

    # Create directory to save files for CLAM
    if(!dir.exists(paste("Cores/", core_number, sep=""))){
        dir.create(paste("Cores/", core_number, sep=""), recursive=TRUE)
    }
    
    # Order dataframe by depths and write to the directory
    clam.table <- clam.table[order(clam.table$depth),]    
    write.csv(clam.table, file=paste("Cores/", core_number, "/", core_number, ".csv", sep=""), na="", row.names=FALSE)
    
    # Extract depth columns for pollen counts and create depths.txt files.
    if(get_dephts==T){
        depths.table <- getDepths(core_number, connection)
        depths.CLAM <- depths_epd2clam(depths.table)
                
        write.table(depths.CLAM, file=paste("Cores/", core_number, "/", core_number, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        write.table(depths.table, file=paste("Cores/", core_number, "/", core_number, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE, sep=",")
    }
}




