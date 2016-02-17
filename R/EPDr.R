
#' Conect to a EPD database
#'
#' \code{"connectToEPD"} establish a connection to a EPD data base that is stored in a DDBB server. By default it assume a local PostgreSQL server. Potentially the function should work with other servers, but it has not been tested. Similarly, the function could connect to remote servers, but this possibility has not been tested yet. To connect to the DDBB the function need the DDBB name, the user name, and the user password. If any of the data are not passed as arguments the function will ask for them interactively.
#'
#' @param DB Character string with the DDBB name. If not provided the function will ask for it before establishing the connection.
#' @param US Character string with the user name. A valid user in the DDBB server. If not provided the function will ask for it before establishing the connection.
#' @param PW Character string with the user password. A valid password for the user in the DDBB server. If not provided the function will ask for it before establishing the connection.
#' @param DRV Character string with the driver used to connect with the DDBB server (default: "PostgreSQL"). This value will depend on the DDBB server used to host the EPD database. For alternatives look at the \code{\link[DBI:dbConnect]{dbConnect}} function.
#' @param HOST Character string with the IP address of the DDBB server (default: "localhost").
#'
#' @return This function return a connection
#' @export
#'
#' @examples
#' # Not run
#' # connectToEPD()
#' # connectToEPD("", "", "")
connectToEPD <- function(DB=NULL, US=NULL, PW=NULL, DRV="PostgreSQL", HOST="localhost"){
    # Ask interactively for parameters if they are not specified
    if(is.null(DRV))DRV <- readline("EPD DB driver:")
    if(is.null(DB))DB <- readline("EPD DB name:")
    if(is.null(US))US <- readline("EPD DB user:")
    if(is.null(PW))PW <- readline("EPD DB password:")
    
    # Establish connection to PoststgreSQL
    con <- dbConnect(DRV, dbname=DB, host=HOST, user=US, password=PW)
    return(con)
}


# Disconnect a connection to a EPD database
#' Title
#'
#' @param con TBW
#'
#' @return TBW
#' @export
#'
#' @examples
#' # TBW
disconnectFromEPD <- function(con=NULL){
    # Close PostgreSQL connection
    if(is.null(con))stop("You have to define a working connection to the EPD to be stoped")
    dbDisconnect(con)
}



extractC14 <- function(core.num, conn) {
    sqlQuery <- paste("SELECT * FROM c14 WHERE e_=", core.num, ";", sep="")
    c14 <- dbGetQuery(conn, sqlQuery)
    
    sqlQuery <-paste("SELECT * FROM geochron WHERE e_=", core.num, ";", sep="")
    geochron <- dbGetQuery(conn, sqlQuery)
    
    c14geochron <- merge(c14, geochron, by=c("e_","sample_"))
    
    return(c14geochron)
}

extractChronologies <- function(core.num, conn) {
    output <- list()
    
    sqlQuery <-paste("SELECT * FROM chron WHERE e_=", core.num, ";", sep="")
    chron <- dbGetQuery(conn, sqlQuery)
    
    sqlQuery <- paste("SELECT * FROM agebasis WHERE e_=", core.num, ";", sep="")
    agebasis <- dbGetQuery(conn, sqlQuery)
    
    output$num.chron <- nrow(chron)
    output$chron.default <- chron$chron_[which(chron$defaultchron == "Y")]
    
    output$chron <- chron
    output$agebasis <- agebasis
    
    output$no_C14 <- subset(agebasis, rcode != "C14")
    
    return(output)
}

c14toCLAM <- function(c14geochron) {
    output <- data.frame(lab_ID=c14geochron$labnumber, C14_age=c14geochron$agebp)
    output$cal_age <- NA        
    output$error <- c14geochron$agesdup
    output$reservoir <- NA
    output$depth <- c14geochron$depthcm
    output$thicknesses <- c14geochron$thickness
    
    return(output)
}


# Mirar en la web http://www.europeanpollendatabase.net el c?digo del testigo que nos interesa, (usar ese c?digo en core.num)
#' Title
#'
#' @param core.num TBW
#' @param conn TBW
#' @param get.dephts TBW
#'
#' @return TBW
#' @export
#'
#' @examples
#' # TBW
core4Clam <- function(core.num, conn, get.dephts=TRUE){
    # Just for testing. Do no uncomment
    # core.num <- "1"
    # conn <- connEPD
    # get.dephts <- TRUE
    
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
    
    .addExtraChronData <- function(c14, extra){
        output <- data.frame(lab_ID=paste("EPDr_", extra$e_, "_CH", extra$sample_, sep=""), cal_age=extra$age, depth=extra$depthcm, thicknesses=extra$thickness)
        output$C14_age <- NA
        output$error <- 1
        output$reservoir <- NA
        output <- rbind(c14, output)
        return(output)
    }
    
    .addEventData <- function(c14, event){
        output <- data.frame(lab_ID=paste("EPDr_", event$e_, "_EV", event$event_, sep=""), cal_age=event$agebp, depth=event$depthcm, thicknesses=event$thickness)
        output$C14_age <- NA
        output$error <- 1
        output$reservoir <- NA
        output <- rbind(c14, output)
        return(output)
    }
    
    # Get C14 data for a specific core
    c14geochron <- extractC14(core.num, conn)
    
    # Extract C14 data in CLAM format and plot them
    clam.table <- c14toCLAM(c14geochron)
    cat("THESE ARE THE C14 DATA FOR CORE:", core.num, "\n")
    .printC14(clam.table)
    
    # Extract existing chronologies for this particular core in the DDBB
    cat("\nCOMPARING WITH PREVIOUS CHRONOLOGIES IN THE EPD DDBB:\n")
    chrons <- extractChronologies(core.num, conn)
    
    # Check for information on the chronology object and interactively ask for data use if default is not specified
    if(chrons$num.chron == 1){
        cat("  Core", core.num, "has", chrons$num.chron,"chronology...\n")
        cat("    with this NO C14 data:\n")
        .printNoC14(chrons)
        include.extradata <- as.logical(readline("Include extra data in the calibration files? (Yes: T then Intro, No: F then Intro)"))
        while(!exists("include.extradata") | is.na(include.extradata) | !is.logical(include.extradata)){
            warning("Sorry! Invalid value.", call.=F, immediate.=T)
            include.extradata <- as.logical(readline("Include extra data in the calibration files? (Yes: T then Intro, No: F then Intro)"))
        }
    }else{
        cat("  Core", core.num, "has", chrons$num.chron, "chronologies...\n")
        cat("  Default is chronology", chrons$chron.default, "\n")
        
        lapply(1:chrons$num.chron, function(x, y){.printNoC14(y, x)}, chrons)        
        
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
    if(chrons$num.chron == 1){
        if(include.extradata){
            clam.table <- .addExtraChronData(clam.table, chrons$no_C14)
        }
    }else{
        if(include.extradata){
            if(use.all.chron){
                which.chron <- 1:chrons$num.chron
            }
            extradata <- subset(chrons$no_C14, chron_ %in% which.chron)
            clam.table <- .addExtraChronData(clam.table, extradata)
        }
    }

    # Get events data
    sqlQuery <-paste("SELECT * FROM synevent WHERE e_ =", core.num, ";", sep="")
    synevent.table <- dbGetQuery(conn, sqlQuery)
    
    # Check for event data and ask interactively for data use
    if(nrow(synevent.table) == 0){
        cat("This core has no events in the chronology.")
    }else{
        warning("This core has events in the chronology.", immediate.=T, call.=F)
        sqlQuery <-paste("SELECT * FROM event WHERE event_ IN (", paste(synevent.table$event_, collapse=","), ");", sep="")
        event.table <- dbGetQuery(conn, sqlQuery)
        event.table <- merge(synevent.table, event.table, by="event_")
        .printEvents(event.table)
        include.events <- as.logical(readline("Include events information in the files? (Yes: T then Intro, No: F then Intro)"))
        while(!exists("include.events") | is.na(include.events) | !is.logical(include.events)){
            warning("Sorry! Invalid value.", call.=F, immediate.=T)
            include.events <- as.logical(readline("Do you want to include events information in the files? (Yes: T then Intro, No: F then Intro)"))
        }
        if(include.events){
            clam.table <- .addEventData(clam.table, event.table)
        }
    }
    
    # Create directory to save files for CLAM
    if(!dir.exists(paste("Cores/", core.num, sep=""))){
        dir.create(paste("Cores/", core.num, sep=""), recursive=TRUE)
    }
    
    # Order dataframe by depths and write to the directory
    clam.table <- clam.table[order(clam.table$depth),]    
    write.csv(clam.table, file=paste("Cores/", core.num, "/", core.num, ".csv", sep=""), na="", row.names=FALSE)
    
    # Extract depth columns for pollen counts and create depths.txt files.
    if(get.dephts==T){
        sqlQuery <- paste("select * from p_sample where e_=", core.num, ";", sep="")
        depth.table <- dbGetQuery(conn, sqlQuery)
        depth.table$lab_ID <- paste("EPDr", depth.table$e_, "_PO", depth.table$sample_, sep="")
        depth.table <- depth.table[order(depth.table$depthcm),]
        
        write.table(depth.table$depthcm, file=paste("Cores/", core.num, "/", core.num, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        write.table(depth.table[,c("lab_ID", "depthcm")], file=paste("Cores/", core.num, "/", core.num, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE, sep=",")
    }
}


# Creo que esto será necesario para cuando lancemos CLAM, pero no ahora
#
# sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", core.num, ";", sep="")
# site.num <- as.character(dbGetQuery(conn, sqlQuery))
# 
# sqlQuery <-paste("SELECT * FROM siteloc WHERE site_=", site.num, ";", sep="")
# siteloc.table <- dbGetQuery(conn, sqlQuery)
#
