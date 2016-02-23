#' Conect to a EPD database
#'
#' \code{connectToEPD} establish a connection to a EPD data base that is stored in a DDBB server. By default it assume a local
#' PostgreSQL server. The function can connect with remote servers in different formats (MySQL, etc; see RPostgreSQL documentation
#' for supported formats). To connect to the DDBB the function need the DDBB name, the user name, and the user password. If any of
#' the data are not passed as arguments the function will ask for them interactively.
#'
#' @param database Character string with the DDBB name. If not provided the function will ask for it before establishing the connection.
#' @param user Character string with the user name. A valid user in the DDBB server. If not provided the function will ask for it
#' before establishing the connection.
#' @param password Character string with the user password. A valid password for the user in the DDBB server. If not provided the
#' function will ask for it before establishing the connection.
#' @param driver Character string with the driver used to connect with the DDBB server (default: "PostgreSQL"). This value will depend
#' on the DDBB server used to host the EPD database. For alternatives look at the \code{\link[DBI:dbConnect]{dbConnect}} function.
#' @param host Character string with the IP address of the DDBB server (default: "localhost").
#'
#' @return This function return a RPostgreSQL connection object.
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD()
#' epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#'                                  password="epdrpw", host="diegonl.ugr.es")
#' epd.connection
#' # To list all the tables in the database we have connected with
#' dbListTables(epd.connection)
#' # Query data from the connection with a SQL statement
#' dbGetQuery(epd.connection, "SELECT e_ FROM synevent;")
#' disconnectFromEPD(connection=epd.connection)
connectToEPD <- function(database=NULL, user=NULL, password=NULL, driver="PostgreSQL", host="localhost"){
    # Ask interactively for parameters if they are not specified
    if(is.null(driver))driver <- readline("EPD DB driver:")
    if(is.null(database))database <- readline("EPD DB name:")
    if(is.null(user))user <- readline("EPD DB user:")
    if(is.null(password))password <- readline("EPD DB password:")
    
    # Establish connection to PoststgreSQL
    con <- dbConnect(driver, dbname=database, host=host, user=user, password=password)
    return(con)
}



#' Disconnect a connection to a EPD database
#' \code{disconnectFromEPD} turns down a connection to a EPD DDBB server.
#'
#' @param connection The connection object created with \code{\link[EPDr:connectToEPD]{connectToEPD}} to stablish the connection
#'
#' @return NULL It just disconnect from the EPD DDBB server and modify the connection object to reflect the new status.
#' 
#' @export
#' 
#' @examples
#' epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#'                                  password="epdrpw", host="diegonl.ugr.es")
#' disconnectFromEPD(connection=epd.connection)
#' epd.connection
disconnectFromEPD <- function(connection=NULL){
    # Close PostgreSQL connection
    if(is.null(connection))stop("You have to define a working connection to the EPD to be stoped")
    dbDisconnect(connection)
}



#' Extract C14 data for a particular core (entity in the EPD DDBB)
#' 
#' Given a core number (as in the EPD DDBB: e_) the function returns a matrix with the C14 data associated to this core. This values
#' come from two different tables of the EPD: c14 and geochron.
#'
#' @param core_number Integer or string with the core (entity) number for which C14 data want to be extracted.
#' @param connection Connection object to a EPD DDBB where the query is made.
#'
#' @return Data frame with combined information from c14 and geochron tables in the EPD. Columns in the data frame follows terminology
#' in the original database and are as follows:
#' \itemize{
#'  \item \code{e_}: Core (entity) identifier.
#'  \item \code{sample_}: C14 sample identifier.
#'  \item \code{agebp}: Uncalibrated C14 age.
#'  \item \code{agesdup}:
#'  \item \code{agesdlo}:
#'  \item \code{grthanage}:
#'  \item \code{basis}:
#'  \item \code{enriched}:
#'  \item \code{labnumber}:
#'  \item \code{deltac13}:
#'  \item \code{notes}:
#'  \item \code{method}:
#'  \item \code{depthcm}:
#'  \item \code{thickness}:
#'  \item \code{materialdated}:
#'  \item \code{publ_}: Publication identifier.
#' } 
#' @export
#'
#' @examples
#' epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#'                                  password="epdrpw", host="diegonl.ugr.es")
#' getC14(1, epd.connection)
#' getC14(400, epd.connection)
#' disconnectFromEPD(connection=epd.connection)
getC14 <- function(core_number, connection) {
    sqlQuery <- paste("SELECT * FROM c14 WHERE e_=", core_number, ";", sep="")
    c14 <- dbGetQuery(connection, sqlQuery)
    
    sqlQuery <-paste("SELECT * FROM geochron WHERE e_=", core_number, ";", sep="")
    geochron <- dbGetQuery(connection, sqlQuery)
    
    if(nrow(c14) == 0){stop("This core (entity) does not have C14 data.", call.=FALSE)}
    
    c14geochron <- merge(c14, geochron, by=c("e_","sample_"))
    
    return(c14geochron)
}



#' Extract chronologies associated with a core (entity) in the pollen database
#' 
#' Given a core (entity) number, \code{\link[EPDr:getChronologies]{getChronologies}} extract all the information about chronologies associated with this core
#' in the EPD DDBB. This information comes from two different tables in the DDBB: chron and agebasis.
#'
#' @param core_number Integer or string with the core (entity) number for which C14 data want to be extracted.
#' @param connection Connection object to a EPD DDBB where the query is made.
#'
#' @return List with 5 elements:
#' \itemize{
#'   \item \code{number_of_chronologies}: Integer indicating the number of chronologies associated with the core (entity).
#'   \item \code{default_chronology}: Integer indicating which is the default chronology according to the EPD DDBB.
#'   \item \code{chron}: Data frame with the meta information on how each calibration was built.
#'   \item \code{agebasis}: Data frame with all the information (depth and age for C14 and no-C14 data) used to build the chronologies.
#'   \item \code{no_C14}: Data frame with no-C14 data used to build the chronologies.
#' }
#' 
#' @export
#'
#' @examples
#' epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#'                                  password="epdrpw", host="diegonl.ugr.es")
#' getChronologies(1, epd.connection)
#' getChronologies(400, epd.connection)
#' disconnectFromEPD(connection=epd.connection)
getChronologies <- function(core_number, connection) {
    output <- list()
    
    sqlQuery <-paste("SELECT * FROM chron WHERE e_=", core_number, ";", sep="")
    chron <- dbGetQuery(connection, sqlQuery)
    
    sqlQuery <- paste("SELECT * FROM agebasis WHERE e_=", core_number, ";", sep="")
    agebasis <- dbGetQuery(connection, sqlQuery)
    
    output$number_of_chronologies <- nrow(chron)
    output$default_chronology <- chron$chron_[which(chron$defaultchron == "Y")]
    
    output$chron <- chron
    output$agebasis <- agebasis
    
    return(output)
}



#output$C14 <- print(try(getC14(core_number, connection), TRUE))
#output$no_C14 <- output$agebasis[!(output$agebasis$depthcm %in% output$C14$depthcm),]



#' Extract events associated with a specific core (entity) in the EPD DDBB
#'
#' Given a specific core number and connection to the EPD, this function return the information of events associated with the core.
#'
#' @param core_number Integer or string with the core (entity) number for which C14 data want to be extracted.
#' @param connection Connection object to a EPD DDBB where the query is made.
#'
#' @return NA or data frame, depending on whether there are events associated with the requested core. If there are events the
#' function return a data frame with combined information from synevent and event tables in the EPD. Columns in the data
#' frame follows terminology in the original database and are as follows:
#' \itemize{
#'  \item \code{event_}: Event identifier.
#'  \item \code{e_}: Core (entity) identifier.
#'  \item \code{depthcm}: Depth of the event in cm.
#'  \item \code{thickness}: Thickness of the event in cm.
#'  \item \code{event}: Code for the type of event. See EPD documentation for details.
#'  \item \code{name}: Name of the event.
#'  \item \code{agebp}: Known age of the event in BP.
#'  \item \code{ageuncertup}:
#'  \item \code{ageuncertlo}:
#'  \item \code{publ_}: Publication identifier.
#' } 
#' 
#' @export
#'
#' @examples
#' epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#'                                  password="epdrpw", host="diegonl.ugr.es")
#' getEvents(1, epd.connection)
#' getEvents(51, epd.connection)
#' disconnectFromEPD(connection=epd.connection)
getEvents <- function(core_number, connection){
    sqlQuery <-paste("SELECT * FROM synevent WHERE e_ =", core_number, ";", sep="")
    synevent <- dbGetQuery(connection, sqlQuery)
    
    # Check for event data and ask interactively for data use
    if(nrow(synevent) == 0){
        return(NA)
    }else{
        sqlQuery <- paste("SELECT * FROM event WHERE event_ IN (", paste(synevent$event_, collapse=","), ");", sep="")
        event <- dbGetQuery(connection, sqlQuery)
        event <- merge(synevent, event, by="event_")
        return(event)
    }
}



#' Depths of pollen samples
#'
#' Given a specific core (entity) this function return the information at which depths samples were taken for pollen data.
#'
#' @param core_number Integer or string with the core (entity) number for which C14 data want to be extracted.
#' @param connection Connection object to a EPD DDBB where the query is made.
#'
#' @return Data frame with all the information for pollen samples as in the p_sample table of the EPD DDBB.
#' 
#' @export
#'
#' @examples
#' epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#'                                  password="epdrpw", host="diegonl.ugr.es")
#' getDepths(1, epd.connection)
#' getDepths(51, epd.connection)
#' disconnectFromEPD(connection=epd.connection)
getDepths <- function(core_number, connection){
    sqlQuery <- paste("select * from p_sample where e_=", core_number, ";", sep="")
    output <- dbGetQuery(connection, sqlQuery)
    output$lab_ID <- paste("EPDr", output$e_, "_PO", output$sample_, sep="")
    return(output)    
}



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
#' c14.clam <- c142clam(c14)
c142clam <- function(C14) {
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
#' @param noC14 Data frame with no-C14 data as those in a chronology list returned by \code{\link[EPDr:getChronologies]{getChronologies}}. 
#'
#' @return Data frame with no-C14 data in CLAM format. This data frame can be easily combined with C14 data from \code{\link[EPDr:getChronologies]{c142clam}} using \code{rbind}.
#' 
#' @export
#'
#' @examples
#' library(EPDr)
#' epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#'                                user="epdr", password="epdrpw")
#' c14 <- getC14(400, epd.connection)
#' c14.clam <- c142clam(c14)
#' chron <- getChronologies(400, epd.connection)
#' noc14.clam <- agebasis2clam(chron$agebasis)
#' all.clam <- rbind(c14.clam, noc14.clam)
agebasis2clam <- function(agebasis){
    output <- data.frame(lab_ID=paste("EPDr_", agebasis$e_, "_CH", agebasis$sample_, sep=""), C14_age=agebasis$age, error=agebasis$ageup - agebasis$age, depth=agebasis$depthcm, thickness=agebasis$thickness)
    output$cal_age <- NA
    output$error[which(is.na(output$error | output$error == 0))] <- 1
    output$reservoir <- NA
    output <- output[,c("lab_ID", "C14_age", "cal_age", "error", "reservoir", "depth", "thickness")]
    return(output)
}



#' TBW
#'
#' @param chronology TBW
#' @param c14 TBW
#' @param chronology_number TBW
#' @param include_chron_not_in_c14 TBW
#' @param include_c14_not_in_chron TBW
#' @param use_c14_conf_age TBW
#' @param use_c14_conf_depth TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
compareC14Chron4CLAM <- function(chronology, c14, chronology_number=NA, include_chron_not_in_c14=NA, include_c14_not_in_chron=NA,
                            use_c14_conf_age=NA, use_c14_conf_depth=NA){
    # Define internal functions
    .printCLAM <- function(data){
        cat(c("lab_ID", "C14_age", "cal_age", "error", "reserv.", "depth", "thickn.\n"), sep="\t")
        apply(data, "\n", MARGIN=1, FUN=cat, sep="\t")
    }
    
    if(is.na(chronology_number)){
        chronology_number <- chronology$default_chronology
    }else{
        if(!chronology_number %in% 1:chronology$number_of_chronologies){stop("The chronology does not exist.")}
    }
    agebasis <- chronology$agebasis
    agebasis <- subset(agebasis, chron_ == chronology_number)
    
    chron <- agebasis2clam(agebasis)
    c14 <- c142clam(c14)
    
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
        cat("Chronology has coincident data with C14 table and, hence, the later will be used\n")
        cat("C14 data:\n")
        .printCLAM(c14[c14_in_chron,])
        cat("Chronology data:\n")
        .printCLAM(chron[chron_in_c14,])
        
    }
    if(length(chron_not_in_c14) > 0){
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
        cat("Chronology data:\n")
        .printCLAM(chron[chron_conf_depth,])
        while(is.na(use_c14_conf_depth) || !is.logical(use_c14_conf_depth)){
            use_c14_conf_depth <- as.logical(readline("Use depths from the C14 table? If not depths from the chronology will be used. (Yes: T then Intro, No: F then Intro)"))
            if(is.na(use_c14_conf_depth) || !is.logical(use_c14_conf_depth)){
                warning("Sorry! Invalid value.", call.=F, immediate.=T)
            }
        }
    }else{use_c14_conf_depth <- FALSE}
    
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
    
    output <- output[order(output$depth),]    
    
    return(output)
}


#' Reshape a chronology object for CLAM
#'
#' @param chronology Chronology object as returned by \code{\link[EPDr:getChronologies]{getChronologies}}.
#' @param C14 Data frame with C14 data as returned by \code{\link[EPDr:getC14]{getC14}}.
#' @param chronology_number Integer indicating the number of the chronology to be used. By default it uses the default chronology as in the EPD DDBB.
#'
#' @return Data frame with C14 and noC14 data ready for CLAM
#' 
#' @export
#'
#' @examples
#' library(EPDr)
#' epd.connection <- connectToEPD(host="diegonl.ugr.es", database="epd_ddbb",
#'                                user="epdr", password="epdrpw")
#' c14 <- getC14(4, epd.connection)
#' chron <- getChronologies(4, epd.connection)
#' chron.clam <- chron2clam(chron, c14)
chron2clam <- function(chronology, C14, chronology_number=NA){
    if(is.na(chronology_number)){
        chronology_number <- chronology$default_chronology
    }else{
        if(!chronology_number %in% 1:chronology$number_of_chronologies){stop("The chronology does not exist.")}
    }
    agebasis <- chronology$agebasis
    agebasis <- subset(agebasis, chron_ == chronology_number)
    output <- agebasis2clam(agebasis)
    c14.clam <- c142clam(C14)
    c14.clam$lab_ID <- sapply(c14.clam$lab_ID, as.character)
    output$lab_ID <- sapply(output$lab_ID, as.character)
    output[which(output$depth %in% c14.clam$depth),] <- c14.clam[which(c14.clam$depth %in% output$depth),]
    output$lab_ID <- as.factor(output$lab_ID)
    return(output)
}



#' Reshape event data to CLAM format
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
#' event.clam <- event2clam(event)
event2clam <- function(event){
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
#' depths2clam(depths.1)
#' disconnectFromEPD(connection=epd.connection)
depths2clam <- function(depths){
    output <- depths[order(depths$depthcm),]
    output <- output$depthcm
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
core4Clam <- function(core_number, connection, get_dephts=TRUE){
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
    clam.table <- c142clam(c14geochron)
    cat("THESE ARE THE C14 DATA FOR CORE:", core_number, "\n")
    .printC14(clam.table)
    
    # Extract existing chronologies for this particular core in the DDBB
    cat("\nCOMPARING WITH PREVIOUS CHRONOLOGIES IN THE EPD DDBB:\n")
    chrons <- getChronologies(core_number, connection)
    
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
            noC14.CLAM <- agebasis2clam(chrons$no_C14)
            clam.table <- rbind(clam.table, noC14.CLAM)
        }
    }else{
        if(include.extradata){
            if(use.all.chron){
                which.chron <- 1:chrons$number_of_chronologies
            }
            noC14.data <- subset(chrons$no_C14, chrons$no_C14$chron_ %in% which.chron)
            noC14.CLAM <- agebasis2clam(noC14.data)
            clam.table <- rbind(clam.table, noC14.CLAM)
        }
    }

    event.table <- getEvents(core_number, connection)
    
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
            event.CLAM <- event2clam(event.table)
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
        depths.CLAM <- depths2clam(depths.table)
                
        write.table(depths.CLAM, file=paste("Cores/", core_number, "/", core_number, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        write.table(depths.table, file=paste("Cores/", core_number, "/", core_number, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE, sep=",")
    }
}




#' CLAM calibration with automatic postbomb zone selection
#'
#'  Recalibrate using CLAM but automagically capturing the coordinates of the core to get the right postbomb zone
#'  to be used in the calibration. Similar to the \code{\link[EPDr:clam]{clam}} function, \code{epdrCLAM} read calibration files
#'  from the following folder structure: \code{Cores/code_number}. 
#'
#' @param core_number Character indicating the number of the site to be calibrated with CLAM
#' @param connection The connection to the EPD to get the geographical position of the core
#'
#' @return The same set of 
#' 
#' @export
#'
#' @examples
epdrClam <- function(core_number, connection){
    sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", core_number, ";", sep="")
    site_num <- as.character(dbGetQuery(connection, sqlQuery))

    sqlQuery <-paste("SELECT * FROM siteloc WHERE site_=", site_num, ";", sep="")
    siteloc <- dbGetQuery(connection, sqlQuery)

    coord <- siteloc[, c("londd", "latdd")]     
    
    pb_zone <- over(SpatialPoints(coord), postbomb.map)
    if(pb_zone$Zone == "NH1") pb <- 1
    if(pb_zone$Zone == "NH2") pb <- 2
    if(pb_zone$Zone == "NH3") pb <- 3
    if(pb_zone$Zone == "SH12") pb <- 4
    if(pb_zone$Zone == "SH3") pb <- 5

    clam(as.character(core_number), postbomb=pb)
}
