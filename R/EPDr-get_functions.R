


#' Title TBW
#'
#' TBW
#'
#' @param core_number TBW
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' #TBW
getSite <- function(core_number, connection){
    sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", core_number, ";", sep="")
    site_num <- as.character(dbGetQuery(connection, sqlQuery))
    
    sqlQuery <-paste("SELECT * FROM siteloc WHERE site_=", site_num, ";", sep="")
    site <- dbGetQuery(connection, sqlQuery)   
    
    return(site)
}



#' Title
#'
#' @param core_number 
#' @param connection 
#'
#' @return
#' @export
#'
#' @examples
getEntity <- function(core_number, connection){
    sqlQuery <- paste("SELECT * FROM entity WHERE e_=", core_number, ";", sep="")
    results <- dbGetQuery(connection, sqlQuery)
    return(results)
}








#' Extract C14 data for a particular core (entity in the EPD DDBB)
#' 
#' Given a core number (as in the EPD DDBB: e_) the function returns a matrix with the C14 data associated to this core. This values
#' come from two different tables of the EPD: c14 and geochron.
#'
#' @param core_number Integer with the core (entity) number for which C14 data want to be extracted.
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
#' 
#' @export
#'
#' @examples
#' #epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' #getC14(1, epd.connection)
#' #getC14(400, epd.connection)
#' #disconnectFromEPD(connection=epd.connection)
getC14 <- function(core_number, connection) {
    rest <- getRestriction(core_number, connection)
    
    sqlQuery <- paste("SELECT * FROM c14 WHERE e_=", core_number, ";", sep="")
    c14 <- dbGetQuery(connection, sqlQuery)
    
    sqlQuery <-paste("SELECT * FROM geochron WHERE e_=", core_number, ";", sep="")
    geochron <- dbGetQuery(connection, sqlQuery)
    
    if(nrow(c14) == 0){
        warning("This core (entity) does not have C14 data.", call.=FALSE)
        c14 <- data.frame(e_=NA,sample_=NA,agebp=NA,agesdup=NA,agesdlo=NA,grthanage=NA,basis=NA,enriched=NA,labnumber=NA,
            deltac13=NA,notes=NA)[-1,] 
        geochron <- data.frame(e_=NA,sample_=NA,method=NA,depthcm=NA,thickness=NA,materialdated=NA,publ_=NA)[-1,]
    }
    
    c14geochron <- merge(c14, geochron, by=c("e_","sample_"))
    
    return(c14geochron)
}



#' Extract chronologies associated with a core (entity) in the pollen database
#' 
#' Given a core (entity) number, \code{\link[EPDr:getChronology]{getChronology}} extract all the information about chronologies associated with this core
#' in the EPD DDBB. This information comes from two different tables in the DDBB: chron and agebasis.
#'
#' @param core_number Integer with the core (entity) number for which C14 data want to be extracted.
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
#' #epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' #getChronology(1, epd.connection)
#' #getChronology(400, epd.connection)
#' #disconnectFromEPD(connection=epd.connection)
getChronology <- function(core_number, connection) {
    rest <- getRestriction(core_number, connection)
    
    site <- getSite(core_number, connection)
    
    sqlQuery <-paste("SELECT * FROM chron WHERE e_=", core_number, ";", sep="")
    chron <- dbGetQuery(connection, sqlQuery)
    
    sqlQuery <- paste("SELECT * FROM agebasis WHERE e_=", core_number, ";", sep="")
    agebasis <- dbGetQuery(connection, sqlQuery)
    
    number_of_chronologies <- nrow(chron)

    default_chronology <- chron$chron_[which(chron$defaultchron == "Y")]
    if(is.null(default_chronology)){default_chronology <- 0}
    if(length(default_chronology) == 0){default_chronology <- 0}
    
    if(number_of_chronologies == 0){
        warning("This core (entity) does not have chronologies.", call.=F)
        output <- chronology()
    }else{
        output <- chronology(core_number=core_number, restriction=rest, site=site, number_of_chronologies=number_of_chronologies, default_chronology=default_chronology, chron=chron, agebasis=agebasis)
    }
    
    return(output)
}




#' Extract events associated with a specific core (entity) in the EPD DDBB
#'
#' Given a specific core number and connection to the EPD, this function return the information of events associated with the core.
#'
#' @param core_number Integer with the core (entity) number for which C14 data want to be extracted.
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
#' #epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#' #                                password="epdrpw", host="localhost")
#' #getEvents(1, epd.connection)
#' #getEvents(51, epd.connection)
#' #disconnectFromEPD(connection=epd.connection)
getEvents <- function(core_number, connection){
    rest <- getRestriction(core_number, connection)

    sqlQuery <-paste("SELECT * FROM synevent WHERE e_ =", core_number, ";", sep="")
    synevent <- dbGetQuery(connection, sqlQuery)
    
    # Check for event data and ask interactively for data use
    if(nrow(synevent) == 0){
        event <- data.frame(event_=NA, e_=NA, depthcm=NA, thickness=NA, event=NA, name=NA, agebp=NA, ageuncertup=NA, ageuncertlo=NA,
                            publ_=NA)[-1,]
        return(event)
    }else{
        sqlQuery <- paste("SELECT * FROM event WHERE event_ IN (", paste(synevent$event_, collapse=","), ");", sep="")
        event <- dbGetQuery(connection, sqlQuery)
        event <- merge(synevent, event, by="event_")
        event <- subset(event, agebp != 0)
        return(event)
    }
}



#' Depths of pollen samples
#'
#' Given a specific core (entity) this function return the information at which depths samples were taken for pollen data.
#'
#' @param core_number Integer with the core (entity) number for which C14 data want to be extracted.
#' @param connection Connection object to a EPD DDBB where the query is made.
#'
#' @return Data frame with all the information for pollen samples as in the p_sample table of the EPD DDBB.
#' 
#' @export
#'
#' @examples
#' #epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' #getDepths(1, epd.connection)
#' #getDepths(51, epd.connection)
#' #disconnectFromEPD(connection=epd.connection)
getDepths <- function(core_number, connection){
    rest <- getRestriction(core_number, connection)

    sqlQuery <- paste("select * from p_sample where e_=", core_number, ";", sep="")
    output <- dbGetQuery(connection, sqlQuery)
    output$lab_ID <- paste("EPDr", output$e_, "_PO", output$sample_, sep="")
    
    return(output)    
}

#' Title TBW
#'
#' TBW
#' 
#' @param core_number TBW
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
getRestriction <- function(core_number, connection){
    sqlQuery <- paste("select * from p_entity where e_=", core_number, ";", sep="")
    output <- dbGetQuery(connection, sqlQuery)
    if(output$usestatus == "R"){
        warning(paste("Data for this core has restriction in their use. Please contact the data owner (", output$datasource,
                      ") before publishing this data", sep=""), call.=F)
    }
    return(output)
}

#' Title TBW
#'
#' TBW
#'
#' @param core_number TBW
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
getDatation <- function(core_number, connection){
    rest <- getRestriction(core_number, connection)
    site <- getSite(core_number, connection)
    coord <- site[, c("londd", "latdd")]     
    pb_zone <- over(SpatialPoints(coord), postbomb.map)$Zone
    chronology <- getChronology(core_number, connection)
    c14 <- getC14(core_number, connection)
    events <- getEvents(core_number, connection)
    depths <- getDepths(core_number, connection)
    output <- datation(core_number=core_number, restriction=rest, site=site, postbomb_zone=pb_zone, chronology=chronology, c14=c14, events=events, depths=depths)
    return(output)
}
    


#' Title TBW
#'
#' @param core_number TBW
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
getCounts <- function(core_number, connection){
    rest <- getRestriction(core_number, connection)
    
    site <- getSite(core_number, connection)
    
    sqlQuery <- paste("SELECT sample_, count, varname FROM p_counts NATURAL JOIN p_vars WHERE e_=", core_number, ";", sep="")
    counts.raw <- dbGetQuery(connection, sqlQuery)
    if(is.data.frame(counts.raw) && nrow(counts.raw) == 0){
        warning("This core does not have count data.", call.=FALSE)
        counts.cast <- data.frame(0)[,-1]
        taxa.names <- character(0)
        sample_ <- numeric(0)
        taxa.groupid <- character(0)
        taxa.accepted <- numeric(0)
        taxa.mhvar <- numeric(0)
    }else{
        counts.cast <- dcast(counts.raw, sample_ ~ varname, value.var='count')
        counts.cast[is.na(counts.cast)] <- 0
        sample_ <- counts.cast[,1]
        counts.cast <- counts.cast[,-1]
        
        taxa.names <- colnames(counts.cast)
        
        sqlQuery <- paste("SELECT varname, groupid, accvar_, mhvar_ FROM p_vars NATURAL JOIN p_group WHERE varname IN ('", paste(taxa.names, collapse="','"), "');", sep="")
        groupid <- dbGetQuery(connection, sqlQuery)
        groupid <- groupid[match(taxa.names, groupid$varname),]
        
        taxa.groupid <- groupid$groupid
        taxa.accepted <- groupid$accvar_
        taxa.mhvar <- groupid$mhvar_
    }

    
    data_type <- factor("Counts", levels=c("Counts", "Percentages"))
    data_processing <- factor("Samples", levels=c("Samples", "Interpolated", "Ranged means"))
    sample_label <- as.character(sample_)
    
    counts <- counts(core_number=core_number, restriction=rest, site=site, data_type=data_type, data_processing=data_processing, taxa_names=taxa.names, taxa_groupid=taxa.groupid, taxa_accepted=taxa.accepted, taxa_mhvar=taxa.mhvar, sample_=sample_, sample_label=sample_label, counts=counts.cast)

    return(counts)
}

#' Title TBW
#'
#' @param core_number  TBW
#' @param connection  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
getAges <- function(core_number, connection){
    rest <- getRestriction(core_number, connection)
    
    site <- getSite(core_number, connection)
    
    sqlQuery<- paste("SELECT sample_, chron_, agebp FROM p_agedpt WHERE e_=", core_number, ";", sep="")
    ages <- dbGetQuery(connection, sqlQuery)
    
    sqlQuery <-paste("SELECT * FROM chron WHERE e_=", core_number, ";", sep="")
    chron <- dbGetQuery(connection, sqlQuery)

    depths <- getDepths(core_number, connection)
    
    default_chronology <- chron$chron_[which(chron$defaultchron == "Y")]
    if(is.null(default_chronology)){default_chronology <- 0}
    
    sample_ <- depths$sample_
    depthcm <- depths$depthcm
    sample_label <- as.character(sample_)
    
    if(is.data.frame(ages) && nrow(ages) == 0){
        warning("This core  does not have age data.", call.=FALSE)
        ages.cast <- data.frame(a=NA)
        colnames(ages.cast) <- c("1")
    }else{
        ages.cast <- dcast(ages, sample_ ~ chron_, value.var='agebp')
        ages.cast <- ages.cast[match(sample_, ages.cast$sample_),]
        ages.cast <- ages.cast[,-1]
    }
    
    if(class(ages.cast) == "numeric"){
        ages.cast <- data.frame(ages.cast)
        colnames(ages.cast) <- 1
    }
    
    if(core_number %in% giesecke.EpdAgeCut$ID){
        is.in.giesecke <- TRUE
        ages.giesecke <- giesecke.EpdAgeCut[which(giesecke.EpdAgeCut$ID == core_number), c("ID", "Event", "Depth..m.", "Age.dated..ka.", "Age.min..ka.", "Age.max..ka.")]
        ages.giesecke$depthcm <- ages.giesecke$Depth..m. * 100
        ages.giesecke <- ages.giesecke[match(round(depths[,"depthcm"], 1), round(ages.giesecke$depthcm, 1)), ]
        ages.giesecke$agesbp <- ages.giesecke$Age.dated..ka. * 1000

        column.names <- colnames(ages.cast)
        ages.cast <- cbind(ages.cast, ages.giesecke$agesbp)
        colnames(ages.cast) <- c(column.names, "giesecke")
    }else{
        is.in.giesecke <- FALSE
    }

    ages.final <- ages(core_number=core_number, restriction=rest, site=site, default_chronology=default_chronology, giesecke=is.in.giesecke, sample_=sample_, sample_label=sample_label, depthcm=depthcm, depths=depths, depth_ages=ages.cast)
    return(ages.final)
}


#' Title TBW
#'
#' @param core_number TBW
#' @param connection TBW 
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
getAgedCounts <- function(core_number, connection){
    rest <- getRestriction(core_number, connection)
    site <- getSite(core_number, connection)
    
    ages <- getAges(core_number, connection)
    counts <- getCounts(core_number, connection)

    agedcounts <- agedcounts(core_number=core_number, restriction=rest, site=site, ages=ages, counts=counts)  
    return(agedcounts)
}





