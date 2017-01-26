#' Query Site information of EPD entities
#'
#' This function allows to query the database to request all information about the site in
#' which an entity was sampled. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function return a data.frame with all the information on the SITELOC table of the database (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # site.400 <- getSite(400, epd.connection)
#' # site.400
#' # disconnectFromEPD(epd.connection)
#' 
getSite <- function(e_, connection){
    sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", e_, ";", sep="")
    site_num <- as.character(RPostgreSQL::dbGetQuery(connection, sqlQuery))
    
    sqlQuery <-paste("SELECT * FROM siteloc WHERE site_=", site_num, ";", sep="")
    site <- RPostgreSQL::dbGetQuery(connection, sqlQuery)   
    
    return(site)
}



#' Query details of EPD entities
#'
#' This function queries the database to return details of the specified entity. It requires 
#' the number of the entity that want to be queried and a valid connection to the database 
#' server, as returned by \code{\link[EPDr:connectToEPD]{connectToEPD}}. Hence, the 
#' following parameters are mandatory. 
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function return a data.frame with all the information on the ENTITY table of the database (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd_ddbb",
#' #                               user="epdr", password="epdrpw")
#' # e.400 <- getEntity(400, epd.connection)
#' # e.400
#' # disconnectFromEPD(epd.connection)
#' 
getEntity <- function(e_, connection){
    sqlQuery <- paste("SELECT * FROM entity WHERE e_=", e_, ";", sep="")
    results <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(results)
}




#' Query the taxonomy table of the EPD
#' 
#' The function queries the whole taxonomy of the database by combining 
#' information from the P_VARS and P_GROUP tables. Because it queries all 
#' the records in the database it only requires a valid connection to the 
#' database server as parameter.
#' 
#' Some users may find this function useful but it has been mainly 
#' implemented to be used by other functions in the \code{EPDr} package.
#'
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function return a data.frame with the combined information from P_VARS and P_GROUP tables of the database (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd_ddbb",
#' #                               user="epdr", password="epdrpw")
#' # epd.taxonomy <- getTaxonomy(epd.connection)
#' # epd.taxonomy
#' # disconnectFromEPD(epd.connection)
#' 
getTaxonomyEPD <- function(connection){
    sqlQuery <- paste("SELECT * FROM p_vars NATURAL JOIN p_group")
    results <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(results)
}



#' Query C14 data of EPD Entities
#' 
#' This function queries the database to request all information about the C14 data 
#' of an specified entity. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#' 
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#'
#' @return Data frame with all combined information from C14 and GEOCHRON tables in the EPD  (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getC14(1, epd.connection)
#' # getC14(400, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getC14 <- function(e_, connection) {
    rest <- getRestriction(e_, connection)
    
    sqlQuery <- paste("SELECT * FROM c14 WHERE e_=", e_, ";", sep="")
    c14 <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    sqlQuery <-paste("SELECT * FROM geochron WHERE e_=", e_, ";", sep="")
    geochron <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    if(nrow(c14) == 0){
        warning("This core (entity) does not have C14 data.", call.=FALSE)
        c14 <- data.frame(e_=NA,sample_=NA,agebp=NA,agesdup=NA,agesdlo=NA,grthanage=NA,basis=NA,enriched=NA,labnumber=NA,
                          deltac13=NA,notes=NA)[-1,] 
        geochron <- data.frame(e_=NA,sample_=NA,method=NA,depthcm=NA,thickness=NA,materialdated=NA,publ_=NA)[-1,]
    }
    
    c14geochron <- merge(c14, geochron, by=c("e_","sample_"))
    
    return(c14geochron)
}



#' Query chronologies of EPD entities
#' 
#' This function queries the database to request all information about the chronologies  
#' of an specified entity. A particular entity might have several chronologies developed in different
#' projects by differen researchers. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#'
#' @return chronology. Object of class \code{\link[EPDr:chronology]{chronology}}. This object store in an organized and systematic way all combined information from CHRON and AGEBASIS tables in the EPD  (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getChronology(1, epd.connection)
#' # getChronology(400, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getChronology <- function(e_, connection) {
    rest <- getRestriction(e_, connection)
    
    site <- getSite(e_, connection)
    entity <- getEntity(e_, connection)
    
    sqlQuery <-paste("SELECT * FROM chron WHERE e_=", e_, ";", sep="")
    chron <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    sqlQuery <- paste("SELECT * FROM agebasis WHERE e_=", e_, ";", sep="")
    agebasis <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    number_of_chronologies <- nrow(chron)
    
    default_chronology <- chron$chron_[which(chron$defaultchron == "Y")]
    if(is.null(default_chronology)){default_chronology <- 0}
    if(length(default_chronology) == 0){default_chronology <- 0}
    
    if(number_of_chronologies == 0){
        warning("This core (entity) does not have chronologies.", call.=F)
        output <- chronology()
    }else{
        output <- chronology(e_=e_, restriction=rest, entity=entity, site=site, number_of_chronologies=number_of_chronologies, default_chronology=default_chronology, chron=chron, agebasis=agebasis)
    }
    
    return(output)
}




#' Query events of EPD entities
#' 
#' This function queries the database to request all information about the stratigraphic events 
#' of an specified entity. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame. Data frame with all combined information from the EVENT and SYNEVENT tables in the database for that particular entity (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}). If the entity has no events the dataframe is empty.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                password="epdrpw", host="localhost")
#' # getEvents(1, epd.connection)
#' # getEvents(51, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getEvents <- function(e_, connection){
    rest <- getRestriction(e_, connection)
    
    sqlQuery <-paste("SELECT * FROM synevent WHERE e_ =", e_, ";", sep="")
    synevent <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    # Check for event data and ask interactively for data use
    if(nrow(synevent) == 0){
        event <- data.frame(event_=NA, e_=NA, depthcm=NA, thickness=NA, event=NA, name=NA, agebp=NA, ageuncertup=NA, ageuncertlo=NA,
                            publ_=NA)[-1,]
        return(event)
    }else{
        sqlQuery <- paste("SELECT * FROM event WHERE event_ IN (", paste(synevent$event_, collapse=","), ");", sep="")
        event <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
        event <- merge(synevent, event, by="event_")
        event <- event[event$agebp != 0,]
        return(event)
    }
}


#' Query palynological samples of EPD entities
#'
#' This function queries the database to request all information about palynological samples 
#' of an specified entity. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame. Data frame with all information from the P_SAMPLE table in the database for that particular entity (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getPSamples(1, epd.connection)
#' # getPSamples(51, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getPSamples <- function(e_, connection){
    rest <- getRestriction(e_, connection)
    
    sqlQuery <- paste("select * from p_sample where e_=", e_, ";", sep="")
    output <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    output$lab_ID <- paste("EPDr", output$e_, "_PO", output$sample_, sep="")
    
    return(output)    
}


#' Query restriction of use for EPD entities
#'
#' This function queries the database to request information about use restrictions 
#' of an specified entity. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame. Data frame with all information from the P_ENTITY table in the database for that particular entity (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}). If the entity is restricted the function release a warning with the data provider name to be contacted.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getRestriction(1, epd.connection)
#' # getRestriction(51, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getRestriction <- function(e_, connection){
    sqlQuery <- paste("SELECT * FROM p_entity WHERE e_=", e_, ";", sep="")
    output <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    if(output$usestatus == "R"){
        warning(paste("Data for this core has restriction in their use. Please contact the data owner (", output$datasource,
                      ") before publishing this data", sep=""), call.=F)
    }
    return(output)
}


#' Query datation of EPD entities
#'
#' This function queries the database to request information about datation 
#' of an specified entity. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return datation. Object of class \code{\link[EPDr:datation]{datation}} with all the information about datation of that entity.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getDatation(1, epd.connection)
#' # getDatation(51, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getDatation <- function(e_, connection){
    rest <- getRestriction(e_, connection)
    entity <- getEntity(e_, connection)
    site <- getSite(e_, connection)
    coord <- site[, c("londd", "latdd")]     
    pb_zone <- sp::over(sp::SpatialPoints(coord), postbomb.map)$Zone
    chronology <- getChronology(e_, connection)
    c14 <- getC14(e_, connection)
    events <- getEvents(e_, connection)
    depths <- getPSamples(e_, connection)
    output <- datation(e_=e_, restriction=rest, entity=entity, site=site, postbomb_zone=pb_zone, chronology=chronology, c14=c14, events=events, depths=depths)
    return(output)
}



#' Query palynological counts of EPD entities
#'
#' This function queries the database to request information about palynological counts 
#' of an specified entity. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return counts. Object of class \code{\link[EPDr:counts]{counts}}.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getCounts(1, epd.connection)
#' # getCounts(51, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getCounts <- function(e_, connection){
    rest <- getRestriction(e_, connection)
    
    entity <- getEntity(e_, connection)
    site <- getSite(e_, connection)
    
    sqlQuery <- paste("SELECT sample_, count, varname FROM p_counts NATURAL JOIN p_vars WHERE e_ =", e_, ";", sep="")
    counts.raw <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    depth <- getPSamples(e_, connection)
    depthcm <- depth$depthcm
    
    if(is.data.frame(counts.raw) && nrow(counts.raw) == 0){
        warning("This core does not have count data.", call.=FALSE)
        counts.cast <- data.frame(0)[,-1]
        taxa.names <- character(0)
        sample_ <- numeric(0)
        taxa.groupid <- character(0)
        taxa.id <- numeric(0)
        taxa.accepted <- numeric(0)
        taxa.mhvar <- numeric(0)
    }else{
        counts.cast <- reshape2::dcast(counts.raw, sample_ ~ varname, value.var='count')
        counts.cast[is.na(counts.cast)] <- 0
        sample_ <- counts.cast[,1]
        counts.cast <- counts.cast[,-1]
        
        taxa.names <- colnames(counts.cast)
        
        sqlQuery <- paste("SELECT var_, varname, groupid, accvar_, mhvar_ FROM p_vars NATURAL JOIN p_group WHERE varname IN ('", paste(taxa.names, collapse="','"), "');", sep="")
        groupid <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
        groupid <- groupid[match(taxa.names, groupid$varname),]
        
        taxa.groupid <- groupid$groupid
        taxa.id <- groupid$var_
        taxa.accepted <- groupid$accvar_
        taxa.mhvar <- groupid$mhvar_
    }
    
    counts_type <- factor("Counts", levels=c("Counts", "Percentages"))
    counts_processing <- factor("Samples", levels=c("Samples", "Interpolated", "Ranged means"))
    taxa_type <- factor("Samples", levels=c("Samples", "Accepted", "Unified"))
    taxa_processing <- factor("Original", levels=c("Original", "Expanded", "Taxize"))
    
    sample_label <- as.character(sample_)
    
    ages <- getAges(e_, connection)
    
    default_chronology <- ages@default_chronology
    if(default_chronology != 0){
        if(ages@giesecke == T){
            default_chronology <- "giesecke"
        }
        default_ages <- ages@depth_ages[,as.character(default_chronology)]
    }else{
        default_ages <- numeric(0)
    }
    
    counts <- counts(e_=e_, restriction=rest, entity=entity, site=site, counts_type=counts_type, counts_processing=counts_processing, taxa_type=taxa_type, taxa_processing=taxa_processing, taxa_names=taxa.names, taxa_=taxa.id, taxa_groupid=taxa.groupid, taxa_accepted=taxa.accepted, taxa_mhvar=taxa.mhvar, sample_=sample_, sample_label=sample_label, default_ages=default_ages, depthcm=depthcm, counts=counts.cast)
    
    return(counts)
}


#' Query palynological-samples ages of EPD entities
#'
#' This function queries the database to request information about estimated ages of palynological
#' samples of an specified entity. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return ages Object of class \code{\link[EPDr:ages]{ages}}.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getAges(1, epd.connection)
#' # getAges(51, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getAges <- function(e_, connection){
    rest <- getRestriction(e_, connection)
    
    entity <- getEntity(e_, connection)
    site <- getSite(e_, connection)
    
    sqlQuery<- paste("SELECT sample_, chron_, agebp FROM p_agedpt WHERE e_=", e_, ";", sep="")
    ages <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    sqlQuery <-paste("SELECT * FROM chron WHERE e_=", e_, ";", sep="")
    chron <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    
    depths <- getPSamples(e_, connection)
    
    default_chronology <- chron$chron_[which(chron$defaultchron == "Y")]
    if(is.null(default_chronology) | length(default_chronology) == 0){default_chronology <- 0}
    
    sample_ <- depths$sample_
    depthcm <- depths$depthcm
    sample_label <- as.character(sample_)
    
    if(is.data.frame(ages) && nrow(ages) == 0){
        warning("This core does not have age data.", call.=FALSE)
        ages.cast <- data.frame()
    }else{
        ages.cast <- reshape2::dcast(ages, sample_ ~ chron_, value.var='agebp')
        ages.cast <- ages.cast[match(sample_, ages.cast$sample_),]
        ages.cast <- ages.cast[,-1]
    }
    
    if(class(ages.cast) == "numeric"){
        ages.cast <- data.frame(ages.cast)
        colnames(ages.cast) <- 1
    }
    
    if(e_ %in% giesecke.EpdAgeCut$ID){
        is.in.giesecke <- TRUE
        ages.giesecke <- giesecke.EpdAgeCut[which(giesecke.EpdAgeCut$ID == e_), c("ID", "Event", "Depth..m.", "Age.dated..ka.", "Age.min..ka.", "Age.max..ka.")]
        ages.giesecke$depthcm <- ages.giesecke$Depth..m. * 100
        ages.giesecke <- ages.giesecke[match(round(depths[,"depthcm"], 1), round(ages.giesecke$depthcm, 1)), ]
        ages.giesecke$agesbp <- ages.giesecke$Age.dated..ka. * 1000
        
        if(nrow(ages.cast) == 0){
            ages.cast <- data.frame(giesecke=ages.giesecke$agesbp)
        }else{
            column.names <- colnames(ages.cast)
            ages.cast <- cbind(ages.cast, ages.giesecke$agesbp)
            colnames(ages.cast) <- c(column.names, "giesecke")
        }
    }else{
        is.in.giesecke <- FALSE
    }
    
    ages.final <- ages(e_=e_, restriction=rest, entity=entity, site=site, default_chronology=default_chronology, giesecke=is.in.giesecke, sample_=sample_, sample_label=sample_label, depthcm=depthcm, depths=depths, depth_ages=ages.cast)
    return(ages.final)
}


#' Query counts and ages of palynological-samples of EPD entities
#'
#' This function queries the database to request information about counts and estimated ages
#' of palynological samples of an specified entity. To perform the query the function requires
#' the number of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return agedcounts Object of class \code{\link[EPDr:agedcounts]{agedcounts}}.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getAgedCounts(1, epd.connection)
#' # getAgedCounts(51, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getAgedCounts <- function(e_, connection){
    rest <- getRestriction(e_, connection)
    entity <- getEntity(e_, connection)
    site <- getSite(e_, connection)
    
    ages <- getAges(e_, connection)
    counts <- getCounts(e_, connection)
    
    agedcounts <- agedcounts(e_=e_, restriction=rest, entity=entity, site=site, ages=ages, counts=counts)  
    return(agedcounts)
}

