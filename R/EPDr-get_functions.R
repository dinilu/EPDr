#' Query Site information of EPD entities
#'
#' This function allows to query the database to request all information about the site in
#' which an entity was sampled. To perform the query the function requires the number
#' of the entity that want to be queried and a valid connection to the database. Hence,
#' the following parameters are mandatory:
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' be queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned
#' by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function return a data.frame with all the information on the SITELOC
#' table of the database (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
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
  
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getSite' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function return a data.frame with all the information on the ENTITY
#' table of the database (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
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
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getEntity' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function return a data.frame with the combined information from P_VARS
#' and P_GROUP tables of the database (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return Data frame with all combined information from C14 and GEOCHRON tables in the
#' EPD (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # getC14_old(1, epd.connection)
#' # getC14_old(400, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
getC14_old <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getC14_old' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#'
#' @return chronology. Object of class \code{\link[EPDr:chronology]{chronology}}. This object
#' store in an organized and systematic way all combined information from CHRON and AGEBASIS tables
#' in the EPD  (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
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
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getChronology' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame. Data frame with all combined information from the EVENT and SYNEVENT
#' tables in the database for that particular entity (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' If the entity has no events the dataframe is empty.
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
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getEvents' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame. Data frame with all information from the P_SAMPLE table in the database
#' for that particular entity (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
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
  
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getPSamples' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
  rest <- getRestriction(e_, connection)
  
  sqlQuery <- paste("SELECT * FROM p_sample WHERE e_=", e_, ";", sep="")
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection}
#' as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame. Data frame with all information from the P_ENTITY table in the
#' database for that particular entity (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' If the entity is restricted the function release a warning with the data provider name
#' to be contacted.
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
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getRestriction' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return datation. Object of class \code{\link[EPDr:datation]{datation}} with all the
#' information about datation of that entity.
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
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getDatation' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
  rest <- getRestriction(e_, connection)
  entity <- getEntity(e_, connection)
  site <- getSite(e_, connection)
  coord <- site[, c("londd", "latdd")]     
  pb_zone <- sp::over(sp::SpatialPoints(coord), postbomb.map)$Zone
  chronology <- getChronology(e_, connection)
  c14 <- getC14_old(e_, connection)
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection}
#' as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
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
  
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getCounts' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
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
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getAges' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
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
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
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
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'getAgedCounts' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  
  rest <- getRestriction(e_, connection)
  entity <- getEntity(e_, connection)
  site <- getSite(e_, connection)
  
  ages <- getAges(e_, connection)
  counts <- getCounts(e_, connection)
  
  agedcounts <- agedcounts(e_=e_, restriction=rest, entity=entity, site=site, ages=ages, counts=counts)  
  return(agedcounts)
}


#' Retrieve publications from their publ ID number
#'
#' This function is mainly intended for internal use. It retrieves information of publications by querying the database by the publication identification number.
#'
#' @param publ_ numeric with the publ_ 
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data frame with information about the publication whole reference
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # .getPubl(1, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
.getPubl <- function(publ_, connection){
  if(is.numeric(publ_)){
    publ_ <- paste(publ_, collapse="','")
    sqlQuery <-paste("SELECT * FROM publ WHERE publ_ IN ('", publ_, "');", sep="")
    sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    if(nrow(sqlOut) == 0){
      sqlOut <- data.frame(publ_=NA, acc_=NA, yearofpubl=NA, citation=NA)[-1,]
    }
  }else{
    sqlOut <- data.frame(publ_=NA, acc_=NA, yearofpubl=NA, citation=NA)[-1,]
  }
  return(sqlOut)
}



# getGeochron functions ---------------------------------------------------


#' Retrieving information for an entity in the EPD
#' 
#' Functions in this group retrieve different sort of information from an specific entity in the database. 
#' 
#'  All functions here are designed to retrieve information from a single entity. If multiple entity numbers are requested, the functions return data only for the first one. Each function retrieve data from a specific table or set of tables that are conveniently combined if necessary.
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that is queried.
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' @return Data frame with all information from specific tables in the EPD (see documentation of the EPD: \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}) for the requested entity. Columns names in the resulting data frames will vary among functions.
#' 
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' # .getC14(1, epd.connection)
#' # .getC14(400, epd.connection)
#' #
#' # getGeochron(400, epd.connection)
#' # disconnectFromEPD(connection=epd.connection)
#' 
#' 
#' @section getGeochron:
#' This function returns information in the GEOCHRON table for the specified entity. This corresponds with the common geochronological data for the entity that have been analysed for that particular entity.
#' @rdname getGeochron
#' 
#' @export
#' 
getGeochron <- function(e_, connection) {
  geochron <- .getGeochron(e_, connection)
  aar <- .getAAR(e_, connection)
  c14 <- .getC14(e_, connection)
  esr <- .getESR(e_, connection)
  ft <- .getFT(e_, connection)
  kar <- .getKAR(e_, connection)
  pb210 <- .getPB210(e_, connection)
  si32 <- .getSI32(e_, connection)
  tl <- .getTL(e_, connection)
  useries <- .getUSERIES(e_, connection)
  publ <- .getPubl(geochron$publ_, connection)
  output <- geochron(geochron=geochron, aar=aar, c14=c14, esr=esr, ft=ft, kar=kar, pb210=pb210, si32=si32, tl=tl, useries=useries, publ=publ)
  return(output)
  }


#' @section .getGeochron:
#' This function returns information in the GEOCHRON table for the specified entity. This corresponds with the common geochronological data for the entity that have been analysed for that particular entity.
#' @rdname getGeochron
#' @export
.getGeochron <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getGeochron' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <-paste("SELECT * FROM geochron WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    sqlOut <- data.frame(e_=NA, sample_=NA, method=NA, depthcm=NA, thickness=NA, materialdated=NA, publ_=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getAAR:
#' This function returns information in the AAR table for the specified entity. This corresponds with Amino Acid Racemization data for datation samples.
#' @rdname getGeochron
#' @export
.getAAR <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getAAR' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM AAR WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have AAR data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, errorlimits=NA, taxondated=NA, labnumber=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getC14:
#' This function returns information in the C14 table for the specified entity. This corresponds with C14 data for all radiocarbon samples that have been analysed for that particular entity.
#' @rdname getGeochron
#' @export
.getC14 <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getC14' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM c14 WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have C14 data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, agesdup=NA, agesdlo=NA, grthanage=NA, basis=NA, enriched=NA, labnumber=NA, deltac13=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getESR:
#' This function returns information in the ESR table for the specified entity. This corresponds with Electron Spin Resonance data for datation samples.
#' @rdname getGeochron
#' @export
.getESR <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getESR' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM ESR WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have ESR data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, errorlimits=NA, labnumber=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getFT:
#' This function returns information in the FT table for the specified entity. This corresponds with Fission Track data for datation samples.
#' @rdname getGeochron
#' @export
.getFT <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getFT' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM FT WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have FT data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, errorlimits=NA, labnumber=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getKAR:
#' This function returns information in the KAR table for the specified entity. This corresponds with Fission Track data for datation samples.
#' @rdname getGeochron
#' @export
.getKAR <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getKAR' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM KAR WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have KAR data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, errorlimits=NA, labnumber=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getPB210:
#' This function returns information in the PB210 table for the specified entity. This corresponds with \eqn{210^{Pb}}{[Pb]^210} Isotope data for datation samples.
#' @rdname getGeochron
#' @export
.getPB210 <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getPB210' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM PB210 WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have PB210 data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agead=NA, agesdup=NA, agesdlo=NA, grthanage=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getSI32:
#' This function returns information in the SI32 table for the specified entity. This corresponds with Silicon-32 data for datation samples.
#' @rdname getGeochron
#' @export
.getSI32 <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getSI32' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM SI32 WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have SI32 data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, agesdup=NA, agesdlo=NA, grthanage=NA, labnumber=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getTL:
#' This function returns information in the TL table for the specified entity. This corresponds with Thermoluminescence data for datation samples.
#' @rdname getGeochron
#' @export
.getTL <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getTL' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM TL WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have TL data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, errorlimits=NA, grainsize=NA, labnumber=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}


#' @section .getUSERIES:
#' This function returns information in the USERIES table for the specified entity. This corresponds with Uranium-series data for datation samples.
#' @rdname getGeochron
#' @export
.getUSERIES <- function(e_, connection) {
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getUSERIES' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT * FROM USERIES WHERE e_=", e_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  if(nrow(sqlOut) == 0){
    warning("This core (entity) does not have USERIES data.", call.=FALSE)
    sqlOut <- data.frame(e_=NA, sample_=NA, agebp=NA, errorlimits=NA, labnumber=NA, notes=NA)[-1,]
  }
  return(sqlOut)
}






# getSite functions -------------------------------------------------------


#' Title
#'
#' Description
#' 
#' Details
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the database that want to be queried.
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
.getSiteloc <- function(e_, connection){
  if(length(e_) > 1){
    e_ <- e_[[1]]
    warning("'.getSiteloc' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
  }
  sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", e_, ";", sep="")
  site_ <- as.character(RPostgreSQL::dbGetQuery(connection, sqlQuery))
  
  sqlQuery <-paste("SELECT * FROM siteloc sl LEFT JOIN poldiv1 p1 ON sl.poldiv1 = p1.poldiv1 LEFT JOIN poldiv2 p2 ON sl.poldiv1 = p2.poldiv1 AND sl.poldiv2 = p2.poldiv2 LEFT JOIN poldiv3 p3 ON sl.poldiv1 = p3.poldiv1 AND sl.poldiv2 = p3.poldiv2 AND sl.poldiv3 = p3.poldiv3 WHERE site_=", site_, ";", sep="")
  sqlOut <- RPostgreSQL::dbGetQuery(connection, sqlQuery)   
  
  return(sqlOut)
}


