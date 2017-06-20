#' #' Query counts and ages of palynological-samples of EPD entities
#' #'
#' #' This function queries the database to request information about counts and estimated ages
#' #' of palynological samples of an specified entity. To perform the query the function requires
#' #' the number of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return agedcounts Object of class \code{\link[EPDr:agedcounts]{agedcounts}}.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getAgedCounts(1, epd.connection)
#' #' # getAgedCounts(51, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getAgedCounts <- function(e_, connection){
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getAgedCounts' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   rest <- getRestriction(e_, connection)
#'   entity <- getEntity(e_, connection)
#'   site <- getSite(e_, connection)
#'   
#'   ages <- getAges(e_, connection)
#'   counts <- getCounts(e_, connection)
#'   
#'   agedcounts <- agedcounts(e_=e_, restriction=rest, entity=entity, site=site, ages=ages, counts=counts)  
#'   return(agedcounts)
#' }
#' 
#' 
#' 
#' #' Query Site information of EPD entities
#' #'
#' #' This function allows to query the database to request all information about the site in
#' #' which an entity was sampled. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as returned
#' #' by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return data.frame The function return a data.frame with all the information on the SITELOC
#' #' table of the database (see documentation of the EPD:
#' #' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # library(EPDr)
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' #                               user="epdr", password="epdrpw")
#' #' # site.400 <- getSite(400, epd.connection)
#' #' # site.400
#' #' # disconnectFromEPD(epd.connection)
#' #' 
#' getSite_old <- function(e_, connection){
#'   
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getSite' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", e_, ";", sep="")
#'   site_num <- as.character(RPostgreSQL::dbGetQuery(connection, sqlQuery))
#'   
#'   sqlQuery <-paste("SELECT * FROM siteloc WHERE site_=", site_num, ";", sep="")
#'   site <- RPostgreSQL::dbGetQuery(connection, sqlQuery)   
#'   
#'   return(site)
#' }
#' 
#' 
#' 
#' 
#' #' Query details of EPD entities
#' #'
#' #' This function queries the database to return details of the specified entity. It requires 
#' #' the number of the entity that want to be queried and a valid connection to the database 
#' #' server, as returned by \code{\link[EPDr:connectToEPD]{connectToEPD}}. Hence, the 
#' #' following parameters are mandatory. 
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return data.frame The function return a data.frame with all the information on the ENTITY
#' #' table of the database (see documentation of the EPD:
#' #' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # library(EPDr)
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd_ddbb",
#' #' #                               user="epdr", password="epdrpw")
#' #' # e.400 <- getEntity_old(400, epd.connection)
#' #' # e.400
#' #' # disconnectFromEPD(epd.connection)
#' #' 
#' getEntity_old <- function(e_, connection){
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getEntity' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   sqlQuery <- paste("SELECT * FROM entity WHERE e_=", e_, ";", sep="")
#'   results <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   return(results)
#' }
#' 
#' 
#' #' Query events of EPD entities
#' #' 
#' #' This function queries the database to request all information about the stratigraphic events 
#' #' of an specified entity. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return data.frame. Data frame with all combined information from the EVENT and SYNEVENT
#' #' tables in the database for that particular entity (see documentation of the EPD:
#' #' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' #' If the entity has no events the dataframe is empty.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                password="epdrpw", host="localhost")
#' #' # getEvents(1, epd.connection)
#' #' # getEvents(51, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getEvents_old <- function(e_, connection){
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getEvents' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   rest <- getRestriction_old(e_, connection)
#'   
#'   sqlQuery <-paste("SELECT * FROM synevent WHERE e_ =", e_, ";", sep="")
#'   synevent <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   # Check for event data and ask interactively for data use
#'   if(nrow(synevent) == 0){
#'     event <- data.frame(event_=NA, e_=NA, depthcm=NA, thickness=NA, event=NA, name=NA, agebp=NA, ageuncertup=NA, ageuncertlo=NA,
#'                         publ_=NA)[-1,]
#'     return(event)
#'   }else{
#'     sqlQuery <- paste("SELECT * FROM event WHERE event_ IN (", paste(synevent$event_, collapse=","), ");", sep="")
#'     event <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'     event <- merge(synevent, event, by="event_")
#'     event <- event[event$agebp != 0,]
#'     return(event)
#'   }
#' }
#' 
#' 
#' #' Query palynological samples of EPD entities
#' #'
#' #' This function queries the database to request all information about palynological samples 
#' #' of an specified entity. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return data.frame. Data frame with all information from the P_SAMPLE table in the database
#' #' for that particular entity (see documentation of the EPD:
#' #' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getPSamples(1, epd.connection)
#' #' # getPSamples(51, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getPSamples_old <- function(e_, connection){
#'   
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getPSamples' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   rest <- getRestriction_old(e_, connection)
#'   
#'   sqlQuery <- paste("SELECT * FROM p_sample WHERE e_=", e_, ";", sep="")
#'   output <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   output$lab_ID <- paste("EPDr", output$e_, "_PO", output$sample_, sep="")
#'   
#'   return(output)    
#' }
#' 
#' 
#' #' Query restriction of use for EPD entities
#' #'
#' #' This function queries the database to request information about use restrictions 
#' #' of an specified entity. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection}
#' #' as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return data.frame. Data frame with all information from the P_ENTITY table in the
#' #' database for that particular entity (see documentation of the EPD:
#' #' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' #' If the entity is restricted the function release a warning with the data provider name
#' #' to be contacted.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getRestriction(1, epd.connection)
#' #' # getRestriction(51, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getRestriction_old <- function(e_, connection){
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getRestriction' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   sqlQuery <- paste("SELECT * FROM p_entity WHERE e_=", e_, ";", sep="")
#'   output <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   if(output$usestatus == "R"){
#'     warning(paste("Data for this core has restriction in their use. Please contact the data owner (", output$datasource,
#'                   ") before publishing this data", sep=""), call.=F)
#'   }
#'   return(output)
#' }
#' 
#' 
#' #' Class for Chronologies of an entity
#' #' 
#' #' Class "chronology" store in an organized and systematic way information about 
#' #' the chronologies for an specified entity in the European Pollen Database (EPD). This
#' #' object is created by \code{\link[EPDr:getChronology]{getChronology}}.
#' #'
#' #' It has different elements all of which correspond to a unique entity in the database.
#' #'
#' #' @slot e_ numeric. The entity number (e_) as in the EPD.
#' #' @slot restriction data.frame. Restriction of use information for that particular entity
#' #' in the database. It is important to know if we can freely use the data or should ask
#' #' for authorization
#' #' @slot entity data.frame. Details of the entity.
#' #' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' #' several entities can belong to the same site.
#' #' @slot number_of_chronologies numeric. The number of chronologies that are stored in
#' #' the database for this particular entity.
#' #' @slot default_chronology numeric. Which of the available chronologies is tagged as 
#' #' the default in the database.
#' #' @slot chron data.frame. Details about the chronologies stored in the database, like 
#' #' the author, full name, etc.
#' #' @slot agebasis data.frame. Agebasis used in the chronologies to calibrate radiocarbon 
#' #' dates.
#' #'
#' #' @export
#' #' @import methods
#' #' 
#' chronology <- setClass("chronology",
#'                        slots=c(
#'                          e_="numeric",
#'                          restriction="data.frame",
#'                          entity="data.frame",
#'                          site="data.frame",
#'                          number_of_chronologies="numeric",
#'                          default_chronology="numeric",
#'                          chron="data.frame",
#'                          agebasis="data.frame"
#'                        ),
#'                        prototype=list(
#'                          e_=numeric(0),
#'                          restriction=data.frame(
#'                            e_=NA,
#'                            contact_=NA,
#'                            datasource=NA,
#'                            dataform=NA,
#'                            usestatus=NA,
#'                            datacoop=NA)[-1,],
#'                          entity=data.frame(
#'                            e_=NA,
#'                            site_=NA,
#'                            sigle=NA,
#'                            name=NA,
#'                            iscore=NA,
#'                            issect=NA,
#'                            isssamp=NA,
#'                            descriptor=NA,
#'                            hasanlam=NA,
#'                            entloc=NA,
#'                            localveg=NA,
#'                            coll_=NA,
#'                            sampdate=NA,
#'                            depthatloc=NA,
#'                            icethickcm=NA,
#'                            sampdevice=NA,
#'                            corediamcm=NA,
#'                            c14depthadj=NA,
#'                            notes=NA)[-1,],
#'                          site=data.frame(
#'                            site_=NA,
#'                            sitename=NA,
#'                            sitecode=NA,
#'                            siteexists=NA,
#'                            poldiv1=NA,
#'                            poldiv2=NA,
#'                            poldiv3=NA,
#'                            latdeg=NA,
#'                            latmin=NA,
#'                            latsec=NA,
#'                            latns=NA,
#'                            latdd=NA,
#'                            latdms=NA,
#'                            londeg=NA,
#'                            lonmin=NA,
#'                            lonsec=NA,
#'                            lonew=NA,
#'                            londd=NA,
#'                            londms=NA,
#'                            elevation=NA,
#'                            areaofsite=NA)[-1,],
#'                          number_of_chronologies=numeric(0),
#'                          default_chronology=numeric(0),
#'                          chron=data.frame(
#'                            e_=NA,
#'                            chron_=NA,
#'                            defaultchron=NA,
#'                            name=NA,
#'                            preparedby=NA,
#'                            dataprepared=NA,
#'                            model=NA,
#'                            notes=NA)[-1,],
#'                          agebasis=data.frame(
#'                            e_=NA,
#'                            chron_=NA,
#'                            sample_=NA,
#'                            depthcm=NA,
#'                            thickness=NA,
#'                            age=NA,
#'                            ageup=NA,
#'                            agelo=NA,
#'                            rcode=NA,
#'                            nrow = 0)
#'                        )
#' )
#' 
#' 
#' #' Class for Datation of an entity
#' #' 
#' #' Class "datation" store all information about datation of an specified entity in the 
#' #' European Pollen Database (EPD). These objects are created by \code{\link[EPDr:getDatation]{getDatation}}.
#' #' 
#' #' Now, it include only C14 data and events, it should include in the future other
#' #' sort of data that are included in the EPD.
#' #'
#' #' It has different elements all of which correspond to a unique entity in the database.
#' #'
#' #' @slot e_ numeric. The entity number (e_) as in the EPD.
#' #' @slot restriction data.frame. Restriction of use information for that particular entity
#' #' in the database. It is important to know if we can freely use the data or should ask
#' #' for authorization
#' #' @slot entity data.frame. Details of the entity.
#' #' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' #' several entities can belong to the same site.
#' #' @slot postbomb_zone factor. Indicate the postbomb zone in which the entity (site)
#' #' was sampled. This information is useful when calibrating radiocarbon dates with
#' #' \code{clam} or \code{bacon}.
#' #' @slot chronology chronology. Object of class \code{\link{chronology}} for the entity.
#' #' @slot c14 data.frame. Details on radiocarbon (C14) data and analysis.
#' #' @slot events data.frame. Details about events (e.g. tephra) that could appear in
#' #' the entity.
#' #' @slot depths data.frame. Details about the samples (including depths) at which samples
#' #' were taken in the entity for palynological (pollen, spores, etc) analysis.
#' #'
#' #' @export
#' #' @import methods
#' #'
#' datation <- setClass("datation",
#'                      slots=c(
#'                        e_="numeric",
#'                        restriction="data.frame",
#'                        entity="data.frame",
#'                        site="data.frame",
#'                        postbomb_zone="factor",
#'                        chronology="chronology",
#'                        c14="data.frame",
#'                        events="data.frame",
#'                        depths="data.frame"
#'                      ),
#'                      prototype=list(
#'                        e_=numeric(0),
#'                        restriction=data.frame(
#'                          e_=NA,
#'                          contact_=NA,
#'                          datasource=NA,
#'                          dataform=NA,
#'                          usestatus=NA,
#'                          datacoop=NA)[-1,],
#'                        entity=data.frame(
#'                          e_=NA,
#'                          site_=NA,
#'                          sigle=NA,
#'                          name=NA,
#'                          iscore=NA,
#'                          issect=NA,
#'                          isssamp=NA,
#'                          descriptor=NA,
#'                          hasanlam=NA,
#'                          entloc=NA,
#'                          localveg=NA,
#'                          coll_=NA,
#'                          sampdate=NA,
#'                          depthatloc=NA,
#'                          icethickcm=NA,
#'                          sampdevice=NA,
#'                          corediamcm=NA,
#'                          c14depthadj=NA,
#'                          notes=NA)[-1,],
#'                        site=data.frame(
#'                          site_=NA,
#'                          sitename=NA,
#'                          sitecode=NA,
#'                          siteexists=NA,
#'                          poldiv1=NA,
#'                          poldiv2=NA,
#'                          poldiv3=NA,
#'                          latdeg=NA,
#'                          latmin=NA,
#'                          latsec=NA,
#'                          latns=NA,
#'                          latdd=NA,
#'                          latdms=NA,
#'                          londeg=NA,
#'                          lonmin=NA,
#'                          lonsec=NA,
#'                          lonew=NA,
#'                          londd=NA,
#'                          londms=NA,
#'                          elevation=NA,
#'                          areaofsite=NA)[-1,],
#'                        postbomb_zone=factor(levels=c("NH1", "NH2", "NH3", "SH12", "SH3")),
#'                        chronology=chronology(),
#'                        c14=data.frame(
#'                          e_=NA,
#'                          sample_=NA,
#'                          agebp=NA,
#'                          agesdup=NA,
#'                          agesdlo=NA,
#'                          grthanage=NA,
#'                          basis=NA,
#'                          enriched=NA,
#'                          labnumber=NA,
#'                          deltac13=NA,
#'                          notes=NA,
#'                          method=NA,
#'                          depthcm=NA,
#'                          thickness=NA,
#'                          materialdated=NA,
#'                          publ_=NA)[-1,],
#'                        events=data.frame(
#'                          event_=NA,
#'                          e_=NA,
#'                          depthcm=NA,
#'                          thickness=NA,
#'                          event=NA,
#'                          name=NA,
#'                          agebp=NA,
#'                          ageuncertup=NA,
#'                          ageuncertlo=NA,
#'                          publ_=NA)[-1,],
#'                        depths=data.frame(
#'                          e_=NA,
#'                          sample_=NA,
#'                          depthcm=NA,
#'                          thickness=NA,
#'                          analyst=NA,
#'                          analydate=NA,
#'                          notes=NA,
#'                          lab_ID=NA,
#'                          nrow = 0)
#'                      )
#' )
#' 
#' #' Class for Counts of an entity
#' #' 
#' #' Class "counts" store in an organized and systematic way information about 
#' #' the particles counts in an specified entity in the European Pollen Database (EPD). This
#' #' object is created by \code{\link[EPDr:getCounts]{getCounts}}.
#' #'
#' #' It has different elements all of which correspond to a unique entity 
#' #'
#' #' @slot e_ numeric. The entity number (e_) as in the EPD.
#' #' @slot restriction data.frame. Restriction of use information for that particular entity
#' #' in the database. It is important to know if we can freely use the data or should ask
#' #' for authorization
#' #' @slot counts_type factor. Indicating the type of counts stored in slot \code{@counts}.
#' #' They can be one of two values "Counts" or "Percentages". "Counts" indicates that values
#' #' in \code{@counts} are raw counts as in the EPD. "Percentages" indicates that values in 
#' #' \code{@counts} are percentages calculated, for instance, with \code{\link[EPDr:trans2Percentages]{trans2Percentages}}. 
#' #' @slot counts_processing factor. Indicating whether the data in slot \code{@counts} has
#' #' been processed. It can be one of three values: "Samples", "Interpolated" or "Ranged 
#' #' means". "Samples" indicate that values in \code{@counts} correspond with the counts
#' #' for the palynological samples. "Interpolated" indicates that values in \code{@counts}
#' #' correspond to interpolated data at specific depth/ages among the palynological samples, 
#' #' using \code{\link[EPDr:interpolateCounts]{interpolateCounts}}. "Ranged means"
#' #' indicates that values in \code{@counts} represent mean values among all palynological samples for specified age/depth ranges calculated using \code{\link[EPDr:intervalsCounts]{intervalsCounts}}
#' #' @slot taxa_type factor. Indicating the taxa used to calculate values in slot
#' #' \code{@counts}. The EPD allow establish three levels of taxonomy: the taxonomic level
#' #' determined by analyst when processing the entity, "accepted" name to resolve
#' #' synonymies, and "higher" to collapse taxa into higher taxonomical levels (i.e.,
#' #' species in the same genus, genus in the same family, etc). \code{taxa_type} can take 
#' #' any of three values: "Samples", "Accepted" or "Higher". "Samples" indicates that 
#' #' taxonomy in \code{@coutns} is the same the analyst submitted to the EPD. "Accepted"
#' #' indicates that taxonomy in \code{@counts} is modified using the accepted taxa according
#' #' to last review of the EPD. "Higher" indicates that taxonomy was modified to group
#' #' taxa into higher taxonomical levels. "Samples" is always found when data comes
#' #' directly from the EPD with \code{\link[EPDr:getCounts]{getCounts}}, whereas "Accepted" and "Higher" are specified when data are transformed using \code{\link[EPDr:taxa2AcceptedTaxa]{taxa2AcceptedTaxa}} and \code{\link[EPDr:taxa2HigherTaxa]{taxa2HigherTaxa}}
#' #' respectively.
#' #' @slot taxa_processing factor. Indicating if taxonomy in \code{@counts} has been
#' #' processed. It can take any of three values: "Original", "Expanded", or "Taxized". 
#' #' "Original" indicates that taxonomy has not been modified. "Expanded" indicates that
#' #' taxonomy has been expanded beyond the taxa specified by the data provider, using
#' #' \code{\link[EPDr:filterTaxa]{filterTaxa}} or
#' #' \code{\link[EPDr:unifyTaxonomy]{unifyTaxonomy}}. "Taxized" indicates that taxonomy in
#' #' \code{@counts} has been resolved using the package \code{link[taxize]{taxize}}.
#' #' Function for this are on schedule but not implemented yet.
#' #' @slot entity data.frame. Details of the entity.
#' #' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' #' several entities can belong to the same site.
#' #' @slot taxa_names character. Character vector with all taxa used in \code{@counts}.
#' #' @slot taxa_ numeric. Numeric vector indicating the taxa number for each of the taxa 
#' #' used in \code{@counts}.
#' #' @slot taxa_accepted numeric. Numeric vector indicating the taxa number of 
#' #' corresponding accepted taxa for each taxa used in \code{@counts}.
#' #' @slot taxa_mhvar numeric. Numeric vector indicating the taxa number of 
#' #' corresponding higher taxa for each taxa used in \code{@counts}.
#' #' @slot taxa_groupid character. Character vector indicating the corresponding groupid
#' #' to which belong each taxa used in \code{@counts}. 
#' #' @slot sample_ numeric. Numeric vector indicating the sample number of each sample used
#' #' in \code{@counts}. Sample here is each of the palynological samples in the entity.
#' #' @slot sample_label character. Character vector indicating the sample name (or code)
#' #' of each sample used in \code{@counts}. Sample here is each of the palynological samples in the entity.
#' #' @slot default_ages numeric. Numeric vector indicating the ages estimated for each
#' #' palynological sample according to the default chronology in the EPD for that entity.
#' #' @slot depthcm numeric. Numeric vector indicating the depth (in cm) in which each
#' #' palynological sample was collected.
#' #' @slot counts data.frame. Data frame with counts (raw counts or percentages) for each
#' #' taxon at each sample (original samples, interpolated or ranged). This takes the form
#' #' of a age/depth (rows) by taxon (columns).
#' #'
#' #' @export 
#' #' @import methods
#' #'
#' counts <- setClass("counts",
#'                    slots=c(
#'                      e_="numeric",
#'                      restriction="data.frame",
#'                      counts_type="factor",
#'                      counts_processing="factor",
#'                      taxa_type="factor",
#'                      taxa_processing="factor",
#'                      entity="data.frame",
#'                      site="data.frame",
#'                      taxa_names="character",
#'                      taxa_="numeric",
#'                      taxa_accepted="numeric",
#'                      taxa_mhvar="numeric",
#'                      taxa_groupid="character",
#'                      sample_="numeric",
#'                      sample_label="character",
#'                      default_ages="numeric",
#'                      depthcm="numeric",
#'                      counts="data.frame"
#'                    ),
#'                    prototype=list(
#'                      e_=numeric(0),
#'                      restriction=data.frame(
#'                        e_=NA,
#'                        contact_=NA,
#'                        datasource=NA,
#'                        dataform=NA,
#'                        usestatus=NA,
#'                        datacoop=NA)[-1,],
#'                      counts_type=factor(levels=c("Counts", "Percentages")),
#'                      counts_processing=factor(levels=c("Samples", "Interpolated", "Ranged means")),
#'                      taxa_type=factor(levels=c("Samples", "Accepted", "Higher")),
#'                      taxa_processing=factor(levels=c("Original", "Expanded", "Taxized")),
#'                      entity=data.frame(
#'                        e_=NA,
#'                        site_=NA,
#'                        sigle=NA,
#'                        name=NA,
#'                        iscore=NA,
#'                        issect=NA,
#'                        isssamp=NA,
#'                        descriptor=NA,
#'                        hasanlam=NA,
#'                        entloc=NA,
#'                        localveg=NA,
#'                        coll_=NA,
#'                        sampdate=NA,
#'                        depthatloc=NA,
#'                        icethickcm=NA,
#'                        sampdevice=NA,
#'                        corediamcm=NA,
#'                        c14depthadj=NA,
#'                        notes=NA)[-1,],
#'                      site=data.frame(
#'                        site_=NA,
#'                        sitename=NA,
#'                        sitecode=NA,
#'                        siteexists=NA,
#'                        poldiv1=NA,
#'                        poldiv2=NA,
#'                        poldiv3=NA,
#'                        latdeg=NA,
#'                        latmin=NA,
#'                        latsec=NA,
#'                        latns=NA,
#'                        latdd=NA,
#'                        latdms=NA,
#'                        londeg=NA,
#'                        lonmin=NA,
#'                        lonsec=NA,
#'                        lonew=NA,
#'                        londd=NA,
#'                        londms=NA,
#'                        elevation=NA,
#'                        areaofsite=NA)[-1,],
#'                      taxa_names=character(0),
#'                      taxa_=numeric(0),
#'                      taxa_accepted=numeric(0),
#'                      taxa_mhvar=numeric(0),
#'                      taxa_groupid=character(0),
#'                      sample_=numeric(0),
#'                      sample_label=character(0),
#'                      default_ages=numeric(0),
#'                      depthcm=numeric(0),
#'                      counts=data.frame()
#'                    )
#' )
#' 
#' 
#' #' Class for Ages of an entity
#' #' 
#' #' Class "ages" store in an organized and systematic way information about 
#' #' estimated ages for palynological samples in an specified entity in the European
#' #' Pollen Database (EPD). This object is created by \code{\link[EPDr:getAges]{getAges}}.
#' #'
#' #' It has different elements all of which correspond to a unique entity 
#' #' 
#' #' @slot e_ numeric. The entity number (e_) as in the EPD.
#' #' @slot restriction data.frame. Restriction of use information for that particular entity
#' #' in the database. It is important to know if we can freely use the data or should ask
#' #' for authorization
#' #' @slot entity data.frame. Details of the entity.
#' #' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' #' several entities can belong to the same site.
#' #' @slot default_chronology numeric. Which of the available chronologies is tagged as 
#' #' the default in the database.
#' #' @slot giesecke logical. Indicating \code{TRUE} if there are ages revised by Giesecke
#' #' et al. (2013) for this entity or \code{FALSE} on the contrary.
#' #' @slot sample_ numeric. Numeric vector indicating the sample number of each sample used
#' #' in \code{@counts}. Sample here is each of the palynological samples in the entity.
#' #' @slot sample_label character. Character vector indicating the sample name (or code)
#' #' of each sample used in \code{@counts}. Sample here is each of the palynological samples
#' #' in the entity.
#' #' @slot depthcm numeric. Numeric vector indicating the depth (in cm) in which each
#' #' palynological sample was collected.
#' #' @slot depths data.frame. Details about the samples (including depths) at which samples
#' #' were taken in the entity for palynological (pollen, spores, etc) analysis.
#' #' @slot depth_ages data.frame. Data frame with ages for each palynological sample
#' #' according to the different chronologies and giesecke, if available.
#' #' @slot data_quality data.frame. Data frame with data quality index for counts on
#' #' each sample (original, interpolated, or ranged) according to metrics in Blois
#' #' et al. (2013).
#' #'
#' #' @references Giesecke, Thomas; Davis, Basil A S; Brewer, Simon; Finsinger, Walter;
#' #' Wolters, Steffen; Blaauw, Maarten; de Beaulieu, Jacques-Louis; Binney, Heather;
#' #' Fyfe, Ralph M; Gaillard, Marie-Jose; Gil-Romera, Graciela; van der Knaap, Pim Willem O;
#' #' Kunes, Petr; Kuhl, Norbert; van Leeuwen, Jaqueline F N; Leydet, Michelle;
#' #' Lotter, Andre F; Ortu, Elena; Semmler, Malte Sebastian Swen;
#' #' Bradshaw, Richard H W (2013). Towards mapping the late Quaternary vegetation change
#' #' of Europe. Vegetation History and Archaeobotany, 23(1): 75-86.
#' #' doi:10.1007/s00334-012-0390-y
#' #' @references \url{https://doi.pangaea.de/10.1594/PANGAEA.804597}
#' #' @references Blois, Jessica L; Williams, John W; Fitzpatrick, Matthew C; Ferrier, Simon;
#' #' Veloz, Samuel D; He, Feng; Liu, Zhengyu; Manion, Glenn; Otto-Bliesner, Bette (2013).
#' #' Modeling the Climatic Drivers of Spatial Patterns in Vegetation Composition since the
#' #' Last Glacial Maximum. Ecography, 36(4): 460-473. doi:10.1111/j.1600-0587.2012.07852.x.
#' #' 
#' #' @export 
#' #' @import methods
#' #'
#' ages <- setClass("ages",
#'                  slots=c(
#'                    e_="numeric",
#'                    restriction="data.frame",
#'                    entity="data.frame",
#'                    site="data.frame",
#'                    default_chronology="numeric",
#'                    giesecke="logical",
#'                    sample_="numeric",
#'                    sample_label="character",
#'                    depthcm="numeric",
#'                    depths="data.frame",
#'                    depth_ages="data.frame",
#'                    data_quality="data.frame"
#'                  ),
#'                  prototype=list(
#'                    e_=numeric(0),
#'                    restriction=data.frame(
#'                      e_=NA,
#'                      contact_=NA,
#'                      datasource=NA,
#'                      dataform=NA,
#'                      usestatus=NA,
#'                      datacoop=NA)[-1,],
#'                    entity=data.frame(
#'                      e_=NA,
#'                      site_=NA,
#'                      sigle=NA,
#'                      name=NA,
#'                      iscore=NA,
#'                      issect=NA,
#'                      isssamp=NA,
#'                      descriptor=NA,
#'                      hasanlam=NA,
#'                      entloc=NA,
#'                      localveg=NA,
#'                      coll_=NA,
#'                      sampdate=NA,
#'                      depthatloc=NA,
#'                      icethickcm=NA,
#'                      sampdevice=NA,
#'                      corediamcm=NA,
#'                      c14depthadj=NA,
#'                      notes=NA)[-1,],
#'                    site=data.frame(
#'                      site_=NA,
#'                      sitename=NA,
#'                      sitecode=NA,
#'                      siteexists=NA,
#'                      poldiv1=NA,
#'                      poldiv2=NA,
#'                      poldiv3=NA,
#'                      latdeg=NA,
#'                      latmin=NA,
#'                      latsec=NA,
#'                      latns=NA,
#'                      latdd=NA,
#'                      latdms=NA,
#'                      londeg=NA,
#'                      lonmin=NA,
#'                      lonsec=NA,
#'                      lonew=NA,
#'                      londd=NA,
#'                      londms=NA,
#'                      elevation=NA,
#'                      areaofsite=NA)[-1,],
#'                    default_chronology=numeric(0),
#'                    giesecke=logical(0),
#'                    sample_=numeric(0),
#'                    sample_label=character(0),
#'                    depthcm=numeric(0),
#'                    depth=data.frame(
#'                      e_=NA,
#'                      sample_=NA,
#'                      depthcm=NA,
#'                      thickness=NA,
#'                      analyst_=NA,
#'                      analydate=NA,
#'                      notes=NA,
#'                      lab_ID=NA)[-1,],
#'                    depth_ages=data.frame(),
#'                    data_quality=data.frame()
#'                  )
#' )
#' 
#' 
#' #' Class for Aged-Counts of an entity
#' #' 
#' #' Class "agedcounts" store in an organized and systematic way information about 
#' #' counts and ages for palynological samples in an specified entity in the European
#' #' Pollen Database (EPD). This object is created by
#' #' \code{\link[EPDr:getAgedCounts]{getAgedCounts}}.
#' #' 
#' #' It has different elements all of which correspond to a unique entity 
#' #'
#' #' @slot e_ numeric. The entity number (e_) as in the EPD.
#' #' @slot restriction data.frame. Restriction of use information for that particular entity
#' #' in the database. It is important to know if we can freely use the data or should ask
#' #' for authorization
#' #' @slot entity data.frame. Details of the entity.
#' #' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' #' several entities can belong to the same site.
#' #' @slot ages ages. Object of class \code{\link{ages}} for the entity.
#' #' @slot counts counts. Object of class \code{\link{counts}} for the entity.
#' #'
#' #' @export 
#' #' @import methods
#' #'
#' agedcounts <- setClass("agedcounts",
#'                        slots=c(
#'                          e_="numeric",
#'                          restriction="data.frame",
#'                          entity="data.frame",
#'                          site="data.frame",
#'                          ages="ages",
#'                          counts="counts"
#'                        ),
#'                        prototype=list(
#'                          e_=numeric(0),
#'                          restriction=data.frame(
#'                            e_=NA,
#'                            contact_=NA,
#'                            datasource=NA,
#'                            dataform=NA,
#'                            usestatus=NA,
#'                            datacoop=NA)[-1,],
#'                          entity=data.frame(
#'                            e_=NA,
#'                            site_=NA,
#'                            sigle=NA,
#'                            name=NA,
#'                            iscore=NA,
#'                            issect=NA,
#'                            isssamp=NA,
#'                            descriptor=NA,
#'                            hasanlam=NA,
#'                            entloc=NA,
#'                            localveg=NA,
#'                            coll_=NA,
#'                            sampdate=NA,
#'                            depthatloc=NA,
#'                            icethickcm=NA,
#'                            sampdevice=NA,
#'                            corediamcm=NA,
#'                            c14depthadj=NA,
#'                            notes=NA)[-1,],
#'                          site=data.frame(
#'                            site_=NA,
#'                            sitename=NA,
#'                            sitecode=NA,
#'                            siteexists=NA,
#'                            poldiv1=NA,
#'                            poldiv2=NA,
#'                            poldiv3=NA,
#'                            latdeg=NA,
#'                            latmin=NA,
#'                            latsec=NA,
#'                            latns=NA,
#'                            latdd=NA,
#'                            latdms=NA,
#'                            londeg=NA,
#'                            lonmin=NA,
#'                            lonsec=NA,
#'                            lonew=NA,
#'                            londd=NA,
#'                            londms=NA,
#'                            elevation=NA,
#'                            areaofsite=NA
#'                          )[-1,],
#'                          default_chronology=numeric(0),
#'                          ages=ages(),
#'                          counts=counts()
#'                        )
#' )
#' 
#' 
#' #' Query C14 data of EPD Entities
#' #' 
#' #' This function queries the database to request all information about the C14 data 
#' #' of an specified entity. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #' 
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return Data frame with all combined information from C14 and GEOCHRON tables in the
#' #' EPD (see documentation of the EPD:
#' #' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getC14_old(1, epd.connection)
#' #' # getC14_old(400, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getC14_old <- function(e_, connection) {
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getC14_old' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   rest <- getRestriction_old(e_, connection)
#'   
#'   sqlQuery <- paste("SELECT * FROM c14 WHERE e_=", e_, ";", sep="")
#'   c14 <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   sqlQuery <-paste("SELECT * FROM geochron WHERE e_=", e_, ";", sep="")
#'   geochron <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   if(nrow(c14) == 0){
#'     warning("This core (entity) does not have C14 data.", call.=FALSE)
#'     c14 <- data.frame(e_=NA,sample_=NA,agebp=NA,agesdup=NA,agesdlo=NA,grthanage=NA,basis=NA,enriched=NA,labnumber=NA, deltac13=NA,notes=NA)[-1,] 
#'     geochron <- data.frame(e_=NA,sample_=NA,method=NA,depthcm=NA,thickness=NA,materialdated=NA,publ_=NA)[-1,]
#'   }
#'   
#'   c14geochron <- merge(c14, geochron, by=c("e_","sample_"))
#'   
#'   return(c14geochron)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #' Query chronologies of EPD entities
#' #' 
#' #' This function queries the database to request all information about the chronologies  
#' #' of an specified entity. A particular entity might have several chronologies developed in different
#' #' projects by differen researchers. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #'
#' #' @return chronology. Object of class \code{\link[EPDr:chronology]{chronology}}. This object
#' #' store in an organized and systematic way all combined information from CHRON and AGEBASIS tables
#' #' in the EPD  (see documentation of the EPD:
#' #' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getChronology(1, epd.connection)
#' #' # getChronology(400, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getChronology_old <- function(e_, connection) {
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getChronology' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   rest <- getRestriction_old(e_, connection)
#'   
#'   site <- getSite_old(e_, connection)
#'   entity <- getEntity_old(e_, connection)
#'   
#'   sqlQuery <-paste("SELECT * FROM chron WHERE e_=", e_, ";", sep="")
#'   chron <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   sqlQuery <- paste("SELECT * FROM agebasis WHERE e_=", e_, ";", sep="")
#'   agebasis <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   number_of_chronologies <- nrow(chron)
#'   
#'   default_chronology <- chron$chron_[which(chron$defaultchron == "Y")]
#'   if(is.null(default_chronology)){default_chronology <- 0}
#'   if(length(default_chronology) == 0){default_chronology <- 0}
#'   
#'   if(number_of_chronologies == 0){
#'     warning("This core (entity) does not have chronologies.", call.=F)
#'     output <- chronology()
#'   }else{
#'     output <- chronology(e_=e_, restriction=rest, entity=entity, site=site, number_of_chronologies=number_of_chronologies, default_chronology=default_chronology, chron=chron, agebasis=agebasis)
#'   }
#'   
#'   return(output)
#' }
#' 
#' 
#' #' Query datation of EPD entities
#' #'
#' #' This function queries the database to request information about datation 
#' #' of an specified entity. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return datation. Object of class \code{\link[EPDr:datation]{datation}} with all the
#' #' information about datation of that entity.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getDatation(1, epd.connection)
#' #' # getDatation(51, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getDatation_old <- function(e_, connection){
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getDatation' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   rest <- getRestriction_old(e_, connection)
#'   entity <- getEntity_old(e_, connection)
#'   site <- getSite_old(e_, connection)
#'   coord <- site[, c("londd", "latdd")]     
#'   pb_zone <- sp::over(sp::SpatialPoints(coord), postbomb.map)$Zone
#'   chronology <- getChronology(e_, connection)
#'   c14 <- getC14_old(e_, connection)
#'   events <- getEvents(e_, connection)
#'   depths <- getPSamples(e_, connection)
#'   output <- datation(e_=e_, restriction=rest, entity=entity, site=site, postbomb_zone=pb_zone, chronology=chronology, c14=c14, events=events, depths=depths)
#'   return(output)
#' }
#' 
#' 
#' #' Query palynological counts of EPD entities
#' #'
#' #' This function queries the database to request information about palynological counts 
#' #' of an specified entity. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection}
#' #' as returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return counts. Object of class \code{\link[EPDr:counts]{counts}}.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getCounts(1, epd.connection)
#' #' # getCounts(51, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getCounts_old <- function(e_, connection){
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getCounts' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   rest <- getRestriction_old(e_, connection)
#'   
#'   entity <- getEntity_old(e_, connection)
#'   site <- getSite_old(e_, connection)
#'   
#'   sqlQuery <- paste("SELECT sample_, count, varname FROM p_counts NATURAL JOIN p_vars WHERE e_ =", e_, ";", sep="")
#'   counts.raw <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   depth <- getPSamples(e_, connection)
#'   depthcm <- depth$depthcm
#'   
#'   if(is.data.frame(counts.raw) && nrow(counts.raw) == 0){
#'     warning("This core does not have count data.", call.=FALSE)
#'     counts.cast <- data.frame(0)[,-1]
#'     taxa.names <- character(0)
#'     sample_ <- numeric(0)
#'     taxa.groupid <- character(0)
#'     taxa.id <- numeric(0)
#'     taxa.accepted <- numeric(0)
#'     taxa.mhvar <- numeric(0)
#'   }else{
#'     counts.cast <- reshape2::dcast(counts.raw, sample_ ~ varname, value.var='count')
#'     counts.cast[is.na(counts.cast)] <- 0
#'     sample_ <- counts.cast[,1]
#'     counts.cast <- counts.cast[,-1]
#'     
#'     taxa.names <- colnames(counts.cast)
#'     
#'     sqlQuery <- paste("SELECT var_, varname, groupid, accvar_, mhvar_ FROM p_vars NATURAL JOIN p_group WHERE varname IN ('", paste(taxa.names, collapse="','"), "');", sep="")
#'     groupid <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'     groupid <- groupid[match(taxa.names, groupid$varname),]
#'     
#'     taxa.groupid <- groupid$groupid
#'     taxa.id <- groupid$var_
#'     taxa.accepted <- groupid$accvar_
#'     taxa.mhvar <- groupid$mhvar_
#'   }
#'   
#'   counts_type <- factor("Counts", levels=c("Counts", "Percentages"))
#'   counts_processing <- factor("Samples", levels=c("Samples", "Interpolated", "Ranged means"))
#'   taxa_type <- factor("Samples", levels=c("Samples", "Accepted", "Unified"))
#'   taxa_processing <- factor("Original", levels=c("Original", "Expanded", "Taxize"))
#'   
#'   sample_label <- as.character(sample_)
#'   
#'   ages <- getAges(e_, connection)
#'   
#'   default_chronology <- ages@default_chronology
#'   if(default_chronology != 0){
#'     if(ages@giesecke == T){
#'       default_chronology <- "giesecke"
#'     }
#'     default_ages <- ages@depth_ages[,as.character(default_chronology)]
#'   }else{
#'     default_ages <- numeric(0)
#'   }
#'   
#'   counts <- counts(e_=e_, restriction=rest, entity=entity, site=site, counts_type=counts_type, counts_processing=counts_processing, taxa_type=taxa_type, taxa_processing=taxa_processing, taxa_names=taxa.names, taxa_=taxa.id, taxa_groupid=taxa.groupid, taxa_accepted=taxa.accepted, taxa_mhvar=taxa.mhvar, sample_=sample_, sample_label=sample_label, default_ages=default_ages, depthcm=depthcm, counts=counts.cast)
#'   
#'   return(counts)
#' }
#' 
#' 
#' #' Query palynological-samples ages of EPD entities
#' #'
#' #' This function queries the database to request information about estimated ages of palynological
#' #' samples of an specified entity. To perform the query the function requires the number
#' #' of the entity that want to be queried and a valid connection to the database. Hence,
#' #' the following parameters are mandatory:
#' #'
#' #' @param e_ numeric. Value indicating the entity number (e_) of the database that want to 
#' #' be queried.
#' #' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' #' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' #'
#' #' @return ages Object of class \code{\link[EPDr:ages]{ages}}.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # epd.connection <- connectToEPD(database="epd", user="epdr",
#' #' #                                 password="epdrpw", host="localhost")
#' #' # getAges(1, epd.connection)
#' #' # getAges(51, epd.connection)
#' #' # disconnectFromEPD(connection=epd.connection)
#' #' 
#' getAges_old <- function(e_, connection){
#'   if(length(e_) > 1){
#'     e_ <- e_[[1]]
#'     warning("'getAges' function is designed to retrieve information for single entities. You have provided several entity ID values (e_) but only the first one is going to be used.")
#'   }
#'   
#'   rest <- getRestriction_old(e_, connection)
#'   
#'   entity <- getEntity_old(e_, connection)
#'   site <- getSite_old(e_, connection)
#'   
#'   sqlQuery<- paste("SELECT sample_, chron_, agebp FROM p_agedpt WHERE e_=", e_, ";", sep="")
#'   ages <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   sqlQuery <-paste("SELECT * FROM chron WHERE e_=", e_, ";", sep="")
#'   chron <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
#'   
#'   depths <- getPSamples_old(e_, connection)
#'   
#'   default_chronology <- chron$chron_[which(chron$defaultchron == "Y")]
#'   if(is.null(default_chronology) | length(default_chronology) == 0){default_chronology <- 0}
#'   
#'   sample_ <- depths$sample_
#'   depthcm <- depths$depthcm
#'   sample_label <- as.character(sample_)
#'   
#'   if(is.data.frame(ages) && nrow(ages) == 0){
#'     warning("This core does not have age data.", call.=FALSE)
#'     ages.cast <- data.frame()
#'   }else{
#'     ages.cast <- reshape2::dcast(ages, sample_ ~ chron_, value.var='agebp')
#'     ages.cast <- ages.cast[match(sample_, ages.cast$sample_),]
#'     ages.cast <- ages.cast[,-1]
#'   }
#'   
#'   if(class(ages.cast) == "numeric"){
#'     ages.cast <- data.frame(ages.cast)
#'     colnames(ages.cast) <- 1
#'   }
#'   
#'   if(e_ %in% giesecke.EpdAgeCut$ID){
#'     is.in.giesecke <- TRUE
#'     ages.giesecke <- giesecke.EpdAgeCut[which(giesecke.EpdAgeCut$ID == e_), c("ID", "Event", "Depth..m.", "Age.dated..ka.", "Age.min..ka.", "Age.max..ka.")]
#'     ages.giesecke$depthcm <- ages.giesecke$Depth..m. * 100
#'     ages.giesecke <- ages.giesecke[match(round(depths[,"depthcm"], 1), round(ages.giesecke$depthcm, 1)), ]
#'     ages.giesecke$agesbp <- ages.giesecke$Age.dated..ka. * 1000
#'     
#'     if(nrow(ages.cast) == 0){
#'       ages.cast <- data.frame(giesecke=ages.giesecke$agesbp)
#'     }else{
#'       column.names <- colnames(ages.cast)
#'       ages.cast <- cbind(ages.cast, ages.giesecke$agesbp)
#'       colnames(ages.cast) <- c(column.names, "giesecke")
#'     }
#'   }else{
#'     is.in.giesecke <- FALSE
#'   }
#'   
#'   ages.final <- ages(e_=e_, restriction=rest, entity=entity, site=site, default_chronology=default_chronology, giesecke=is.in.giesecke, sample_=sample_, sample_label=sample_label, depthcm=depthcm, depths=depths, depth_ages=ages.cast)
#'   return(ages.final)
#' }
#' 
#' 
#' 
#' 
#' 






#' #' Filter counts by taxa groups
#' #' 
#' #' This function removes taxa from the slot @@counts in \code{\link[EPDr:counts]{counts}} or
#' #' \code{\link[EPDr:agedcounts]{agedcounts}} objects based on specified groups of taxa.
#' #' For example, the user can select to work only with pollen from trees and shrubs (TRSH)
#' #' or algae (ALGA). The function automatically remove counts from any particle added by
#' #' the entity analyst to calculate particle concentrations in the samples
#' #' (e.g., Lycopodium (added)).
#' #' 
#' #' @param counts \code{\link[EPDr:counts]{counts}} or
#' #' \code{\link[EPDr:agedcounts]{agedcounts}} objects.
#' #' @param taxa_groups Character vector indicating taxa groups to be selected
#' #' 
#' #' @return The function returns a modified version of the \code{counts} object with
#' #' information on @counts only for taxa belonging to the specified taxa groups.
#' #' 
#' #' @export
#' #' 
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # agedcount <- getAgedCounts(1, epd.connection)
#' #' # agedcount.pollen <- filterTaxaGroups(agedcount, "TRSH")
#' #' # str(agedcount@counts@counts)
#' #' # str(agedcount.pollen@counts@counts)
#' #' 
#' filterTaxaGroups <- function(counts, taxa_groups){
#' 
#'   if(!class(counts) %in% c("counts", "agedcounts")){
#'     stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
#'   }
#'   if(class(counts) == "agedcounts"){
#'     counts.tmp <- counts@counts
#'   }else{
#'     counts.tmp <- counts
#'   }
#' 
#'   addedparticles <- c("Eucalyptus (added)",
#'                       "Eucalyptus (counted)",
#'                       "Exotics (counted)",
#'                       "Lycopodium (added)",
#'                       "Lycopodium (counted)",
#'                       "Lycopodium (counted) for charcoal",
#'                       "Lycopodium (counted) for Pediastrum",
#'                       "Marker (added)",
#'                       "Marker (counted)",
#'                       "Microspheres (counted)",
#'                       "Microspheres suspension (added)",
#'                       "Microsphere suspension (volume added, ml)",
#'                       "Number of counted lines",
#'                       "Number of spike added",
#'                       "Tablets (added)",
#'                       "Volume of spike added")
#' 
#'   index1 <- which(counts.tmp@taxa_groupid %in% taxa_groups)
#'   index2 <- which(counts.tmp@taxa_names %in% addedparticles)
#'   index <- unique(c(index1, index2))
#' 
#'   counts.tmp@taxa_names <- counts.tmp@taxa_names[index]
#'   counts.tmp@taxa_ <- counts.tmp@taxa_[index]
#'   counts.tmp@taxa_accepted <- counts.tmp@taxa_accepted[index]
#'   counts.tmp@taxa_mhvar <- counts.tmp@taxa_mhvar[index]
#'   counts.tmp@taxa_groupid<- counts.tmp@taxa_groupid[index]
#'   counts.tmp@counts <- counts.tmp@counts[,index]
#'   if(class(counts) == "agedcounts"){
#'     counts@counts <- counts.tmp
#'   }else{
#'     counts <- counts.tmp
#'   }
#'   return(counts)
#' }
#' 
#' 


#' #' Calculate counts percentages
#' #'
#' #' This function transforms counts in the slot @@counts in \code{\link[EPDr:counts]{counts}}
#' #' or \code{\link[EPDr:agedcounts]{agedcounts}} objects to percentages relative to 
#' #' the total amount of particles in each sample (row).
#' #'
#' #' @param counts \code{\link[EPDr:counts]{counts}} or
#' #' \code{\link[EPDr:agedcounts]{agedcounts}} objects.
#' #'
#' #' @return The function returns a modified version of the \code{counts} object, in which
#' #' values in slot @@counts represent percentages instead of raw counts.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # agedcount <- getAgedCounts(1, epd.connection)
#' #' # agedcount.pollen <- filterTaxaGroups(agedcount, c("DWAR", "HERB", "LIAN",
#' #' # "TRSH", "UPHE", "INUN")) ## All pollen taxa groups
#' #' # agedcount.percent <- trans2Percentages(agedcount.pollen)
#' #' # head(agedcount.pollen@counts@counts)
#' #' # head(agedcount.percent@counts@counts)
#' #' 
#' trans2Percentages <- function(counts){
#'   
#'   if(!class(counts) %in% c("counts", "agedcounts")){
#'     stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
#'   }
#'   if(class(counts) == "agedcounts"){
#'     counts.tmp <- counts@counts@counts
#'   }else{
#'     counts.tmp <- counts@counts
#'   }
#'   
#'   counts_type <- factor("Percentages", levels=c("Counts", "Percentages"))
#'   totals <- rowSums(counts.tmp)
#'   counts.tmp <- (counts.tmp / totals) * 100
#'   
#'   if(class(counts) == "agedcounts"){
#'     counts@counts@counts <- counts.tmp
#'     counts@counts@counts_type <- counts_type
#'   }else{
#'     counts@counts <- counts.tmp
#'     counts@counts_type <- counts_type
#'   }
#'   return(counts)
#' }
#' 



#' #' Interpolate counts to specific time periods
#' #'
#' #' This function uses data (sample ages and sample counts) from an
#' #' \code{\link[EPDr:agedcounts]{agedcounts}} object to estimate by linear interpolation
#' #' the counts at specific time periods defined by the user. This can be used to
#' #' estimate counts for the same time periods for multiple entities 
#' #' or cores in the database, standardizing them for integrative analysis.
#' #' 
#' #' Data for time periods in \code{time} but not recorded in the entity are fill with 
#' #' \code{NA}. This is convenient if analysis are carried out with multiple entities.
#' #'
#' #' @param agedcounts An \code{\link[EPDr:agedcounts]{agedcounts}} object as returned by
#' #' the \code{\link[EPDr:getAgedCounts]{getAgedCounts}} function.
#' #' @param time Vector with time periods, in the same system (i.e., cal BP) than
#' #' "ages" in agedcounts, in which counts have to be estimated.
#' #' @param chronology Number specifying the chronology from which ages should be
#' #' used to calculate the interpolations. If none is
#' #' provided the function uses the default chronology from the object (see
#' #' \code{\link[EPDr:gieseckeDefaultChronology]{gieseckeDefaultChronology}}).
#' #' @param method interpolation method, should be an unambiguous abbreviation of
#' #' either linear, loess, sspline or aspline. See details.
#' #' @param rep_negt logical to indicate whether or not to replace negative values with zero in the interpolated data.
#' #' @param span span for loess, default=0.25.
#' #' @param df degress of freedome for smoothing spline, default is the lower of
#' #' 20 or 0.7 * number of samples.
#' #' @param ...	additional arguments to loess, smooth.spline and aspline.
#' #' 
#' #' @details  Interpolation can be done using linear interpolation between data points
#' #' in the original series (default) using \code{\link[stats:approx]{approx}}, using
#' #' a fitted \code{\link[stats:loess]{loess}} locally weighted regression, or by 
#' #' \code{\link[stats:smooth.spline]{smooth.spline}}. The latter two methods will
#' #' also smooth the dataand additional arguments may be passed to these functions
#' #' to control the amount of smoothing.
#' #'
#' #' @return The function returns an \code{\link[EPDr:agedcounts]{agedcounts}} object,
#' #' similar to \code{agedcounts} in which ages and counts has been modified to the
#' #' time periods specified in time and the counts estimated for these periods.
#' #' Accordingly, \code{default_chronology} is also modified to 1, so subsequent analysis
#' #' will automatically pick up the first (and only) column with the new time periods.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # t <- c(seq(0, 21000, by=500))
#' #' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' #' # agedcounts.1.int <- interpolateCounts(agedcounts.1, t)
#' #' #
#' #' # agedcounts.3 <- getAgedCounts(3, connEPD)
#' #' # agedcounts.3.int <- interpolateCounts(agedcounts.3, t)
#' #' # agedcounts.3.int <- interpolateCounts(agedcounts.3, t, 1)
#' #' # agedcounts.3.int <- interpolateCounts(agedcounts.3, t, 2)
#' #' 
#' interpolateCounts <- function(epd.entity.df, time, chronology=NULL,
#'                               method=c("linear", "loess", "sspline"),
#'                               rep_negt=TRUE, span=0.25,
#'                               df=min(20, nrow(epd.entity.df@commdf@counts) * 0.7),
#'                               ...){
#'   #     agedcounts <- getAgedCounts(100, connEPD)
#'   #     time <- c(seq(0, 21000, by=500))
#'   #     chronology <- NULL
#'   if(is.null(chronology)){
#'     chronology <- epd.entity.df@defaultchron
#'   }
#'   
#'   if(chronology == 9999){
#'     chronology <- "giesecke"
#'   }
#'   
#'   sample.ages <- epd.entity.df@agesdf@depthages[,as.character(chronology)]
#'   sample.depthcm <- epd.entity.df@agesdf@depthcm
#'   sample.id <- epd.entity.df@commdf@sample_
#'   sample.counts <- epd.entity.df@commdf@counts
#'   
#'   # remove data from depths whithout ages associated
#'   index1 <- which(!is.na(sample.ages))
#'   index2 <- 1:length(sample.id)
#'   index <- intersect(index2, index1) 
#'   
#'   sample.ages <- sample.ages[index]
#'   sample.depthcm <- sample.depthcm[index]
#'   sample.id <- sample.id[index]
#'   sample.counts <- sample.counts[index,]
#'   
#'   ## set the time bounds
#'   min.sample.age <- min(sample.ages, na.rm=T)
#'   max.sample.age <- max(sample.ages, na.rm=T)
#'   min.time <- time[which(time >= min.sample.age)][1]
#'   max.time <- time[which(time <= max.sample.age)][length(which(time <= max.sample.age))]
#'   interp.ages <- time[which(time >= min.time & time <= max.time)]
#'   
#'   ## interpolate the relative data to the nearest 1000 years & create the final site data frame
#'   interp.counts <- as.data.frame(matrix(nrow=length(interp.ages), ncol=ncol(sample.counts)))
#'   interp.depthcm <- numeric(0)
#'   if(length(interp.ages) != 0){
#'     
#'     method <- match.arg(method)
#'     if (is.null(method)) 
#'       stop("Interpolation method not recognised")
#'     if (method == "linear") {
#'       lin.f <- function(y, x, xout) {
#'         stats::approx(x, y, xout)$y
#'       }
#'       interp.counts <- apply(sample.counts, MARGIN=2, lin.f, x=sample.ages, xout=interp.ages, ...)
#'       interp.depthcm <- lin.f(sample.depthcm, sample.ages, xout=interp.ages)
#'       # res <- apply(y, 2, lin.f, x1 = x, xout = xout, ...)
#'       # interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
#'     }
#'     else if (method == "loess") {
#'       lo.f <- function(y, x, xout, span, ...) {
#'         fit <- stats::loess(y ~ x, span=span, ...)
#'         stats::predict(fit, newdata=data.frame(x=xout))
#'       }
#'       interp.counts <- apply(sample.counts, MARGIN=2, lo.f, x=sample.ages, xout=interp.ages, span=span, ...)
#'       interp.depthcm <- lo.f(sample.depthcm, sample.ages, xout=interp.ages, span=span, ...)
#'       # res <- apply(y, 2, lo.f, x1 = x, xout = xout, span = span, ...)
#'       # interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
#'     }
#'     else if (method == "sspline") {
#'       ss.f <- function(y, x, xout, df, ...) {
#'         fit <- stats::smooth.spline(y ~ x, df=df, ...)
#'         stats::predict(fit, x=data.frame(x=xout))$y[, 1]
#'       }
#'       interp.counts <- apply(sample.counts, MARGIN=2, ss.f, x=sample.ages, xout=interp.ages)
#'       interp.depthcm <- ss.f(sample.depthcm, sample.ages, xout=interp.ages, df=df, ...)
#'       # res <- apply(y, 2, ss.f, x1 = x, xout = xout, df = df, ...)
#'       # interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
#'     }
#'     if (rep_negt) {
#'       interp.counts[interp.counts < 0] <- 0
#'     }
#'     
#'     #    interp.counts <- apply(sample.counts, MARGIN=2, FUN=function(x, y, z){stats::approx(y, x, xout=z)$y}, sample.ages, interp.ages)
#'     # interp.depthcm <- stats::approx(sample.ages, sample.depthcm, xout=interp.ages)$y
#'     
#'     if(length(interp.ages) == 1){
#'       interp.counts <- as.data.frame(t(interp.counts))
#'     }else{
#'       interp.counts <- as.data.frame(interp.counts)
#'     }
#'   }
#'   colnames(interp.counts) <- colnames(sample.counts)
#'   
#'   # create output object with NA and fill with interpolated values when appropriate
#'   output.counts <- as.data.frame(matrix(nrow=length(time), ncol=ncol(sample.counts)))
#'   colnames(output.counts) <- colnames(sample.counts)
#'   output.depthcm <- rep(NA, length(time))
#'   output.ages <- time
#'   index <- which(output.ages %in% interp.ages)
#'   
#'   output.counts[index,] <- interp.counts
#'   output.depthcm[index] <- interp.depthcm
#'   
#'   # create final object with new values for different slots
#'   epd.entity.df@defaultchron <- 1
#'   epd.entity.df@commdf@countsprocessing <- factor("Interpolated", levels=c("Samples", "Interpolated", "Ranged means"))
#'   epd.entity.df@commdf@sample_ <- epd.entity.df@agesdf@sample_ <- seq(20001, length.out=length(output.ages))
#'   epd.entity.df@commdf@samplelabel <- epd.entity.df@agesdf@samplelabel <- as.character(output.ages)
#'   epd.entity.df@agesdf@depthages <- output.ages
#'   epd.entity.df@agesdf@depthcm <- output.depthcm
#'   epd.entity.df@commdf@counts <- output.counts
#'   
#'   return(epd.entity.df)    
#' }




#' #' Mean counts for specific time intervals
#' #'
#' #' This function uses data (sample ages and sample counts) from an
#' #' \code{\link[EPDr:agedcounts]{agedcounts}} object to calculate mean counts
#' #' for samples within specific time intervals defined by the user. This can be
#' #' used to estimate mean counts for the same time intervals for multiple entities 
#' #' or cores in the database, standardizing them for integrative analysis.
#' #' 
#' #' Time intervals without sample (data) in the entity are fill with 
#' #' \code{NA}. This is convenient if analysis are carried out with multiple entities.
#' #'
#' #' @param agedcounts An \code{\link[EPDr:agedcounts]{agedcounts}} object as returned by
#' #' the \code{\link[EPDr:getAgedCounts]{getAgedCounts}} function.
#' #' @param tmin Numeric vector indicating the lower limits (in years cal. BP) for the 
#' #' time intervals.
#' #' @param tmax Numeric vector indicating the upper limits (in years cal. BP) for the
#' #' time intervals
#' #' @param labels Character vector with labels for each time intervals, if none are 
#' #' provided the functions generate them with the following format \code{tmin}-\code{tmax}.
#' #' @param chronology Number specifying the chronology from which ages should be
#' #' used to calculate the interpolations. If none is provided the function uses
#' #' the default chronology from the object (see
#' #' \code{\link[EPDr:gieseckeDefaultChronology]{gieseckeDefaultChronology}}).
#' #'
#' #' @return The function returns a \code{\link[EPDr:agedcounts]{agedcounts}} object, similar
#' #' to \code{agedcounts} in which ages and counts has been modified to the
#' #' time intervarls specified and the counts estimated for these periods.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' #' # agedcounts.1.int <- intervalsCounts(agedcounts.1, tmin=seq(0, 21000, by=1000),
#' #' # tmax=seq(999, 21999, by=1000))
#' #' #
#' #' # agedcounts.3 <- getAgedCounts(3, connEPD)
#' #' # agedcounts.3.int <- intervalsCounts(agedcounts.3, tmin=seq(0, 21000, by=1000),
#' #' # tmax=seq(999, 21999, by=1000))
#' #' # agedcounts.3.int <- intervalsCounts(agedcounts.3, tmin=seq(0, 21000, by=1000),
#' #' # tmax=seq(999, 21999, by=1000), 2)
#' #' 
#' intervalsCounts <- function(agedcounts, tmin, tmax, labels=NULL, chronology=NULL){
#'   if(!class(agedcounts) == "agedcounts"){
#'     stop("Agedcount has to be an 'agedcounts' object. See ?getAgedCounts")
#'   }
#'   if(length(tmin) != length(tmax)){
#'     stop("length(tmin) != length(tmax). Please, specify two vectors of the same length")
#'   }
#'   
#'   if(is.null(labels)){
#'     labels <- paste(tmin, "-", tmax, sep="")
#'   }
#'   
#'   if(is.null(chronology)){
#'     chronology <- agedcounts@ages@default_chronology
#'   }
#'   if(chronology == -9999){
#'     chronology <- "giesecke"
#'   }
#'   
#'   sample.ages <- agedcounts@ages@depth_ages[,as.character(chronology)]
#'   sample.depthcm <- agedcounts@counts@depthcm
#'   sample.id <- agedcounts@counts@sample_
#'   sample.counts <- agedcounts@counts@counts
#'   
#'   index1 <- which(!is.na(sample.ages))
#'   index2 <- 1:length(sample.id)
#'   index <- intersect(index2, index1) 
#'   
#'   sample.ages <- sample.ages[index]
#'   sample.depthcm <- sample.depthcm[index]
#'   sample.id <- sample.id[index]
#'   sample.counts <- sample.counts[index,]
#'   
#'   .is.between <- function(x, a, b) {
#'     x >= a & x <= b
#'   }    
#'   
#'   index <- mapply(function(a, b, x){.is.between(x, a, b)}, tmin, tmax, MoreArgs=list(sample.ages))
#'   if(is.vector(index)){
#'     intervalid <- which(index)
#'     index <- 1
#'   }else{
#'     intervalid <- unlist(apply(index, MARGIN=1, FUN=which))
#'     index <- unlist(apply(index, MARGIN=2, FUN=which))
#'   }
#'   
#'   range.agedcounts <- agedcounts
#'   
#'   output.depthcm <- rep(NA, length(labels))
#'   output.ages <- rowMeans(cbind(tmin, tmax), na.rm=TRUE)
#'   output.counts <- as.data.frame(matrix(NA, nrow=length(labels), ncol=ncol(sample.counts)))
#'   colnames(output.counts) <- colnames(sample.counts)
#'   
#'   if(length(index) == 0 | !all(index %in% range.agedcounts@counts@sample_)){
#'   }else{
#'     range.counts <- sample.counts[index,]
#'     range.depthcm <- sample.depthcm[index]
#'     range.ages <- sample.ages[index]
#'     
#'     range.means <- apply(range.counts, MARGIN=2, FUN=function(x, y, z){stats::aggregate(x, by=list(y=y), FUN=z)}, intervalid, mean)
#'     range.means <- reshape2::dcast(reshape2::melt(range.means, id.vars="y"), y ~ L1)
#'     range.means <- range.means[,-which(colnames(range.means) == "y")]
#'     
#'     range.depth.means <- stats::aggregate(range.depthcm, by=list(y=intervalid), FUN=mean)
#'     range.depth.means <- range.depth.means$x
#'     
#'     range.ages.means <- stats::aggregate(range.ages, by=list(y=intervalid), FUN=mean)
#'     range.ages.means <- range.ages.means$x
#'     
#'     output.counts[unique(intervalid),] <- range.means
#'     output.depthcm[unique(intervalid)] <- range.depth.means
#'     output.ages[unique(intervalid)] <- range.ages.means
#'   }
#'   
#'   range.agedcounts@counts@counts_processing <- factor("Ranged means", levels=c("Samples","Interpolated", "Ranged means"))
#'   range.agedcounts@counts@sample_ <- 10000 + 1:length(labels)
#'   range.agedcounts@counts@sample_label <- labels
#'   
#'   range.agedcounts@counts@default_ages <- output.ages
#'   range.agedcounts@counts@depthcm <- as.numeric(output.depthcm)
#'   range.agedcounts@counts@counts <- output.counts
#'   
#'   return(range.agedcounts)    
#' }



#' #' Change taxa to accepted taxa names
#' #' 
#' #' This function modifies the taxa names in the \code{@@counts} slot of EPDr objects
#' #' (\code{\link[EPDr:agedcounts]{agedcounts}} and \code{\link[EPDr:counts]{counts}}).
#' #' More specifically this function compares the taxa name with the taxonomy of the EPD
#' #' to use the accepted names. If these changes result in duplicated columns of the same
#' #' taxa their values are unified by summing them.
#' #'
#' #' @param counts \code{\link[EPDr:agedcounts]{agedcounts}} or
#' #' \code{\link[EPDr:counts]{counts}} objects with a \code{@@counts} slot.
#' #' @param epd_taxonomy Data frame with the taxonomy from the EPD as from the
#' #' \code{\link[EPDr:getTaxonomyEPD]{getTaxonomyEPD}} function.
#' #'
#' #' @return \code{\link[EPDr:agedcounts]{agedcounts}} or \code{\link[EPDr:counts]{counts}}
#' #' object with new taxa names.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' #' # agedcounts.1@counts@taxa_names
#' #' # colnames(agedcounts.1@counts@counts)
#' #' # agedcounts.1.acc <- taxa2AcceptedTaxa(agedcounts.1, getTaxonomyEPD(epd.connection))
#' #' # agedcounts.1.acc@counts@taxa_names
#' #' # colnames(agedcounts.1.acc@counts@counts)
#' #' 
#' taxa2AcceptedTaxa <- function(counts, epd_taxonomy){
#'   if(!class(counts) %in% c("counts", "agedcounts")){
#'     stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
#'   }
#'   if(class(counts) == "agedcounts"){
#'     counts.tmp <- counts@counts
#'   }else{
#'     counts.tmp <- counts
#'   }
#'   
#'   taxa_names <- counts.tmp@taxa_names
#'   taxa_ <- counts.tmp@taxa_
#'   taxa_acc <- counts.tmp@taxa_accepted
#'   
#'   new_taxa_type <- factor("Accepted", levels=c("Samples", "Accepted", "Higher"))
#'   
#'   new_taxa_names <- epd_taxonomy$varname[match(taxa_acc, epd_taxonomy$var_)]
#'   
#'   new_counts <- counts.tmp@counts
#'   colnames(new_counts) <- new_taxa_names
#'   
#'   if(nrow(new_counts) == 0){
#'     new_counts <- new_counts[,1:length(new_taxa_names)]
#'     colnames(new_counts) <- sort(unique(new_taxa_names))
#'   }else{
#'     new_counts$sample_ <- 1:nrow(new_counts)
#'     new_counts <- reshape2::melt(new_counts, id.vars=c("sample_"))
#'     new_counts <- reshape2::dcast(new_counts, sample_ ~ variable, fun.aggregate=sum, value.var="value")[,-1]
#'   }
#'   
#'   new_taxa_names <- colnames(new_counts)
#'   new_taxa_ <- epd_taxonomy$var_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_acc <- epd_taxonomy$accvar_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_mhvar <- epd_taxonomy$mhvar_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_groupid <- epd_taxonomy$groupid[match(new_taxa_names, epd_taxonomy$varname)]
#'   
#'   counts.tmp@counts <- new_counts 
#'   counts.tmp@taxa_type <- new_taxa_type
#'   counts.tmp@taxa_names <- new_taxa_names
#'   counts.tmp@taxa_ <- new_taxa_
#'   counts.tmp@taxa_accepted <- new_taxa_acc
#'   counts.tmp@taxa_mhvar <- new_taxa_mhvar
#'   counts.tmp@taxa_groupid <- new_taxa_groupid
#'   
#'   if(class(counts) == "agedcounts"){
#'     counts@counts <- counts.tmp
#'   }else{
#'     counts <- counts.tmp
#'   }
#'   return(counts)
#' }
#' 


#' #' Expand EPDr objects with new taxa
#' #' 
#' #' This functions modifies EPDr objects with \code{@@counts} slot
#' #' (\code{\link[EPDr:agedcounts]{agedcounts}} or \code{\link[EPDr:counts]{counts}})
#' #' to filter taxa columns to match \code{taxa_list}. The function add empty columns
#' #' (\code{NA}) if a new taxa is defined int \code{taxa_list} or remove columns for the 
#' #' taxa not included in \code{taxa_list}. The function may look useless for a single 
#' #' entity but it is useful when standardizing data from multiple entities.
#' #'
#' #' @param counts \code{\link[EPDr:agedcounts]{agedcounts}} or
#' #' \code{\link[EPDr:counts]{counts}} objects to be modified.
#' #' @param taxa_list Character vector indicating the new taxa in the \code{@@counts} slot. 
#' #' @param epd_taxonomy Data frame with the taxonomy from the EPD as from the
#' #' \code{\link[EPDr:getTaxonomyEPD]{getTaxonomyEPD}} function.
#' #'
#' #' @return \code{\link[EPDr:agedcounts]{agedcounts}} or
#' #' \code{\link[EPDr:counts]{counts}} objects with the modified \code{@@counts} slot.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' #' # agedcounts.1@counts@taxa_names
#' #' # colnames(agedcounts.1@counts@counts)
#' #' # agedcounts.1.ft <- filterTaxa(agedcounts.1,
#' #' # c(agedcounts.1@counts@taxa_names, "prueba"), getTaxonomyEPD(epd.connection))
#' #' # colnames(agedcounts.1.ft@counts@counts)
#' #' # agedcounts.1.ft@counts@taxa_names
#' #' # head(agedcounts.1.ft@counts@counts)
#' #' 
#' filterTaxa <- function(counts, taxa_list, epd_taxonomy){
#'   if(!class(counts) %in% c("counts", "agedcounts")){
#'     stop("Counts has to be a 'counts' or 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
#'   }
#'   if(class(counts) == "agedcounts"){
#'     counts.tmp <- counts@counts
#'   }else{
#'     counts.tmp <- counts
#'   }
#'   
#'   site_taxa <- counts.tmp@taxa_names
#'   diff_names <- setdiff(taxa_list, site_taxa)
#'   
#'   if(nrow(counts.tmp@counts) == 0){
#'     new_counts <- data.frame(matrix(ncol=length(taxa_list), nrow=0))
#'     colnames(new_counts) <- taxa_list
#'   }else{
#'     new_counts <- counts.tmp@counts
#'     new_counts[,diff_names] <- NA
#'   }
#'   
#'   new_counts <- new_counts[,taxa_list]
#'   
#'   new_taxa_type <- factor("Expanded", levels=c("Samples", "Accepted", "Higher"))
#'   
#'   new_taxa_names <- colnames(new_counts)
#'   new_taxa_ <- epd_taxonomy$var_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_acc <- epd_taxonomy$accvar_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_mhvar <- epd_taxonomy$mhvar_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_groupid <- epd_taxonomy$groupid[match(new_taxa_names, epd_taxonomy$varname)]
#'   
#'   counts.tmp@counts <- new_counts 
#'   counts.tmp@taxa_type <- new_taxa_type
#'   counts.tmp@taxa_names <- new_taxa_names
#'   counts.tmp@taxa_ <- new_taxa_
#'   counts.tmp@taxa_accepted <- new_taxa_acc
#'   counts.tmp@taxa_mhvar <- new_taxa_mhvar
#'   counts.tmp@taxa_groupid <- new_taxa_groupid
#'   
#'   if(class(counts) == "agedcounts"){
#'     counts@counts <- counts.tmp
#'   }else{
#'     counts <- counts.tmp
#'   }
#'   return(counts)
#' }



#' #' Change taxa to higher taxa level
#' #' 
#' #' This function modifies the taxa names in the \code{@@counts} slot of EPDr objects
#' #' (\code{\link[EPDr:agedcounts]{agedcounts}} and \code{\link[EPDr:counts]{counts}}).
#' #' More specifically this function compares the taxa name with the taxonomy of the EPD
#' #' to use the higher taxa names. If these changes result in duplicated columns of the same
#' #' taxa their values are unified by summing them.
#' #'
#' #' @param counts \code{\link[EPDr:agedcounts]{agedcounts}} or
#' #' \code{\link[EPDr:counts]{counts}} objects with a \code{@@counts} slot.
#' #' @param epd_taxonomy Data frame with the taxonomy from the EPD as from the
#' #' \code{\link[EPDr:getTaxonomyEPD]{getTaxonomyEPD}} function.
#' #'
#' #' @return \code{\link[EPDr:agedcounts]{agedcounts}} or \code{\link[EPDr:counts]{counts}}
#' #' object with new taxa names.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' #' # agedcounts.1@counts@taxa_names
#' #' # colnames(agedcounts.1@counts@counts)
#' #' # agedcounts.1.hn <- taxa2HigherTaxa(agedcounts.1, getTaxonomyEPD(epd.connection))
#' #' # agedcounts.1.hn@counts@taxa_names
#' #' # colnames(agedcounts.1.hn@counts@counts)
#' #' 
#' taxa2HigherTaxa <- function(counts, epd_taxonomy){
#'   
#'   if(!class(counts) %in% c("counts", "agedcounts")){
#'     stop("Counts has to be a 'counts' object or an 'agedcounts' object. See ?getCounts and ?getAgedCounts functions.")
#'   }
#'   if(class(counts) == "agedcounts"){
#'     counts.tmp <- counts@counts
#'   }else{
#'     counts.tmp <- counts
#'   }
#'   
#'   taxa_names <- counts.tmp@taxa_names
#'   taxa_ <- counts.tmp@taxa_
#'   taxa_mhvar <- counts.tmp@taxa_mhvar
#'   
#'   taxa_mhvar[which(is.na(taxa_mhvar))] <- taxa_[which(is.na(taxa_mhvar))]
#'   
#'   new_taxa_type <- factor("Higher", levels=c("Samples", "Accepted", "Higher"))
#'   
#'   new_taxa_names <- epd_taxonomy$varname[match(taxa_mhvar, epd_taxonomy$var_)]
#'   
#'   new_counts <- counts.tmp@counts
#'   colnames(new_counts) <- new_taxa_names
#'   
#'   if(nrow(new_counts) == 0){
#'     new_counts <- new_counts[,1:length(new_taxa_names)]
#'     colnames(new_counts) <- sort(unique(new_taxa_names))
#'   }else{
#'     new_counts$sample_ <- 1:nrow(new_counts)
#'     new_counts <- reshape2::melt(new_counts, id.vars=c("sample_"))
#'     new_counts <- reshape2::dcast(new_counts, sample_ ~ variable, fun.aggregate=sum, value.var="value")[,-1]
#'   }
#'   
#'   new_taxa_names <- colnames(new_counts)
#'   new_taxa_ <- epd_taxonomy$var_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_acc <- epd_taxonomy$accvar_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_mhvar <- epd_taxonomy$mhvar_[match(new_taxa_names, epd_taxonomy$varname)]
#'   new_taxa_groupid <- epd_taxonomy$groupid[match(new_taxa_names, epd_taxonomy$varname)]
#'   
#'   counts.tmp@counts <- new_counts 
#'   counts.tmp@taxa_type <- new_taxa_type
#'   counts.tmp@taxa_names <- new_taxa_names
#'   counts.tmp@taxa_ <- new_taxa_
#'   counts.tmp@taxa_accepted <- new_taxa_acc
#'   counts.tmp@taxa_mhvar <- new_taxa_mhvar
#'   counts.tmp@taxa_groupid <- new_taxa_groupid
#'   
#'   if(class(counts) == "agedcounts"){
#'     counts@counts <- counts.tmp
#'   }else{
#'     counts <- counts.tmp
#'   }
#'   return(counts)
#' }
#' 





#' #' Blois quality index for palynological samples
#' #' 
#' #' This function apply the quality index described in Blois et al. (2013).
#' #' 
#' #' From the Ecography 2013 paper, Appendix 3: "For each site at a particular 1 kyr
#' #' time period, site data-quality was calculated as the mean normalized distance
#' #' of the nearest pollen sample and the nearest chronological control.  We
#' #' calculated the distance in years of the nearest pollen sample and the nearest
#' #' chronological control to each 1 kyr time period.  We eliminated sites where
#' #' the nearest pollen sample was over 2000 years away or the nearest
#' #' chronological control was over 5000 years away.  For the remaining sites in
#' #' each 1 kyr period, we created a summary measure of site data-quality by 
#' #' rescaling the two distances in years to a 0 - 1 scale and calculating the
#' #' mean.  For example, if the nearest sample to the 1 kyr BP time period at a
#' #' given site was at 1.050 kyr BP and the nearest chronological control was at
#' #' 1.100 kyr BP, the raw distances would be 50 years and 100 years, respectively.
#' #' These equate to scaled values of 0.975 (i.e., 1 - 50/2000) and 0.98 (i.e., 1 -
#' #' 100/5000) for sample and chronological quality, respectively, with a mean
#' #' data-quality for this site at the 1 kyr BP time period of 0.9775."
#' #' 
#' #' To replicate the calculation the function allows to specify different maximum
#' #' distances as parameters of the function.
#' #' 
#' #' @param agedcounts \code{\link[EPDr:agedcounts]{agedcounts}} object.
#' #' @param datation \code{\link[EPDr:datation]{datation}} object for the same entity
#' #' in \code{agedcounts}. This is mandatory to calculate distances between samples and 
#' #' control points.
#' #' @param max_sample_dist Maximum numeric distance to be considered to the
#' #' palynological samples for interpolated or ranged data.
#' #' @param max_c14_dist Maximum numeric distance to be considered to the (C14) control
#' #' points.
#' #'
#' #' @return \code{\link[EPDr:agedcounts]{agedcounts}} object with no empty
#' #' \code{@@ages@@data_quality} slot.
#' #' 
#' #' @references Blois, J.L, J.W. Williams, M.C. Fitzpatrick, S. Ferrier, S.D. Veloz, F. He, Z. Liu, G. Manion, and Bette Otto-Bliesner (2013). Modeling the Climatic Drivers of Spatial Patterns in Vegetation Composition since the Last Glacial Maximum. Ecography 36(4): 460-473. doi:10.1111/j.1600-0587.2012.07852.x.
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #' # user="epdr", password="epdrpw")
#' #' # agedcounts.1 <- getAgedCounts(1, epd.connection)
#' #' # datation.1 <- getDatation(1, epd.connection)
#' #' # agedcounts.1.qi <- bloisQualityIndex(agedcounts.1, datation.1)
#' #' # agedcounts.1.qi@ages@data_quality
#' #' # 
#' #' # agedcounts.1.ran <- intervalsCounts(agedcounts.1, tmin=seq(0, 21000, by=1000),
#' #' # tmax=seq(999, 21999, by=1000))
#' #' # agedcounts.1.ran.qi <- bloisQualityIndex(agedcounts.1.ran, datation.1)
#' #' # agedcounts.1.ran.qi@ages@data_quality
#' #' # 
#' #' # t <- c(seq(0, 21000, by=500))
#' #' # agedcounts.1.int <- interpolateCounts(agedcounts.1, t)
#' #' # agedcounts.1.int.qi <- bloisQualityIndex(agedcounts.1.int, datation.1)
#' #' # agedcounts.1.int.qi@ages@data_quality
#' bloisQualityIndex <- function(agedcounts, datation, max_sample_dist=2000, max_c14_dist=5000){
#'   # agedcounts <- counts.wa.uni[[44]]
#'   # datation <- datation.co.wa.uni[[44]]
#'   # max_sample_dist <- 2000
#'   # max_c14_dist <- 5000
#'   
#'   if(!class(agedcounts) %in% c("agedcounts")){
#'     stop("Agedcounts has to be an 'agedcounts' object. See ?getAgedCounts.")
#'   }
#'   if(!class(datation) %in% c("datation")){
#'     stop("Datation has to be a 'datation' object. See ?getDatation.")
#'   }
#'   
#'   ages <- agedcounts@ages
#'   counts <- agedcounts@counts
#'   
#'   e_ <- ages@e_
#'   sample.ages <- ages@depth_ages
#'   
#'   data.ages <- counts@default_ages
#'   
#'   nchron <- datation@chronology@number_of_chronologies
#'   agebasis <- datation@chronology@agebasis
#'   
#'   .mindiff <- function(x, y){
#'     sorted.y <- sort(y)
#'     myfun <- stats::stepfun(sorted.y, 0:length(y))
#'     indices <- pmin(pmax(1, myfun(x)), length(sorted.y) - 1)
#'     mindist <- pmin(abs(x - sorted.y[indices]), abs(x - sorted.y[indices + 1]))
#'     return(mindist)
#'   }
#'   
#'   .c14Diff <- function(ii, x, y, max_diff){
#'     y.ii <- y[y$chron_ == ii, "age"]
#'     diff <- .mindiff(x, y.ii)
#'     if(!is.null(max_diff)){
#'       diff <- pmin(diff, max_diff)
#'     }
#'     return(diff)
#'   }
#'   
#'   .sampleDiff <- function(ii, x, y, max_diff){
#'     y.ii <- y[,as.character(ii)]
#'     diff <- .mindiff(x, y.ii)
#'     if(!is.null(max_diff)){
#'       diff <- pmin(diff, max_diff)
#'     }
#'     return(diff)
#'   }
#'   
#'   c14.dist <- sapply(1:nchron, .c14Diff, data.ages, agebasis, max_c14_dist)
#'   sample.dist <- sapply(1:nchron, .sampleDiff, data.ages, sample.ages, max_sample_dist)
#'   
#'   c14.dist <-  1 - (c14.dist/max_c14_dist)
#'   sample.dist <- 1 - (sample.dist/max_sample_dist)
#'   
#'   data.quality <- as.data.frame((c14.dist + sample.dist) / 2)
#'   colnames(data.quality) <- 1:nchron
#'   
#'   agedcounts@ages@data_quality <- data.quality
#'   
#'   return(agedcounts)
#' }
#' 
