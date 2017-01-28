#' List taxa groups in the EPD
#' 
#' This function looks into the database and returns all information in the GROUPS table (see
#' documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' This is useful if the user want to restrict searches for particular groups of particles
#' in the palynological samples. For instance, if somebody is only interested in Algae. Because 
#' the function return the whole table, it only requires a valid connection to the database server.
#' 
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function returns a data frame with three columns: \code{groupid}, \code{groupcode}, and \code{groupname}.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listTaxagroups(epd.connection)
#' # disconnectFromEPD(epd.connection)
#' 
listTaxagroups <- function(connection){
    sqlQuery <- paste("SELECT * FROM groups;")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}


#' List taxa in the EPD
#' 
#' This function looks into the database and returns combined information in the P_VARS and
#' P_GROUP tables (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' The function allows to restrict the list to taxa in an specific group, using parameter
#' \code{group_id} along with a valid connection to the database.
#' 
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' @param group_id character. Character vector with group ids (four character strings; see
#' \code{\link[EPDr:listTaxagroups]{listTaxagroups}} to check correspondece between groups and
#' group ids). The user can specify as many groups as desired in a single call of the function.
#'
#' @return data.frame The function returns a data frame with five columns: \code{var_},
#' \code{varname}, \code{varcode}, \code{mhvar}, and \code{groupid}.
#' \itemize{
#'  \item{"var_ is"}{ the taxa identification number in the database.}
#'  \item{"varname"}{ is the taxa name.}
#'  \item{"varcode"}{ is the short taxa name.}
#'  \item{"mhvar"}{ is the identification number of the higher taxonomical level.}
#'  \item{"groupid"}{ is the identification number of the group that the taxa belongs to.}
#' }
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listTaxa(epd.connection, "HERB")
#' # listTaxa(epd.connection, c("HERB","TRSH"))
#' # disconnectFromEPD(epd.connection)
#' 
listTaxa <- function(connection, group_id=NULL){
    if(is.null(groups)){
        groups <- listTaxagroups(connection)$groupid
    }
    groups.c <- paste(group_id, collapse="','")
    sqlQuery <- paste("SELECT var_, varname, varcode, mhvar_, groupid FROM p_vars NATURAL JOIN p_group WHERE groupid IN ('", groups.c, "') ORDER BY varname;", sep="")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}


#' List countries in the EPD
#' 
#' This function looks into the database and returns all information in the POLDIV1 table (see
#' documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' This belong to the country level in political and administrative level. Because the function
#' returns a whole table it only need a parameter with a valid connection to the database.
#' 
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#'
#' @return data.frame The function returns a data frame with two columns: \code{poldiv1} and \code{name}.
#' \itemize{
#'   \item{"poldiv1"}{ is a three letter code for each country.}
#'   \item{"name"}{ is the full country name.}
#' }
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listCountries(epd.connection)
#' # disconnectFromEPD(epd.connection)
#' 
listCountries <- function(connection){
    sqlQuery <- paste("SELECT * FROM poldiv1;")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}


#' List (administrative) regions in the EPD
#' 
#' This function looks into the database and returns all combined information in the POLDIV1 and
#' POLDIV2 tables (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' Although we tried to be consistent with names in the database, here we provided both country and 
#' region names for each record, which required some name tuning compared to the database.
#' Because the function returns a whole table it only need a parameter with a valid connection
#' to the database. The list can be restricted to specific countries using parameter \code{country}.
#' 
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' @param country Character vector indicating the three letters code or the full name of the desired
#' countries.
#'
#' @return data.frame with four columns:
#' \itemize{
#'   \item{"poldiv2"}{Region code consisting of two digit integer. This code is not a key field in 
#'   the database and thus can be two regions with the same poldiv2 value. This field allows to
#'   differentiate regions within the same country.}
#'   \item{"poldiv1"}{Country code consisting of three capital letters. This code is unique for
#'   each country.}
#'   \item{"regionname"}{Region full name.}
#'   \item{"countryname"}{Country full name.}
#' }
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listRegions(epd.connection)
#' # listRegions(epd.connection, "ESP")
#' # listRegions(epd.connection, "Spain")
#' # listRegions(epd.connection, c("Spain", "France", "Germany"))
#' # disconnectFromEPD(epd.connection)
#' 
listRegions <- function(connection, country=NULL){
    if(is.null(country)){
        countries <- listCountries(connection)$poldiv1
    }
    countries <- paste(country, collapse="','")
    sqlQuery <- paste("SELECT a.poldiv2, a.poldiv1, a.name AS regionname, b.name AS countryname FROM poldiv2 a JOIN poldiv1 b ON a.poldiv1 = b.poldiv1 WHERE b.poldiv1 IN ('", countries, "') OR b.name IN ('", countries, "');", sep="")
    RPostgreSQL::dbGetQuery(connection, sqlQuery)
}


#' List sites in the EPD
#' 
#' This function looks into the database and returns all combined information in the SITELOC and
#' SITEDESC tables (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}). This information refers to the sites where palynological samples were taken.
#' Because the function returns a whole table, only a valid connection to the database is mandatory.
#' However, the list of sites can be further restricted to specific countries, administrative
#' regions, or a geographical region defined by geographical coordinates (longitude and latitude)
#' of its corners.
#'
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' @param country Character vector indicating the three letters code or the full name of the
#' desired countries.
#' @param region Character vector indicating two digit integer code or the full name of the
#' desired regions.
#' @param coords Vector of four numeric values, indicating the spatial extent in decimal degrees.
#' The extent should be specified using the following format \code{c(xmin, xmax, ymin, ymax)}.
#' Where \code{x} represents longitude and \code{y} represents latitude.
#'
#' @return Data frame (data.frame) with eleven columns:
#' \itemize{
#'   \item{"site_"}{Site identification number.}
#'   \item{"sitename"}{Site full name.}
#'   \item{"sitecode"}{Site code.}
#'   \item{"latdd"}{Latitude of the site in decimal degrees.}
#'   \item{"londd"}{Longitude of the site in decimal degrees.}
#'   \item{"elevation"}{Elevation of the site in meters.}
#'   \item{"areaofsite"}{Area of the site in hectares.}
#'   \item{"sitedescript"}{Description of the nature of the site.}
#'   \item{"physiography"}{Description of the physiography of the site.}
#'   \item{"surroundveg"}{Description of the surrounding vegetation of the site.}
#'   \item{"vegformation"}{Name of the vegetation formation in the site.}
#' }
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listSites(epd.connection)
#' # listSites(epd.connection, "ESP")
#' # listSites(epd.connection, "Spain", "01")
#' # summary(listSites(epd.connection))
#' # listSites(epd.connection, coords=c(2.5, 22, 45.5, 53.5))
#' # summary(listSites(epd.connection, coords=c(2.5, 22, 45.5, 53.5)))
#' # disconnectFromEPD(epd.connection)
#' 
listSites <- function(connection, country=NULL, region=NULL, coords=NULL){
    if(length(country) == 0 & !is.null(region)){
        stop("No country specified: If you want to specify a region you have to provide also the country.")
    }
    if(length(country) > 1 & !is.null(region)){
        warning("Multiple countries specified with region: Providing region for multiple countries might result in undesired results. Provide region only for queries with single countries.")
    }
    
    if(is.null(country)){
      country <- listCountries(connection)$poldiv1
    }else{
        tmp <- listCountries(connection)
        country <- tmp[tmp$poldiv1 %in% country | tmp$name %in% country, "poldiv1"]
    }
    countries <- paste(country, collapse="','")
    
    if(is.null(region)){
        region <- listRegions(connection, country)$poldiv2
    }else{
        tmp <- listRegions(connection, country)
        region <- tmp[tmp$regionname %in% region | tmp$poldiv2 %in% region, "poldiv2"]
    }
    regions <- paste(region, collapse="','")
    
    if(is.null(coords)){
        sqlQuery <- paste("SELECT site_, sitename, sitecode, latdd, londd, elevation, areaofsite, sitedescript, physiography, surroundveg, vegformation FROM siteloc NATURAL LEFT JOIN sitedesc WHERE poldiv1 IN ('", countries, "') AND poldiv2 IN ('", regions, "');", sep = "")
    }else{
        xmin <- coords[1]
        xmax <- coords[2]
        ymin <- coords[3]
        ymax <- coords[4]
        sqlQuery <- paste("SELECT site_, sitename, sitecode, latdd, londd, elevation, areaofsite, sitedescript, physiography, surroundveg, vegformation FROM siteloc NATURAL LEFT JOIN sitedesc WHERE poldiv1 IN ('", countries, "') AND poldiv2 IN ('", regions, "') AND (londd BETWEEN ",  xmin, " AND ", xmax, ") AND (latdd BETWEEN ", ymin, " AND ", ymax, ");", sep = "")
    }
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}


#' List publications in the EPD
#' 
#' This function looks into the database and returns all combined information in the PUBLENT and
#' PUBL tables (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' This information refers to the original publications in which entities were described.
#' Because the function returns a whole table, only a valid connection to the database is mandatory.
#' However, the list of publications can be further restricted to specific entities, countries,
#' administrative regions, sites, or a geographical region defined by geographical
#' coordinates (longitude and latitude) of its corners.
#'
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' @param e_ Numeric vector indicating the desired entities identification number. 
#' @param country Character vector indicating the three letters code or the full name of the
#' desired countries.
#' @param region Character vector indicating two digit integer code or the full name of the
#' desired regions.
#' @param site Vector indicating the desired sites. Sites can be defined by the site identification
#' number, the site code, or the site name.
#' @param coords Vector of four numeric values, indicating the spatial extent in decimal degrees.
#' The extent should be specified using the following format \code{c(xmin, xmax, ymin, ymax)}.
#' Where \code{x} represents longitude and \code{y} represents latitude.
#'
#' @return Data frame with 5 columns:
#' \itemize{
#'   \item{"publ_"}{Publication identification number.}
#'   \item{"e_"}{Entity identification number.}
#'   \item{"acc_"}{Short integers for local library accession number}
#'   \item{"yearofpubl"}{Year of publication.}
#'   \item{"citation"}{Complete bibliographic reference in the style of the journal "Ecology".}
#' }
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listPubl(epd.connection)
#' # listPubl(epd.connection, e_=1)
#' # listPubl(epd.connection, site="1")
#' # listPubl(epd.connection, country="ESP", site=1)
#' # listPubl(epd.connection, country="Spain", region="01")
#' # str(listPubl(epd.connection))
#' # listPubl(epd.connection, coords=c(2.5, 22, 45.5, 53.5))
#' # str(listPubl(epd.connection, coords=c(2.5, 22, 45.5, 53.5)))
#' # disconnectFromEPD(epd.connection)
#' 
listPubl <- function(connection, e_=NULL, country=NULL, region=NULL, site=NULL, coords=NULL){
    if(is.null(e_)){
        e_ <- listE(connection=connection, country=country, region=region, site=site, coords=coords)$e_
    }
    e_ <- paste(e_, collapse="','")
    sqlQuery <- paste("select * FROM publent NATURAL JOIN publ WHERE e_ IN ('", e_, "');", sep = "") 
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}

#' List entities in the EPD
#' 
#' This function allows to search entities records by different search criteria that can be
#' combined as desired.
#'
#' @param connection A valid connection to the database see
#' \code{\link[EPDr:connectToEPD]{connectToEPD}}.
#' @param site Numeric or character vector indicating the site identification number or
#' the site name to which entities will be retrieved.
#' @param coords Numeric vector with four elements indicating the geographical coordinates
#' to which the search is limited.
#' @param lastname Character vector indicating the last name of data contributor for which
#' entities will be retrieved.
#' @param firstname Character vector indicating the first name of data contributor
#' for which entities will be retrieved.
#' @param initials Character vector indicating the initials of data contributor for which
#' entities will be retrieved.
#' @param publ Numeric vector indicating the publication identification number
#' to which search is constrained.
#' @param country Character vector indicating the three letters code or the full
#' english name for the countries to which entities are desired.
#' @param region  Character vector indicating the two letters code or the full
#' name for the regions to which entities are desired. To specify a region you have to
#' specify one, and only one, country. You can specify multiple countries but the results 
#' can be funky.
#' @param restrictions Character letter ("U" or "R") specifying the usestatus of the data 
#' for the entities that are searched. U is for unrestricted and R is for restricted. 
#' @param logical_operator Character string indicating whether the effects of 
#' search criteria are additive ("OR") or substracting ("AND"). If "OR" is specified,
#' entities complying with any of the search criteria will be returned. If "AND" is
#' specified, only entities that comply with all search criteria will be returned. 
#' 
#' @return This function return a data.frame with columns as in the \code{entity} table 
#' of the EPD database (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}). If search criteria are all empty, the function returns the whole
#' table. If criteria are specified the table is complemented with columns from diferent
#' tables in the database that store the required information to filter entities
#' accordingly.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listE(epd.connection)
#' # listE(epd.connection, site=1)
#' # listE(epd.connection, site="Adange")
#' # listE(epd.connection, site=c(1:10))
#' # listE(epd.connection, country="ESP")
#' # listE(epd.connection, country="Spain")
#' # listE(epd.connection, country="Spain", region="Andalucia")
#' # listE(epd.connection, lastname="Tzedakis")
#' # listE(epd.connection, restrictions="R")
#' 
listE <- function(connection, site=NULL, coords=NULL, lastname=NULL, firstname=NULL,
                  initials=NULL, publ=NULL, country=NULL, region=NULL,
                  restrictions=NULL, logical_operator="AND"){
  if(all(is.null(country), !is.null(region))){stop("Because regions are not unique
                                                      in the database you need to specify a country in which to look for specific regions.")}
  
  joinQuery <- NULL
  
  if(any(!is.null(site), !is.null(coords), !is.null(country))){
    sitelocJoin <- "JOIN siteloc b ON a.site_ = b.site_"
    joinQuery <- append(joinQuery, sitelocJoin)
    if(!is.null(country)){
      poldiv1Join <- "JOIN poldiv1 f ON b.poldiv1 = f.poldiv1"
      joinQuery <- append(joinQuery, poldiv1Join)
      if(!is.null(region)){
        poldiv2Join <- "JOIN poldiv2 g ON b.poldiv1 = g.poldiv1 AND b.poldiv2 = g.poldiv2"
        joinQuery <- append(joinQuery, poldiv2Join)
      }
    }
  }
  if(any(!is.null(lastname), !is.null(firstname), !is.null(initials))){
    workersJoin <- "JOIN workers c ON a.coll_ = c.worker_"
    joinQuery <- append(joinQuery, workersJoin)
  }
  if(!is.null(publ)){
    publentJoin <- "JOIN publent d ON a.e_ = d.e_ JOIN publ e ON d.publ_ = e.publ_"
    joinQuery <- append(joinQuery, publentJoin)
  }
  if(!is.null(restrictions)){
    restrictionsSelect <- "JOIN p_entity h ON a.e_ = h.e_"
    joinQuery <- append(joinQuery, restrictionsSelect)
  }
  
  selectQuery <- NULL
  
  if(!is.null(site)){
    if(all(class(site) != "numeric", class(site) != "character")){
      stop("site has to be a number, a character or a vector of the formers.")
    }    
    if(is.character(site)){
      site <- paste(site, collapse="','")
      siteSelect <- paste("b.sitename IN ('", site, "')", sep="")
    }else{
      site <- paste(site, collapse="','")
      siteSelect <- paste("b.site_ IN ('", site, "')", sep="")
    }
    selectQuery <- append(selectQuery, siteSelect)
  }
  
  if(!is.null(coords)){
    if(!is.numeric(coords)){
      stop("Object coord has to be a numeric vector with 4 elements: xmin, xmax, ymin, and ymax")
    }
    if(length(coords) != 4){
      stop("Object coord has to have exactly 4 elements: xmin, xmax, ymin, and ymax")
    }
    xmin <- coords[1]
    xmax <- coords[2]
    ymin <- coords[3]
    ymax <- coords[4]
    coordsSelect <- paste("b.londd BETWEEN ",  xmin, " AND ", xmax, " AND b.latdd BETWEEN ", ymin, " AND ", ymax, sep="")
    
    selectQuery <- append(selectQuery, coordsSelect)
  }
  
  if(!is.null(lastname)){
    if(class(lastname) != "character"){stop("lastname has to be a character string")}
    lastname <- paste(lastname, collapse="','")
    lastnameSelect <- paste("c.lastname IN ('", lastname, "')", sep="")
    selectQuery <- append(selectQuery, lastnameSelect)
  }
  if(!is.null(firstname)){
    if(class(firstname) != "character"){stop("firstname has to be a character string")}
    firstname <- paste(firstname, collapse="','")
    firstnameSelect <- paste("c.firstname IN ('", firstname, "')", sep="")
    selectQuery <- append(selectQuery, firstnameSelect)
  }
  if(!is.null(initials)){
    if(class(initials) != "character"){stop("initials has to be a character string")}
    initials <- paste(initials, collapse="','")
    initialsSelect <- paste("c.initials IN ('", initials, "')", sep="")
    selectQuery <- append(selectQuery, initialsSelect)
  }
  
  if(!is.null(publ)){
    if(class(publ) != "numeric"){stop("publ has to be a numeric indicating the publication identification number.")}
    publ <- paste(publ, collapse="','")
    publSelect <- paste("d.publ_ IN ('", publ, "')", sep = "")
    selectQuery <- append(selectQuery, publSelect)
  }
  
  if(!is.null(country)){
    if(class(country) != "character"){
      stop("country has to be a character or a vector of character elements.")
    }    
    if(length(country) > 1 & !is.null(region)){warning("Multiple countries specified with region: Providing region for multiple countries might result in undesired results. Provide region only for queries with one country.")}
    
    country <- paste(country, collapse="','")
    
    if(all(sapply(country, nchar) == 3)){
      countrySelect <- paste("f.poldiv1 IN ('", country, "')", sep="")
    }else{
      countrySelect <- paste("f.name IN ('", country, "')", sep="")
    }
    selectQuery <- append(selectQuery, countrySelect)
    
    if(!is.null(region)){
      if(all(class(region) != "character")){
        stop("region has to be a character or a vector of the characters.")
      }    
      if(any(nchar(region) > 2)){
        regionSelect <- paste("g.name IN ('", region, "')", sep="")
      }else{
        regionSelect <- paste("g.poldiv2 IN ('", region, "')", sep="")
      }
      selectQuery <- append(selectQuery, regionSelect)
    }
  }
  
  if(!is.null(restrictions)){
    if(class(restrictions) != "character"){stop("restrictions has to be a character 'U' or 'R'.")}
    
    restrictions <- paste(restrictions, collapse="','")
    restrictionsSelect <- paste("usestatus IN ('", restrictions, "')", sep = "")
    selectQuery <- append(selectQuery, restrictionsSelect)
  }
  
  basicQuery <- "SELECT * FROM entity a"
  
  if(is.null(selectQuery)){
    sqlQuery <- basicQuery
  }else{
    logical_operator <- paste(" ", logical_operator, " ", sep="")
    joinQuery <- paste(joinQuery, collapse=" ")
    selectQuery <- paste(selectQuery, collapse=logical_operator)
    
    sqlQuery <- paste(basicQuery, joinQuery, "WHERE", selectQuery)
  }
  sqlQuery <- enc2utf8(sqlQuery)
  result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
  return(result)
  }


