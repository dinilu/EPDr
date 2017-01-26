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


#' List entities in the EPD
#' 
#' This function looks into the database and returns all combined information in the SITELOC, DESCR,
#' and ENTITY tables (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' This information refers to the entities (or cores) sampled.
#' Because the function returns a whole table, only a valid connection to the database is mandatory.
#' However, the list of entities can be further restricted to specific countries, administrative
#' regions, sites, or a geographical region defined by geographical coordinates
#' (longitude and latitude) of its corners.
#'
#' @param connection PostgreSQLConnection. Object of class \code{PostgreSQLConnection} as
#' returned by function \code{\link[EPDr:connectToEPD]{connectToEPD}}.
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
#' @return Data frame with 17 columns:
#' \itemize{
#'   \item{"e_"}{Entity identification number.}
#'   \item{"site_"}{Site identification number.}
#'   \item{"sigle"}{Unique code name (unique combination of uppercase letters, numbers, and 
#'   special characters) for the entity.}
#'   \item{"name"}{Entity name that modify the site name in the original publication when
#'   multiple entities are sampled in the same site.}
#'   \item{"iscore"}{"Y" or "N" indicating whether the entity is a core.}
#'   \item{"issect"}{"Y" or "N" indicating whether the entity is a section.}
#'   \item{"isssamp"}{"Y" or "N" indicating whether the entity is a modern surface sample.}
#'   \item{"descriptor"}{Unique four character identifier for the entity site-descriptions.}
#'   \item{"higherdescr"}{Identifier of the next higher descriptor in the hierarchy.}
#'   \item{"description"}{Identifier for the description of modern entity site.}
#'   \item{"entloc"}{Location where the sample or core was taken (e.g. "north side of lake").}
#'   \item{"localveg"}{Local vegetation at the collection site.}
#'   \item{"sampdate"}{Date that the sample or core was collected. Enter date as yyyy-mm-dd.}
#'   \item{"sampdevice"}{Name of the sampling or coring device.}
#'   \item{"corediamcm"}{Diameter of the core (cm). Applies to cores only.}
#'   \item{"c14depthadj"}{Adjustment for 14C depths needed to relate the depth of the dated
#'   samples to the pollen or other samples. This is necessary since depths cited for samples
#'   taken for radiocarbon and other geochronological dates may have been measured from a
#'   different datum than those for pollen and other samples (cm).}
#'   \item{"notes"}{Additional information on the description of the entity.}
#' }
#'  
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connectToEPD(host="localhost", database="epd",
#' #                               user="epdr", password="epdrpw")
#' # listE(epd.connection)
#' # listE(epd.connection, country="ESP")
#' # listE(epd.connection, country="Spain", region="01")
#' # summary(listE(epd.connection))
#' # listE(epd.connection, coords=c(2.5, 22, 45.5, 53.5))
#' # summary(listE(epd.connection, coords=c(2.5, 22, 45.5, 53.5)))
#' # disconnectFromEPD(epd.connection)
#' 
listE <- function(connection, country=NULL, region=NULL, site=NULL, coords=NULL){
    if(length(country) == 0 & !is.null(region)){
        stop("No country specified: If you want to specify a region you have to provide also the country.")
    }
    if(length(country) > 1 & !is.null(region)){
        warning("Multiple country specified with region: Providing region for multiple country might result in undesired results. Provide region only for queries with single country.")
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

    if(is.null(site)){
        site <- suppressWarnings(listSites(connection, country, region)$site_)
    }else{
        tmp <- suppressWarnings(listSites(connection, country, region))
        site <- tmp[tmp$site_ %in% site | tmp$sitename %in% site | tmp$sitecode %in% site, "site_"]
    }
    sites <- paste(site, collapse="','")
    
    if(is.null(coords)){
        sqlQuery <- paste("SELECT e_, site_, sigle, name, iscore, issect, isssamp, descriptor, higherdescr, description, entloc, localveg, sampdate, sampdevice, corediamcm, c14depthadj, notes FROM entity NATURAL LEFT JOIN descr NATURAL LEFT JOIN siteloc WHERE poldiv1 IN ('", countries, "') AND poldiv2 IN ('", regions, "') AND site_ IN ('", sites, "');", sep = "")
    }else{
        xmin <- coords[1]
        xmax <- coords[2]
        ymin <- coords[3]
        ymax <- coords[4]
        sqlQuery <- paste("SELECT e_, site_, sigle, name, iscore, issect, isssamp, descriptor, higherdescr, description, entloc, localveg, sampdate, sampdevice, corediamcm, c14depthadj, notes FROM entity NATURAL LEFT JOIN descr NATURAL LEFT JOIN siteloc WHERE poldiv1 IN ('", countries, "') AND poldiv2 IN ('", regions, "') AND site_ IN ('", sites, "') AND (londd BETWEEN ",  xmin, " AND ", xmax, ") AND (latdd BETWEEN ", ymin, " AND ", ymax, ");", sep = "")
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

