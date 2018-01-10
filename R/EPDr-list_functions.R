#' List countries in the EPD
#' 
#' This function looks into the database and returns all 
#' information in the POLDIV1 table (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}). This belong to the country level in political and 
#' administrative level. Because the function returns a whole table, 
#' it only needs a parameter with a valid connection to the database.
#' 
#' @param connection PostgreSQLConnection Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return data.frame The function returns a data frame with 
#' two columns: \code{poldiv1} and \code{name}.
#' \itemize{
#'   \item{"poldiv1"}{ is a three letter code for each country.}
#'   \item{"name"}{ is the full country name.}
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                                  user = "epdr", password = "epdrpw")
#' list_countries(epd.connection)
#' }
list_countries <- function(connection){
  sql_query <- paste("SELECT * FROM poldiv1;")
  result <- RPostgreSQL::dbGetQuery(connection, sql_query)
  return(result)
}


#' List entities in the EPD
#' 
#' This function allows to search entities records by different search 
#' criteria that can be combined as desired.
#'
#' @param connection PostgreSQLConnection A valid connection to the 
#' database. See \code{\link[EPDr]{connect_to_epd}}.
#' @param site numeric Numeric or character vector indicating the site 
#' identification number or the site name to which entities will be retrieved.
#' @param coords numeric Numeric vector with four elements indicating the 
#' geographical coordinates to which the search is limited.
#' @param lastname character Character vector indicating the last name 
#' of data contributor for which entities will be retrieved.
#' @param firstname character Character vector indicating the first name 
#' of data contributor for which entities will be retrieved.
#' @param initials character Character vector indicating the initials 
#' of data contributor for which entities will be retrieved.
#' @param publ numeric Numeric vector indicating the publication 
#' identification number to which search is constrained.
#' @param country character Character vector indicating the three 
#' letters code or the full english name for the countries to which 
#' entities are desired.
#' @param region  character Character vector indicating the two letters 
#' code or the full name for the regions to which entities are desired. 
#' To specify a region you have to specify one, and only one, country. 
#' You can specify multiple countries but the results  can be funky.
#' @param shapefile SpatialPolygonsDataFrame SpatialPolygonsDataFrame object to which the
#' query is going to be restricted. 
#' @param restrictions character Character letter ("U" or "R") specifying 
#' the usestatus of the data for the entities that are searched. U is 
#' for unrestricted and R is for restricted. 
#' @param logical_operator character Character string indicating whether 
#' the effects of search criteria are additive ("OR") or substracting 
#' ("AND"). If "OR" is specified, entities complying with any of the 
#' search criteria will be returned. If "AND" is specified, only entities 
#' that comply with all search criteria will be returned. 
#' 
#' @return This function return a data.frame with columns as in 
#' the \code{entity} table of the EPD database (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}). If search criteria are all empty, the function returns 
#' the whole table. If criteria are specified the table is complemented 
#' with columns from diferent tables in the database that store the 
#' required information to filter entities accordingly.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                               user = "epdr", password = "epdrpw")
#' list_e(epd.connection)
#' list_e(epd.connection, site = 1)
#' list_e(epd.connection, site = "Adange")
#' list_e(epd.connection, site = c(1:10))
#' list_e(epd.connection, country = "ESP")
#' list_e(epd.connection, country = "Spain")
#' list_e(epd.connection, country = "Spain", region = "Andalucia")
#' list_e(epd.connection, lastname = "Tzedakis")
#' list_e(epd.connection, restrictions = "R")
#' }
list_e <- function(connection, site = NULL, coords = NULL, lastname = NULL,
                   firstname = NULL, initials = NULL, publ = NULL,
                   country = NULL, region = NULL, shapefile = NULL,
                   restrictions = NULL, logical_operator = "AND"){
  if (all(is.null(country), !is.null(region))){
    stop(paste0("Because regions are not unique in the database you need to ",
                "specify a country in which to look for specific regions."))
  }
  join_query <- NULL
  if (any(!is.null(site), !is.null(coords), !is.null(country))){
    siteloc_join <- "JOIN siteloc b ON a.site_ = b.site_"
    join_query <- append(join_query, siteloc_join)
    if (!is.null(country)){
      poldiv1Join <- "JOIN poldiv1 f ON b.poldiv1 = f.poldiv1"
      join_query <- append(join_query, poldiv1Join)
      if (!is.null(region)){
        poldiv2Join <- paste0("JOIN poldiv2 g ON b.poldiv1 = g.poldiv1 AND ",
                              "b.poldiv2 = g.poldiv2")
        join_query <- append(join_query, poldiv2Join)
      }
    }
  }
  if (any(!is.null(lastname), !is.null(firstname), !is.null(initials))){
    workers_join <- "JOIN workers c ON a.coll_ = c.worker_"
    join_query <- append(join_query, workers_join)
  }
  if (!is.null(publ)){
    publent_join <- paste0("JOIN publent d ON a.e_ = d.e_ JOIN publ e ON ",
                           "d.publ_ = e.publ_")
    join_query <- append(join_query, publent_join)
  }
  if (!is.null(shapefile)){
    shapefile_select <- "JOIN siteloc j ON a.site_ = j.site_"
    join_query <- append(join_query, shapefile_select)
  }
  if (!is.null(restrictions)){
    restrictions_select <- "JOIN p_entity h ON a.e_ = h.e_"
    join_query <- append(join_query, restrictions_select)
  }
  select_query <- NULL
  if (!is.null(site)){
    if (all(class(site) !=  "numeric", class(site) !=  "character")){
      stop("site has to be a number, a character or a vector of the formers.")
    }
    if (is.character(site)){
      site <- paste(site, collapse = "','")
      site_select <- paste("b.sitename IN ('", site, "')", sep = "")
    }else{
      site <- paste(site, collapse = "','")
      site_select <- paste("b.site_ IN ('", site, "')", sep = "")
    }
    select_query <- append(select_query, site_select)
  }
  if (!is.null(coords)){
    if (!is.numeric(coords)){
      stop("Object coord has to be a numeric vector with 4 elements:
           xmin, xmax, ymin, and ymax")
    }
    if (length(coords) !=  4){
      stop("Object coord has to have exactly 4 elements:
           xmin, xmax, ymin, and ymax")
    }
    xmin <- coords[1]
    xmax <- coords[2]
    ymin <- coords[3]
    ymax <- coords[4]
    coords_select <- paste("b.londd BETWEEN ",  xmin, " AND ", xmax,
                           " AND b.latdd BETWEEN ", ymin, " AND ",
                           ymax, sep = "")
    select_query <- append(select_query, coords_select)
    }
  if (!is.null(lastname)){
    if (class(lastname) !=  "character"){
      stop("lastname has to be a character string")
    }
    lastname <- paste(lastname, collapse = "','")
    lastname_select <- paste("c.lastname IN ('", lastname, "')", sep = "")
    select_query <- append(select_query, lastname_select)
  }
  if (!is.null(firstname)){
    if (class(firstname) !=  "character"){
      stop("firstname has to be a character string")
    }
    firstname <- paste(firstname, collapse = "','")
    firstname_select <- paste("c.firstname IN ('", firstname, "')", sep = "")
    select_query <- append(select_query, firstname_select)
  }
  if (!is.null(initials)){
    if (class(initials) !=  "character"){
      stop("initials has to be a character string")
    }
    initials <- paste(initials, collapse = "','")
    initial_select <- paste("c.initials IN ('", initials, "')", sep = "")
    select_query <- append(select_query, initial_select)
  }
  if (!is.null(publ)){
    if (class(publ) !=  "numeric"){
      stop("publ has to be a numeric indicating the publication identification
           number.")
    }
    publ <- paste(publ, collapse = "','")
    publ_select <- paste("d.publ_ IN ('", publ, "')", sep = "")
    select_query <- append(select_query, publ_select)
    }
  if (!is.null(country)){
    if (class(country) !=  "character"){
      stop("country has to be a character or a vector of character elements.")
    }
    if (length(country) > 1 & !is.null(region)){
      warning("Multiple countries specified with region: Providing region
              for multiple countries might result in undesired results.
              Provide region only for queries with one country.")
    }
    country <- paste(country, collapse = "','")
    if (all(vapply(country, nchar, FUN.VALUE = numeric(1))  ==  3)){
      country_select <- paste("f.poldiv1 IN ('", country, "')", sep = "")
    }else{
      country_select <- paste("f.name IN ('", country, "')", sep = "")
    }
    select_query <- append(select_query, country_select)
    if (!is.null(region)){
      if (all(class(region) !=  "character")){
        stop("region has to be a character or a vector of the characters.")
      }
      if (any(nchar(region) > 2)){
        region_select <- paste("g.name IN ('", region, "')", sep = "")
      }else{
        region_select <- paste("g.poldiv2 IN ('", region, "')", sep = "")
      }
      select_query <- append(select_query, region_select)
    }
    }
  if (!is.null(shapefile)){
    if (class(shapefile) != "SpatialPolygons"){
      stop("shapefile has to be a 'SpatialPolygons' object.")
    }
    sql_query <- "SELECT site_, latdd, londd FROM siteloc"
    sql_result <- RPostgreSQL::dbGetQuery(connection, sql_query)
    sql_spatial <- sp::SpatialPointsDataFrame(sql_result[, c("londd", "latdd")],
                                              sql_result["site_"])
    sp::proj4string(sql_spatial) <- sp::CRS("+proj=longlat")
    new_crs <- sp::proj4string(shapefile)
    new_crs <- sp::CRS(new_crs)
    sql_spatial <- sp::spTransform(sql_spatial, new_crs)
    sites_overlap <- sp::over(sql_spatial, shapefile)
    sites_overlap <- sql_spatial[!is.na(sites_overlap),]
    sites_overlap <- sites_overlap$site_
    sites_overlap <- paste(sites_overlap, collapse = "','")
    shapefile_select <- paste("j.site_ IN ('", sites_overlap, "')",
                              sep = "")
    select_query <- append(select_query, shapefile_select)
  }
  if (!is.null(restrictions)){
    if (class(restrictions) != "character"){
      stop("restrictions has to be a character 'U' or 'R'.")
    }
    restrictions <- paste(restrictions, collapse = "','")
    restrictions_select <- paste("usestatus IN ('", restrictions, "')",
                                 sep = "")
    select_query <- append(select_query, restrictions_select)
  }
  basic_query <- "SELECT * FROM entity a"
  if (is.null(select_query)){
    sql_query <- basic_query
  }else{
    logical_operator <- paste(" ", logical_operator, " ", sep = "")
    join_query <- paste(join_query, collapse = " ")
    select_query <- paste(select_query, collapse = logical_operator)
    sql_query <- paste(basic_query, join_query, "WHERE", select_query)
  }
  sql_query <- enc2utf8(sql_query)
  result <- RPostgreSQL::dbGetQuery(connection, sql_query)
  return(result)
  }


#' List publications in the EPD
#' 
#' This function looks into the database and returns all combined 
#' information in the PUBLENT and PUBL tables (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}). This information refers to the original publications 
#' in which entities were described. Because the function returns a whole 
#' table, only a valid connection to the database is mandatory. However, 
#' the list of publications can be further restricted to specific entities, 
#' countries, administrative regions, sites, or a geographical region 
#' defined by geographical coordinates (longitude and latitude) of its corners.
#'
#' @param connection PostgreSQLConnection Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#' @param e_ numeric Numeric vector indicating the desired entities 
#' identification number. 
#' @param country character Character vector indicating the three letters 
#' code or the full name of the desired countries.
#' @param region character Character vector indicating two digit integer 
#' code or the full name of the desired regions.
#' @param site numeric Vector indicating the desired sites. Sites can be 
#' defined by the site identification number, the site code, or the site name.
#' @param coords numeric Vector of four numeric values, indicating the 
#' spatial extent in decimal degrees. The extent should be specified using 
#' the following format \code{c(xmin, xmax, ymin, ymax)}. Where \code{x} 
#' represents longitude and \code{y} represents latitude.
#'
#' @return Data frame with 5 columns:
#' \itemize{
#'   \item{"publ_"}{Publication identification number.}
#'   \item{"e_"}{Entity identification number.}
#'   \item{"acc_"}{Short integers for local library accession number}
#'   \item{"yearofpubl"}{Year of publication.}
#'   \item{"citation"}{Complete bibliographic reference in the style of the 
#'   journal "Ecology".}
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                                  user = "epdr", password = "epdrpw")
#' list_publ(epd.connection)
#' list_publ(epd.connection, e_ = 1)
#' list_publ(epd.connection, site = "1")
#' list_publ(epd.connection, country = "ESP", site = 1)
#' list_publ(epd.connection, country = "Spain", region = "01")
#' str(list_publ(epd.connection))
#' list_publ(epd.connection, coords = c(2.5, 22, 45.5, 53.5))
#' str(list_publ(epd.connection, coords = c(2.5, 22, 45.5, 53.5)))
#' }
list_publ <- function(connection, e_ = NULL, country = NULL, region = NULL,
                      site = NULL, coords = NULL){
  if (is.null(e_)){
    e_ <- list_e(connection = connection,
                 country = country,
                 region = region,
                 site = site,
                 coords = coords)$e_
  }
  e_ <- paste(e_, collapse = "','")
  sql_query <- paste("select * FROM publent NATURAL JOIN publ WHERE e_ IN ('",
                     e_, "');", sep = "")
  result <- RPostgreSQL::dbGetQuery(connection, sql_query)
  return(result)
}


#' List (administrative) regions in the EPD
#' 
#' This function looks into the database and returns all combined 
#' information in the POLDIV1 and POLDIV2 tables (see documentation 
#' of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' Because the function returns a whole table, it only needs a parameter 
#' with a valid connection to the database. The list can be restricted 
#' to specific countries using parameter \code{country}.
#' 
#' @param connection PostgreSQLConnection. Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#' @param country Character vector indicating the three 
#' letters code or the full name of the desired countries.
#'
#' @return data.frame with four columns:
#' \itemize{
#'   \item{"poldiv2"}{Region code consisting of two digit integer. 
#'   This code is not a key field in the database and thus can be two regions 
#'   with the same poldiv2 value. This field allows to differentiate regions 
#'   within the same country.}
#'   \item{"poldiv1"}{Country code consisting of three capital letters. This 
#'   code is unique for each country.}
#'   \item{"regionname"}{Region full name.}
#'   \item{"countryname"}{Country full name.}
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                               user = "epdr", password = "epdrpw")
#' list_regions(epd.connection)
#' list_regions(epd.connection, "ESP")
#' list_regions(epd.connection, "Spain")
#' list_regions(epd.connection, c("Spain", "France", "Germany"))
#' }
list_regions <- function(connection, country = NULL){
    if (is.null(country)){
        country <- list_countries(connection)$poldiv1
    }
    countries <- paste(country, collapse = "','")
    sql_query <- paste("SELECT a.poldiv2, a.poldiv1, a.name AS regionname, ",
                       "b.name AS countryname FROM poldiv2 a JOIN poldiv1 ",
                       "b ON a.poldiv1 = b.poldiv1 WHERE b.poldiv1 IN ('",
                       countries, "') OR b.name IN ('", countries, "');",
                       sep = "")
    RPostgreSQL::dbGetQuery(connection, sql_query)
}


#' List sites in the EPD
#' 
#' This function looks into the database and returns all combined 
#' information in the SITELOC and SITEDESC tables (see documentation 
#' of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' This information refers to the sites where palynological samples 
#' were taken. Because the function returns a whole table, only a valid 
#' connection to the database is mandatory. However, the list of sites 
#' can be further restricted to specific countries, administrative
#' regions, or a geographical region defined by geographical coordinates 
#' (longitude and latitude) of its corners.
#'
#' @param connection PostgreSQLConnection. Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#' @param country Character vector indicating the three letters code 
#' or the full name of the desired countries.
#' @param region Character vector indicating two digit integer code or 
#' the full name of the desired regions.
#' @param coords Vector of four numeric values, indicating the spatial 
#' extent in decimal degrees. The extent should be specified using the 
#' following format \code{c(xmin, xmax, ymin, ymax)}. Where \code{x} 
#' represents longitude and \code{y} represents latitude.
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
#'   \item{"surroundveg"}{Description of the surrounding vegetation of 
#'   the site.}
#'   \item{"vegformation"}{Name of the vegetation formation in the site.}
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                                  user = "epdr", password = "epdrpw")
#' list_sites(epd.connection)
#' list_sites(epd.connection, "ESP")
#' list_sites(epd.connection, "Spain", "01")
#' summary(list_sites(epd.connection))
#' list_sites(epd.connection, coords = c(2.5, 22, 45.5, 53.5))
#' summary(list_sites(epd.connection, coords = c(2.5, 22, 45.5, 53.5)))
#' }
list_sites <- function(connection,
                       country = NULL,
                       region = NULL,
                       coords = NULL){
    if (length(country)  ==  0 & !is.null(region)){
        stop("No country specified: If you want to specify a region you have
             to provide also the country.")
    }
    if (length(country) > 1 & !is.null(region)){
        warning("Multiple countries specified with region: Providing region
                for multiple countries might result in undesired results.
                Provide region only for queries with single countries.")
    }
    if (is.null(country)){
      country <- list_countries(connection)$poldiv1
    }else{
        tmp <- list_countries(connection)
        country <- tmp[tmp$poldiv1 %in% country | tmp$name %in% country,
                       "poldiv1"]
    }
    countries <- paste(country, collapse = "','")
    if (is.null(region)){
        region <- list_regions(connection, country)$poldiv2
    }else{
        tmp <- list_regions(connection, country)
        region <- tmp[tmp$regionname %in% region | tmp$poldiv2 %in% region,
                      "poldiv2"]
    }
    regions <- paste(region, collapse = "','")
    if (is.null(coords)){
        sql_query <- paste("SELECT site_, sitename, sitecode, latdd, londd, ",
                           "elevation, areaofsite, sitedescript, ",
                           "physiography, surroundveg, vegformation FROM ",
                           "siteloc NATURAL LEFT JOIN sitedesc WHERE ",
                           "poldiv1 IN ('", countries, "') AND poldiv2 IN ('",
                           regions, "');", sep = "")
    }else{
        xmin <- coords[1]
        xmax <- coords[2]
        ymin <- coords[3]
        ymax <- coords[4]
        sql_query <- paste("SELECT site_, sitename, sitecode, latdd, londd, ",
                           "elevation, areaofsite, sitedescript, ",
                           "physiography, surroundveg, vegformation FROM ",
                           "siteloc NATURAL LEFT JOIN sitedesc WHERE poldiv1 ",
                           "IN ('", countries, "') AND poldiv2 IN ('", regions,
                           "') AND (londd BETWEEN ", xmin, " AND ", xmax,
                           ") AND (latdd BETWEEN ", ymin, " AND ", ymax,
                           ");", sep = "")
    }
    result <- RPostgreSQL::dbGetQuery(connection, sql_query)
    return(result)
}


#' List taxa in the EPD
#' 
#' This function looks into the database and returns combined 
#' information in the P_VARS and P_GROUP tables (see documentation 
#' of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' The function allows to restrict the list to taxa in an specific 
#' group, using parameter \code{group_id} along with a valid connection 
#' to the database.
#' 
#' @param connection PostgreSQLConnection. Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#' @param group_id character. Character vector with group ids (four 
#' character strings; see \code{\link[EPDr]{list_taxagroups}} to check 
#' correspondence between groups and group ids). The user can specify as 
#' many groups as desired in a single call of the function.
#'
#' @return data.frame The function returns a data frame with five 
#' columns: \code{var_}, \code{varname}, \code{varcode}, \code{mhvar}, 
#' and \code{groupid}.
#' \itemize{
#'  \item{"var_ is"}{ the taxa identification number in the database.}
#'  \item{"varname"}{ is the taxa name.}
#'  \item{"varcode"}{ is the short taxa name.}
#'  \item{"mhvar"}{ is the identification number of the higher taxonomical 
#'  level.}
#'  \item{"groupid"}{ is the identification number of the group that the 
#'  taxa belongs to.}
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                                  user = "epdr", password = "epdrpw")
#' list_taxa(epd.connection, "HERB")
#' list_taxa(epd.connection, c("HERB", "TRSH"))
#' }
list_taxa <- function(connection, group_id = NULL){
  if (is.null(group_id)){
    group_id <- list_taxagroups(connection)$groupid
  }
  groups.c <- paste(group_id, collapse = "','")
  sql_query <- paste("SELECT var_, varname, varcode, mhvar_, groupid ",
                     "FROM p_vars NATURAL JOIN p_group WHERE groupid ",
                     "IN ('", groups.c, "') ORDER BY varname;", sep = "")
  result <- RPostgreSQL::dbGetQuery(connection, sql_query)
  return(result)
}


#' List taxa groups in the EPD
#' 
#' This function looks into the database and returns all information in 
#' the GROUPS table (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' This is useful if the user want to restrict searches for particular 
#' groups of particles in the palynological samples. For instance, if somebody 
#' is only interested in Algae. Because the function return the whole table, 
#' it only requires a valid connection to the database server.
#' 
#' @param connection PostgreSQLConnection. Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return data.frame The function returns a data frame with three columns: 
#' \code{groupid}, \code{groupcode}, and \code{groupname}.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                                  user = "epdr", password = "epdrpw")
#' list_taxagroups(epd.connection)
#' }
list_taxagroups <- function(connection){
  sql_query <- paste("SELECT * FROM groups;")
  result <- RPostgreSQL::dbGetQuery(connection, sql_query)
  return(result)
}
