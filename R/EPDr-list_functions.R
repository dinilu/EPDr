#' Title TBW
#'
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
list_taxagroups <- function(connection){
    sqlQuery <- paste("SELECT groupid, groupcode, groupname FROM groups;")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}


#' Title TBW
#'
#' @param connection TBW
#' @param groups TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
list_taxa <- function(connection, groups=NULL){
    if(is.null(groups)){
        groups <- list_taxagroups(connection)$groupid
    }
    groups.c <- paste(groups, collapse="','")
    sqlQuery <- paste("SELECT var_, varname, varcode, mhvar_, groupid FROM p_vars NATURAL JOIN p_group WHERE groupid IN ('", groups.c, "') ORDER BY varname;", sep="")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}


#' Title TBW
#'
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
list_countries <- function(connection){
    sqlQuery <- paste("SELECT poldiv1, name FROM poldiv1;")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}

#' Title TBW
#'
#' @param connection TBW
#' @param countries TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
list_regions <- function(connection, countries=NULL){
    if(is.null(countries)){
        countries <- list_countries(connection)$poldiv1
    }
    countries.c <- paste(countries, collapse="','")
    sqlQuery <- paste("SELECT a.poldiv2, a.poldiv1, a.name AS regionname, b.name AS countryname FROM poldiv2 a JOIN poldiv1 b ON a.poldiv1 = b.poldiv1 WHERE b.poldiv1 IN ('", countries.c, "') OR b.name IN ('", countries.c, "');", sep="")
    RPostgreSQL::dbGetQuery(connection, sqlQuery)
}

#' Title TBW
#'
#' @param connection TBW
#' @param countries TBW
#' @param regions TBW
#' @param coords TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
list_sites <- function(connection, countries=NULL, regions=NULL, coords=NULL){
    if(length(countries) == 0 & !is.null(regions)){
        stop("No country specified: If you want to specify a region you have to provide also the country.")
    }
    if(length(countries) > 1 & !is.null(regions)){
        warning("Multiple countries specified with regions: Providing regions for multiple countries might result in undesired results. Provide regions only for queries with single countries.")
    }
    
    if(is.null(countries)){
        countries <- list_countries(connection)$poldiv1
    }else{
        tmp <- list_countries(connection)
        countries <- tmp[tmp$poldiv1 %in% countries | tmp$name %in% countries, "poldiv1"]
    }
    countries.c <- paste(countries, collapse="','")
    
    if(is.null(regions)){
        regions <- list_regions(connection, countries)$poldiv2
    }else{
        tmp <- list_regions(connection, countries)
        regions <- tmp[tmp$regionname %in% regions | tmp$poldiv2 %in% regions, "poldiv2"]
    }
    regions.c <- paste(regions, collapse="','")
    
    if(is.null(coords)){
        sqlQuery <- paste("SELECT site_, sitename, sitecode, latdd, londd, elevation, areaofsite, sitedescript, physiography, surroundveg, vegformation FROM siteloc NATURAL LEFT JOIN sitedesc WHERE poldiv1 IN ('", countries.c, "') AND poldiv2 IN ('", regions.c, "');", sep = "")
    }else{
        xmin <- coords[1]
        xmax <- coords[2]
        ymin <- coords[3]
        ymax <- coords[4]
        sqlQuery <- paste("SELECT site_, sitename, sitecode, latdd, londd, elevation, areaofsite, sitedescript, physiography, surroundveg, vegformation FROM siteloc NATURAL LEFT JOIN sitedesc WHERE poldiv1 IN ('", countries.c, "') AND poldiv2 IN ('", regions.c, "') AND (londd BETWEEN ",  xmin, " AND ", xmax, ") AND (latdd BETWEEN ", ymin, " AND ", ymax, ");", sep = "")
    }
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}

#' Title TBW
#'
#' @param connection TBW
#' @param countries TBW
#' @param regions TBW
#' @param sites TBW
#' @param coords TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
list_e <- function(connection, countries=NULL, regions=NULL, sites=NULL, coords=NULL){
    if(length(countries) == 0 & !is.null(regions)){
        stop("No country specified: If you want to specify a region you have to provide also the country.")
    }
    if(length(countries) > 1 & !is.null(regions)){
        warning("Multiple countries specified with regions: Providing regions for multiple countries might result in undesired results. Provide regions only for queries with single countries.")
    }
    
    if(is.null(countries)){
        countries <- list_countries(connection)$poldiv1
    }else{
        tmp <- list_countries(connection)
        countries <- tmp[tmp$poldiv1 %in% countries | tmp$name %in% countries, "poldiv1"]
    }
    countries.c <- paste(countries, collapse="','")
    
    if(is.null(regions)){
        regions <- list_regions(connection, countries)$poldiv2
    }else{
        tmp <- list_regions(connection, countries)
        regions <- tmp[tmp$regionname %in% regions | tmp$poldiv2 %in% regions, "poldiv2"]
    }
    regions.c <- paste(regions, collapse="','")

    if(is.null(sites)){
        sites <- suppressWarnings(list_sites(connection, countries, regions)$site_)
    }else{
        tmp <- suppressWarnings(list_regions(connection, countries, regions))
        sites <- tmp[tmp$site_ %in% sites | tmp$sitename %in% sites | tmp$sitecode %in% sites, "site_"]
    }
    sites.c <- paste(sites, collapse="','")
    
    if(is.null(coords)){
        sqlQuery <- paste("SELECT e_, site_, sigle, name, iscore, issect, isssamp, descriptor, higherdescr, description, entloc, localveg, sampdate, sampdevice, corediamcm, c14depthadj, notes FROM entity NATURAL LEFT JOIN descr NATURAL LEFT JOIN siteloc WHERE poldiv1 IN ('", countries.c, "') AND poldiv2 IN ('", regions.c, "') AND site_ IN ('", sites.c, "');", sep = "")
    }else{
        xmin <- coords[1]
        xmax <- coords[2]
        ymin <- coords[3]
        ymax <- coords[4]
        sqlQuery <- paste("SELECT e_, site_, sigle, name, iscore, issect, isssamp, descriptor, higherdescr, description, entloc, localveg, sampdate, sampdevice, corediamcm, c14depthadj, notes FROM entity NATURAL LEFT JOIN descr NATURAL LEFT JOIN siteloc WHERE poldiv1 IN ('", countries.c, "') AND poldiv2 IN ('", regions.c, "') AND site_ IN ('", sites.c, "') AND (londd BETWEEN ",  xmin, " AND ", xmax, ") AND (latdd BETWEEN ", ymin, " AND ", ymax, ");", sep = "")
    }
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}


#' Title TBW
#'
#' @param connection TBW
#' @param e_ TBW
#' @param countries TBW
#' @param regions TBW
#' @param sites TBW
#' @param coords TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
list_publ <- function(connection, e_=NULL, countries=NULL, regions=NULL, sites=NULL, coords=NULL){
    if(is.null(e_)){
        e_ <- list_e(connection=connection, countries=countries, regions=regions, sites=sites, coords=coords)$e_
    }
    e_ <- paste(e_, collapse="','")
    sqlQuery <- paste("select * FROM publent NATURAL JOIN publ WHERE e_ IN ('", e_, "');", sep = "") 
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result)
}

