
#' Title TBW
#'
#' @param site TBW
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
e_by_site <- function(site, connection){
    if(all(class(site) != "numeric", class(site) != "character")){
        stop("site has to be a number, a character or a vector of the formers.")
    }    
    
    if(is.character(site)){
        site <- paste(site, collapse="','")
        sqlQuery <- paste("select e_ FROM entity NATURAL JOIN siteloc WHERE sitename IN ('", site, "');", sep="")
    }else{
        site <- paste(site, collapse="','")
        sqlQuery <- paste("select e_ FROM entity NATURAL JOIN siteloc WHERE site_ IN ('", site, "');", sep="")
    }
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result$e_)
}

#' Title TBW
#'
#' @param coords TBW
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
e_by_geocoord <- function(coords, connection){
    if(!is.numeric(coords)){stop("Object coord has to be a numeric vector with 4 elements: xmin, xmax, ymin, and ymax")}
    if(length(coords) != 4){stop("Object coord has to have exactly 4 elements: xmin, xmax, ymin, and ymax")}
    xmin <- coords[1]
    xmax <- coords[2]
    ymin <- coords[3]
    ymax <- coords[4]
    sqlQuery <- paste("SELECT e_ FROM entity NATURAL JOIN siteloc WHERE londd BETWEEN ",  xmin, " AND ", xmax, " AND latdd BETWEEN ", ymin, " AND ", ymax, ";", sep = "")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)    
    return(result$e_)
}

#' Title TBW
#'
#' @param lastname TBW
#' @param connection TBW
#' @param firstname TBW
#' @param initials TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
e_by_worker <- function(lastname, connection, firstname=NULL, initials=NULL){
    
    if(class(lastname) != "character"){stop("lastname has to be a character string")}
    
    if(!is.null(firstname) & !is.null(initials)){stop("Provide firstname or initials if desired, but do not provide both at the same time.")}
    
    lastname <- paste(lastname, collapse="','")
    
    sqlQuery <- paste("SELECT e_ FROM entity a JOIN workers b ON a.coll_ = b.worker_ WHERE lastname IN ('", lastname, "');", sep="")
    if(!is.null(firstname)){
        firstname <- paste(firstname, collapse="','")
        sqlQuery <- paste("SELECT e_ FROM entity a JOIN workers b ON a.coll_ = b.worker_ WHERE lastname IN ('", lastname, "') AND firstname IN ('", firstname, "');", sep="")
    }
    if(!is.null(initials)){
        initials <- paste(initials, collapse="','")
        sqlQuery <- paste("SELECT e_ FROM entity a JOIN workers b ON a.coll_ = b.worker_ WHERE lastname IN ('", lastname, "') AND initials IN ('", initials, "');", sep="")
    }
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)
    return(result$e_)
}    


#' Title TBW
#'
#' @param publ TBW
#' @param connection TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
e_by_publ <- function(publ, connection){
    publ <- paste(publ, collapse="','")
    
    sqlQuery <- paste("select e_ FROM publ NATURAL JOIN publent WHERE publ_ IN ('", publ, "');", sep = "")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)$e_
    return(result)	
}


#' Title TBW
#'
#' @param countries TBW
#' @param connection TBW
#' @param regions TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
e_by_countries <- function(countries, connection, regions=NULL){
    if(class(countries) != "character"){
        stop("Countries has to be a character or a vector of the character elements.")
    }    
    if(length(countries) > 1 & !is.null(regions)){warning("Multiple countries specified with regions: Providing regions for multiple countries might result in undesired results. Provide regions only for queries with single countries.")}
    
    tmp <- listCountries(connection)
    countries <- tmp[tmp$poldiv1 %in% countries | tmp$name %in% countries, "poldiv1"]

    if(is.null(regions)){
        regions <- listRegions(connection, countries)$poldiv2
    }else{
        tmp <- listRegions(connection, countries)
        regions <- tmp[tmp$regionname %in% regions | tmp$poldiv2 %in% regions, "poldiv2"]
    }
    
    countries <- paste(countries, collapse="','")
    regions.c <- paste(regions, collapse="','")
    
    sqlQuery <- paste("SELECT e_ FROM entity NATURAL JOIN siteloc WHERE poldiv1 IN ('", countries, "') AND poldiv2 IN ('", regions.c, "') ;", sep="")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)

    return(result[,1])
}



#' Title TBW
#'
#' @param restriction  TBW
#' @param connection  TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' #  TBW
e_by_restriction <- function(restriction, connection){
    restriction <- paste(restriction, collapse="','")
    
    sqlQuery <- paste("select e_ FROM p_entity WHERE usestatus IN ('", restriction, "');", sep = "")
    result <- RPostgreSQL::dbGetQuery(connection, sqlQuery)$e_
    return(result)	
}
