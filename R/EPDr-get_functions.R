# get_chron functions ------------------------------------------------------


#' Retrieve chronological information for EPD entities
#'
#' Functions in this family retrieves information relative to the 
#' chronologies used to calculate samples ages for a particular 
#' entity. The main function (\code{\link[EPDr]{get_chron}}) requires 
#' a valid connection to the database and the entity ID for the entity 
#' of interest. All other functions (starting with a dot [.]) use 
#' different arguments depending on the piece of information they retrieve.
#' 
#' @param e_ numeric Value indicating the entity number (e_) of the 
#' database that want to be queried.
#' @param rcode character Three letter code for the rational.
#' @param event_ numeric Value indicating the event number (event_) of 
#' the database of the requested event.
#' @param connection PostgreSQLConnection Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return \code{\link[EPDr]{chron}} object. This is an EPDr object 
#' with information from different tables. See documentation of the EPD: 
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                                  user = "epdr", password = "epdrpw")
#' # chron.400 <- get_chron(400, epd.connection)
#' # chron.400
#' 
#' @section get_chron:
#' This function returns a \code{\link[EPDr]{chron}} object with several 
#' information from the rest of the functions for a particular entity.
#' @rdname get_chron
#' @export
get_chron <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  chron <- .get_chron(e_, connection)
  agebound <- .get_agebound(e_, connection)
  agebasis <- .get_agebasis(e_, connection)
  rcode_ <- agebasis$rcode
  rational <- .get_rational(rcode_, connection)
  alsegs <- .get_alsegs(e_, connection)
  panldpt <- .get_panldpt(e_, connection)
  synevent <- .get_synevent(e_, connection)
  event_ <- synevent$event_
  event <- .get_event(event_, connection)
  publ <- get_publ(event$publ_, connection)
  chron_output <- new("chron",
                      chron = chron,
                      agebound = agebound,
                      agebasis = agebasis,
                      rational = rational,
                      alsegs = alsegs,
                      panldpt = panldpt,
                      synevent = synevent,
                      event = event,
                      publ = publ)
  return(chron_output)
}

#' @section .get_chron:
#' This functions returns a data.frame with information in in the CHRON table for the 
#' specified entity. This corresponds with chronologies for that entity.
#' @rdname get_chron
#' @export
.get_chron <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "chron"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_agebound:
#' This functions returns a data.frame with information in in the AGEBOUND table for the 
#' chronologies in that entity. This corresponds with age limits for 
#' that entity calculated according to each chronology.
#' @rdname get_chron
#' @export
.get_agebound <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "agebound"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_agebasis:
#' This functions returns a data.frame with information in in the AGEBASIS table for the 
#' specified entity. This corresponds with the depth and C14 data 
#' used to calibrate the age-depth model in each chronology.
#' @rdname get_chron
#' @export
.get_agebasis <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "agebasis"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_rational:
#' This functions returns a data.frame with information in in the RATIONAL table for the 
#' specified entity. This corresponds with the rational to use each 
#' sample in the AGEBASIS table to calibrate the age-depth model.
#' @rdname get_chron
#' @export
.get_rational <- function(rcode, connection){
  rcode <- paste(rcode, collapse = "','")
  table <- "rational"
  sql_query <- paste("SELECT * FROM ", table, " WHERE rcode IN ('",
                     rcode, "');", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_alsegs:
#' This functions returns a data.frame with information in in the ALSEGS table for the 
#' specified entity. This corresponds with segments of annual 
#' laminations in the entity.
#' @rdname get_chron
#' @export
.get_alsegs <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "alsegs"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_panldpt:
#' This functions returns a data.frame with information in in the P_ANLDPT table for the 
#' specified entity. This corresponds with details on the lamination 
#' for each segment on the ALSEGS table for each entity.
#' @rdname get_chron
#' @export
.get_panldpt <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "p_anldpt"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_synevent:
#' This functions returns a data.frame with information in in the SYNEVENT table for the 
#' specified entity. This corresponds with the geological events that 
#' affect that entity.
#' @rdname get_chron
#' @export
.get_synevent <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "synevent"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_event:
#' This functions returns a data.frame with information in in the EVENT table for the 
#' specified entity. This corresponds with details on the geological 
#' event specified for that entity in the SYNEVENT table.
#' @rdname get_chron
#' @export
.get_event <- function(event_, connection){
  table <- "event"
  if (length(event_) !=  0){
    event_ <- paste(event_, collapse = "','")
    sql_query <- paste("SELECT * FROM ", table, " WHERE event_ IN ('",
                       event_, "');", sep = "")
    sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  }
  if (length(event_)  ==  0 || nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}



# get_ent functions ---------------------------------------------------

#' Retrieve description for EPD entities
#'
#' Functions in this family retrieves information relative to the 
#' description of the entity itself. The main function 
#' (\code{\link[EPDr]{get_ent}}) requires a valid connection to the 
#' database and the entity ID for the entity of interest. All other 
#' functions (starting with a dot [.]) use different arguments 
#' depending on the piece of information they retrieve.
#' 
#' @param e_ numeric Value indicating the entity number (e_) of 
#' the database that want to be queried.
#' @param descriptor character Three/four letter code for the 
#' description of the entity.
#' @param connection PostgreSQLConnection Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return \code{\link[EPDr]{entity}} object. This is an EPDr object 
#' with information from different tables. See documentation of the EPD: 
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                                  user = "epdr", password = "epdrpw")
#' # ent.400 <- get_ent(400, epd.connection)
#' # ent.400
#' 
#' @section get_entity:
#' This function returns an \code{\link[EPDr]{entity}} object with several 
#' information from the rest of the functions for a particular entity.
#' @rdname get_ent
#' @export
get_ent <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  ent <- .get_entity(e_, connection)
  pentity <- .get_pentity(e_, connection)
  contact <- get_workers(pentity$contact_, connection)
  coredrive <- .get_coredrive(e_, connection)
  descr <- .get_descr(ent$descriptor, connection)
  lithology <- .get_lithology(e_, connection)
  loi <- .get_loi(e_, connection)
  section <- .get_section(e_, connection)
  publent <- .get_publent(e_, connection)
  publ <- get_publ(publent$publ_, connection)
  entity <- new("entity",
                entity = ent,
                pentity = pentity,
                contact = contact,
                coredrive = coredrive,
                descr = descr,
                lithology = lithology,
                loi = loi,
                section = section,
                publent = publent,
                publ = publ)
  return(entity)
}


#' @section .get_entity:
#' This functions returns a data.frame with information in in the ENTITY table for the 
#' specified entity. This corresponds with description information 
#' for that entity.
#' @rdname get_ent
#' @export
.get_entity <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "entity"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '", e_,
                     "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


#' @section .get_pentity:
#' This functions returns a data.frame with information in in the P_ENTITY table for the 
#' specified entity. This corresponds with details on the status of 
#' the data, contact person, and restriction of use for the data in 
#' that entity.
#' @rdname get_ent
#' @export
.get_pentity <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "p_entity"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


#' @section .get_coredrive:
#' This functions returns a data.frame with information in in the COREDRIVE table for the 
#' specified entity. This corresponds with details on the drirve used 
#' to sample the entity.
#' @rdname get_ent
#' @export
.get_coredrive <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided several ",
                  "entity ID values (e_) but only the first one is going to ",
                  "be used.", sep = ""))
  }
  table <- "coredriv"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


#' @section .get_descr:
#' This functions returns a data.frame with information in in the DESCR table for the 
#' specified entity. This corresponds with a longer description 
#' of the entity.
#' @rdname get_ent
#' @export
.get_descr <- function(descriptor, connection){
  descriptor <- paste(descriptor, collapse = "','")
  table <- "descr"
  sql_query <- paste("SELECT * FROM ", table, " WHERE descriptor IN ('",
                     descriptor, "');", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


#' @section .get_lithology:
#' This functions returns a data.frame with information in in the LITHOLOGY table for 
#' the specified entity. This corresponds with details on the 
#' lithology found when drilling the entity.
#' @rdname get_ent
#' @export
.get_lithology <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "litholgy"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


#' @section .get_loi:
#' This functions returns a data.frame with information in in the LOI table for the 
#' specified entity. This corresponds with details on the 
#' loss-on-ignition for samples in that particular entity the entity.
#' @rdname get_ent
#' @export
.get_loi <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "loi"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


#' @section .get_section:
#' This functions returns a data.frame with information in in the SECTION table for the 
#' specified entity. This corresponds with details on the sections 
#' used to sample the entity.
#' @rdname get_ent
#' @export
.get_section <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "section"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


#' @section .get_publent:
#' This functions returns a data.frame with information in in the PUBLENT table for the 
#' specified entity. This corresponds with publications in which data 
#' for that entity have been published.
#' @rdname get_ent
#' @export
.get_publent <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  table <- "publent"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}


# get_geochron functions ---------------------------------------------------

#' Retrieving geochronological information for an entity in the EPD
#' 
#' Functions in this group retrieve different sort of geochronological 
#' information from an specific entity in the database. 
#' 
#' All functions here are designed to retrieve information from a single 
#' entity. If multiple entity numbers are requested, the functions return 
#' data only for the first one. Each function retrieve data from a specific 
#' table or set of tables that are conveniently combined if necessary.
#'
#' @param e_ numeric. Value indicating the entity number (e_) of the 
#' database that is queried.
#' @param connection PostgreSQLConnection. Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#' 
#' @return Data frame with all information from specific tables in the EPD 
#' (see documentation of the EPD: 
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}) for the requested entity. Columns names in the resulting 
#' data frames will vary among functions.
#' 
#' @examples
#' # Not run
#' # epd.connection <- connect_to_epd(database = "epd", user = "epdr",
#' #                                 password = "epdrpw", host = "localhost")
#' # .get_c14(1, epd.connection)
#' # .get_c14(400, epd.connection)
#' #
#' # get_geochron(400, epd.connection)
#' 
#' @section get_geochron:
#' This function returns a \code{\link[EPDr]{geochron}} object, that store 
#' information from different tables for a particular entity.
#' @rdname get_geochron
#' 
#' @export
#' 
get_geochron <- function(e_, connection) {
  geochron <- .get_geochron(e_, connection)
  aar <- .get_aar(e_, connection)
  c14 <- .get_c14(e_, connection)
  esr <- .get_esr(e_, connection)
  ft <- .get_ft(e_, connection)
  kar <- .get_kar(e_, connection)
  pb210 <- .get_pb210(e_, connection)
  si32 <- .get_si32(e_, connection)
  tl <- .get_tl(e_, connection)
  useries <- .get_useries(e_, connection)
  publ <- get_publ(geochron$publ_, connection)
  output <- new("geochron",
                geochron = geochron,
                aar = aar,
                c14 = c14,
                esr = esr,
                ft = ft,
                kar = kar,
                pb210 = pb210,
                si32 = si32,
                tl = tl,
                useries = useries,
                publ = publ)
  return(output)
  }


#' @section .get_geochron:
#' This functions returns a data.frame with information in in the GEOCHRON table for the 
#' specified entity. This corresponds with the common geochronological 
#' data for the entity that have been analysed for that particular entity.
#' @rdname get_geochron
#' @export
.get_geochron <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM geochron WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(e_ = NA, sample_ = NA, method = NA, depthcm = NA,
                          thickness = NA, materialdated = NA, publ_ = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_aar:
#' This functions returns a data.frame with information in in the AAR table for the specified 
#' entity. This corresponds with Amino Acid Racemization data for datation 
#' samples.
#' @rdname get_geochron
#' @export
.get_aar <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM AAR WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have AAR data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, errorlimits = NA,
                          taxondated = NA, labnumber = NA, notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_c14:
#' This functions returns a data.frame with information in in the C14 table for the specified 
#' entity. This corresponds with C14 data for all radiocarbon samples that 
#' have been analysed for that particular entity.
#' @rdname get_geochron
#' @export
.get_c14 <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first ",
                  "one is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM c14 WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have C14 data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, agesdup = NA,
                          agesdlo = NA, grthanage = NA, basis = NA,
                          enriched = NA, labnumber = NA, deltac13 = NA,
                          notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_esr:
#' This functions returns a data.frame with information in in the ESR table for the specified 
#' entity. This corresponds with Electron Spin Resonance data for datation 
#' samples.
#' @rdname get_geochron
#' @export
.get_esr <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM ESR WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have ESR data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, errorlimits = NA,
                          labnumber = NA, notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_ft:
#' This functions returns a data.frame with information in in the FT table for the specified 
#' entity. This corresponds with Fission Track data for datation samples.
#' @rdname get_geochron
#' @export
.get_ft <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM FT WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have FT data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, errorlimits = NA,
                          labnumber = NA, notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_kar:
#' This functions returns a data.frame with information in in the KAR table for the specified 
#' entity. This corresponds with Fission Track data for datation samples.
#' @rdname get_geochron
#' @export
.get_kar <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM KAR WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have KAR data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, errorlimits = NA,
                          labnumber = NA, notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_pb210:
#' This functions returns a data.frame with information in in the PB210 table for the specified 
#' entity. This corresponds with \eqn{^{210}Pb}{^[210]Pb} isotope data for 
#' datation samples.
#' @rdname get_geochron
#' @export
.get_pb210 <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM PB210 WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have PB210 data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agead = NA, agesdup = NA,
                          agesdlo = NA, grthanage = NA, notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_si32:
#' This functions returns a data.frame with information in in the SI32 table for the specified 
#' entity. This corresponds with \eqn{^{32}Si}{^[32]Si} (Silicon-32) data for datation samples.
#' @rdname get_geochron
#' @export
.get_si32 <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM SI32 WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have SI32 data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, agesdup = NA,
                          agesdlo = NA, grthanage = NA, labnumber = NA,
                          notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_tl:
#' This functions returns a data.frame with information in in the TL table for the specified 
#' entity. This corresponds with Thermoluminescence data for datation 
#' samples.
#' @rdname get_geochron
#' @export
.get_tl <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM TL WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have TL data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, errorlimits = NA,
                          grainsize = NA, labnumber = NA, notes = NA)[-1, ]
  }
  return(sql_out)
}


#' @section .get_useries:
#' This functions returns a data.frame with information in in the USERIES table for the specified
#' entity. This corresponds with Uranium-series data for datation samples.
#' @rdname get_geochron
#' @export
.get_useries <- function(e_, connection) {
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided several ",
                  "entity ID values (e_) but only the first one is going to ",
                  "be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM USERIES WHERE e_ = ", e_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    warning("This core (entity) does not have USERIES data.", call. = FALSE)
    sql_out <- data.frame(e_ = NA, sample_ = NA, agebp = NA, errorlimits = NA,
                          labnumber = NA, notes = NA)[-1, ]
  }
  return(sql_out)
}






# get_samples functions ---------------------------------------------------

#' Retrieve samples information for EPD entities
#'
#' Functions in this group retrieve information relative to the samples
#' analized in an entity. The main function (\code{\link[EPDr]{get_samples}})
#' requires a valid connection to a database and the entity ID for the entity
#' of interest. All other functions (starting with a dot [.]) use different
#' arguments depending on the piece of information they retrieve
#' 
#' @param e_ numeric Value indicating the entity number (e_) of the 
#' database that want to be queried.
#' @param var_ numeric Value indicating the variable identification number
#' (var_) of the variable that is queried. Variables in the EPD are any
#' data that is counted and stored for each of the samples. They can be
#' biological particles (e.g., pollen), non-biological particles (e.g., 
#' added particles in the samples to calculate concentrations), or
#' summary of the previous computed afterwards (e.g., total pollen sum), 
#' among others.
#' @param syntype character One-character code indicating the type of 
#' synonym. It must be one of the following: M=morphological; 
#' N=nomenclatural; 1=name of a globally-monospecific genus (e.g. Zea); 
#' 2=name of a genus that is monospecific within a continent, but not 
#' globally (e.g. Tilia americana in North America); or <null> for accepted 
#' variables.
#' @param groupid numeric Value indicating the group identification number
#' that is required. 
#' @param connection PostgreSQLConnection Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return \code{\link[EPDr]{samples}} object. This is an EPDr object 
#' with information from different tables. See documentation of the EPD: 
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                                  user = "epdr", password = "epdrpw")
#' # chron.400 <- get_samples(400, epd.connection)
#' # chron.400
#' 
#' @section get_samples:
#' This function returns a \code{\link[EPDr]{samples}} object with several 
#' information from the rest of the functions in the group for a 
#' particular entity.
#' @rdname get_samples
#' @export
get_samples <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for a single entity. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  psamples <- .get_psamples(e_, connection)
  pagedpt <- .get_pagedpt(e_, connection)
  pcounts <- .get_pcounts(e_, connection)
  pvars <- .get_pvars(pcounts$var_, connection)
  syntype <- .get_syntype(pvars$syntype, connection)
  pvtrans <- .get_pvtrans(pcounts$var_, connection)
  pgroup <- .get_pgroup(pcounts$var_, connection)
  groups <- .get_groups(pgroup$groupid, connection)
  analysts <- get_workers(psamples$analyst_, connection)
  samples <- new("samples",
                 psamples = psamples,
                 analysts = analysts,
                 pagedpt = pagedpt,
                 pcounts = pcounts,
                 pvars = pvars,
                 syntype = syntype,
                 pvtrans = pvtrans,
                 pgroup = pgroup,
                 groups = groups)
  return(samples)
}

#' @section .get_psamples:
#' This functions returns a data.frame with information in in the P_SAMPLE table for the specified
#' entity. This corresponds with the sample depths for an entity. This table 
#' also stores an identifier for the worker who analyzed the sample. 
#' @rdname get_samples
#' @export
.get_psamples <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one is ",
                  "going to be used.", sep = ""))
  }
  table <- "p_sample"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_pagedpt:
#' This functions returns a data.frame with information in in the P_AGEDPT table for the specified
#' entity. This corresponds with fitted age estimates for the sample depths 
#' for each chronology specified for an entity. Names for each chronology 
#' are stored in the CHRON table. The age-depth data are generated by applying 
#' the age-model to the data in the AGEBASIS table.
#' @rdname get_samples
#' @export
.get_pagedpt <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided several ",
                  "entity ID values (e_) but only the first one is going to ",
                  "be used.", sep = ""))
  }
  table <- "p_agedpt"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_pcounts:
#' This functions returns a data.frame with information in in the P_COUNTS table for the specified
#' entity. This corresponds with the pollen counts for an entity. This table
#' stores the non-zero counts for each combination of variable and sample 
#' depth (the depths values are stored in table P_SAMPLE).
#' @rdname get_samples
#' @export
.get_pcounts <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one is ",
                  "going to be used.", sep = ""))
  }
  table <- "p_counts"
  sql_query <- paste("SELECT * FROM ", table, " WHERE e_ = '",
                     e_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_pvars:
#' This functions returns a data.frame with information in in the P_VARS table for the specified
#' entity. This corresponds with details on the variables (taxa, charcoals, etc)
#' that have been counted in the biological samples (not for chronology).
#' @rdname get_samples
#' @export
.get_pvars <- function(var_, connection){
  table <- "p_vars"
  if (length(var_) !=  0){
    var_ <- paste(var_, collapse = "','")
    sql_query <- paste("SELECT * FROM ", table, " WHERE var_ IN ('",
                       var_, "');", sep = "")
    sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  }
  if (length(var_)  ==  0 || nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_pvtrans:
#' This functions returns a data.frame with information in in the P_VTRANS table for the specified
#' entity. This corresponds with the names, codes and hierarchy information 
#' for each pollen variable recognized by the Pollen Database, 
#' including synonyms.
#' @rdname get_samples
#' @export
.get_pvtrans <- function(var_, connection){
  table <- "p_vtrans"
  if (length(var_) !=  0){
    var_ <- paste(var_, collapse = "','")
    sql_query <- paste("SELECT * FROM ", table, " WHERE var_ IN ('",
                       var_, "');", sep = "")
    sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  }
  if (length(var_)  ==  0 || nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_syntype:
#' This functions returns a data.frame with information in in the SYNTYPE table for the specified
#' entity. This corresponds with full description of the synonym type codes 
#' used in P_VARS and M_VARS
#' @rdname get_samples
#' @export
.get_syntype <- function(syntype, connection){
  table <- "syntype"
  syntype <- stats::na.omit(syntype)
  if (length(syntype) !=  0){
    syntype <- paste(syntype, collapse = "','")
    sql_query <- paste("SELECT * FROM ", table, " WHERE syntype IN ('",
                       syntype, "');", sep = "")
    sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  }
  if (length(syntype)  ==  0 || nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}

#' @section .get_pgroup:
#' This functions returns a data.frame with information in in the P_GROUP table for the specified
#' entity. This corresponds with  group to which each pollen variable is 
#' assigned. These tables enable the user to define taxa into groups based 
#' on the life form and habitat of each taxon (e.g. trees, aquatics).
#' @rdname get_samples
#' @export
.get_pgroup <- function(var_, connection){
  table <- "p_group"
  if (length(var_) !=  0){
    var_ <- paste(var_, collapse = "','")
    sql_query <- paste("SELECT * FROM ", table, " WHERE var_ IN ('",
                       var_, "');", sep = "")
    sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  }
  if (length(var_)  ==  0 || nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  sql_out$groupid[which(sql_out$var_  ==  "5148")] <- "AQUA"
  sql_out$groupid[which(sql_out$var_  ==  "5205")] <- "NOPO"
  return(sql_out)
}

#' @section .get_groups:
#' This functions returns a data.frame with information in in the GROUPS table for the specified
#' entity. This corresponds with a look-up table for the names and codes for
#' variable groups.
#' @rdname get_samples
#' @export
.get_groups <- function(groupid, connection){
  table <- "groups"
  if (length(groupid) !=  0){
    groupid <- paste(groupid, collapse = "','")
    sql_query <- paste("SELECT * FROM ", table, " WHERE groupid IN ('",
                       groupid, "');", sep = "")
    sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  }
  if (length(groupid)  ==  0 || nrow(sql_out)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }
  return(sql_out)
}



# get_site functions -------------------------------------------------------

#' Retrieve site information for EPD entities
#'
#' Functions in this family retrieve information relative to 
#' the site  where the entity has been sampled. The main function 
#' (\code{\link[EPDr]{get_site}}) requires a valid connection to the 
#' database and the entity ID for the entity of interest. All other 
#' functions (starting with a dot [.]) use different arguments 
#' depending on the piece of information they retrieve.
#' 
#' @param e_ numeric Value indicating the entity number (e_) of the 
#' database that want to be queried.
#' @param site_ numeric Value indicating the site number (site_) of 
#' interest in the database.
#' @param poldiv1_ character Three character string. The three 
#' character string are the international country code.
#' @param poldiv2_ character Two number string. This string with 
#' length equal two and with numbers represent the regions code for 
#' administrative regions in each country. The code is not unique 
#' so to capture an specific region in a country you need to provide 
#' always country code (poldiv1_) and region code (poldiv2_).
#' @param poldiv3_ character Three number string. This string with 
#' length equal three and with numbers represent the 3rd level 
#' administrative regions in each country.  The code is not unique 
#' so to capture an specific 3rd level region in a country you need 
#' to provide always country code (poldiv1_), region code (poldiv2_), 
#' and 3rd level region code (poldiv3_).
#' @param igcptype character Representing the IGCP type code.
#' @param icode character Three letter string representing the 
#' information code.
#' @param connection PostgreSQLConnection Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return \code{\link[EPDr]{site}} object. This is an EPDr object 
#' with information from different tables. See documentation of the EPD: 
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # site.400 <- get_site(400, epd.connection)
#' # site.400
#' 
#' @section get_site:
#' This function returns a \code{\link[EPDr]{site}} object with several 
#' information from the rest of the functions in the group for a 
#' particular entity.
#' @rdname get_site
#' @export
get_site <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single entities. You have provided ",
                  "several entity ID values (e_) but only the first one ",
                  "is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT site_ FROM entity WHERE e_  = ", e_, ";", sep = "")
  site_ <- as.character(RPostgreSQL::dbGetQuery(connection, sql_query))
  siteloc <- .get_siteloc(site_, connection)
  sitedesc <- .get_sitedesc(site_, connection)
  siteinfo <- .get_siteinfo(site_, connection)
  country <- .get_poldiv1(siteloc$poldiv1, connection)
  region <- .get_poldiv2(siteloc$poldiv2, siteloc$poldiv1, connection)
  region3rd <- .get_poldiv3(siteloc$poldiv3, siteloc$poldiv2,
                            siteloc$poldiv1, connection)
  igcptype <- .get_igcptype(sitedesc$igcptype, connection)
  infotype <- .get_infotype(siteinfo$icode, connection)
  publ <- get_publ(siteinfo$publ_, connection)
  site <- new("site",
              siteloc = siteloc,
              sitedesc = sitedesc,
              siteinfo = siteinfo,
              country = country,
              region = region,
              region3rd = region3rd,
              igcptype = igcptype,
              infotype = infotype,
              publ = publ)
  return(site)
}

#' @section .get_siteloc:
#' This functions returns a data.frame with information in in the SITELOC table for the 
#' specified entity. This corresponds with location data for the site 
#' where samples were taken.
#' @rdname get_site
#' @export
.get_siteloc <- function(site_, connection){
  if (length(site_) > 1){
    site_ <- site_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single sites. You have provided several ",
                  "site ID values (site_) but only the first one is going ",
                  "to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM siteloc WHERE site_  = ",
                     site_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(site_ = NA, sitename = NA, sitecode = NA,
                          siteexist = NA, poldiv1 = NA, poldiv2 = NA,
                          poldiv3 = NA, latdeg = NA, latmin = NA,
                          latseg = NA, latns = NA, latdd = NA,
                          latdms = NA, londeg = NA, lonmin = NA,
                          lonseg = NA, lonns = NA, londd = NA,
                          londms = NA, elevation = NA,
                          areaofsite = NA)[-1, ]
  }
  return(sql_out)
}

#' @section .get_sitedesc:
#' This functions returns a data.frame with information in in the SITEDESC table for the 
#' specified entity. This corresponds with a description of the site 
#' where samples were taken.
#' @rdname get_site
#' @export
.get_sitedesc <- function(site_, connection){
  if (length(site_) > 1){
    site_ <- site_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single site. You have provided several ",
                  "site ID values (site_) but only the first one is going ",
                  "to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM sitedesc WHERE site_ = ",
                     site_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(site_ = NA, sitedescript = NA, physiography = NA,
                          surroundveg = NA, vegformation = NA,
                          igcptype = NA)[-1, ]
  }
  return(sql_out)
}

#' @section .get_siteinfo:
#' This functions returns a data.frame with information in in the SITEINFO table for the 
#' specified entity. This corresponds with a summary data of all type 
#' of information in the database for that particular entity 
#' (chronological, palynological, etc).
#' @rdname get_site
#' @export
.get_siteinfo <- function(site_, connection){
  if (length(site_) > 1){
    site_ <- site_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single site. You have provided several ",
                  "site ID values (site_) but only the first one is going ",
                  "to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM siteinfo WHERE site_ = ",
                     site_, ";", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(site_ = NA, icode = NA, publ_ = NA)[-1, ]
  }
  return(sql_out)
}

#' @section .get_poldiv1:
#' This functions returns a data.frame with information in in the POLDIV1 table for the 
#' specified country (poldiv1_ is the country code). This corresponds 
#' with data of the country in which a site belong to.
#' @rdname get_site
#' @export
.get_poldiv1 <- function(poldiv1_, connection){
  if (length(poldiv1_) > 1){
    poldiv1_ <- poldiv1_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single country. You have provided ",
                  "several country ID values (poldiv1_) but only the ",
                  "first one is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM poldiv1 WHERE poldiv1 = '",
                     poldiv1_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(poldiv1_ = NA, name = NA)[-1, ]
  }
  return(sql_out)
}

#' @section .get_poldiv2:
#' This functions returns a data.frame with information in in the POLDIV2 table for the 
#' specified region (poldiv2_ is the region code). This corresponds 
#' with data of the region in which a site belong to.
#' @rdname get_site
#' @export
.get_poldiv2 <- function(poldiv2_, poldiv1_, connection){
  if (length(poldiv1_) > 1 | length(poldiv2_) > 1){
    poldiv1_ <- poldiv1_[[1]]
    poldiv2_ <- poldiv2_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single countries and regions. You have ",
                  "provided several countries and/or regions ID values ",
                  "(poldiv1_ or poldiv2_) but only the first one of each is ",
                  "going to be used.", sep = ""))
  }

  sql_query <- paste("SELECT * FROM poldiv2 WHERE poldiv1 = '", poldiv1_,
                     "' AND poldiv2 = '", poldiv2_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(poldiv1 = NA, poldiv2 = NA, postcode = NA,
                          name = NA)[-1, ]
  }
  return(sql_out)
}

#' @section .get_poldiv3:
#' This functions returns a data.frame with information in in the POLDIV3 table for the 
#' specified 3rd level region (poldiv3_ is the 3rd level region code). 
#' This corresponds with data of the 3rd level region in which a site
#' belong to.
#' @rdname get_site
#' @export
.get_poldiv3 <- function(poldiv3_, poldiv2_, poldiv1_, connection){
  if (length(poldiv1_) > 1 | length(poldiv2_) > 1 | length(poldiv3_) > 1){
    poldiv1_ <- poldiv1_[[1]]
    poldiv2_ <- poldiv2_[[1]]
    poldiv3_ <- poldiv3_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single third level regions. You have ",
                  "provided several countries, regions and/or third level ",
                  "regions ID values (poldiv1_, poldiv2_ and/or poldiv3_) ",
                  "but only the first one of each is going to be used.",
                  sep = ""))
  }
  sql_query <- paste("SELECT * FROM poldiv3 WHERE poldiv1 = '", poldiv1_,
                     "' AND poldiv2 = '", poldiv2_, "' AND poldiv3 = '",
                     poldiv3_, "';", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(poldiv1 = NA, poldiv2 = NA, poldiv3 = NA,
                          name = NA)[-1, ]
  }
  return(sql_out)
}

#' @section .get_igcptype:
#' This functions returns a data.frame with information in in the IGCPTYPE table for the 
#' specified IGCP region. This corresponds with data of the IGCP 
#' region in which a site belong to.
#' @rdname get_site
#' @export
.get_igcptype <- function(igcptype, connection){
  if (length(igcptype) > 1){
    igcptype <- igcptype[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve ",
                  "information for single IGCP type. You have provided ",
                  "several IGCP type values (igcptype) but only the ",
                  "first one is going to be used.", sep = ""))
  }
  sql_query <- paste("SELECT * FROM igcptype WHERE igcptype = '",
                     igcptype, "';")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(igcptype = NA, regionname = NA)[-1, ]
  }
  return(sql_out)
}

#' @section .get_infotype:
#' This functions returns a data.frame with information in in the INFOTYPE table for the 
#' specified info code (icode). This corresponds with a longer 
#' description of the info type codes returned by \code{.get_siteinfo}.
#' @rdname get_site
#' @export
.get_infotype <- function(icode, connection){
  icode <- paste(icode, collapse = "','")
  sql_query <- paste("SELECT * FROM infotype WHERE icode IN ('",
                     icode, "');", sep = "")
  sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
  if (nrow(sql_out)  ==  0){
    sql_out <- data.frame(icode = NA, infotype = NA)[-1, ]
  }
  return(sql_out)
}




# General get functions ---------------------------------------------------

#' Retrieve publications from their publ ID number
#'
#' This function is mainly intended for internal use. It retrieves 
#' information of publications by querying the database by the 
#' publication identification number.
#'
#' @param publ_ numeric Value indicating the publication identification number (publ_)
#' @param connection PostgreSQLConnection Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return data.frame Data frame containing information about the whole
#' publication reference.
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connect_to_epd(database = "epd", user = "epdr",
#' #                                 password = "epdrpw", host = "localhost")
#' # get_publ(1, epd.connection)
#' 
get_publ <- function(publ_, connection){
  publ_ <- stats::na.omit(publ_)
  table <- "publ"
  if (is.logical(publ_) | length(publ_)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }else{
    if (is.numeric(publ_)){
      publ_ <- paste(publ_, collapse = "','")
      sql_query <- paste("SELECT * FROM ", table, " WHERE publ_ IN ('",
                         publ_, "');", sep = "")
      sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
      if (nrow(sql_out)  ==  0){
        names <- RPostgreSQL::dbListFields(connection, table)
        sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
        colnames(sql_out) <- names
      }
    }else{
      stop("publ_ should be numeric.")
    }
  }
  return(sql_out)
}



#' Retrieves workers information
#'
#' This function retrieves details on the workers specified in different 
#' tables of the EPD, such as analyst of chronological and/or palynological
#' samples, etc. The information comes from the WORKERS table.
#'
#' @param worker_ numeric Value indicating the worker identification number.
#' @param connection PostgreSQLConnection. Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return data.frame The function returns a data frame with all information
#' in the WORKERS table for that particular worker id.
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connect_to_epd(database = "epd", user = "epdr",
#' #                                 password = "epdrpw", host = "localhost")
#' # get_workers(1, epd.connection)
#' 
get_workers <- function(worker_, connection){
  table <- "workers"
  worker_ <- stats::na.omit(worker_)
  if (is.logical(worker_) | length(worker_)  ==  0){
    names <- RPostgreSQL::dbListFields(connection, table)
    sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
    colnames(sql_out) <- names
  }else{
    if (is.numeric(worker_)){
      worker_ <- paste(worker_, collapse = "','")
      sql_query <- paste("SELECT * FROM ", table, " WHERE worker_ IN ('",
                         worker_, "');", sep = "")
      sql_out <- RPostgreSQL::dbGetQuery(connection, sql_query)
      if (nrow(sql_out)  ==  0){
        names <- RPostgreSQL::dbListFields(connection, table)
        sql_out <- data.frame(t(rep(NA, length(names))))[-1, ]
        colnames(sql_out) <- names
      }
    }else{
      stop("worker_ should be numeric.")
    }
  }
  return(sql_out)
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
#' @param connection PostgreSQLConnection. Object of class 
#' \code{PostgreSQLConnection} as returned by function 
#' \code{\link[EPDr]{connect_to_epd}}.
#'
#' @return data.frame The function return a data.frame with the 
#' combined information from P_VARS and P_GROUP tables of the 
#' database (see documentation of the EPD:
#' \url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
#' 
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # epd.taxonomy <- get_taxonomy_epd(epd.connection)
#' # epd.taxonomy
#' 
get_taxonomy_epd <- function(connection){
  sql_query <- paste("SELECT * FROM p_vars NATURAL JOIN p_group")
  results <- RPostgreSQL::dbGetQuery(connection, sql_query)
  return(results)
}



#' Retrieve all information for an EPD entity
#'
#' This function wraps all other \code{get_} and \code{.get_} functions
#' to retrieve and store in a single object all the information relative to
#' a particular entity in the European Pollen Database (EPD). 
#' 
#' The function also import in the objects information from Giesecke et al.
#' (2013), if available for that entity. The chronology used by Giesecke are
#' numbered as 9999 to distinguish them from those stored in the EPD. It also
#' check the geographical location of the entity and import the postbomb
#' zone of the world.
#' 
#' @param e_ numeric Number indicating the identification number (e_) of the
#' entity that is desired. If multiple numbers are passed the function 
#' retrieves data only for the first one.
#' @param connection PostgreSQLConnection A valid PostgreSQL connection to
#' the EPD database server.
#'
#' @return The function returns an \code{\link[EPDr]{epd.entity}} object, which
#' countains several objects with several tables.
#' 
#' @references Giesecke, T., Davis, B., Brewer, S., Finsinger, W., 
#' Wolters, S., Blaaw, M., de Beaulieu, J.L., Binney, H., Fyfe, R.M.,
#' Gaillard, M.J., Gil-Romera, G., van der Knaap, W.O. Kunes, P.,
#' Kuhl, N., van Leeuwen, J.F.N, Leydet, M., Lotter, A.F., Ortu, E.,
#' Semmler, M., and Bradshaw, R.H.W (2013). Towards mapping the late
#' Quaternary vegetation change of Europe. Vegetation History and
#' Archaeobotany, 23, 75-86.
#'
#' @export
#'
#' @examples
#' # Not run
#' # library(EPDr)
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # epd.400 <- get_entity(400, epd.connection)
#' 
get_entity <- function(e_, connection){
  if (length(e_) > 1){
    e_ <- e_[[1]]
    warning(paste(match.call()[[1]], " function is designed to retrieve
                  information for single entities. You have provided several
                  entity ID values (e_) but only the first one is going to
                  be used.", sep = ""))
  }
  entity <- get_ent(e_, connection)
  site <- get_site(e_, connection)
  geochron <- get_geochron(e_, connection)
  chron <- get_chron(e_, connection)
  samples <- get_samples(e_, connection)

  if (e_ %in% giesecke.EpdAgeCut$ID){
    isingiesecke <- TRUE
    g.ages <- giesecke.EpdAgeCut[which(giesecke.EpdAgeCut$ID  ==  e_), ]
    g.ages$depthcm <- round(g.ages$Depth..m. * 100, 1)
    g.ages <- g.ages[match(round(samples@psamples$depthcm, 1),
                           g.ages$depthcm), ]
    g.ages <- merge(g.ages, samples@psamples, by = "depthcm")
    g.pagedpt <- subset(g.ages, select = c("e_", "sample_", "Age.dated..ka.",
                                           "Age.min..ka.", "Age.max..ka."))
    g.pagedpt$agebp <- g.ages$Age.dated..ka. * 1000
    g.pagedpt$agelo <- g.ages$Age.min..ka. * 1000
    g.pagedpt$ageup <- g.ages$Age.max..ka. * 1000
    g.pagedpt$chron_ <- 9999
    g.pagedpt$deptime <- NA
    g.pagedpt <- subset(g.pagedpt, select = c("e_", "chron_", "sample_",
                                              "agebp", "ageup", "agelo",
                                              "deptime"))
    samples@pagedpt <- rbind(samples@pagedpt, g.pagedpt)
    g.agebasis <- giesecke.agebasis[which(giesecke.agebasis$ID  ==  e_), ]
    g.agebasis <- g.agebasis[order(g.agebasis$Depth..m.), ]
    g.agebasis$depthcm <- round(g.agebasis$Depth..m. * 100, 1)
    g.agebasis$thickness <- round(g.agebasis$Thickness..m. * 100, 1)
    ages.a <- g.agebasis$Age.dated..ka...Age.expressed.in.radiocarbon.....
    ages.b <- g.agebasis$Age.dated..ka...Age.expressed.in.calendar.kil....
    g.agebasis$age <- ifelse(!is.na(ages.a), ages.a, ages.b)
    g.agebasis$age <- g.agebasis$age * 1000
    g.agebasis$error <- g.agebasis$Age.e..A.. * 1000
    g.agebasis$ageup <- g.agebasis$age + g.agebasis$error
    g.agebasis$agelo <- g.agebasis$age - g.agebasis$error

    g.agebasis <- subset(g.agebasis,
                      select = c("ID",
                                 "depthcm",
                                 "thickness",
                                 "age",
                                 "ageup",
                                 "agelo",
                                 "Code")
                      )
    colnames(g.agebasis) <- c("e_", "depthcm", "thickness", "age",
                              "ageup", "agelo", "rcode")
    g.agebasis$chron_ <- 9999
    g.agebasis$sample_ <- seq_len(nrow(g.agebasis))
    chron@agebasis <- rbind(chron@agebasis, g.agebasis)
  }else{
    isingiesecke <- FALSE
  }

  coords <- subset(site@siteloc, select = c("londd", "latdd"))
  postbombzone <- sp::over(sp::SpatialPoints(coords), postbomb.map)$Zone
  numberofchron <- nrow(chron@chron)
  defaultchron <- chron@chron$chron_[which(chron@chron$defaultchron  ==  "Y")]
  if (is.null(defaultchron)){
    defaultchron <- 0
  }
  if (length(defaultchron)  ==  0){
    defaultchron <- 0
  }

  epd.entity <- new("epd.entity",
                    e_ = e_,
                    postbombzone = postbombzone,
                    numberofchron = numberofchron,
                    isingiesecke = isingiesecke,
                    defaultchron = defaultchron,
                    entity = entity,
                    site = site,
                    geochron = geochron,
                    chron = chron,
                    samples = samples)
  return(epd.entity)
}
