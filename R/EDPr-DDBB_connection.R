#' Conect to a EPD database
#'
#' \code{connect_to_epd} establish a connection to a EPD data base that is 
#' stored in a DDBB server. By default it assume a local PostgreSQL server.
#' The function can connect with remote servers in different formats (MySQL, 
#' etc.; see RPostgreSQL documentation for supported formats). To connect to 
#' the DDBB the function need the DDBB name, the user name, and the user 
#' password. If any of the data are not passed as arguments the function 
#' will ask for them interactively.
#' 
#' A companion vignette is provided in the package to explain how to 
#' \href{../doc/EPD-PostgreSQL.html}{set up the EPD in a PostgreSQL server}.
#'
#' @param database character Character string with the DDBB name. If not provided the 
#' function will ask for it before establishing the connection.
#' @param user character Character string with the user name. A valid user in the 
#' DDBB server. If not provided the function will ask for it before 
#' establishing the connection.
#' @param password character Character string with the user password. A valid 
#' password for the user in the DDBB server. If not provided the
#' function will ask for it before establishing the connection.
#' @param driver character Character string with the driver used to connect 
#' with the DDBB server (default: "PostgreSQL"). This value will depend
#' on the DDBB server used to host the EPD database. Look at the 
#' \code{\link[DBI]{dbConnect}} function for alternatives.
#' @param host character Character string with the IP address of the DDBB 
#' server (default: "localhost").
#'
#' @return This function returns a RPostgreSQL connection object.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd()
#' epd.connection <- connect_to_epd(database="epd", user="epdr",
#'                                 password="epdrpw", host="localhost")
#' epd.connection
#' # To list all the tables in the database we have connected with
#' dbListTables(epd.connection)
#' Query data from the connection with a SQL statement
#' dbGetQuery(epd.connection, "SELECT e_ FROM synevent;")
#' disconnect_from_epd(connection=epd.connection)
#' }
connect_to_epd <- function(database=NULL, user=NULL, password=NULL,
                         driver="PostgreSQL", host="localhost"){
    if (is.null(driver)) driver <- readline("EPD DB driver:")
    if (is.null(database)) database <- readline("EPD DB name:")
    if (is.null(user)) user <- readline("EPD DB user:")
    if (is.null(password)) password <- readline("EPD DB password:")
    con <- RPostgreSQL::dbConnect(driver, dbname = database, host = host,
                                  user = user, password = password)
    return(con)
}



#' Disconnect a connection to a EPD database
#' 
#' \code{disconnect_from_epd} turns down a connection to a EPD DDBB server.
#' 
#' @param connection PostgreSQLConnection The connection object created with 
#' \code{\link[EPDr]{connect_to_epd}} to stablish the connection
#' 
#' @return NULL It just disconnects from the EPD DDBB server and modifies 
#' the connection object to reflect the new status.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(database="epd", user="epdr",
#'                                 password="epdrpw", host="localhost")
#' disconnect_from_epd(connection=epd.connection)
#' epd.connection
#' }
disconnect_from_epd <- function(connection=NULL){
    if (is.null(connection)) stop("You have to define a working connection to
                                   the EPD to be stoped")
    RPostgreSQL::dbDisconnect(connection)
}
