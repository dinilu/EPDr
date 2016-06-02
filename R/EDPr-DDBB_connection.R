#' Conect to a EPD database
#'
#' \code{connectToEPD} establish a connection to a EPD data base that is stored in a DDBB server. By default it assume a local
#' PostgreSQL server. The function can connect with remote servers in different formats (MySQL, etc; see RPostgreSQL documentation
#' for supported formats). To connect to the DDBB the function need the DDBB name, the user name, and the user password. If any of
#' the data are not passed as arguments the function will ask for them interactively.
#'
#' @param database Character string with the DDBB name. If not provided the function will ask for it before establishing the connection.
#' @param user Character string with the user name. A valid user in the DDBB server. If not provided the function will ask for it
#' before establishing the connection.
#' @param password Character string with the user password. A valid password for the user in the DDBB server. If not provided the
#' function will ask for it before establishing the connection.
#' @param driver Character string with the driver used to connect with the DDBB server (default: "PostgreSQL"). This value will depend
#' on the DDBB server used to host the EPD database. For alternatives look at the \code{\link[DBI:dbConnect]{dbConnect}} function.
#' @param host Character string with the IP address of the DDBB server (default: "localhost").
#'
#' @return This function return a RPostgreSQL connection object.
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD()
#' #epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' #epd.connection
#' # To list all the tables in the database we have connected with
#' #dbListTables(epd.connection)
#' # Query data from the connection with a SQL statement
#' #dbGetQuery(epd.connection, "SELECT e_ FROM synevent;")
#' #disconnectFromEPD(connection=epd.connection)
connectToEPD <- function(database=NULL, user=NULL, password=NULL, driver="PostgreSQL", host="localhost"){
    # Ask interactively for parameters if they are not specified
    if(is.null(driver))driver <- readline("EPD DB driver:")
    if(is.null(database))database <- readline("EPD DB name:")
    if(is.null(user))user <- readline("EPD DB user:")
    if(is.null(password))password <- readline("EPD DB password:")
    
    # Establish connection to PoststgreSQL
    con <- dbConnect(driver, dbname=database, host=host, user=user, password=password)
    return(con)
}



#' Disconnect a connection to a EPD database
#' \code{disconnectFromEPD} turns down a connection to a EPD DDBB server.
#'
#' @param connection The connection object created with \code{\link[EPDr:connectToEPD]{connectToEPD}} to stablish the connection
#'
#' @return NULL It just disconnect from the EPD DDBB server and modify the connection object to reflect the new status.
#' 
#' @export
#' 
#' @examples
#' #epd.connection <- connectToEPD(database="epd_ddbb", user="epdr",
#' #                                 password="epdrpw", host="localhost")
#' #disconnectFromEPD(connection=epd.connection)
#' #epd.connection
disconnectFromEPD <- function(connection=NULL){
    # Close PostgreSQL connection
    if(is.null(connection))stop("You have to define a working connection to the EPD to be stoped")
    dbDisconnect(connection)
}

