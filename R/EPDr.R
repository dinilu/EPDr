
#' Conect to a EPD database
#'
#' \code{"connectToEPD"} establish a connection to a EPD data base that is stored in a DDBB server. By default it assume a local PostgreSQL server. Potentially the function should work with other servers, but it has not been tested. Similarly, the function could connect to remote servers, but this possibility has not been tested yet. To connect to the DDBB the function need the DDBB name, the user name, and the user password. If any of the data are not passed as arguments the function will ask for them interactively.
#'
#' @param DB Character string with the DDBB name. If not provided the function will ask for it before establishing the connection.
#' @param US Character string with the user name. A valid user in the DDBB server. If not provided the function will ask for it before establishing the connection.
#' @param PW Character string with the user password. A valid password for the user in the DDBB server. If not provided the function will ask for it before establishing the connection.
#' @param DRV Character string with the driver used to connect with the DDBB server (default: "PostgreSQL"). This value will depend on the DDBB server used to host the EPD database. For alternatives look at the \code{\link[DBI:dbConnect]{dbConnect}} function.
#' @param HOST Character string with the IP address of the DDBB server (default: "localhost").
#'
#' @return This function return a connection
#' @export
#'
#' @examples
#' # Not run
#' # connectToEPD()
#' # connectToEPD("", "", "")
connectToEPD <- function(DB=NULL, US=NULL, PW=NULL, DRV="PostgreSQL", HOST="localhost"){

    # Ask interactively for parameters if they are not specified
    if(is.null(DRV))DRV <- readline("EPD DB driver:")
    if(is.null(DB))DB <- readline("EPD DB name:")
    if(is.null(US))US <- readline("EPD DB user:")
    if(is.null(PW))PW <- readline("EPD DB password:")

    # Establish connection to PoststgreSQL
    con <- dbConnect(DRV, dbname=DB, host=HOST, user=US, password=PW)

    return(con)
}


# Disconnect a connection to a EPD database
#' Title
#'
#' @param con TBC
#'
#' @return TBC
#' @export
#'
#' @examples
#' # TBC
disconnectFromEPD <- function(con=NULL){
    # Close PostgreSQL connection
    if(is.null(con))stop("You have to define a working connection to the EPD to be stoped")
    dbDisconnect(con)
}


# Mirar en la web http://www.europeanpollendatabase.net el c?digo del testigo que nos interesa, (usar ese c?digo en site.num)
#' Title
#'
#' @param site.num TBC
#' @param ddbb TBC
#' @param get.dephts TBC
#'
#' @return TBC
#' @export
#'
#' @examples
#' # TBC
getSiteForClam <- function(site.num, ddbb, get.dephts=TRUE){
    site.num <- "2200"
    ddbb <- connEPD
    get.dephts <- TRUE
    # Creaci?n de las tablas siteTable y c14Table a partir de agebasis y c14 respectivamente
    sqlQuery <- paste("select * from agebasis where e_=", site.num, ";", sep="")
    siteTable <- dbGetQuery(ddbb, sqlQuery)

    sqlQuery <- paste("select * from c14 where e_=", site.num, ";", sep="")
    c14Table <- dbGetQuery(ddbb, sqlQuery)

    #  This has to be checked
    #if(all(any(siteTable$age == 0), any(c14Table$agebp != 0))) c14Table$sample_ <- c14Table$sample_ + 1
    #newTable <- merge(siteTable, c14Table, by=c("e_", "sample_"), all.x = T)

    # Combinaci?n de las tablas siteTable y c14Table (a partir de e_ y sample_ en la EPD)
    c14Table$age <- c14Table$agebp
    newTable <- merge(siteTable, c14Table, by=c("e_", "age"), all.x = T)

    # Extraer las columnas que interesan para el archivo .csv de clam
    subNewTable <- newTable[,c("age", "agesdup", "depthcm", "thickness", "rcode")]
    subNewTable$age[subNewTable$rcode != "C14"] <- NA

    # Renombrar las columnas seg?n los par?metros de clam.

    colnames(subNewTable) <- c("C14_age", "error", "depth", "thicknesses", "rcode")

    subNewTable$lab_ID <- paste(newTable$e_, "-", newTable$sample_.x, sep="")
    subNewTable$cal_age <- NA
    subNewTable$cal_age[subNewTable$rcode != "C14"] <- newTable$age[subNewTable$rcode != "C14"]
    subNewTable$error[is.na(subNewTable$error)] <- 1
    subNewTable$reservoir <- NA

    # Creaci?n del archivo .csv para el sitio seleccionado con las siguientes columnas.
    subNewTable <- subNewTable[c("lab_ID","C14_age","cal_age","error","reservoir","depth","thicknesses")]

    # Creamos una carpeta para guardar los ficheros necesarios para lanzar clam
    if(!dir.exists(paste("Cores/", site.num, sep=""))){
        dir.create(paste("Cores/", site.num, sep=""), recursive=TRUE)
    }

    # Extraer la columna de las profundidades y crear una tabla nombresitio_depths.txt.
    write.csv(subNewTable, file=paste("Cores/", site.num, "/", site.num, ".csv", sep=""), na="", row.names=FALSE)

    if(get.dephts==T){
        sqlQuery <- paste("select * from p_sample where e_=", site.num, ";", sep="")
        depthTable <- dbGetQuery(ddbb, sqlQuery)
        depthTable$lab_ID <- paste(depthTable$e_, "-", depthTable$sample_, sep="")

        write.table(depthTable$depthcm, file=paste("Cores/", site.num, "/", site.num, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        write.table(depthTable[,c("lab_ID", "depthcm")], file=paste("Cores/", siteNum, "/", siteNum, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE)
    }
}




