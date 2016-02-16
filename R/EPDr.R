
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
#' @param con TBW
#'
#' @return TBW
#' @export
#'
#' @examples
#' # TBW
disconnectFromEPD <- function(con=NULL){
    # Close PostgreSQL connection
    if(is.null(con))stop("You have to define a working connection to the EPD to be stoped")
    dbDisconnect(con)
}


# Mirar en la web http://www.europeanpollendatabase.net el c?digo del testigo que nos interesa, (usar ese c?digo en core.num)
#' Title
#'
#' @param core.num TBW
#' @param conn TBW
#' @param get.dephts TBW
#'
#' @return TBW
#' @export
#'
#' @examples
#' # TBW
getSiteForClam <- function(core.num, conn, get.dephts=TRUE){
    # Just for testing. Do no uncomment
    # core.num <- "555"
    # conn <- connEPD
    # get.dephts <- TRUE

    sqlQuery <- paste("SELECT site_ FROM entity WHERE e_=", core.num, ";", sep="")
    site.num <- as.character(dbGetQuery(conn, sqlQuery))
    
    sqlQuery <-paste("SELECT * FROM siteloc WHERE site_=", site.num, ";", sep="")
    siteloc.table <- dbGetQuery(conn, sqlQuery)
    
    sqlQuery <- paste("SELECT * FROM agebasis WHERE e_=", core.num, ";", sep="")
    agebasis.table <- dbGetQuery(conn, sqlQuery)
    
    sqlQuery <- paste("SELECT * FROM c14 WHERE e_=", core.num, ";", sep="")
    c14.table <- dbGetQuery(conn, sqlQuery)
    
    sqlQuery <-paste("SELECT * FROM geochron WHERE e_=", core.num, ";", sep="")
    geochron.table <- dbGetQuery(conn, sqlQuery)

    c14geochron.table <- merge(c14.table, geochron.table, by=c("e_","sample_"))
    
    output.table <- data.frame(lab_ID=c14geochron.table$labnumber, C14_age=c14geochron.table$agebp)
    output.table$cal_age <- NA        
    output.table$error <- c14geochron.table$agesdup
    output.table$reservoir <- NA
    output.table$depth <- c14geochron.table$depthcm
    output.table$thicknesses <- c14geochron.table$thickness
    
    sqlQuery <-paste("SELECT * FROM chron WHERE e_=", core.num, ";", sep="")
    chron.table <- dbGetQuery(conn, sqlQuery)

    sqlQuery <-paste("SELECT * FROM section WHERE e_=", core.num, ";", sep="")
    section.table <- dbGetQuery(conn, sqlQuery)

    sqlQuery <-paste("SELECT * FROM synevent WHERE e_=", core.num, ";", sep="")
    synevent.table <- dbGetQuery(conn, sqlQuery)
    
    if(nrow(synevent.table) == 0){
        cat("This core has not events in the chronology.")
    }else{
        sqlQuery <-paste("SELECT * FROM event WHERE event_ IN (", paste(synevent.table[,"event_"], collapse=","), ");", sep="")
        event.table <- dbGetQuery(conn, sqlQuery)
        event.table <- merge(synevent.table, event.table, by="event_")
        warning("This core has events in the chronology.", immediate.=T, call.=F)

        include.events <- readline("Do you want to include this information in the files? (Yes: Press Y then Intro, No: Anything else)")
        if(include.events == "Y"){
            warning("Functionality not implemented yet. Thank you and stay tuned.", call.=F)
        }
    }


    # # ALL THIS TO BE REMOVED
    # # Combinaci?n de las tablas siteTable y c14Table (a partir de e_ y sample_ en la EPD)
    # c14.table$age <- c14.table$agebp
    # new.table <- merge(agebasis.table, c14.table, by=c("e_", "age"), all.x = T)
    # 
    # # Extraer las columnas que interesan para el archivo .csv de clam
    # subNew.table <- new.table[,c("age", "agesdup", "depthcm", "thickness", "rcode")]
    # subNew.table$age[subNew.table$rcode != "C14"] <- NA
    # 
    # # Renombrar las columnas seg?n los par?metros de clam.
    # 
    # colnames(subNew.table) <- c("C14_age", "error", "depth", "thicknesses", "rcode")
    # 
    # subNew.table$lab_ID <- paste(new.table$e_, "-", new.table$sample_.x, sep="")
    # subNew.table$cal_age <- NA
    # subNew.table$cal_age[subNew.table$rcode != "C14"] <- new.table$age[subNew.table$rcode != "C14"]
    # subNew.table$error[is.na(subNew.table$error)] <- 1
    # subNew.table$reservoir <- NA
    # 
    # # Creaci?n del archivo .csv para el sitio seleccionado con las siguientes columnas.
    # subNew.table <- subNew.table[c("lab_ID","C14_age","cal_age","error","reservoir","depth","thicknesses")]
    # # REMOVED UNTIL HERE
    
    # Creamos una carpeta para guardar los ficheros necesarios para lanzar clam
    if(!dir.exists(paste("Cores/", core.num, sep=""))){
        dir.create(paste("Cores/", core.num, sep=""), recursive=TRUE)
    }
    
    # Order dataframe by depths and write to the directory
    output.table <- output.table[order(output.table$depth),]    
    write.csv(output.table, file=paste("Cores/", core.num, "/", core.num, ".csv", sep=""), na="", row.names=FALSE)
    
    # Extraer la columna de las profundidades y crear una tabla nombresitio_depths.txt.
    if(get.dephts==T){
        sqlQuery <- paste("select * from p_sample where e_=", core.num, ";", sep="")
        depth.table <- dbGetQuery(conn, sqlQuery)
        depth.table$lab_ID <- paste(depth.table$e_, "-", depth.table$sample_, sep="")
        depth.table <- depth.table[order(depth.table$depthcm),]
        
        write.table(depth.table$depthcm, file=paste("Cores/", core.num, "/", core.num, "_depths.txt", sep=""), col.names=FALSE, na="", row.names=FALSE)
        write.table(depth.table[,c("lab_ID", "depthcm")], file=paste("Cores/", core.num, "/", core.num, "_depths_ID.txt", sep=""), col.names = F, na="", row.names=FALSE, sep=",")
    }
}



