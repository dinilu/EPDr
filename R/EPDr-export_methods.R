# export_agebasis -----------------------------------------------

#' Reshape agebasis table to CLAM or BACON format
#' 
#' This function takes agebasis data, as those from 
#' \code{\link[EPDr]{.get_agebasis}} or from an \code{\link[EPDr]{epd.entity}},
#' used to calibrate chronologies of that entity in the EPD, and modifies
#' them to fit into a new table that complies with CLAM or BACON format.
#'
#' @param format character Character string indicating whether to export 
#' to "clam" or to "bacon" format.
#' @param x data.frame Data frame or \code{\link[EPDr]{epd.entity}} with
#' agebasis data as those in an agebasis table returned by 
#' \code{\link[EPDr]{.get_agebasis}} or \code{\link[EPDr]{get_entity}}.
#'
#' @return Data frame with no-C14 data in CLAM or BACON format. This 
#' data frame can be easily combined with C14 data from
#' \code{\link[EPDr]{export_c14}} using \code{rbind}.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @examples
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # epd.400 <- get_entity(400, epd.connection)
#' # epd.400.c14 <- export_c14("clam", epd.400)
#' # epd.400.ageb <- export_agebasis("clam", epd.400)
#' # rbind(epd.400.c14, epd.400.ageb)
#' # epd.400.c14 <- export_c14("bacon", epd.400)
#' # epd.400.ageb <- export_agebasis("bacon", epd.400)
#' # rbind(epd.400.c14, epd.400.ageb)
#' @rdname export_agebasis
#' @exportMethod export_agebasis
setGeneric("export_agebasis", function(format, x){
  standardGeneric("export_agebasis")
})

#' @rdname export_agebasis
setMethod("export_agebasis", signature(format = "character", x = "data.frame"),
          function(format = c("clam", "bacon"), x){
            if (!format %in% c("clam", "bacon")){
              stop(paste0("Incorrect output format. format has to ",
                          "be 'clam' or 'bacon'."))
            }
            if (nrow(x) == 0){
              warning(paste0("Table without chronology data. Returning ",
                             "an empty data.frame."))
              if (format == "clam"){
                output <- data.frame(lab_ID = NA, C14_age = NA,
                                     cal_age = NA, error = NA,
                                     reservoir = NA, depth = NA,
                                     thickness = NA)[-1, ]
              }else{
                output <- data.frame(labID = NA, age = NA,
                                     error = NA, depth = NA)[-1, ]
              }
              return(output)
            }
            output <- data.frame(lab_ID = paste("E", x$e_,
                                                "_CH", x$chron_,
                                                "_S", x$sample_,
                                                sep = ""),
                                 C14_age = x$age,
                                 error = x$ageup - x$age,
                                 depth = x$depthcm,
                                 thickness = x$thickness)
            output$cal_age <- NA
            output$error[which(is.na(output$error) | output$error == 0)] <- 1
            output$reservoir <- NA
            if (format == "clam"){
              output <- subset(output, select = c("lab_ID", "C14_age",
                                                  "cal_age", "error",
                                                  "reservoir", "depth",
                                                  "thickness"))
            }
            if (format == "bacon"){
              output <- subset(output, select = c("lab_ID", "C14_age",
                                                  "error", "depth"))
              colnames(output) <- c("labID", "age", "error", "depth")
            }
            return(output)
          }
)

#' @rdname export_agebasis
setMethod("export_agebasis", signature(format = "character", x = "epd.entity"),
          function(format, x){
            export_agebasis(format, x@chron@agebasis)
          })


# export_c14 -----------------------------------------------

#' Reshape C14 table to CLAM or BACON format
#' 
#' This function takes C14 table (data.frame), as those extracted from
#' \code{\link[EPDr]{.get_c14}} or from an \code{\link[EPDr]{epd.entity}} 
#' object, and modifies it to fit into a new table that complies with CLAM
#' or BACON format. 
#'
#' @param format character Character string indicating whether to export to 
#' "clam" or to "bacon" format.
#' @param x data.frame Data frame or \code{\link[EPDr]{epd.entity}} object 
#' with C14 data as those extracted from \code{\link[EPDr]{.get_c14}} or
#' \code{\link[EPDr]{get_entity}}.
#' @param y data.frame Data frame with geochron table, as the one extracted with
#' \code{\link[EPDr]{.get_geochron}}. If x is an
#' \code{\link[EPDr]{epd.entity}} object, y is not used.
#'
#' @return Data frame with C14 data in CLAM or BACON format.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @examples
#' # Not run
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # epd.400 <- get_entity(400, epd.connection)
#' # epd.400.c14 <- export_c14("clam", epd.400)
#' # epd.400.ageb <- export_agebasis("clam", epd.400)
#' # rbind(epd.400.c14, epd.400.ageb)
#' # epd.400.c14 <- export_c14("bacon", epd.400)
#' # epd.400.ageb <- export_agebasis("bacon", epd.400)
#' # rbind(epd.400.c14, epd.400.ageb)
#' @rdname export_c14
#' @exportMethod export_c14
setGeneric("export_c14", function(format, x, y){
  standardGeneric("export_c14")
})

#' @rdname export_c14
setMethod("export_c14", signature(format = "character", x = "data.frame",
                                 y = "data.frame"), function(format, x, y){
  if (!format %in% c("clam", "bacon")){
    stop(paste0("Incorrect output format. format has to ",
                "be 'clam' or 'bacon'."))
  }
  if (nrow(x) == 0){
    warning(paste0("Table without c14 data. Returning an ",
                   "empty data.frame."))
    if (format == "clam"){
      output <- data.frame(lab_ID = NA, C14_age = NA, cal_age = NA,
                           error = NA, reservoir = NA, depth = NA,
                           thickness = NA)[-1, ]
    }else{
      output <- data.frame(labID = NA, age = NA, error = NA,
                           depth = NA)[-1, ]
    }
    return(output)
  }
  z <- merge(x, y, by = "sample_")
  output <- data.frame(lab_ID = z$labnumber, C14_age = z$agebp,
                       error = z$agesdup)
  output$depth <- z$depthcm
  output$thickness <- z$thickness
  output$cal_age <- NA
  output$reservoir <- NA
  if (format == "clam"){
    output <- subset(output, select = c("lab_ID", "C14_age",
                                        "cal_age", "error",
                                        "reservoir", "depth",
                                        "thickness"))
  }
  if (format == "bacon"){
    output <- subset(output, select = c("lab_ID", "C14_age",
                                        "error", "depth"))
    colnames(output) <- c("labID", "age",
                          "error", "depth")
  }
  return(output)
})

#' @rdname export_c14
setMethod("export_c14", signature(format = "character", x = "epd.entity",
                                 y = "missing"), function(format, x){
  export_c14(format = format, x = x@geochron@c14, y = x@geochron@geochron)
})
  

# export_depths -----------------------------------------------

#' Reshape depths of biological samples to CLAM or BACON format
#'
#' This function takes depths data, as returned by 
#' \code{\link[EPDr]{.get_psamples}} or extracted from a 
#' \code{\link[EPDr]{epd.entity}} object, and transforms them
#' to comply with CLAM or BACON format.
#'
#' @param x data.frame Data frame with at least a column called \code{depthcm}
#' or \code{\link[EPDr]{epd.entity}} object to extract this information from.
#'
#' @return Vector with depths in ascending order.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' @examples
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # depths.1 <- .get_psamples(1, epd.connection)
#' # export_depths(depths.1)
#' # epd.1 <- get_entity(1, epd.connection)
#' # export_depths(epd.1)
#' @rdname export_depths
#' @exportMethod export_depths
setGeneric("export_depths", function(x){
  standardGeneric("export_depths")
})

#' @rdname export_depths
setMethod("export_depths", signature(x = "data.frame"), function(x){
  output <- x[order(x$depthcm), ]$depthcm
  return(output)
})

#' @rdname export_depths
setMethod("export_depths", signature(x = "epd.entity"), function(x){
  export_depths(x@samples@psamples)
})
  



# export_entity -----------------------------------------------

#' Reshape epd.entity objects to CLAM or BACON format
#' 
#' This function takes \code{\link[EPDr]{epd.entity}} or
#' \code{\link[EPDr]{epd.entity.df}} objects, as those returned by
#' \code{\link[EPDr]{get_entity}} or \code{\link[EPDr]{entity_to_matrices}},
#' extracts datation information and reshapes it to complies with
#' CLAM or BACON format.
#' 
#' The function has an interactive implementation. If some of the parameters
#' \code{incl_chron_not_in_c14}, \code{incl_c14_not_in_chron}, 
#' \code{use_c14_conf_age}, and/or \code{use_c14_conf_depth} are not 
#' specified, the function will check the data and ask the user how 
#' it should proceed regarding some conflicts in the data between C14 data
#' and agebasis tables.
#'
#' @param format character Character string indicating whether to export to "clam" 
#' or to "bacon" format.
#' @param x epd.entity \code{\link[EPDr]{epd.entity}} or 
#' \code{\link[EPDr]{epd.entity.df}} objects from which all information 
#' will be extracted to compose the clam or bacon table and file.
#' @param ... Not used with current methods.
#' @param chronology numeric Number indicating the chronology from which 
#' to extract the agebasis for the "clam" or "bacon" file. If 
#' unspecified, the function use the default chronology according to 
#' the EPD for that particular entity.
#' @param incl_chron_not_in_c14 logical Logical value indicating whether the 
#' function should include in the result agebasis present in the specified 
#' (or the default) chronology (TRUE) but not in the C14 table, or should 
#' not include them (FALSE).
#' @param incl_c14_not_in_chron logical Logical value indicating whether the 
#' function should include in the result agebasis present in the C14 
#' data but not present in the specified (or default) chronology (TRUE), 
#' or should not include them (FALSE).
#' @param use_c14_conf_age logical Logical value indicating whether the 
#' function should use data from C14 table when there are conflicting 
#' ages between the C14 table or the chronology (TRUE), or it should 
#' take data from the chronology instead (FALSE).
#' @param use_c14_conf_depth logical Logical value indicating whether the function 
#' should use data from C14 table when there are conflicting depths 
#' between the C14 table or the chronology (TRUE), or it should take data 
#' from the chronology instead (FALSE).
#' @param include_depths logical Logical value indicating whether depths of pollen 
#' samples should be exported too. The default value is TRUE. This is 
#' helpful because running CLAM or BACON with depths calculate the 
#' calibrated ages for those samples in the same step.
#' @param incl_envents logical Logical value indicating whether events information 
#' should be included (TRUE), or not (FALSE).
#'
#' @return Data frame with specific format for "CLAM" or "BACON" age-depth
#' modelling softwares. CLAM format has 7 columns: \code{$lab_ID}, 
#' \code{$C14_age}, \code{$cal_age}, \code{$error}, \code{$reservoir}, 
#' \code{$depth}, and \code{$thickness}. \code{$lab_ID} is the code 
#' of the radiocarbon (C14) samples. \code{$C14_age} is the radiocarbon (C14)
#' dates. \code{$error} is the error estimated in the radiocarbon (C14) 
#' datation of the samples. \code{$reservoir} is to specify if the 
#' samples have marine reservoir effects. \code{$depth} is the depth in
#' cm of the radiocarbon samples. \code{$thickness} is the tickness of 
#' the radiocarbon samples. BACON format has 4 columns: \code{$labID},
#' \code{$age}, \code{$error}, and \code{$depth}. \code{$labID} is the 
#' code of the radiocarbon (C14) samples. \code{$age} is the radiocarbon 
#' (C14) date. \code{$error} is the error estimated in the radiocarbon 
#' (C14) datation for the samples. \code{$depth} is the depth in cm of 
#' the radiocarbon samples. The function also produce the files required
#' by CLAM or BACON in the working directory according to this folder 
#' structure: \code{~/{format}/Cores/{entity_number}/}
#' The function creates a file \code{.csv} (\code{{entity_number}.csv}) with 
#' the radiocarbon data and two \code{.txt} (\code{{entity_number}_depths.txt}
#' and \code{{entity_number}_depthsID.txt}). The last one is not used by
#' CLAM or BACON but it might be useful to follow track of samples depths.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @examples
#' # Not run
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # epd.400 <- get_entity(400, epd.connection)
#' # export_entity("clam", epd.400) # Also check new folders in your working directory
#' # export_entity("bacon", epd.400) # Also check new folders in your working directory
#' # epd.1 <- get_entity(1, epd.connection)
#' # export_entity("clam", epd.1)
#' # export_entity("bacon", epd.1)
#' 
#' @rdname export_entity
#' @exportMethod export_entity
setGeneric("export_entity", function(format,
                                    x,
                                    ...,
                                    chronology = NULL,
                                    incl_chron_not_in_c14 = NULL,
                                    incl_c14_not_in_chron = NULL,
                                    use_c14_conf_age = NULL,
                                    use_c14_conf_depth = NULL,
                                    include_depths = TRUE,
                                    incl_envents = NULL){
  standardGeneric("export_entity")
})

#' @rdname export_entity
setMethod("export_entity", signature(format = "character", x = "epd.entity"),
          function(format, x, ...){
  # format <- "clam"
  # x <- entity.list[[1]]
  # chronology <- NULL
  # incl_chron_not_in_c14 <- NULL
  # incl_c14_not_in_chron <- NULL
  # use_c14_conf_age <- NULL
  # use_c14_conf_depth <- NULL
  # include_depths <- NULL
  # incl_envents <- NULL

            # Define internal functions
            .print_data <- function(data, format){
              if (format == "clam"){
                cat(c("lab_ID", "C14_age", "cal_age", "error",
                      "reserv.", "depth", "thickn.\n"), sep = "\t")
              }
              if (format == "bacon"){
                cat(c("labID", "age", "error", "depth\n"))
              }
              apply(data, "\n", MARGIN = 1, FUN = cat, sep = "\t")
            }
            .print_events <- function(w){
              cat(c("event_", "e_", "depthcm", "thickn.", "event",
                    "name", "agebp", "ageup", "agelo", "publ"),
                  sep = "\t", fill = TRUE)
              apply(w, "\n",  MARGIN = 1, FUN = cat, sep = "\t")
            }
            .is_true_false <- function(z){
              is.logical(z) && !is.na(z)
            }
            if (!format %in% c("clam", "bacon")){
              stop(paste0("Incorrect output format. format has to be ",
                          "'clam' or 'bacon'."))
            }
            # Check if datation object has C14 and chronology information
            if (nrow(x@geochron@c14) == 0){
              warning(paste0("x object for a core without c14 data. Not ",
                             "performing conversion, useless for CLAM ",
                             "or BACON."))
              return(NULL)
            }
            # Default chronology if no defined
            if (is.null(chronology)){
              chronology <- x@defaultchron
            }else{
              if (!chronology %in% 1:x@numberofchron){
                stop(paste0("The chronology does not exist."))
              }
            }

            # Get sub-objects from datation object
            e_ <- x@e_
            c14 <- x@geochron@c14
            geochron <- x@geochron@geochron
            synevent <- x@chron@synevent
            event <- x@chron@event
            events <- merge(synevent, event, by = "event_")
            psamples <- x@samples@psamples
            agebasis <- x@chron@agebasis
            agebasis <- agebasis[agebasis$chron_ == chronology, ]
            chron <- export_agebasis(format, agebasis)
            c14 <- export_c14(format, c14, geochron)
            # Check for data in c14 and the chronology and for conflicting data
            if (format == "clam"){
              c14_in_chron <- which(c14$C14_age %in% chron$C14_age &
                                      c14$depth %in% chron$depth)
              c14_not_in_chron <- which(!c14$C14_age %in% chron$C14_age &
                                          !c14$depth %in% chron$depth)
              c14_conf_age <- which(!c14$C14_age %in% chron$C14_age &
                                      c14$depth %in% chron$depth)
              c14_conf_depth <- which(c14$C14_age %in% chron$C14_age &
                                        !c14$depth %in% chron$depth)
              chron_in_c14 <- which(chron$C14_age %in% c14$C14_age &
                                      chron$depth %in% c14$depth)
              chron_not_in_c14 <- which(!chron$C14_age %in% c14$C14_age &
                                          !chron$depth %in% c14$depth)
              chron_conf_age <- which(!chron$C14_age %in% c14$C14_age &
                                        chron$depth %in% c14$depth)
              chron_conf_depth <- which(chron$C14_age %in% c14$C14_age &
                                          !chron$depth %in% c14$depth)
            }
            if (format == "bacon"){
              c14_in_chron <- which(c14$age %in% chron$age &
                                      c14$depth %in% chron$depth)
              c14_not_in_chron <- which(!c14$age %in% chron$age &
                                          !c14$depth %in% chron$depth)
              c14_conf_age <- which(!c14$age %in% chron$age &
                                      c14$depth %in% chron$depth)
              c14_conf_depth <- which(c14$age %in% chron$age &
                                        !c14$depth %in% chron$depth)
              chron_in_c14 <- which(chron$age %in% c14$age &
                                      chron$depth %in% c14$depth)
              chron_not_in_c14 <- which(!chron$age %in% c14$age &
                                          !chron$depth %in% c14$depth)
              chron_conf_age <- which(!chron$age %in% c14$age &
                                        chron$depth %in% c14$depth)
              chron_conf_depth <- which(chron$age %in% c14$age &
                                          !chron$depth %in% c14$depth)
            }
            # Check for information on the tables and interactively ask for data
            # use if there are conflicts
            if (length(c14_in_chron) > 0){
              cat("\n")
              cat(paste0("Chronology has coincident data with C14 data and, ",
                         "hence, the later will be used\n"))
              cat("C14 data:\n")
              .print_data(c14[c14_in_chron, ], format)
              cat("\n")
              cat("Chronology data:\n")
              .print_data(chron[chron_in_c14, ], format)
            }
            if (length(chron_not_in_c14) > 0){
              cat("\n")
              cat("Chronology has additional no-C14 data.\n")
              cat("Chronology data:\n")
              .print_data(chron[chron_not_in_c14, ], format)
              while (!.is_true_false(incl_chron_not_in_c14)){
                incl_chron_not_in_c14 <- as.logical(readline(paste0("Incorpo",
                                                                   "rate ",
                                                                   "these ",
                                                                   "data to ",
                                                                   "the chron",
                                                                   "ology? ",
                                                                   "(Yes: ",
                                                                   "TRUE ",
                                                                   "then ",
                                                                   "Intro, ",
                                                                   "No: ",
                                                                   "FALSE ",
                                                                   "then ",
                                                                   "Intro)")))
                if (!.is_true_false(incl_chron_not_in_c14)){
                  warning("Sorry! Invalid value.",
                          call. = FALSE,
                          immediate. = TRUE)
                }
              }
            }else{
              incl_chron_not_in_c14 <- FALSE
            }
            if (length(c14_not_in_chron) > 0){
              cat("\n")
              cat(paste0("There are additional C14 data not included in the ",
                         "chronology.\n"))
              cat("C14 data:\n")
              .print_data(c14[c14_not_in_chron, ], format)
              while (!.is_true_false(incl_c14_not_in_chron)){
                incl_c14_not_in_chron <- as.logical(readline(paste0("Incorp",
                                                                    "orate ",
                                                                    "these ",
                                                                    "data to ",
                                                                    "the ",
                                                                    "chrono",
                                                                    "logy? ",
                                                                    "(Yes: ",
                                                                    "TRUE ",
                                                                    "then ",
                                                                    "Intro, ",
                                                                    "No: ",
                                                                    "FALSE ",
                                                                    "then",
                                                                    "Intro)")))
                if (!.is_true_false(incl_c14_not_in_chron)){
                  warning("Sorry! Invalid value.",
                          call. = FALSE,
                          immediate. = TRUE)
                }
              }
            }else{
              incl_c14_not_in_chron <- FALSE
            }
            if (length(c14_conf_age) > 0){
              cat("\n")
              cat(paste0("There are age conflicts between c14 data and the ",
                         "chronology.\n"))
              cat("C14 data:\n")
              .print_data(c14[c14_conf_age, ], format)
              cat("Chronology data:\n")
              .print_data(chron[chron_conf_age, ], format)
              while (!.is_true_false(use_c14_conf_age)){
                use_c14_conf_age <- as.logical(readline(paste0("Use ages ",
                                                               "from the C14 ",
                                                               "table? If ",
                                                               "not, ages ",
                                                               "from the ",
                                                               "chronology ",
                                                               "will be ",
                                                               "used. ",
                                                               "(Yes: TRUE ",
                                                               "then Intro, ",
                                                               "No: FALSE ",
                                                               "then Intro)")))
                if (!.is_true_false(use_c14_conf_age)){
                  warning("Sorry! Invalid value.",
                          call. = FALSE,
                          immediate. = TRUE)
                }
              }
            }else{
              use_c14_conf_age <- FALSE
            }
            if (length(c14_conf_depth) > 0){
              cat(paste0("There are age conflicts between c14 data ",
                         "and the chronology.\n"))
              cat("C14 data:\n")
              .print_data(c14[c14_conf_depth, ], format)
              cat("\n")
              cat("Chronology data:\n")
              .print_data(chron[chron_conf_depth, ], format)
              while (!.is_true_false(use_c14_conf_depth)){
                use_c14_conf_depth <- as.logical(readline(paste0("Use depths ",
                                                                 "from the ",
                                                                 "C14 table? ",
                                                                 "If not, ",
                                                                 "depths ",
                                                                 "from the ",
                                                                 "chronology ",
                                                                 "will be ",
                                                                 "used. ",
                                                                 "(Yes: TRUE ",
                                                                 "then ",
                                                                 "Intro, No: ",
                                                                 "FALSE then ",
                                                                 "Intro)")))
                if (!.is_true_false(use_c14_conf_depth)){
                  warning("Sorry! Invalid value.",
                          call. = FALSE,
                          immediate. = TRUE)
                }
              }
            }else{
              use_c14_conf_depth <- FALSE
            }
            # Combine chron and c14 accordingly to specified data use
            output <- chron[NULL, ]
            output <- rbind(output, c14[c14_in_chron, ])
            if (incl_chron_not_in_c14){
              output <- rbind(output, chron[chron_not_in_c14, ])
            }
            if (incl_c14_not_in_chron){
              output <- rbind(output, c14[c14_not_in_chron, ])
            }
            if (use_c14_conf_age){
              output <- rbind(output, c14[c14_conf_age, ])
            }else{
              output <- rbind(output, chron[chron_conf_age, ])
            }
            if (use_c14_conf_depth){
              output <- rbind(output, c14[c14_conf_depth, ])
            }else{
              output <- rbind(output, chron[chron_conf_depth, ])
            }
            # Check for events in the datation object
            if (nrow(events) > 0){
              warning(paste0("There are dated events for this ",
                             "core (entity)."),
                      immediate. = TRUE,
                      call. = FALSE)
              cat("\n")
              cat("Events data:\n")
              .print_events(events)
              while (!.is_true_false(incl_envents) &&
                     !is.numeric(incl_envents)){
                incl_envents <- readline(paste0("Include events information ",
                                                "in the files? (Yes: TRUE ",
                                                "then Intro, No: FALSE ",
                                                "then Intro, or write ",
                                                "c(n1,n2, ...) to ",
                                                "specify which data should ",
                                                "be included.)"))
                try(incl_envents <- eval(parse(text = incl_envents)))
                if (!.is_true_false(incl_envents) &&
                    !is.numeric(incl_envents)){
                  warning("Sorry! Invalid value.",
                          call. = FALSE,
                          immediate. = TRUE)
                }
              }
            }else{
              incl_envents <- FALSE
            }
            if (isTRUE(incl_envents) || is.numeric(incl_envents)){
              if (is.numeric(incl_envents)){
                events_ <- events[incl_envents, "event_"]
                event <- event[which(event$event_ %in% events_)]
                synevent <- synevent[which(synevent$event_ %in% events_)]
              }
              events.export <- export_events(format, synevent, event)
              output <- rbind(output, events.export)
            }

            # Create directory to save files for CLAM
            if (!dir.exists(paste(format, "/Cores/", e_, sep = ""))){
              dir.create(paste(format, "/Cores/", e_, sep = ""),
                         recursive = TRUE)
            }
            # Order dataframe by depths and write to the directory
            output <- output[order(output$depth), ]
            utils::write.csv(output, file = paste(format,
                                                  "/Cores/",
                                                  e_,
                                                  "/",
                                                  e_,
                                                  ".csv",
                                                  sep = ""),
                             na = "", row.names = FALSE)
            # Extract depth columns for samples and create depths.txt files.
            if (exists("include_depths")){
              depths.export <- export_depths(psamples)
              utils::write.table(depths.export, file = paste(format,
                                                             "/Cores/",
                                                             e_,
                                                             "/",
                                                             e_,
                                                             "_depths.txt",
                                                             sep = ""),
                                 col.names = FALSE, na = "", row.names = FALSE)
              utils::write.table(psamples, file = paste(format,
                                                        "/Cores/",
                                                        e_,
                                                        "/", e_,
                                                        "_depths_ID.txt",
                                                        sep = ""),
                                 col.names = FALSE, na = "",
                                 row.names = FALSE, sep = ",")
            }
            return(output)
})


# export_events -----------------------------------------------

#' Reshape events data to CLAM or BACON format
#' 
#' This function takes event data, as those queried by 
#' \code{\link[EPDr]{.get_synevent}} or stored in a 
#' \code{\link[EPDr]{epd.entity}} object, and modifies them to fit into a new 
#' table that complies with CLAM or BACON format.
#' 
#' If \code{x} is an \code{\link[EPDr]{epd.entity}} or an 
#' \code{\link[EPDr]{epd.entity.df}} object, \code{y} is not used.
#'
#' @param format character Character string indicating whether to export to "clam" or to "bacon" format.
#' @param x data.frame Data frame or \code{\link[EPDr]{epd.entity}} object with event data as
#' those extracted from \code{\link[EPDr]{.get_synevent}} or \code{\link[EPDr]{get_entity}} 
#' @param y data.frame Data frame with event data as those extracted from \code{\link[EPDr]{.get_event}}.
#'
#' @return Data frame with event data in CLAM or BACON format.
#' 
#' @references \url{http://www.chrono.qub.ac.uk/blaauw/clam.html}
#' @references \url{http://chrono.qub.ac.uk/blaauw/bacon.html}
#' 
#' @examples
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                               user = "epdr", password = "epdrpw")
#' # epd.51 <- get_entity(51, epd.connection)
#' # export_events("clam", epd.51)
#' # export_events("clam", .get_synevent(51, epd.connection), .get_event(26, epd.connection))
#' # export_events("bacon", epd.51)
#' # export_events("bacon", .get_synevent(51, epd.connection), .get_event(26, epd.connection))
#' @rdname export_events
#' @exportMethod export_events
setGeneric("export_events", function(format, x, y){
  standardGeneric("export_events")
})

#' @rdname export_events
setMethod("export_events", signature(format = "character",
                                     x = "data.frame",
                                     y = "data.frame"),
          function(format, x, y){
            z <- merge(x, y, by = "event_")
            if (!format %in% c("clam", "bacon")){
              stop(paste0("Incorrect output format. format has to ",
                          "be 'clam' or 'bacon'."))
            }
            if (nrow(z) == 0){
              warning(paste0("Table without dated events. Returning ",
                             "an empty data.frame."))
              if (format == "clam"){
                output <- data.frame(lab_ID = NA, C14_age = NA,
                                     cal_age = NA, error = NA,
                                     reservoir = NA, depth = NA,
                                     thickness = NA)[-1, ]
              }else{
                output <- data.frame(labID = NA, age = NA,
                                     error = NA, depth = NA)[-1, ]
              }
              return(output)
            }
            output <- data.frame(lab_ID = paste("E", z$e_,
                                                "_EV",
                                                z$event_,
                                                sep = ""),
                                 C14_age = z$agebp,
                                 error = z$ageuncertup,
                                 depth = z$depthcm,
                                 thickness = z$thickness)
            output$error[which(is.na(output$error) | output$error == 0)] <- 1
            output$cal_age <- NA
            output$reservoir <- NA
            if (format == "clam"){
              output <- subset(output, select = c("lab_ID", "C14_age",
                                                  "cal_age", "error",
                                                  "reservoir", "depth",
                                                  "thickness"))
            }
            if (format == "bacon"){
              output <- subset(output, select = c("lab_ID", "C14_age",
                                                  "error", "depth"))
              colnames(output) <- c("labID", "age", "error", "depth")
            }
            return(output)
          })

#' @rdname export_events
setMethod("export_events", signature(format = "character",
                                     x = "epd.entity",
                                     y = "missing"),
          function(format, x, y){
            export_events(format, x@chron@synevent, x@chron@event)
          })
