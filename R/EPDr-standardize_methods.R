
# blois_quality -----------------------------------------------

#' Blois quality index for palynological samples
#' 
#' This function apply the quality index described in Blois et al. (2013). 
#' From the Ecography 2013 paper, Appendix 3: "\emph{For each site at a particular 1 kyr
#' time period, site data-quality was calculated as the mean normalized distance
#' of the nearest pollen sample and the nearest chronological control.  We
#' calculated the distance in years of the nearest pollen sample and the nearest
#' chronological control to each 1 kyr time period.  We eliminated sites where
#' the nearest pollen sample was over 2000 years away or the nearest
#' chronological control was over 5000 years away.  For the remaining sites in
#' each 1 kyr period, we created a summary measure of site data-quality by 
#' rescaling the two distances in years to a 0 - 1 scale and calculating the
#' mean.  For example, if the nearest sample to the 1 kyr BP time period at a
#' given site was at 1.050 kyr BP and the nearest chronological control was at
#' 1.100 kyr BP, the raw distances would be 50 years and 100 years, respectively.
#' These equate to scaled values of 0.975 (i.e., 1 - 50/2000) and 0.98 (i.e., 1 -
#' 100/5000) for sample and chronological quality, respectively, with a mean
#' data-quality for this site at the 1 kyr BP time period of 0.9775.}"
#' To replicate the calculation the function allows to specify different maximum
#' distances as parameters of the function.
#' 
#' When \code{x} is an \code{\link[EPDr]{epd.entity}} object, it is
#' first transformed into a \code{\link[EPDr]{epd.entity.df}} 
#' object by \code{\link[EPDr]{entity_to_matrices}} function.
#' 
#' @param x epd.entity \code{\link[EPDr]{epd.entity.df}} or a
#' \code{\link[EPDr]{epd.entity}} object.
#' @param chron numeric Chronology number to look for samples and control points ages.
#' This value become the default chronology in the new object. If not 
#' specified the function check the default chronology in  
#' \code{x}. It can be any of the chronologies in the EPD for that 
#' particular entity, or the one from Giesecke et al. (2013). 
#' @param max_sample_dist numeric Maximum numeric distance in years to be considered
#' to the palynological samples for interpolated or ranged data.
#' @param max_control_dist numeric Maximum numeric distance in years to be considered
#' to the control points (e.g., C14, top, bottom, etc.).
#'
#' @return \code{\link[EPDr]{epd.entity.df}} object with no empty
#' \code{@@agesdf@@dataquality} slot. The default chronology in \code{x@@defaultchron}
#' is changed to the one specified in \code{chron}. 
#' 
#' @references Blois, J.L., Williams, J.W., Fitzpatrick, M.C., Ferrier, S.,
#' Veloz, S.D., He, F., Liu, Z., Manion, G., and Otto-Bliesner, B. (2013).
#' Modeling the climatic drivers of spatial patterns in vegetation composition
#' since the Last Glacial Maximum. Ecography, 36, 460-473.
#' @references Giesecke, T., Davis, B., Brewer, S., Finsinger, W., 
#' Wolters, S., Blaaw, M., de Beaulieu, J.L., Binney, H., Fyfe, R.M.,
#' Gaillard, M.J., Gil-Romera, G., van der Knaap, W.O. Kunes, P.,
#' Kuhl, N., van Leeuwen, J.F.N, Leydet, M., Lotter, A.F., Ortu, E.,
#' Semmler, M., and Bradshaw, R.H.W (2013). Towards mapping the late
#' Quaternary vegetation change of Europe. Vegetation History and
#' Archaeobotany, 23, 75-86.
#'
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1.qi <- blois_quality(epd.1)
#' epd.1.qi@agesdf@dataquality
#' 
#' epd.1.ran <- intervals_counts(epd.1, tmin = seq(0, 21000, by = 1000),
#'                               tmax = seq(999, 21999, by = 1000))
#' epd.1.ran.qi <- blois_quality(epd.1.ran)
#' epd.1.ran.qi@agesdf@dataquality
#' 
#' t <- c(seq(0, 21000, by = 500))
#' epd.1.int <- interpolate_counts(epd.1, t)
#' epd.1.int.qi <- blois_quality(epd.1.int)
#' epd.1.int.qi@agesdf@dataquality
#' }
#' @rdname blois_quality
#' @exportMethod blois_quality
setGeneric("blois_quality", function(x,
                                     chron = NULL,
                                     max_sample_dist = 2000,
                                     max_control_dist = 5000){
  standardGeneric("blois_quality")
})

#' @rdname blois_quality
setMethod("blois_quality", signature(x = "epd.entity.df"),
          function(x, chron, max_sample_dist, max_control_dist){
            if (is.null(chron)){
              if (!check_default_chron(x)){
                stop(paste0("The 'x' object has no sample ages ",
                            "and the quality index cannot be calculated."))
              }
              chron <- x@defaultchron
            }
            index <- which(x@samples@pagedpt$chron_ == chron)
            sample_ages <- x@samples@pagedpt[index, "agebp"]
            data_ages <- x@agesdf@depthages
            index <- which(x@chron@agebasis$chron_ == chron)
            agebasis <- x@chron@agebasis[index, ]
            .mindiff <- function(z, w, max_diff){
              if (length(w) == 0){
                sorted.w <- NA
              }else{
                sorted.w <- sort(w)
              }
              if (length(sorted.w) == 1){
                mindist <- abs(z - sorted.w)
              }else{
                myfun <- stats::stepfun(sorted.w, 0:length(sorted.w))
                indices <- pmin(pmax(1, myfun(z)), length(sorted.w) - 1)
                mindist <- pmin(abs(z - sorted.w[indices]),
                                abs(z - sorted.w[indices + 1]))
              }
              if (!is.null(max_diff)){
                mindist <- pmin(mindist, max_diff)
              }
              return(mindist)
            }
            control_dist <- apply(data_ages,
                                  MARGIN = 2,
                                  FUN = .mindiff,
                                  agebasis[, "age"],
                                  max_control_dist)
            sample_dist <- apply(data_ages,
                                 MARGIN = 2,
                                 FUN = .mindiff,
                                 sample_ages,
                                 max_sample_dist)
            control_dist <- 1 - (control_dist / max_control_dist)
            sample_dist <- 1 - (sample_dist / max_sample_dist)
            data.quality <- (control_dist + sample_dist) / 2
            x@agesdf@dataquality <- as.data.frame(data.quality)
            return(x)
          })

#' @rdname blois_quality
setMethod("blois_quality", signature(x = "epd.entity"),
          function(x, chron, max_sample_dist, max_control_dist){
            x <- entity_to_matrices(x)
            x <- blois_quality(x, chron, max_sample_dist, max_control_dist)
            return(x)
          })


# check_restriction --------------------------------------------------------

#' Check restrictions on EPDr objects
#' 
#' This function check EPDr objects (\code{\link[EPDr]{epd.entity}},
#' and \code{\link[EPDr]{epd.entity.df}}), 
#' and return a logical value indicating whether the data are restricted (TRUE) or
#' unrestricted (FALSE).
#'
#' @param x epd.entity \code{\link[EPDr]{epd.entity}} or
#' \code{\link[EPDr]{epd.entity.df}} object.
#'
#' @return logical value indicating whether the data are restricted (TRUE) or
#' unrestricted (FALSE).
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' check_restriction(epd.1)
#' epd.1046 <- get_entity(1046, epd.connection)
#' check_restriction(epd.1046)
#' }
#' @rdname check_restriction
#' @exportMethod check_restriction
setGeneric("check_restriction", function(x){
  standardGeneric("check_restriction")
})

#' @rdname check_restriction
setMethod("check_restriction", signature(x = "epd.entity"), function(x){
  if (x@entity@pentity$usestatus == "R"){
    return(TRUE)
  }else{
    return(FALSE)
  }
})


# check_default_chron --------------------------------------------------

#' Check default chronology on EPDr objects
#' 
#' The function check EPDr objects with slot @@defaultchron 
#' (\code{\link[EPDr]{epd.entity}}, and
#' \code{\link[EPDr]{epd.entity.df}}) to see if there are a default
#' chronology specified.
#'
#' @param x epd.entity \code{\link[EPDr]{epd.entity}} or
#' \code{\link[EPDr]{epd.entity.df}} objects.
#'
#' @return Logical value indicating whether the object has a default 
#' chronology (TRUE) or not (FALSE).
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' check_default_chron(epd.1)
#' }
#' @rdname check_default_chron
#' @exportMethod check_default_chron
setGeneric("check_default_chron", function(x){
  standardGeneric("check_default_chron")
})

#' @rdname check_default_chron
setMethod("check_default_chron", signature(x = "epd.entity"), function(x){
  if (length(x@defaultchron) == 0){
    return(FALSE)
  }else{
    if (x@defaultchron == 0){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
})


# counts_to_percentage -------------------------------------------------------

#' Calculate counts percentages
#'
#' This function transforms the counts matrix (samples x taxa) in
#' the @@commdf@@counts slot of a \code{\link[EPDr]{epd.entity.df}}
#' object to percentages relative to the total amount of particles 
#' in each sample (row).
#' 
#' Although the function work with \code{\link[EPDr]{epd.entity.df}} and
#' \code{\link[EPDr]{epd.entity}} objects, the later are previously 
#' transformed using \code{\link[EPDr]{entity_to_matrices}} without any
#' posterior data manipulation and transformation. A lot of entities have
#' information from different taxonomical groups (terrestrial plants pollen 
#' and algae or fungi spores). Hence, percentages calculated in this base
#' may be meaningless and its use is unrecommended.
#'
#' @param x epd.entity \code{\link[EPDr]{epd.entity.df}} or 
#' \code{\link[EPDr]{epd.entity}} objects.
#' @param offset numeric Numeric value to avoid dividing by zero 
#' if the total of particles is zero for a particular sample. This may 
#' happen when \code{x} has been filtered by taxa groups. For instance, 
#' if only trees and shrubs ("TRSH") are kept in the \code{x@@commdf@counts} 
#' slot some sites in the last glacial maximum could have no pollen at all, 
#' because all vegetation was herbaceos or small shrubs. The effect of the
#' offset is negligible because it only affect pollen counts of zero. The
#' result is to indicate that when counts are present, the results is 
#' numerical zero and not NA. This is relevant to discriminate sites and
#' times combinations where pollen counts are not available (NA) and not
#' numerically zero.
#' 
#' @return The function returns a \code{\link[EPDr]{epd.entity.df}} object, 
#' which is a modified version of \code{x} (or \code{entity_to_matrices(x)}
#' if x is a \code{\link[EPDr]{epd.entity}} object), in which
#' values in @@counts slot represent percentages instead of raw counts.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1 <- entity_to_matrices(epd.1)
#' epd.1 <- filter_taxagroups(epd.1, c("DWAR", "HERB", "LIAN", "TRSH",
#'                            "UPHE", "INUN")) ## All pollen taxa groups
#' epd.1.percent <- counts_to_percentage(epd.1)
#' head(epd.1.percent@commdf@counts)
#' head(epd.1@commdf@counts)
#' }
#' @rdname counts_to_percentage
#' @exportMethod counts_to_percentage
setGeneric("counts_to_percentage", function(x, offset = 0.0001){
  standardGeneric("counts_to_percentage")
})


#' @rdname counts_to_percentage
setMethod("counts_to_percentage", signature(x = "epd.entity.df"),
          function(x, offset){
            counts <- x@commdf@counts
            countstype <- factor("Percentages",
                                 levels = c("Counts", "Percentages"))
            totals <- rowSums(counts, na.rm = TRUE)
            totals[which(totals == 0)] <- offset
            counts <- (counts / totals) * 100
            x@commdf@counts <- counts
            x@countstype <- countstype
            return(x)
          })

#' @rdname counts_to_percentage
setMethod("counts_to_percentage", signature(x = "epd.entity"), function(x, offset){
  x <- entity_to_matrices(x)
  x <- counts_to_percentage(x, offset)
  return(x)
})


# entity_to_matrices --------------------------------------------------------

#' Calculate data matrices from \code{\link[EPDr]{epd.entity}} objects 
#' 
#' This function takes an \code{\link[EPDr]{epd.entity}} object and transforms
#' data from tables in the object to new tables in the form of matrices. 
#' Information in the new tables, are easier to understand and manipulate than
#' those in the native format of the EPD. Transformed data include samples,
#' ages of that samples, and counts for that samples (both biological and 
#' no-biological particles).
#' 
#' The function return a \code{\link[EPDr]{epd.entity.df}} object, which 
#' is an expanded version of \code{\link[EPDr]{epd.entity}} objects with
#' eight new slots. This new object are intended to be modified for 
#' standardizations but keeping an unmodified version of the data and
#' structure of the \code{\link[EPDr]{epd.entity}} object.
#' Four slots contains the data in the new format, while four others store
#' information to track changes made from standardization and modification
#' functions (e.g., \code{\link[EPDr]{counts_to_percentage}}. 
#' 
#' @param x epd.entity Object of class \code{\link[EPDr]{epd.entity}} to be 
#' modified.
#' 
#' @return epd.entity.df object. Expanded version of \code{x} with certain data
#' reformated into data matrices instead of the native EPD format.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1 <- entity_to_matrices(epd.1)
#' }
#' @rdname entity_to_matrices
#' @exportMethod entity_to_matrices
setGeneric("entity_to_matrices", function(x){
  standardGeneric("entity_to_matrices")
})

#' @rdname entity_to_matrices
setMethod("entity_to_matrices", signature(x = "epd.entity"), function(x){
  samples <- x@samples
  ages <- subset(samples@pagedpt, select = c("sample_", "chron_", "agebp"))
  ages <- unique(ages)
  if (nrow(ages) == 0){
    warning("This core does not have age data.", call. = FALSE)
    ages.cast <- data.frame()
    sample_ <- samples@psamples$sample_
    samplelabel <- as.character(sample_)
    depthcm <- samples@psamples$depthcm[match(sample_,
                                              samples@psamples$sample_)]
    dataquality <- data.frame()
  }else{
    ages.cast <- reshape2::dcast(ages, sample_ ~ chron_,
                                 value.var = "agebp", margin = FALSE)
    sample_ <- samples@psamples$sample_
    samplelabel <- as.character(sample_)
    ages.cast <- ages.cast[match(sample_, ages.cast$sample_), ]
    chronnames <- colnames(ages.cast)[-1]
    chronnames[which(chronnames == 9999)] <- "giesecke"
    ages.cast <- subset(ages.cast, select = -1)
    colnames(ages.cast) <- chronnames
    depthcm <- samples@psamples$depthcm[match(sample_,
                                              samples@psamples$sample_)]
    dataquality <- data.frame()
  }
  samplesdf <- new("samplesdf", sample_ = sample_, samplelabel = samplelabel)
  agesdf <- new("agesdf", depthcm = depthcm, depthages = ages.cast,
                dataquality = dataquality)
  countstype <- factor("Counts", levels = c("Counts", "Percentages"))
  countsprocessing <- factor("Raw",
                             levels = c("Raw", "Interpolated", "Ranged means"))
  taxatype <- factor("Default", levels = c("Default", "Accepted", "Higher"))
  taxaprocessing <- factor("Original", levels = c("Original", "Expanded"))
  if (nrow(samples@pcounts) == 0){
    warning("This core does not have count data.", call. = FALSE)
    counts.cast <- data.frame()
    taxanames <- character(0)
    taxa_ <- numeric(0)
    taxagroupid <- character(0)
    taxaaccepted <- numeric(0)
    taxamhvar <- numeric(0)
    commdf <- new("commdf")
    nopodf <- new("nopodf")
  }else{
    taxa_ <- samples@pvars$var_
    taxanames <- samples@pvars$varname[match(taxa_, samples@pvars$var_)]
    taxagroupid <- samples@pgroup$groupid[match(taxa_, samples@pgroup$var_)]
    taxaaccepted <- samples@pvars$accvar_[match(taxa_, samples@pvars$var_)]
    taxamhvar <- samples@pvars$mhvar_[match(taxa_, samples@pvars$var_)]
    counts.cast <- reshape2::dcast(samples@pcounts,
                                   sample_ ~ var_,
                                   value.var = "count")
    counts.cast[is.na(counts.cast)] <- 0
    counts.cast <- counts.cast[match(sample_, counts.cast$sample_), ]
    counts.cast <- subset(counts.cast, select = -1)
    counts.cast <- counts.cast[match(taxa_, colnames(counts.cast))]
    colnames(counts.cast) <- taxanames
    if (any(taxagroupid == "NOPO")){
      npi <- which(taxagroupid == "NOPO")
      nnpi <- which(taxagroupid != "NOPO")
      commdf <- new("commdf",
                    taxanames = taxanames[nnpi],
                    taxa_ = taxa_[nnpi],
                    taxaaccepted = taxaaccepted[nnpi],
                    taxamhvar = taxamhvar[nnpi],
                    taxagroupid = taxagroupid[nnpi],
                    counts = subset(counts.cast, select = nnpi))
      nopodf <- new("nopodf",
                    varnames = taxanames[npi],
                    var_ = taxa_[npi],
                    varaccepted = taxaaccepted[npi],
                    varmhvar = taxamhvar[npi],
                    vargroupid = taxagroupid[npi],
                    counts = subset(counts.cast, select = npi))
    }else{
      commdf <- new("commdf", taxanames = taxanames,
                    taxa_ = taxa_, taxaaccepted = taxaaccepted,
                    taxamhvar = taxamhvar, taxagroupid = taxagroupid,
                    counts = counts.cast)
      nopodf <- new("nopodf")
    }
  }
  x <- new("epd.entity.df",
           e_ = x@e_,
           postbombzone = x@postbombzone,
           numberofchron = x@numberofchron,
           isingiesecke = x@isingiesecke,
           defaultchron = x@defaultchron,
           taxatype = taxatype,
           taxaprocessing = taxaprocessing,
           countstype = countstype,
           countsprocessing = countsprocessing,
           entity = x@entity,
           site = x@site,
           geochron = x@geochron,
           chron = x@chron,
           samples = x@samples,
           samplesdf = samplesdf,
           agesdf = agesdf,
           commdf = commdf,
           nopodf = nopodf)
  return(x)
})


# filter_taxa -----------------------------------------------

#' Expand EPDr objects with new taxa
#' 
#' This function modifies \code{@@commdf@@counts} slot of
#' (\code{\link[EPDr]{epd.entity.df}} objects to filter taxa columns
#' to match \code{taxa}. The function adds empty columns (\code{NA}) if a new taxa is
#' defined in \code{taxa} or removes columns for the taxa not included in \code{taxa}.
#' The function may look useless for a single entity but it is useful when standardizing
#' data across multiple entities.
#'
#' @param x epd.entity \code{\link[EPDr]{epd.entity.df}} or
#' \code{\link[EPDr]{epd.entity}} object to be modified.
#' @param taxa character Character vector indicating the new 
#' taxa in the \code{@@epd.entity.df} slot. 
#' @param epd.taxonomy data.frame Data frame with the taxonomy from the 
#' EPD as from the \code{\link[EPDr]{get_taxonomy_epd}} function.
#' 
#' @return \code{\link[EPDr]{epd.entity.df}} object with the modified
#' \code{@@commdf@@counts} slot.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1@commdf@taxanames
#' colnames(epd.1@commdf@counts)
#' epd.1.ft <- filter_taxa(epd.1, c(epd.1@commdf@taxa_names, "prueba"),
#'                         get_taxonomy_epd(epd.connection))
#' colnames(epd.1.ft@commdf@counts)
#' epd.1.ft@commdf@taxanames
#' head(epd.1.ft@commdf@counts)
#' }
#' @rdname filter_taxa
#' @exportMethod filter_taxa
setGeneric("filter_taxa", function(x, taxa, epd.taxonomy){
  standardGeneric("filter_taxa")
})

#' @rdname filter_taxa
setMethod("filter_taxa", signature(x = "epd.entity.df",
                                   taxa = "character",
                                   epd.taxonomy = "data.frame"),
          function(x, taxa, epd.taxonomy){
            site_taxa <- x@commdf@taxanames
            diff_names <- setdiff(taxa, site_taxa)
            if (nrow(x@commdf@counts) == 0){
              new_counts <- data.frame(matrix(ncol = length(taxa),
                                              nrow = 0))
              colnames(new_counts) <- taxa
            }else{
              new_counts <- x@commdf@counts
              new_counts[, diff_names] <- NA
            }
            new_counts <- subset(new_counts, select = taxa)
            new_taxa_type <- factor("Expanded",
                                    levels = c("Samples",
                                               "Accepted",
                                               "Higher"))
            new_taxa_names <- colnames(new_counts)
            index <- match(new_taxa_names, epd.taxonomy$varname)
            new_taxa_ <- epd.taxonomy$var_[index]
            new_taxa_acc <- epd.taxonomy$accvar_[index]
            new_taxa_mhvar <- epd.taxonomy$mhvar_[index]
            new_taxa_groupid <- epd.taxonomy$groupid[index]
            x@commdf@counts <- new_counts
            x@taxatype <- new_taxa_type
            x@commdf@taxanames <- new_taxa_names
            x@commdf@taxa_ <- new_taxa_
            x@commdf@taxaaccepted <- new_taxa_acc
            x@commdf@taxamhvar <- new_taxa_mhvar
            x@commdf@taxagroupid <- new_taxa_groupid
            return(x)
          })


#' @rdname filter_taxa
setMethod("filter_taxa", signature(x = "epd.entity", taxa = "character",
                                   epd.taxonomy = "data.frame"),
          function(x, taxa, epd.taxonomy){
            x <- entity_to_matrices(x)
            x <- filter_taxa(x, taxa, epd.taxonomy)
            return(x)
          })


# filter_taxagroups --------------------------------------------------------

#' Filter Taxa Groups
#' 
#' This function removes taxa from the slot @@commdf@@counts in the 
#' \code{\link[EPDr]{epd.entity.df}} object based on specific groups of taxa.
#' For example, the user can select to work only with pollen from trees and 
#' shrubs (TRSH) or with algae (ALGA). If \code{x} is an 
#' \code{\link[EPDr]{epd.entity}} object, this is first converted into a 
#' \code{\link[EPDr]{epd.entity.df}} with 
#' \code{\link[EPDr]{entity_to_matrices}}.
#'
#' @param x epd.entity Object of class \code{\link[EPDr]{epd.entity.df}}
#' or \code{\link[EPDr]{epd.entity}} object.
#' @param taxagroups character Character vector indicating taxa groups 
#' to be selected.
#' 
#' @return The function returns a \code{\link[EPDr]{epd.entity.df}} 
#' with information on \code{@@commdf@@counts} slot 
#' only for taxa belonging to the specified taxa groups.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1.trsh <- filter_taxagroups(epd.1, "TRSH")
#' str(epd.1@commdf@counts)
#' str(epd.1.trsh@commdf@counts)
#' }
#' @rdname filter_taxagroups
#' @exportMethod filter_taxagroups
setGeneric("filter_taxagroups", function(x, taxagroups){
  standardGeneric("filter_taxagroups")
})

#' @rdname filter_taxagroups
setMethod("filter_taxagroups", signature(x = "epd.entity.df",
                                         taxagroups = "character"),
          function(x, taxagroups){
            commdf <- x@commdf
            index <- which(commdf@taxagroupid %in% taxagroups)
            index <- unique(index)
            commdf@taxanames <- commdf@taxanames[index]
            commdf@taxa_ <- commdf@taxa_[index]
            commdf@taxaaccepted <- commdf@taxaaccepted[index]
            commdf@taxamhvar <- commdf@taxamhvar[index]
            commdf@taxagroupid <- commdf@taxagroupid[index]
            commdf@counts <- subset(commdf@counts, select = index)
            x@commdf <- commdf
            return(x)
          })

#' @rdname filter_taxagroups
setMethod("filter_taxagroups", signature(x = "epd.entity",
                                         taxagroups = "character"),
          function(x, taxagroups){
            x <- entity_to_matrices(x)
            x <- filter_taxagroups(x, taxagroups)
            return(x)
          })


# giesecke_default_chron -----------------------------------------------

#' Make Giesecke the default chronology
#' 
#' This function makes chronologies from Giesecke the default to be used in
#' \code{\link[EPDr]{epd.entity}} and \code{\link[EPDr]{epd.entity.df}}
#' objects if they are available for that particular entity.
#'
#' Details The function first check whether that entity has chronological
#' data in Giesecke et al. (2013), and then, if available, change the
#' default chronology number from the EPD to the one provided by Giesecke
#' (i.e., x@@defaultchron == 9999)
#'
#' @param x epd.entity \code{\link[EPDr]{epd.entity}} or 
#' \code{\link[EPDr]{epd.entity.df}} objects.
#'
#' @return The function returns a modified version of the 
#' \code{epd.entity} in which the slot @@defaultchron is changed to 
#' \code{9999} if they recalculated ages for this entity. A numeric code 
#' is used (\code{9999}) because of the object definition is numeric. This 
#' values is used in other functions to grab the right ages from Giesecke.
#' 
#' @references Giesecke, T., Davis, B., Brewer, S., Finsinger, W., 
#' Wolters, S., Blaaw, M., de Beaulieu, J.L., Binney, H., Fyfe, R.M.,
#' Gaillard, M.J., Gil-Romera, G., van der Knaap, W.O. Kunes, P.,
#' Kuhl, N., van Leeuwen, J.F.N, Leydet, M., Lotter, A.F., Ortu, E.,
#' Semmler, M., and Bradshaw, R.H.W (2013). Towards mapping the late
#' Quaternary vegetation change of Europe. Vegetation History and
#' Archaeobotany, 23, 75-86.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1@defaultchron
#' epd.1 <- giesecke_default_chron(epd.1)
#' epd.1@defaultchron
#' }
#' @rdname giesecke_default_chron
#' @exportMethod giesecke_default_chron
setGeneric("giesecke_default_chron", function(x){
  standardGeneric("giesecke_default_chron")
})

#' @rdname giesecke_default_chron
setMethod("giesecke_default_chron",
          signature(x = "epd.entity"), function(x){
            if (x@isingiesecke == TRUE){
              x@defaultchron <- 9999
            }
            return(x)
          })


# interpolate_counts -------------------------------------------------------

#' Interpolate counts to specific time periods
#'
#' This function uses data (sample ages and sample counts) from an
#' \code{\link[EPDr]{epd.entity.df}} object to estimate by linear 
#' interpolation, loess or smooth splines the counts at specific time 
#' periods defined by the user. This can be used to estimate counts for 
#' the same time periods for multiple entities in the database, 
#' standardizing them for integrative analysis.
#' 
#' @param x epd.entity.df An \code{\link[EPDr]{epd.entity.df}} or
#' \code{\link[EPDr]{epd.entity}} object as returned by
#' \code{\link[EPDr]{get_entity}} or
#' \code{\link[EPDr]{entity_to_matrices}} functions.
#' @param time numeric Vector with time periods, in the same system 
#' (i.e., cal BP) than "ages" in epd.entity.df, in which counts have 
#' to be estimated.
#' @param chronology numeric Number specifying the chronology from 
#' which ages should be used to calculate the interpolations. If none is
#' provided the function uses the default chronology from the object (see
#' \code{\link[EPDr]{giesecke_default_chron}} or
#' \code{\link[EPDr]{check_default_chron}}).
#' @param method character Interpolation method, should be an unambiguous 
#' abbreviation of either linear, loess, or sspline. See Details section.
#' @param rep_negt logical logical to indicate whether or not to replace 
#' negative values with zero in the interpolated data.
#' @param span numeric Span for loess, default = 0.25.
#' @param df numeric Degress of freedome for smoothing spline, default is 
#' the lower of 20 or 0.7 * number of samples.
#' @param ...	additional arguments to loess, smooth.spline and aspline.
#'
#' @details Data for time periods in \code{time} but not recorded in the 
#' entity are fill with \code{NA}. This is convenient if analysis are 
#' carried out with multiple entities.
#' 
#' Interpolation can be done using linear interpolation between 
#' data points in the original series (default) using 
#' \code{\link[stats:approx]{approx}}, using a fitted 
#' \code{\link[stats:loess]{loess}} locally weighted regression, or by 
#' \code{\link[stats:smooth.spline]{smooth.spline}}. The latter two methods 
#' will also smooth the data and additional arguments may be passed to 
#' these functions to control the amount of smoothing, or to force replacing
#' negative values with zeros.
#'
#' @return The function returns an \code{\link[EPDr]{epd.entity.df}} object,
#' similar to \code{x} in which ages and counts has been modified to the
#' time periods specified in time and the counts estimated for these periods.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' t <- c(seq(0, 21000, by = 500))
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1.int <- interpolate_counts(epd.1, t)
#' 
#' epd.3 <- get_entity(3, epd.connection)
#' epd.3.int <- interpolate_counts(epd.3, t, method="linear")
#' epd.3.int <- interpolate_counts(epd.3, t, method="loess")
#' epd.3.int <- interpolate_counts(epd.3, t, method="sspline")
#' }
#' @rdname interpolate_counts
#' @exportMethod interpolate_counts
setGeneric("interpolate_counts", function(x,
  time,
  chronology = NULL,
  method = c("linear",
             "loess",
             "sspline"),
  rep_negt = TRUE,
  span = 0.25,
  df = min(20,
           nrow(x@commdf@counts) *
             0.7),
  ...)
  {
  standardGeneric("interpolate_counts")
})

#' @rdname interpolate_counts
setMethod("interpolate_counts", signature(x = "epd.entity.df",
                                          time = "numeric"),
          function(x, time, chronology, method, rep_negt, span,
                   df, ...){
            if (is.null(chronology)){
              chronology <- x@defaultchron
            }
            if (chronology == 9999){
              chronology <- "giesecke"
            }
            if (nrow(x@agesdf@depthages) == 0){
              stop(paste0("The entity has not ages, and interpolation cannot ",
                          "be performed."))
            }
            sample_ages <- subset(x@agesdf@depthages,
                                  select = as.character(chronology))
            sample_depthcm <- x@agesdf@depthcm
            sample_id <- x@samplesdf@sample_
            sample_counts <- x@commdf@counts
            # remove data from depths whithout ages associated
            index1 <- which(!is.na(sample_ages))
            index2 <- seq_along(sample_id)
            index <- intersect(index2, index1)
            sample_ages <- sample_ages[index, ]
            sample_depthcm <- sample_depthcm[index]
            sample_id <- sample_id[index]
            sample_counts <- sample_counts[index, ]
            # index taxa with less than 2 meassures
            cc_index <- apply(!is.na(sample_counts), MARGIN = 2, FUN = sum)
            cc_index <- cc_index >= 2
            ## set the time bounds
            min_sample_age <- min(sample_ages, na.rm = TRUE)
            max_sample_age <- max(sample_ages, na.rm = TRUE)
            min_time <- time[which(time >= min_sample_age)][1]
            max_time <- time[which(time <= max_sample_age)][length(
              which(time <= max_sample_age))]
            interp_ages <- time[which(time >= min_time & time <= max_time)]
            interp_counts <- as.data.frame(matrix(nrow = length(interp_ages),
                                                  ncol = ncol(sample_counts)))
            interp_depthcm <- numeric(0)
            if (length(interp_ages) != 0){
              method <- match.arg(method)
              if (is.null(method))
                stop("Interpolation method not recognised")
              if (method == "linear") {
                lin.f <- function(y, x, xout) {
                  stats::approx(x, y, xout)$y
                }
                interp_counts[, cc_index] <- apply(sample_counts[, cc_index],
                                                   MARGIN = 2,
                                                   lin.f,
                                                   x = sample_ages,
                                                   xout = interp_ages,
                                                   ...)
                interp_depthcm <- lin.f(sample_depthcm, sample_ages,
                                        xout = interp_ages)
              }
              else if (method == "loess") {
                lo.f <- function(y, x, xout, span, ...) {
                  fit <- stats::loess(y ~ x, span = span, ...)
                  stats::predict(fit, newdata = data.frame(x = xout))
                }
                interp_counts[, cc_index] <- apply(sample_counts[, cc_index],
                                                   MARGIN = 2,
                                                   lo.f,
                                                   x = sample_ages,
                                                   xout = interp_ages,
                                                   span = span,
                                                   ...)
                interp_depthcm <- lo.f(sample_depthcm,
                                       sample_ages,
                                       xout = interp_ages,
                                       span = span,
                                       ...)
              }
              else if (method == "sspline") {
                ss.f <- function(y, x, xout, df, ...) {
                  fit <- stats::smooth.spline(y ~ x, df = df, ...)
                  stats::predict(fit, x = data.frame(x = xout))$y[, 1]
                }
                interp_counts[, cc_index] <- apply(sample_counts[, cc_index],
                                                   MARGIN = 2,
                                                   ss.f,
                                                   x = sample_ages,
                                                   xout = interp_ages)
                interp_depthcm <- ss.f(sample_depthcm,
                                       sample_ages,
                                       xout = interp_ages,
                                       df = df, ...)
              }
              if (rep_negt) {
                interp_counts[interp_counts < 0] <- 0
              }
            }
            colnames(interp_counts) <- colnames(sample_counts)
            output_counts <- as.data.frame(matrix(nrow = length(time),
                                                  ncol = ncol(sample_counts)))
            colnames(output_counts) <- colnames(sample_counts)
            output_depthcm <- rep(NA, length(time))
            output_ages <- time
            index <- which(output_ages %in% interp_ages)
            output_counts[index, ] <- interp_counts
            output_depthcm[index] <- interp_depthcm
            if (chronology == "giesecke"){
              x@defaultchron <- 9999
            }else{
              x@defaultchron <- chronology
            }
            x@countsprocessing <- factor("Interpolated",
                                         levels = c("Samples", "Interpolated",
                                                    "Ranged means"))
            x@samplesdf@sample_ <- seq(20001, length.out = length(output_ages))
            x@samplesdf@samplelabel <- as.character(output_ages)
            x@agesdf@depthages <- data.frame(output_ages)
            colnames(x@agesdf@depthages) <- chronology
            x@agesdf@depthcm <- output_depthcm
            x@commdf@counts <- output_counts
            return(x)
          })

#' @rdname interpolate_counts
setMethod("interpolate_counts", signature(x = "epd.entity", time = "numeric"),
          function(x, time, chronology, method, rep_negt, span,
                   df, ...){
            x <- entity_to_matrices(x)
            x <- interpolate_counts(x, time, chronology, method, rep_negt,
                                    span, df, ...)
            return(x)
          })


# intervals_counts ----------------------------------------------------------

#' Mean counts for specific time intervals
#'
#' This function uses data (sample ages and sample counts) from an
#' \code{\link[EPDr]{epd.entity.df}} object to calculate mean counts
#' for samples within specific time intervals defined by the user. This can be
#' used to estimate mean counts for the same time intervals for multiple 
#' entities or cores in the database, standardizing them for 
#' integrative analysis.
#' 
#' Time intervals without sample (data) in the entity are fill with 
#' \code{NA}. This is convenient if analysis are carried out with 
#' multiple entities.
#'
#' @param x epd.entity.df An \code{\link[EPDr]{epd.entity.df}} object as 
#' returned by the \code{\link[EPDr]{entity_to_matrices}} function.
#' @param tmin numeric Numeric vector indicating the lower limits 
#' (in years cal. BP) for the time intervals.
#' @param tmax numeric Numeric vector indicating the upper limits 
#' (in years cal. BP) for the time intervals
#' @param newlabels character Character vector with labels for each time 
#' intervals, if none are provided the functions generate them with the 
#' following format \code{tmin}-\code{tmax}.
#' @param chronology numeric Number specifying the chronology from which 
#' ages should be used to calculate the interpolations. If none is provided 
#' the function uses the default chronology from the object (see
#' \code{\link[EPDr]{giesecke_default_chron}}).
#' @param ...	Not used with current methods.
#'
#' @return The function returns a \code{\link[EPDr]{epd.entity.df}} object, 
#' similar to \code{epd.entity.df} in which ages and counts has been modified
#' to the time intervarls specified and the counts estimated for these periods.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1.int <- intervals_counts(epd.1, tmin = seq(0, 21000, by = 1000),
#'                               tmax = seq(999, 21999, by = 1000))
#' epd.3 <- get_entity(3, epd.connection)
#' epd.3.int <- intervals_counts(epd.3, tmin = seq(0, 21000, by = 1000),
#'                               tmax = seq(999, 21999, by = 1000))
#' epd.3.int <- intervals_counts(epd.3, tmin = seq(0, 21000, by = 1000),
#'                               tmax = seq(999, 21999, by = 1000), 2)
#' }
#' @rdname intervals_counts
#' @exportMethod intervals_counts
setGeneric("intervals_counts", function(x, tmin, tmax, ...,
                                        newlabels = NULL, chronology = NULL){
  standardGeneric("intervals_counts")
})

#' @rdname intervals_counts
setMethod("intervals_counts", signature(x = "epd.entity.df",
                                        tmin = "numeric", tmax = "numeric"),
          function(x, tmin, tmax, ...){
            if (length(tmin) != length(tmax)){
              stop(paste0("length(tmin) != length(tmax). Please, specify ",
                          "two vectors of the same length"))
            }
            if (is.null(newlabels)){
              newlabels <- paste(tmin, "-", tmax, sep = "")
            }
            if (is.null(chronology)){
              chronology <- x@defaultchron
            }
            if (chronology == 9999){
              chronology <- "giesecke"
            }
            sample_ages <- subset(x@agesdf@depthages,
                                  select = as.character(chronology))
            sample_depthcm <- x@agesdf@depthcm
            sample_id <- x@samplesdf@sample_
            sample_counts <- x@commdf@counts
            index1 <- which(!is.na(sample_ages))
            index2 <- seq_along(sample_id)
            index <- intersect(index2, index1)
            sample_ages <- sample_ages[index, ]
            sample_depthcm <- sample_depthcm[index]
            sample_id <- sample_id[index]
            sample_counts <- sample_counts[index, ]
            .is_between <- function(x, a, b) {
              x >= a & x <= b
            }
            index <- mapply(function(a, b, x){
              .is_between(x, a, b)
            }
            , tmin, tmax, MoreArgs = list(sample_ages))
            if (is.vector(index)){
              intervalid <- which(index)
              index <- 1
            }else{
              intervalid <- unlist(apply(index, MARGIN = 1, FUN = which))
              index <- unlist(apply(index, MARGIN = 2, FUN = which))
            }
            output_depthcm <- rep(NA, length(newlabels))
            output_ages <- rowMeans(cbind(tmin, tmax), na.rm = TRUE)
            output_counts <- as.data.frame(matrix(NA, nrow = length(newlabels),
                                                  ncol = ncol(sample_counts)))
            colnames(output_counts) <- colnames(sample_counts)
            if (length(index) == 0 | !all(index %in% x@samplesdf@sample_)){
            }else{
              range.counts <- sample_counts[index, ]
              range.depthcm <- sample_depthcm[index]
              range.ages <- sample_ages[index]
              range_means <- apply(range.counts, MARGIN = 2,
                                   FUN = function(x, y, z){
                                     stats::aggregate(x, by = list(y = y),
                                                      FUN = z, na.rm = TRUE)
                                   }
                                   , intervalid, mean)
              range_means <- reshape2::dcast(reshape2::melt(range_means,
                                                            id.vars = "y"),
                                             y ~ L1)
              col_index <- -which(colnames(range_means) == "y")
              range_means <- subset(range_means,
                                    select = col_index)
              range_depth_means <- stats::aggregate(range.depthcm,
                                                    by = list(y = intervalid),
                                                    FUN = mean, na.rm = TRUE)
              range_depth_means <- range_depth_means$x
              range_ages_means <- stats::aggregate(range.ages,
                                                   by = list(y = intervalid),
                                                   FUN = mean,
                                                   na.rm = TRUE)
              range_ages_means <- range_ages_means$x
              output_counts[unique(intervalid), ] <- range_means
              output_depthcm[unique(intervalid)] <- range_depth_means
              output_ages[unique(intervalid)] <- range_ages_means
            }
            x@countsprocessing <- factor("Ranged means",
                                         levels = c("Samples",
                                                    "Interpolated",
                                                    "Ranged means"))
            x@samplesdf@sample_ <- 10000 + seq_along(newlabels)
            x@samplesdf@samplelabel <- newlabels
            x@defaultchron <- 1
            x@agesdf@depthages <- data.frame(output_ages)
            colnames(x@agesdf@depthages) <- 1
            x@agesdf@depthcm <- as.numeric(output_depthcm)
            x@commdf@counts <- output_counts
            return(x)
          })

#' @rdname intervals_counts
setMethod("intervals_counts", signature(x = "epd.entity",
                                        tmin = "numeric", tmax = "numeric"),
          function(x, tmin, tmax, ...){
            x <- entity_to_matrices(x)
            x <- intervals_counts(x, tmin, tmax, ...)
            return(x)
          })

# table_by_taxa_age -----------------------------------------------

#' Tabulate counts by taxa and age
#' 
#' This function tabulates data from the \code{@@commdf@@counts} slot 
#' in EPDr objects (\code{\link[EPDr]{epd.entity.df}} or calculated 
#' them from a \code{\link[EPDr]{epd.entity}}) to summarize information 
#' for particular taxa at particular age or time intervals (samples). 
#' This function is useful to reshape the data to be plotted and mapped 
#' by \code{link[ggplot2]{ggplot}}. It was written to be used by 
#' \code{\link[EPDr]{map_taxa_age}} function.
#' 
#' @param x epd.entity.df \code{\link[EPDr]{epd.entity.df}} object where 
#' to extract the data from, or a \code{\link[EPDr]{epd.entity}} that is 
#' automatically transformed into a \code{\link[EPDr]{epd.entity.df}} and 
#' then tabulated.
#' @param taxa character Character vector indicating the taxa to be included 
#' in the table. Several taxa can be specified but the function returns data 
#' for only one taxa by summing all counts, and the taxa name specified 
#' in the output is the first one in \code{taxa}. This is useful when
#' you want to combine data from different taxa in the same genus,
#' for instance.
#' @param sample_label character Character vector indicating the ages or time 
#' intervals to be included in the table.
#'
#' @return Data frame with six columns: \code{e_}, \code{londd}, \code{latdd},
#' \code{count}, \code{sample_label}, and \code{taxa_label}.
#' \itemize{
#' \item{"e_"}{Entity identification number.}
#' \item{"londd"}{Longitude of the site in decimal degrees.}
#' \item{"latdd"}{Latitude of the site in decimal degrees.}
#' \item{"count"}{The count of that taxon in that particular sample
#' (age or time interval).}
#' \item{"sample_label"}{The sample (age or time interval) at which the
#' particulates were counted.}
#' \item{"taxa_label"}{The taxa that has been counted.}
#' }
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#'                                user = "epdr", password = "epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1 <- filter_taxagroups(epd.1, c("HERB", "TRSH", "DWAR",
#'                                       "LIAN", "HEMI", "UPHE"))
#' epd.1 <- giesecke_default_chron(epd.1)
#' 
#' epd.1 <- interpolate_counts(epd.1, seq(0, 22000, by = 1000))
#' epd.taxonomy <- getTaxonomyEPD(epd.connection)
#' epd.1 <- taxa_to_acceptedtaxa(epd.1, epd.taxonomy)
#' 
#' table_by_taxa_age(epd.1, "Cedrus", 
#'                   as.character(seq(0, 21000, by=1000)))
#' }
#' @rdname table_by_taxa_age
#' @exportMethod table_by_taxa_age
setGeneric("table_by_taxa_age", function(x, taxa, sample_label){
  standardGeneric("table_by_taxa_age")
})

#' @rdname table_by_taxa_age
setMethod("table_by_taxa_age", signature(x = "epd.entity.df",
                                         taxa = "character",
                                         sample_label = "character"),
          function(x, taxa, sample_label){
            if (!all(taxa %in% x@commdf@taxanames)){
              stop("taxa has to be a valid taxon in x@commdf@counts
                   and x@commdf@taxanames")
            }
            if (!all(sample_label %in% x@samplesdf@samplelabel)){
              stop("sample_label has to be valid sample labels in
                   x@commdf@counts and x@samplesdf@samplelabel")
            }
            e_ <- extract_e(x)
            londd <- x@site@siteloc$londd
            latdd <- x@site@siteloc$latdd
            sample_index <- which(x@samplesdf@samplelabel %in% sample_label)
            taxa_index <- which(x@commdf@taxanames %in% taxa)
            count <- x@commdf@counts[sample_index, taxa_index]
            if (class(count) == "data.frame"){
              count <- apply(count, MARGIN = 1, FUN = sum)
            }
            if (length(count) == 0){
              count <- NA
            }
            output <- data.frame(e_, londd, latdd, count,
                                 sample_label, taxa[[1]])
            colnames(output) <- c("e_", "londd", "latdd", "count",
                                  "sample_label", "taxa")
            return(output)
            })

#' @rdname table_by_taxa_age
setMethod("table_by_taxa_age", signature(x = "epd.entity",
                                         taxa = "character",
                                         sample_label = "character"),
          function(x, taxa, sample_label){
            x <- entity_to_matrices(x)
            table_by_taxa_age(x, taxa, sample_label)
            return(x)
          })


# taxa_to_acceptedtaxa -----------------------------------------------

#' Change taxa to accepted taxa names
#' 
#' This function modifies the taxa names in the \code{@@commdf@@counts} slot 
#' of \code{\link[EPDr]{epd.entity.df}} objects. More specifically, this 
#' function compares the taxa name with the taxonomy of the EPD
#' to use the accepted names. If these changes result in duplicated columns 
#' of the same taxa their values are unified by summing them.
#'
#' @param x epd.entity.df Object of class \code{\link[EPDr]{epd.entity.df}} 
#' with a \code{@@commdf@@counts} slot.
#' @param epd.taxonomy data.frame Data frame with the taxonomy 
#' from the EPD as from the \code{\link[EPDr]{get_taxonomy_epd}} function.
#'
#' @return \code{\link[EPDr]{epd.entity.df}} object with new taxa names.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1 <- entity_to_matrices(epd.1)
#' epd.1@commdf@taxanames
#' colnames(epd.1@commdf@counts)
#' epd.1.acc <- taxa_to_acceptedtaxa(epd.1, get_taxonomy_epd(epd.connection))
#' epd.1.acc@commdf@taxanames
#' colnames(epd.1.acc@commdf@counts)
#' }
#' @rdname taxa_to_acceptedtaxa
#' @exportMethod taxa_to_acceptedtaxa
setGeneric("taxa_to_acceptedtaxa", function(x, epd.taxonomy){
  standardGeneric("taxa_to_acceptedtaxa")
})

#' @rdname taxa_to_acceptedtaxa
setMethod("taxa_to_acceptedtaxa", signature(x = "epd.entity.df",
                                            epd.taxonomy = "data.frame"),
          function(x, epd.taxonomy){
            taxa_names <- x@commdf@taxanames
            taxa_ <- x@commdf@taxa_
            taxa_acc <- x@commdf@taxaaccepted
            new_taxa_type <- factor("Accepted",
                                    levels = c("Samples", "Accepted",
                                               "Higher"))
            if (length(taxa_) == 0){
              warning(paste0("Entity has no count data for any taxon.",
                             "Returning the original object in the format",
                             "of a 'epd.entity.df' object."))
              return(x)
            }
            new_taxa_names <- epd.taxonomy$varname[match(taxa_acc,
                                                         epd.taxonomy$var_)]
            new_counts <- x@commdf@counts
            colnames(new_counts) <- new_taxa_names
            if (nrow(new_counts) == 0){
              new_counts <- new_counts[, seq_along(new_taxa_names)]
              colnames(new_counts) <- sort(unique(new_taxa_names))
            }else{
              new_counts$sample_ <- seq_len(nrow(new_counts))
              new_counts <- reshape2::melt(new_counts, id.vars = c("sample_"))
              new_counts <- subset(reshape2::dcast(new_counts,
                                                   sample_ ~ variable,
                                                   fun.aggregate = sum,
                                                   value.var = "value"),
                                   select = -1)
            }
            new_taxa_names <- colnames(new_counts)
            index <- match(new_taxa_names, epd.taxonomy$varname)
            new_taxa_ <- epd.taxonomy$var_[index]
            new_taxa_acc <- epd.taxonomy$accvar_[index]
            new_taxa_mhvar <- epd.taxonomy$mhvar_[index]
            new_taxa_groupid <- epd.taxonomy$groupid[index]
            x@commdf@counts <- new_counts
            x@taxatype <- new_taxa_type
            x@commdf@taxanames <- new_taxa_names
            x@commdf@taxa_ <- new_taxa_
            x@commdf@taxaaccepted <- new_taxa_acc
            x@commdf@taxamhvar <- new_taxa_mhvar
            x@commdf@taxagroupid <- new_taxa_groupid
            return(x)
          })

#' @rdname taxa_to_acceptedtaxa
setMethod("taxa_to_acceptedtaxa", signature(x = "epd.entity",
                                            epd.taxonomy = "data.frame"),
          function(x, epd.taxonomy){
            x <- entity_to_matrices(x)
            x <- taxa_to_acceptedtaxa(x, epd.taxonomy)
            return(x)
          })


# taxa_to_highertaxa -----------------------------------------------

#' Change taxa to higher taxa level
#' 
#' This function modifies the taxa names in the \code{@commdf@@counts} slot
#' of \code{\link[EPDr]{epd.entity.df}} objects. More specifically, this 
#' function compares the taxa name with the taxonomy of the EPD to use 
#' the higher taxa names. If these changes result in duplicated columns of 
#' the same taxa their values are unified by summing them.
#'
#' @param x epd.entity \code{\link[EPDr]{epd.entity.df}} object.
#' @param epd.taxonomy data.frame Data frame with the taxonomy from 
#' the EPD as from the \code{\link[EPDr]{get_taxonomy_epd}} function.
#'
#' @return Object of class \code{\link[EPDr]{epd.entity.df}} with new 
#' taxa names.
#' 
#' @examples
#' \dontrun{
#' epd.connection <- connect_to_epd(host="localhost", database="epd",
#'                                  user="epdr", password="epdrpw")
#' epd.1 <- get_entity(1, epd.connection)
#' epd.1 <- entity_to_matrices(epd.1)
#' epd.1@commdf@taxanames
#' colnames(epd.1@commdf@counts)
#' epd.1.hn <- taxa_to_highertaxa(epd.1, get_taxonomy_epd(epd.connection))
#' epd.1.hn@commdf@taxanames
#' colnames(epd.1.hn@commdf@counts)
#' }
#' @rdname taxa_to_highertaxa
#' @exportMethod taxa_to_highertaxa
setGeneric("taxa_to_highertaxa", function(x, epd.taxonomy){
  standardGeneric("taxa_to_highertaxa")
})

#' @rdname taxa_to_highertaxa
setMethod("taxa_to_highertaxa", signature(x = "epd.entity.df",
                                          epd.taxonomy = "data.frame"),
          function(x, epd.taxonomy){
            taxa_names <- x@commdf@taxanames
            taxa_ <- x@commdf@taxa_
            taxa_mhvar <- x@commdf@taxamhvar
            na_index <- which(is.na(taxa_mhvar))
            taxa_mhvar[na_index] <- taxa_[na_index]
            new_taxa_type <- factor("Higher", levels = c("Samples",
                                                         "Accepted",
                                                         "Higher"))
            if (length(taxa_) == 0){
              warning(paste0("Entity has no count data for any taxon.",
                             "Returning the original object in the format",
                             "of a 'epd.entity.df' object."))
              return(x)
            }
            new_taxa_names <- epd.taxonomy$varname[match(taxa_mhvar,
                                                         epd.taxonomy$var_)]
            new_counts <- x@commdf@counts
            colnames(new_counts) <- new_taxa_names
            if (nrow(new_counts) == 0){
              new_counts <- new_counts[, seq_along(new_taxa_names)]
              colnames(new_counts) <- sort(unique(new_taxa_names))
            }else{
              new_counts$sample_ <- seq_len(nrow(new_counts))
              new_counts <- reshape2::melt(new_counts, id.vars = c("sample_"))
              new_counts <- subset(reshape2::dcast(new_counts,
                                                   sample_ ~ variable,
                                                   fun.aggregate = sum,
                                                   value.var = "value"),
                                   select = -1)
            }
            new_taxa_names <- colnames(new_counts)
            index <- match(new_taxa_names, epd.taxonomy$varname)
            new_taxa_ <- epd.taxonomy$var_[index]
            new_taxa_acc <- epd.taxonomy$accvar_[index]
            new_taxa_mhvar <- epd.taxonomy$mhvar_[index]
            new_taxa_groupid <- epd.taxonomy$groupid[index]
            x@commdf@counts <- new_counts
            x@taxatype <- new_taxa_type
            x@commdf@taxanames <- new_taxa_names
            x@commdf@taxa_ <- new_taxa_
            x@commdf@taxaaccepted <- new_taxa_acc
            x@commdf@taxamhvar <- new_taxa_mhvar
            x@commdf@taxagroupid <- new_taxa_groupid
            return(x)
          })

#' @rdname taxa_to_highertaxa
setMethod("taxa_to_highertaxa", signature(x = "epd.entity",
                                          epd.taxonomy = "data.frame"),
          function(x, epd.taxonomy){
            x <- entity_to_matrices(x)
            x <- taxa_to_highertaxa(x, epd.taxonomy)
            return(x)
          })


#' # FUNCTIONNAME -----------------------------------------------
#' 
#' #' Title TBW
#' #' 
#' #' Description TBW
#' #'
#' #' Details TBW
#' #'
#' #' @param parameter TBW
#' #'
#' #' @return TBW
#' #' @examples
#' #' \dontrun{
#' #' TBW
#' #' }
#' #' @rdname TBW
#' #' @exportMethod TBW
#' setGeneric("functionname", function(param){
#'   standardGeneric("functionname")
#' })
#' 
#' #' @rdname functionname
#' setMethod("functionname", signature(param="paramclass"), function(param){
#'   
#' })
