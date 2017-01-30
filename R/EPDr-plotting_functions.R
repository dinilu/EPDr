#' Map pollen counts from agedcount objects
#' 
#' This function use information on multiple \code{\link[EPDr:agedcounts]{agedcounts}} objects to map 
#' counts for a particular taxa in a particular age (or time period). The function use ggplot function
#' and allow for multiple parameters to further tune the resulting map. Each entity in the map is represented by
#' a point, which size, border colour, and fill colour change according to the palynological count. When an entity is
#' provided but it has no data for that particular age (or time period) the points are represented diferently to
#' reflect \code{NA}, avoiding confusion with \code{0 (zero)} values.
#'
#' @param agedcounts List of \code{\link[EPDr:agedcounts]{agedcounts}} objects that are going to be included 
#' in the map.
#' @param taxa Character string indicating the taxa that are going to be mapped.
#' @param sample_label Character string indicating the age (or time period) to be mapped.
#' @param pres_abse Logical value indicating whether the map will represent presence/absence or counts (absolute
#' or percentages).
#' @param pollen_thres Logical value indicating the pollen count threshold to plot an specific count as presence or 
#' absence.
#' @param zoom_coords Numeric vector with 4 elements defining the bounding box of the map as geographical coordinates.
#' It should have the following format \code{c(xmin, xmax, ymin, ymax)}. Where \code{x} represent longitude and \code{y} represent latitude. If not specified the function looks into the data and automatically select an extent
#' that encompases all entities.
#' @param points_pch Any value accepted for \code{pch} by \code{\link[ggplot2:geom_point]{geom_point}}. This controls
#' for the symbol to represent entities in the map.
#' @param points_colour Two elements vector with any values accepted for \code{colour} by
#' \code{\link[ggplot2:geom_point]{geom_point}}. You can use this to change border colours for points.
#' The first element is used to select the border colour of the absence/minimum values, whereas the second value
#' select the border colour for presences/maximum values.
#' @param points_fill Two elements vector with any values accepted for \code{fill} by
#' \code{\link[ggplot2:geom_point]{geom_point}}. You can use this to change fill colours for points. The first
#' element is used to select the fill colour of the absence/minimum values, whereas the second value select the fill 
#' colour for presences/maximum values.
#' @param points_range_size  Two elements vector with any values accepted for \code{size} by
#' \code{\link[ggplot2:geom_point]{geom_point}}. You can use this to change point sizes. The first
#' element is used to select the size of the absence/minimum values, whereas the second value select the 
#' size for presences/maximum values.
#' @param map_title Character string with a title for the map.
#' @param legend_range Two elements vector with numeric values to set different min and max limits of points
#' representation. If you have a dataset where counts goes up to 98 but want the map to represent until 100, you can 
#' set \code{legend_range=c(0,100)}. By default the function uses the min and max values in the dataset.
#' @param legend_title Character string with a title for the legend.
#' @param napoints_size Any value accepted for \code{size} by \code{\link[ggplot2:geom_point]{geom_point}}. This
#' control for the size of points representing \code{NA} values.
#' @param napoints_pch Any value accepted for \code{pch} by \code{\link[ggplot2:geom_point]{geom_point}}. This
#' control for the symbol of points representing \code{NA} values.
#' @param napoints_colour Any value accepted for \code{colour} by \code{\link[ggplot2:geom_point]{geom_point}}. This
#' control for the border colour of points representing \code{NA} values.
#' @param napoints_fill  Any value accepted for \code{fill} by \code{\link[ggplot2:geom_point]{geom_point}}. This
#' control for the fill colour of points representing \code{NA} values.
#' @param countries_fill_colour Any value accepted for \code{fill} by \code{\link[ggplot2:borders]{borders}}. This
#' control for the fill colour of polygons representing countries.
#' @param countries_border_colour Any value accepted for \code{colour} by \code{\link[ggplot2:borders]{borders}}. This
#' control for the border colour of polygons representing countries. 
#'
#' @return The function display a ggplot map with countries in the background and counts for particular taxa and age (or time periods) as points in the foreground.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connectToEPD(host="localhost", database="epd", user="epdr", password="epdrpw")
#' # entity.list <- e_by_countries(c("Spain","Portugal", "France", "Switzerland", "Austria", "Italy",
#' # "Malta", "Algeria", "Tunisia", "Morocco", "Atlantic ocean", "Mediterranean Sea"), epd.connection)
#' # counts.all <- lapply(entity.list, getAgedCounts, epd.connection)
#' # 
#' # counts <- lapply(counts.all, filterTaxaGroups, c("HERB", "TRSH", "DWAR", "LIAN", "HEMI", "UPHE"))
#' # counts <- lapply(counts, gieseckeDefaultChronology)
#' # counts <- removeRestricted(counts)
#' # counts <- removeWithoutAges(counts)
#' # 
#' # counts.int <- lapply(counts, interpolateCounts, seq(0, 22000, by=1000))
#' # counts.ran <- lapply(counts, intervalsCounts, seq(0, 21000, by=1000), seq(999, 21999, by=1000))
#' # 
#' # percent <- lapply(counts, trans2Percentages)
#' # percent.int <- lapply(counts.int, trans2Percentages)
#' # percent.ran <- lapply(counts.ran, trans2Percentages)
#' # 
#' # epd.taxonomy <- getTaxonomyEPD(epd.connection)
#' # 
#' # counts <- lapply(counts, taxa2AcceptedTaxa, epd.taxonomy)
#' # counts.int <- lapply(counts.int, taxa2AcceptedTaxa, epd.taxonomy)
#' # counts.ran <- lapply(counts.ran, taxa2AcceptedTaxa, epd.taxonomy)
#' # percent <- lapply(percent, taxa2AcceptedTaxa, epd.taxonomy)
#' # percent.int <- lapply(percent.int, taxa2AcceptedTaxa, epd.taxonomy)
#' # percent.ran <- lapply(percent.ran, taxa2AcceptedTaxa, epd.taxonomy)
#' # 
#' # counts <- unifyTaxonomy(counts, epd.taxonomy)
#' # counts.int <- unifyTaxonomy(counts.int, epd.taxonomy)
#' # counts.ran <- unifyTaxonomy(counts.ran, epd.taxonomy)
#' # percent <- unifyTaxonomy(percent, epd.taxonomy)
#' # percent.int <- unifyTaxonomy(percent.int, epd.taxonomy)
#' # percent.ran <- unifyTaxonomy(percent.ran, epd.taxonomy)
#' # 
#' # cedrus <- c("Cedrus", "Cedrus atlantica", "Cedrus cf. C. atlantica", "Cedrus-type", "cf. Cedrus")
#' # 
#' # mapTaxaAge(percent.int, cedrus, "21000", pres_abse=F)
#' # mapTaxaAge(percent.int, cedrus, "21000", pres_abse=T, pollen_thres=0)
#' # mapTaxaAge(counts.int, cedrus, "21000", pres_abse=F)
#' # mapTaxaAge(counts.int, cedrus, "21000", pres_abse=T, pollen_thres=0)
#' # 
#' # mapTaxaAge(percent.ran, cedrus, "21000-21999", pres_abse=F)
#' # mapTaxaAge(percent.ran, cedrus, "21000-21999", pres_abse=T, pollen_thres=0)
#' # mapTaxaAge(counts.ran, cedrus, "21000-21999", pres_abse=F)
#' # mapTaxaAge(counts.ran, cedrus, "21000-21999", pres_abse=T, pollen_thres=0)
#' # 
mapTaxaAge <- function(agedcounts, taxa, sample_label, pres_abse=FALSE, pollen_thres=NULL, zoom_coords=NULL,
                       points_pch=21, points_colour=NULL, points_fill=NULL, points_range_size=NULL,
                       map_title=NULL, legend_range=NULL, legend_title=NULL, napoints_size=0.75, napoints_pch=19,
                       napoints_colour="grey45", napoints_fill=NA, countries_fill_colour="grey80",
                       countries_border_colour="grey90"){
  # rm(agedcounts, taxa, sample_label, pres_abse, points_pch, napoints_size, napoints_pch, napoints_colour, countries_fill_colour, countries_border_colour, pollen_thres, zoom_coords, points_colour, points_fill, points_range_size, map_title, legend_range, napoints_fill)
  # agedcounts <- percent.int
  # taxa <- cedrus
  # sample_label <- "21000"
  # pres_abse <- F
  # points_pch <- 21
  # napoints_size <- 0.75
  # napoints_pch <- 19
  # napoints_colour <- "grey45"
  # countries_fill_colour <- "grey80"
  # countries_border_colour <- "grey90"
  # pollen_thres <- zoom_coords <- points_colour <- points_fill <- points_range_size <- map_title <- legend_range <-
  #   legend_title <- NULL
  # napoints_fill <- NA
  if(class(agedcounts) == "list"){
    if(!class(agedcounts[[1]]) %in% c("agedcounts", "data.frame")){
      stop("agedcounts of the wrong class. It has to be a list of agedcount objects (see ?getAgedCount) or data.frames (see ?tableByTaxaAge)")
    }else{
      if(class(agedcounts[[1]]) == "agedcounts"){
        dataList <- lapply(agedcounts, tableByTaxaAge, sample_label, taxa)    
      }else{
        if(setequal(colnames(agedcounts[[1]]), c("e_", "londd", "latdd", "count", "sample_label", "taxa"))){
          dataList <- agedcounts
        }else{
          stop("data.frames in agedcounts of the wrong type. See ?tableByTaxaAge.")
        }
      }
    }
  }else{
    stop("agedcounts of the wrong class. It has to be a list of agedcount objects (see ?getAgedCount) or data.frames (see ?tableByTaxaAge)")
  }
  
  dataList <- do.call(rbind, dataList)
  
  if(is.null(zoom_coords)){
    xmin <- min(dataList$londd) - (0.005 * range(dataList$londd)[[1]])
    xmax <- max(dataList$londd) + (0.005 * range(dataList$londd)[[2]])
    ymin <- min(dataList$latdd) - (0.005 * range(dataList$latdd)[[1]])
    ymax <- max(dataList$latdd) + (0.005 * range(dataList$latdd)[[2]])
  }else{
    xmin <- zoom_coords[1]
    xmax <- zoom_coords[2]
    ymin <- zoom_coords[3]
    ymax <- zoom_coords[4]
  }
  
  counts_type <- agedcounts[[1]]@counts@counts_type
  
  if(pres_abse==TRUE){
    if(is.null(pollen_thres)){
      warning("Pollen threshold (pollen_thres) not provided as argment when requiring presence maps. Data maped using default threshold > 0%)")
      pollen_thres <- 0
    }
    
    dataList$count <- dataList$count > pollen_thres
    dataList_woNA <- dataList[!is.na(dataList$count),]
    dataList_zero <- dataList[!dataList$count == 0,]
    
    if(is.null(map_title)){
      map_title <- paste(taxa, " (>", pollen_thres, "): ", sample_label, " cal BP", sep="")    
    }
    if(is.null(legend_title)){
      legend_title <- paste("Presence (>", pollen_thres, ")", sep="")
    }
    if(is.null(points_colour)){
      points_colour <- c("Red 2", "Dodger Blue 1")
    }
    if(is.null(points_fill)){
      points_fill <- c("Red 2", "Dodger Blue 1")
    }
    if(is.null(points_range_size)){
      points_range_size <- c(2, 3.5)
    }
    
    ggplot2::ggplot(dataList_woNA, ggplot2::aes(x=dataList_woNA$londd, y=dataList_woNA$latdd, fill=dataList_woNA$count, colour=dataList_woNA$count, size=dataList_woNA$count)) +
      ggplot2::borders("world", fill=countries_fill_colour, colour=countries_border_colour) +
      # ggplot2::geom_point(data=dataList, ggplot2::aes(x=dataList$londd, y=dataList$latdd, fill=dataList$count, colour=dataList$count, size=dataList$count), pch=napoints_pch, colour=napoints_colour, fill=napoints_fill, size=napoints_size, show.legend=FALSE) +
      ggplot2::geom_point(data=dataList, ggplot2::aes(x=dataList$londd, y=dataList$latdd), pch=napoints_pch, colour=napoints_colour, fill=napoints_fill, size=napoints_size, show.legend=TRUE) +
      ggplot2::geom_point(data=dataList_zero, ggplot2::aes(x=dataList$londd, y=dataList$latdd), pch=zero_pch, colour=zero_colour, fill=zero_fill, size=zero_size, show.legend=TRUE) +
      ggplot2::geom_point(pch=points_pch) +
      ggplot2::coord_fixed(xlim=c(xmin, xmax), ylim = c(ymin, ymax), ratio=1) +
      ggplot2::ggtitle(map_title) +
      ggplot2::guides(size=ggplot2::guide_legend(title=legend_title), fill=ggplot2::guide_legend(title=legend_title), colour=ggplot2::guide_legend(title=legend_title)) +
      ggplot2::scale_fill_manual(values=points_fill) +
      ggplot2::scale_colour_manual(values=points_colour) +
      ggplot2::scale_size_discrete(range=points_range_size) +
      ggplot2::scale_x_continuous(name="Longitude") +
      ggplot2::scale_y_continuous(name="Latitude") +
      ggplot2::theme_bw() 
    
  }else{
    if(is.null(map_title)){
      map_title <- paste(taxa, ": ", sample_label, " cal BP", sep="")    
    }
    if(is.null(legend_title)){
      if(counts_type == "Percentages"){
        legend_title <- paste(counts_type, " (%)", sep="")
      }else{
        legend_title <- paste(counts_type, " (n)", sep="")
      }
    }
    if(is.null(legend_range)){
      if(counts_type == "Percentages"){
        legend_range <- c(0, 100)
      }else{
        legend_range <- c(0, max(dataList$count))
      }
    }
    if(is.null(points_colour)){
      points_colour <- c("Light Blue 1", "Blue 3")
    }
    if(is.null(points_fill)){
      points_fill <- c("white", "Blue 3")
    }
    if(is.null(points_range_size)){
      points_range_size <- c(2, 7)
    }
    
    dataList_zero <- dataList[!dataList$count == 0,]
    
    ggplot2::ggplot(dataList, ggplot2::aes(x=dataList$londd, y=dataList$latdd, fill=dataList$count, colour=dataList$count, size=dataList$count)) +
      ggplot2::borders("world", fill=countries_fill_colour, colour=countries_border_colour) +
      ggplot2::geom_point(pch=napoints_pch, colour=napoints_colour, fill=napoints_fill, size=napoints_size, show.legend=T) +
      ggplot2::geom_point(data=dataList_zero, ggplot2::aes(x=dataList$londd, y=dataList$latdd), pch=zero_pch, colour=zero_colour, fill=zero_fill, size=zero_size, show.legend=T) +
      ggplot2::geom_point(pch=points_pch) +
      ggplot2::coord_fixed(xlim=c(xmin, xmax), ylim = c(ymin, ymax), ratio=1) +
      ggplot2::ggtitle(map_title) +
      ggplot2::guides(size=ggplot2::guide_legend(title=legend_title), fill=ggplot2::guide_legend(title=legend_title), colour=ggplot2::guide_legend(title=legend_title)) +
      ggplot2::scale_fill_gradient(low=points_fill[1], high=points_fill[2], limits=legend_range) +
      ggplot2::scale_colour_gradient(low=points_colour[1], high=points_colour[2], limits=legend_range) +
      ggplot2::scale_size(range=points_range_size, limits=legend_range) +
      ggplot2::scale_x_continuous(name="Longitude") +
      ggplot2::scale_y_continuous(name="Latitude") +
      ggplot2::theme_bw() 
  }
}

