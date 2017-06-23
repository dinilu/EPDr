# map_taxa_age -----------------------------------------------

#' Map pollen counts from a list of epd.entity.df objects
#' 
#' This function uses information on multiple \code{\link[EPDr]{epd.entity.df}}
#' objects to map counts for a particular taxa in a particular age (or time 
#' period). The function use ggplot function and allow for multiple 
#' parameters to further tune the resulting map. Each entity in the map 
#' is represented by a point, which size, border colour, and fill colour 
#' change according to the palynological count. When an entity is provided 
#' but it has no data for that particular age (or time period) the points 
#' are represented diferently to reflect \code{NA}, avoiding confusion 
#' with \code{0} (zero) values.
#'
#' @param x List of \code{\link[EPDr]{epd.entity.df}} objects that are 
#' going to be included in the map.
#' @param taxa Character string indicating the taxa that are going to 
#' be mapped.
#' @param sample_label Character string indicating the age (or time period) 
#' to be mapped.
#' @param pres_abse Logical value indicating whether the map will represent 
#' presence/absence or counts (absolute or percentages).
#' @param pollen_thres Logical value indicating the pollen count threshold 
#' to plot an specific count as presence or absence.
#' @param zoom_coords Numeric vector with 4 elements defining the bounding 
#' box of the map as geographical coordinates. It should have the 
#' following format \code{c(xmin, xmax, ymin, ymax)}. Where \code{x} 
#' represents longitude and \code{y} represents latitude. If not specified 
#' the function looks into the data and automatically selects an extent that 
#' encompases all entities.
#' @param points_pch Any value accepted for \code{pch} by
#' \code{\link[ggplot2]{geom_point}}. This controls for the symbol to represent
#' entities in the map.
#' @param points_colour Two elements vector with any values accepted for \code{colour}
#' by \code{\link[ggplot2]{geom_point}}. You can use this to change border
#' colours for points. The first element is used to select the border colour of the
#' absence/minimum values, whereas the second value selects the border colour for
#' presences/maximum values.
#' @param points_fill Two elements vector with any values accepted for \code{fill}
#' by \code{\link[ggplot2]{geom_point}}. You can use this to change fill colours
#' for points. The first element is used to select the fill colour of the absence/minimum
#' values, whereas the second value selects the fill colour for presences/maximum values.
#' @param points_range_size  Two elements vector with any values accepted for \code{size}
#' by \code{\link[ggplot2]{geom_point}}. You can use this to change point sizes.
#' The first element is used to select the size of the absence/minimum values, whereas the
#' second value selects the size for presences/maximum values.
#' @param map_title Character string with a title for the map.
#' @param legend_range Two elements vector with numeric values to set different min and max
#' limits of points representation. If you have a dataset where counts goes up to 98 but
#' want the map to represent until 100, you can set \code{legend_range = c(0,100)}. By default
#' the function uses the min and max values in the dataset.
#' @param legend_title Character string with a title for the legend.
#' @param napoints_size Any value accepted for \code{size} by
#' \code{\link[ggplot2]{geom_point}}. This control for the size of points
#' representing \code{NA} values.
#' @param napoints_colour Any value accepted for \code{colour} by
#' \code{\link[ggplot2]{geom_point}}. This control for the border colour of
#' points representing \code{NA} values.
#' @param napoints_fill  Any value accepted for \code{fill} by
#' \code{\link[ggplot2]{geom_point}}. This control for the fill colour of
#' points representing \code{NA} values.
#' @param countries_fill_colour Any value accepted for \code{fill} by
#' \code{\link[ggplot2]{borders}}. This control for the fill colour of polygons
#' representing countries.
#' @param countries_border_colour Any value accepted for \code{colour} by
#' \code{\link[ggplot2]{borders}}. This control for the border colour of polygons
#' representing countries. 
#'
#' @return The function displays a ggplot map with countries in the background and counts for particular taxa and age (or time periods) as points in the foreground.
#' 
#' @export
#'
#' @examples
#' # Not run
#' # epd.connection <- connect_to_epd(host = "localhost", database = "epd",
#' #                                user = "epdr", password = "epdrpw")
#' # entity.list <- list_e(epd.connection, country = c("Spain","Portugal",
#' #                                                   "France", "Switzerland",
#' #                                                   "Austria", "Italy",
#' #                                                   "Malta", "Algeria",
#' #                                                   "Tunisia", "Morocco",
#' #                                                   "Atlantic ocean",
#' #                                                   "Mediterranean Sea"))
#' # epd.all <- lapply(entity.list$e_, get_entity, epd.connection)
#' # epd.all <- lapply(epd.all, filter_taxagroups, c("HERB", "TRSH", "DWAR",
#' #                                                 "LIAN", "HEMI", "UPHE"))
#' # epd.all <- lapply(epd.all, giesecke_default_chron)
#' # epd.all <- remove_restricted(epd.all)
#' # epd.all <- remove_wo_ages(epd.all)
#' # 
#' # epd.int <- lapply(epd.all, interpolate_counts, seq(0, 22000, by = 1000))
#' # epd.taxonomy <- getTaxonomyEPD(epd.connection)
#' # epd.int <- lapply(epd.int, taxa_to_acceptedtaxa, epd.taxonomy)
#' # epd.int <- unify_taxonomy(epd.int, epd.taxonomy)
#' # 
#' # epd.int.per <- lapply(epd.int, counts_to_percentage)
#' # 
#' # map_taxa_age(epd.int, "Cedrus", "21000", pres_abse = F)
#' # map_taxa_age(epd.int, "Cedrus", "21000", pres_abse = T)
#' # map_taxa_age(epd.int.per, "Cedrus", "21000", pres_abse = F)
#' # map_taxa_age(epd.int.per, "Cedrus", "21000", pres_abse = T)
#' # 
map_taxa_age <- function(x, taxa, sample_label, pres_abse = FALSE,
                       pollen_thres = NULL, zoom_coords = NULL,
                       points_pch = 21, points_colour = NULL,
                       points_fill = NULL, points_range_size = NULL,
                       map_title = NULL, legend_range = NULL,
                       legend_title = NULL, napoints_size = 0.75,
                       napoints_colour = "grey45", napoints_fill = "grey45",
                       countries_fill_colour = "grey80",
                       countries_border_colour = "grey90"){
  if (class(x) == "list"){
    if (!(all(vapply(x, class, character(1)) == "epd.entity.df") ||
        all(vapply(x, class, character(1)) == "data.frame"))){
      stop("x of the wrong class. It has to be a list of epd.entity.df objects
           (see ?entityToMatrices) or data.frames (see ?table_by_taxa_age)")
    }else{
      if (class(x[[1]]) == "epd.entity.df"){
        data_list <- lapply(x, table_by_taxa_age, taxa, sample_label)
      }else{
        if (setequal(colnames(x[[1]]), c("e_", "londd", "latdd", "count",
                                         "sample_label", "taxa"))){
          data_list <- x
        }else{
          stop("data.frames in x of the wrong type. See ?table_by_taxa_age.")
        }
      }
    }
  }else{
    stop("x of the wrong class. It has to be a list of epd.entity.df objects
         (see ?getAgedCount) or data.frames (see ?table_by_taxa_age)")
  }
  data_list <- do.call(rbind, data_list)
  index <- which(data_list$londd < -175)
  data_list$londd[index] <- 360 + data_list$londd[index]
  if (is.null(zoom_coords)){
    xmin <- min(data_list$londd) - (0.005 * range(data_list$londd)[[1]])
    xmax <- max(data_list$londd) + (0.005 * range(data_list$londd)[[2]])
    ymin <- min(data_list$latdd) - (0.005 * range(data_list$latdd)[[1]])
    ymax <- max(data_list$latdd) + (0.005 * range(data_list$latdd)[[2]])
  }else{
    xmin <- zoom_coords[1]
    xmax <- zoom_coords[2]
    ymin <- zoom_coords[3]
    ymax <- zoom_coords[4]
  }
  counts_type <- x[[1]]@countstype
  if (pres_abse == TRUE){
    if (is.null(pollen_thres)){
      warning("Pollen threshold (pollen_thres) not provided as argment when
          requiring presence maps. Data maped using default threshold > 0%)")
      pollen_thres <- 0
    }
    data_list$count <- as.factor(data_list$count > pollen_thres)
    if (is.null(map_title)){
      map_title <- paste(taxa, " (>", pollen_thres, "): ",
                         sample_label, " cal BP", sep = "")
    }
    if (is.null(legend_title)){
      legend_title <- paste("Presence (>", pollen_thres, ")", sep = "")
    }
    if (is.null(points_colour)){
      points_colour <- c("Red 4", "Dodger Blue 3")
    }
    if (is.null(points_fill)){
      points_fill <- c("Red 2", "Dodger Blue 1")
    }
    if (is.null(points_range_size)){
      points_range_size <- c(2, 3.5)
    }
    nplot <- ggplot2::ggplot(data_list, ggplot2::aes(x = data_list$londd,
                                            y = data_list$latdd,
                                            fill = data_list$count,
                                            colour = data_list$count,
                                            size = data_list$count)) +
      ggplot2::borders("world",
                       fill = countries_fill_colour,
                       colour = countries_border_colour) +
      ggplot2::geom_point(pch = points_pch) +
      ggplot2::coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
      ggplot2::ggtitle(map_title) +
      ggplot2::guides(size = ggplot2::guide_legend(title = legend_title),
                      fill = ggplot2::guide_legend(title = legend_title),
                      colour = ggplot2::guide_legend(title = legend_title)) +
      ggplot2::scale_fill_manual(values = points_fill,
                                 na.value = napoints_fill) +
      ggplot2::scale_colour_manual(values = points_colour,
                                   na.value = napoints_colour) +
      ggplot2::scale_size_discrete(range = points_range_size,
                                   na.value = napoints_size) +
      ggplot2::scale_x_continuous(name = "Longitude") +
      ggplot2::scale_y_continuous(name = "Latitude") +
      ggplot2::theme_bw()
  }else{
    if (is.null(map_title)){
      map_title <- paste(taxa, ": ", sample_label, " cal BP", sep = "")
    }
    if (is.null(legend_title)){
      if (counts_type == "Percentages"){
        legend_title <- paste(counts_type, " (%)", sep = "")
      }else{
        legend_title <- paste(counts_type, " (n)", sep = "")
      }
    }
    if (is.null(legend_range)){
      if (counts_type == "Percentages"){
        legend_range <- c(0, max(data_list$count))
      }else{
        legend_range <- c(0, max(data_list$count))
      }
    }
    if (is.null(points_colour)){
      points_colour <- c("Blue 1", "Blue 3")
    }
    if (is.null(points_fill)){
      points_fill <- c("Light Blue 1", "Blue 3")
    }
    if (is.null(points_range_size)){
      points_range_size <- c(2, 7)
    }
    nplot <- ggplot2::ggplot(data_list, ggplot2::aes(x = data_list$londd,
                                 y = data_list$latdd,
                                 fill = data_list$count,
                                 colour = data_list$count,
                                 size = data_list$count)) +
      ggplot2::borders("world",
                       fill = countries_fill_colour,
                       colour = countries_border_colour) +
      ggplot2::geom_point(colour = napoints_colour,
                          fill = napoints_fill,
                          size = napoints_size,
                          show.legend = TRUE) +
      ggplot2::geom_point(pch = points_pch) +
      ggplot2::coord_quickmap(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
      ggplot2::ggtitle(map_title) +
      ggplot2::guides(size = ggplot2::guide_legend(title = legend_title),
                      fill = ggplot2::guide_legend(title = legend_title),
                      colour = ggplot2::guide_legend(title = legend_title)) +
      ggplot2::scale_fill_gradient(low = points_fill[1],
                                   high = points_fill[2],
                                   limits = legend_range) +
      ggplot2::scale_colour_gradient(low = points_colour[1],
                                     high = points_colour[2],
                                     limits = legend_range) +
      ggplot2::scale_size(range = points_range_size, limits = legend_range) +
      ggplot2::scale_x_continuous(name = "Longitude") +
      ggplot2::scale_y_continuous(name = "Latitude") +
      ggplot2::theme_bw()
  }
  nplot
  return(nplot)
}



# plot_diagram -----------------------------------------------

#' Plot pollen diagram of an entity
#'
#' Description TBW
#'
#' Details TBW
#'
#' @param x TBW
#' @param chronology TBW
#' @param use_ages TBW
#' @param low_values_exag TBW
#' @param color_by_group TBW
#' @param order_taxa TBW
#' @param ... TBW
#'
#' @return TBW
#' 
#' @references \url{http://blarquez.com/684-2/}
#' 
#' @examples
#' # TBW
#' @rdname plot_diagram
#' @exportMethod plot_diagram
setGeneric("plot_diagram", function(x, ...,
                                    chronology = NULL,
                                    use_ages = TRUE,
                                    low_values_exag = TRUE,
                                    color_by_group = TRUE,
                                    order_taxa = TRUE){
  standardGeneric("plot_diagram")
})

#' @rdname plot_diagram
setMethod("plot_diagram", signature(x = "epd.entity.df"),
          function(x, ...){
  # Plotting pollen diagrams
  if (is.null(chronology)){
    chronology <- x@defaultchron
  }
  if (chronology == 9999){
    chronology <- "giesecke"
  }
  if (use_ages){
    if (nrow(x@agesdf@depthages) == 0){
      warning(paste0("The entity has not ages, and hence depths will be used ",
                     "for the y axis of the pollen diagram."))
      ages <- x@agesdf@depthcm
      xlabel <- "Depth (cm)"
      x_breaks <- seq(0, 100000, 500)
    }else{
      ages <- x@agesdf@depthages[, as.character(chronology)]
      xlabel <- "Age (cal. BP)"
    }
  }else{
    ages <- x@agesdf@depthcm
    xlabel <- "Depth (cm)"
    x_breaks <- seq(0, 10000, 50)
  }
  if (is.null(color_by_group) || color_by_group == TRUE){
    groups <- x@commdf@taxagroupid
  }else{
    groups <- x@commdf@taxanames
  }
  counts <- x@commdf@counts
  maxcounts <- apply(counts, MARGIN = 2, FUN = max, na.rm = TRUE)
  dec_order <- order(maxcounts, decreasing = TRUE)
  if (x@countstype == "Percentages"){
    ylabel <- "Percentage (%)"
    y_breaks <- seq(0, 100, 10)
  }else{
    ylabel <- "Counts (n)"
    y_breaks <- seq(0, max(maxcounts), 10)
  }
  if (low_values_exag){
    maxcounts <- as.data.frame(do.call(rbind,
                                       replicate(nrow(counts),
                                                 maxcounts,
                                                 FALSE)))
    exag <- counts * 10
    exag <- pmin(exag, maxcounts)
  }else{
    exag <- counts
  }
  if (is.logical(order_taxa) && order_taxa == TRUE){
    counts <- counts[, dec_order]
    exag <- exag[, dec_order]
    groups <- groups[dec_order]
  } else if (is.numeric(order_taxa)){
    counts <- counts[, order_taxa]
    exag <- exag[, order_taxa]
    groups <- groups[order_taxa]
  }
  df <- reshape2::melt(counts)
  exag <- reshape2::melt(exag)
  df$yr <- rep(ages, ncol(counts))
  df$group <- rep(groups, each = nrow(counts))
  df$exag <- exag$value
  theme_new <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(), # remove grids
                     panel.background = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     strip.text.x = ggplot2::element_text(size = 10,
                                                 angle = 90,
                                                 vjust = 0),
                     strip.background = ggplot2::element_blank(),
                     strip.text.y = ggplot2::element_text(angle = 0),
                     panel.border = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90,
                                                hjust = 1))
nplot <- ggplot2::ggplot(df) +
    ggplot2::geom_area(ggplot2::aes(df$yr, df$exag, fill = df$group)) +
    ggplot2::geom_area(ggplot2::aes(df$yr, df$value)) +
    ggplot2::scale_x_reverse(breaks = x_breaks) +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::xlab(xlabel) + 
    ggplot2::ylab(ylabel) +
    ggplot2::coord_flip() +
    theme_new +
    ggplot2::facet_grid(~df$variable, scales = "free", space = "free")
  nplot
  return(nplot)
})
