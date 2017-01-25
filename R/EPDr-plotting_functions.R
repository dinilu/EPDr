
#' Title TBW
#'
#' @param agedcounts TBW
#' @param taxa TBW
#' @param sample_label TBW
#' @param pres_abse TBW
#' @param pollen_thres TBW
#' @param zoom_coord TBW
#' @param points_pch TBW
#' @param points_colour TBW
#' @param points_fill TBW
#' @param points_range_size TBW
#' @param map_title TBW
#' @param legend_range TBW
#' @param legend_title TBW
#' @param napoints_size TBW
#' @param napoints_pch TBW
#' @param napoints_colour TBW
#' @param napoints_fill TBW
#' @param countries_bg_colour TBW
#' @param countries_border_colour TBW 
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
map_by_TaxaAge <- function(agedcounts, taxa, sample_label, pres_abse=FALSE, pollen_thres=NULL, zoom_coord=NULL,
                           points_pch=21, points_colour=NULL, points_fill=NULL, points_range_size=NULL, map_title=NULL,
                           legend_range=NULL, legend_title=NULL, napoints_size=0.75, napoints_pch=19, napoints_colour="grey45",
                           napoints_fill=NA, countries_bg_colour="grey80", countries_border_colour="grey90"){
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
    
    if(is.null(zoom_coord)){
        xmin <- min(dataList$londd) - (0.005 * range(dataList$londd)[[1]])
        xmax <- max(dataList$londd) + (0.005 * range(dataList$londd)[[2]])
        ymin <- min(dataList$latdd) - (0.005 * range(dataList$latdd)[[1]])
        ymax <- max(dataList$latdd) + (0.005 * range(dataList$latdd)[[2]])
    }else{
        xmin <- zoom_coord[1]
        xmax <- zoom_coord[2]
        ymin <- zoom_coord[3]
        ymax <- zoom_coord[4]
    }
    
    counts_type <- agedcounts[[1]]@counts@counts_type
    
    if(pres_abse==TRUE){
        if(is.null(pollen_thres)){
            warning("Pollen threshold (pollen_thres) not provided as argment when requiring presence maps. Data maped using default threshold > 0%)")
            pollen_thres <- 0
        }
        
        dataList$count <- dataList$count > pollen_thres
        dataList_woNA <- dataList[!is.na(dataList$count),]
        
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
            ggplot2::borders("world", fill=countries_bg_colour, colour=countries_border_colour) +
            ggplot2::geom_point(data=dataList, pch=napoints_pch, colour=napoints_colour, fill=napoints_fill, size=napoints_size, show.legend=FALSE) +
            ggplot2::geom_point(pch=points_pch) +
            ggplot2::coord_fixed(xlim=c(xmin, xmax), ylim = c(ymin, ymax), ratio=1) +
            ggplot2::ggtitle(map_title) +
            ggplot2::guides(size=ggplot2::guide_legend(title=legend_title), fill=ggplot2::guide_legend(title=legend_title), colour=ggplot2::guide_legend(title=legend_title)) +
            ggplot2::scale_fill_manual(values=points_fill) +
            ggplot2::scale_colour_manual(values=points_colour) +
            ggplot2::scale_size_discrete(range=points_range_size) +
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
        
        dataList_zero <- subset(dataList, dataList$count == 0)
        
        ggplot2::ggplot(dataList, ggplot2::aes(x=dataList$londd, y=dataList$latdd, fill=dataList$count, colour=dataList$count, size=dataList$count)) +
            ggplot2::borders("world", fill=countries_bg_colour, colour=countries_border_colour) +
            ggplot2::geom_point(pch=napoints_pch, colour=napoints_colour, fill=napoints_fill, size=napoints_size, show.legend=FALSE) +
            ggplot2::geom_point(pch=points_pch) +
            ggplot2::coord_fixed(xlim=c(xmin, xmax), ylim = c(ymin, ymax), ratio=1) +
            ggplot2::ggtitle(map_title) +
            ggplot2::guides(size=ggplot2::guide_legend(title=legend_title), fill=ggplot2::guide_legend(title=legend_title), colour=ggplot2::guide_legend(title=legend_title)) +
            ggplot2::scale_fill_gradient(low=points_fill[1], high=points_fill[2], limits=legend_range) +
            ggplot2::scale_colour_gradient(low=points_colour[1], high=points_colour[2], limits=legend_range) +
            ggplot2::scale_size(range=points_range_size, limits=legend_range) +
            ggplot2::theme_bw() 
    }
}
