
#' Title
#'
#' @param agedcounts 
#' @param taxa 
#' @param sample_label 
#' @param pres_abse 
#' @param pollen_thres 
#' @param zoom_coord 
#' @param points_pch 
#' @param points_colour 
#' @param points_fill 
#' @param points_range_size 
#' @param map_title 
#' @param legend_range 
#' @param legend_title 
#' @param napoints_size 
#' @param napoints_pch 
#' @param napoints_colour 
#' @param napoints_fill 
#' @param countries_bg_colour 
#' @param countries_border_colour 
#'
#' @return
#' @export
#'
#' @examples
map_by_TaxaAge <- function(agedcounts, taxa, sample_label, pres_abse=FALSE, pollen_thres=NULL, zoom_coord=NULL,
                           points_pch=21, points_colour=NULL, points_fill=NULL, points_range_size=NULL, map_title=NULL,
                           legend_range=NULL, legend_title=NULL, napoints_size=0.75, napoints_pch=19, napoints_colour="grey45",
                           napoints_fill=NA, countries_bg_colour="grey80", countries_border_colour="grey90"){
    if(class(agedcounts) == "list"){
        if(!class(agedcounts[[1]]) %in% c("agedcounts", "data.frame")){
            stop("agedcounts of the wrong class. It has to be a list of agedcount objects (see ?getAgedCount) or data.frames (see ?table_by_taxaAge)")
        }else{
            if(class(agedcounts[[1]]) == "agedcounts"){
                dataList <- lapply(agedcounts, table_by_taxaAge, sample_label, taxa)    
            }else{
                if(setequal(colnames(agedcounts[[1]]), c("core_number", "londd", "latdd", "count", "sample_label", "taxa"))){
                    dataList <- agedcounts
                }else{
                    stop("data.frames in agedcounts of the wrong type. See ?table_by_taxaAge.")
                }
            }
        }
    }else{
        stop("agedcounts of the wrong class. It has to be a list of agedcount objects (see ?getAgedCount) or data.frames (see ?table_by_taxaAge)")
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
    
    data_type <- agedcounts[[1]]@counts@data_type
    
    library(ggplot2)
    if(pres_abse==TRUE){
        if(is.null(pollen_thres)){
            warning("Pollen threshold (pollen_thres) not provided as argment when requiring presence maps. Data maped using default threshold > 0%)")
            pollen_thres <- 0
        }
        
        dataList$count <- dataList$count > pollen_thres
        dataList_woNA <- subset(dataList, !is.na(dataList$count))
        
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

        ggplot(dataList_woNA, aes(x=londd, y=latdd, fill=count, colour=count, size=count)) +
            borders("world", fill=countries_bg_colour, colour=countries_border_colour) +
            geom_point(data=dataList, pch=napoints_pch, colour=napoints_colour, fill=napoints_fill, size=napoints_size, show.legend=FALSE) +
            geom_point(pch=points_pch) +
            coord_fixed(xlim=c(xmin, xmax), ylim = c(ymin, ymax), ratio=1) +
            ggtitle(map_title) +
            guides(size=guide_legend(title=legend_title), fill=guide_legend(title=legend_title), colour=guide_legend(title=legend_title)) +
            scale_fill_manual(values=points_fill) +
            scale_colour_manual(values=points_colour) +
            scale_size_discrete(range=points_range_size) +
            theme_bw() 
    }else{
        if(is.null(map_title)){
            map_title <- paste(taxa, ": ", sample_label, " cal BP", sep="")    
        }
        if(is.null(legend_title)){
            if(data_type == "Percentages"){
                legend_title <- paste(data_type, " (%)", sep="")
            }else{
                legend_title <- paste(data_type, " (n)", sep="")
            }
        }
        if(is.null(legend_range)){
            if(data_type == "Percentages"){
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
        
        ggplot(dataList, aes(x=londd, y=latdd, fill=count, colour=count, size=count)) +
            borders("world", fill=countries_bg_colour, colour=countries_border_colour) +
            geom_point(pch=napoints_pch, colour=napoints_colour, fill=napoints_fill, size=napoints_size, show.legend=FALSE) +
            geom_point(pch=points_pch) +
            coord_fixed(xlim=c(xmin, xmax), ylim = c(ymin, ymax), ratio=1) +
            ggtitle(map_title) +
            guides(size=guide_legend(title=legend_title), fill=guide_legend(title=legend_title), colour=guide_legend(title=legend_title)) +
            scale_fill_gradient(low=points_fill[1], high=points_fill[2], limits=legend_range) +
            scale_colour_gradient(low=points_colour[1], high=points_colour[2], limits=legend_range) +
            scale_size(range=points_range_size, limits=legend_range) +
            theme_bw() 
    }
}
