mapTaxaAge <- function(list, taxa, age){
    # taxa <- "Abies"
    # age <- 17000
    # list <- interpList
    
    taxaAgeData <- function(agedcounts, age, taxa){
        #agedcounts <- list[[3]]
        core_number <- agedcounts@core_number
        londd <- agedcounts@site$londd
        latdd <- agedcounts@site$latdd
        count <- agedcounts@counts@counts[which(agedcounts@ages@depth_ages == age), which(agedcounts@counts@taxa_names == taxa)]
        if(length(count) == 0){count <- NA}
        output <- data.frame(core_number, londd, latdd, count, age, taxa)
        return(output)
    }
    
    dataList <- lapply(list, function(x, y, z){taxaAgeData(x, y, z)}, age, taxa)
    dataList <- do.call(rbind, dataList)
    ggplot(dataList, aes(x=londd, y=latdd, colour=count, size=count)) +
        borders("world") +
        geom_point() +
        coord_quickmap(xlim=c(-15, 150), ylim = c(30, 75), expand=T) +
        ggtitle(paste(taxa, ": ", age, " cal BP", sep=""))
}

