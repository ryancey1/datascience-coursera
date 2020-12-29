pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        ## create sub-setted list of files based on id
        files <- list.files(directory, full.names = TRUE)[id]
        
        ## extract pollutant columns from specified file into list
        data <- lapply(files, function(x) read.csv(x)[[pollutant]])
        
        ## un-list
        data <- unlist(data)
        
        ## calculate weighted mean
        mean(data, na.rm = TRUE)
}