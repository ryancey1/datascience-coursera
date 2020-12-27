complete <- function(directory, id = 1:332) {
     ## generate sub-setted list of filepaths
     files <- list.files(directory, full.names = TRUE)[id]
     
     ## storage vectors for ids and nobs
     ids <- vector(mode="numeric", length = length(id))
     nobs <- vector(mode = "numeric", length = length(id))
     
     ## reads CSV file, calculates complete cases, adds to storage
     for(i in seq_along(id)) {
          tmp <- read.csv(files[i])
          comp <- sum(complete.cases(tmp[,2:3]))
          ids[i] <- id[i]
          nobs[i] <- comp
     }
     
     ## generates data frame
     data.frame(id = ids, nobs = nobs)
}