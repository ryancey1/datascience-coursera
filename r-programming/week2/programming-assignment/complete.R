complete <- function(directory, id = 1:332) {
     ## generate sub-setted list of filepaths
     files <- list.files(directory, full.names = TRUE)[id]
     
     ## storage vectors for ids and nobs
     nobs <- vector(mode = "numeric", length = length(id))
     
     ## reads CSV file, calculates complete cases, adds to storage
     for(i in seq_along(id)) {
          tmp <- read.csv(files[i])
          nob <- sum(complete.cases(tmp[,2:3]))
          nobs[i] <- nob
     }
     
     ## generates data frame
     data.frame(id = id, nobs = nobs)
}