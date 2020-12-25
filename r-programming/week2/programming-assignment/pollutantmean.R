## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## NOTE: Do not round the result!

pollutantmean <- function(directory, pollutant, id = 1:332) {
     ## vector of file paths
     files <- list.files(directory, full.names = TRUE)
     ## empty vector to fill with means
     mean.vect <- vector("numeric", length = length(id))

     ## calculates mean of specified columns
     for (i in seq_along(id)) {
          tmp <- read.csv(files[id[i]])
          item <- mean(tmp[[pollutant]], na.rm = TRUE)
          mean.vect[i] <- item
          ## if-else to deal with files without pollutant values (NaNs)
          # if(is.nan(item)) {
          #      mean.vect[i] <- 0
          # } else {
          #      mean.vect[i] <- item
          # }
     }
     mean(mean.vect, na.rm = TRUE)
}