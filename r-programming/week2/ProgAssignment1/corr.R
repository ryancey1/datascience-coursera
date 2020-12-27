## Write a function that takes a directory of data files and a 
## threshold for complete cases and calculates the correlation 
## between sulfate and nitrate for monitor locations where the 
## number of completely observed cases (on all variables) is 
## greater than the threshold. The function should return a vector 
## of correlations for the monitors that meet the threshold requirement. 
## If no monitors meet the threshold requirement, then the function 
## should return a numeric vector of length 0.

corr <- function(directory, threshhold = 0) {
     files <- list.files(directory, full.names = TRUE)
     store <- vector("numeric")
     comp <- complete(directory)
     
     for(i in seq_along(files)) {
          if(comp[i, "nobs"] > threshhold) {
               file <- read.csv(files[i])
               store <- c(store, cor(file[,2], file[,3], use = "complete.obs"))
               }
     }
     store
}