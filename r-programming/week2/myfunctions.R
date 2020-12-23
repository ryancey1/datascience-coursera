#first function
add2 <- function(x, y) {
     x + y
}

above10 <- function(x) {
     use <- x > 10 # logical vector return
     x[use] # returns subset of x that's bigger than 10
}

above <- function(x, n = 10) {
     use <-
          x > n  #same as above but sets default value to 10(won't throw error)
     
     x[use]
}

columnMean <- function(x, removeNA = TRUE) {
     nc <- ncol(x)
     means <- numeric(nc)
     
     for (i in 1:nc) {
          means[i] <- mean(x[, i], na.rm = removeNA)
     }
     
     means
}