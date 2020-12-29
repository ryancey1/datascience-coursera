## This function take two arguments: the 2-character abbreviated name of a 
## state and an outcome name. The function reads the outcome-of-care-measures.csv 
## file and returns a character vector with the name of the hospital that has 
## the best (i.e. lowest) 30-day mortality for the specified outcome in that state.

best <- function(state, outcome) {
        ## Reads and formats outcome data -- keeps desired columns
        data <- list.files("rprog_data_ProgAssignment3-data",
                           full.names = TRUE, pattern = ".csv")
        data <- read.csv(data[2])[c(2,7,11,17,23)]
        
        ## Coerces data columns to numeric, suppress warnings
        suppressWarnings(for(i in 3:5) {
                data[,i] <- as.numeric(data[,i])
        })        
        
        ## Vectors of valid queries
        valid.states <- sort(unique(data$State))
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        ## Checks that state and outcome are valid
        if(!any(state == valid.states)) {
                stop("invalid state")
        }
        if(!any(outcome == valid.outcomes)) {
                stop("invalid outcome")
        }
        
        ## Splits and subsets
        byState <- split(data, data$State)
        byState <- byState[[state]]
        
        ## Sorts subsetted data in ascending order 
        if(outcome == valid.outcomes[1]) {
                sorted <- byState[,c(1,3)]
                sorted <- sorted[order(sorted[,2], sorted[,1], na.last = NA), ]
                sorted <- sorted[1,1]
        } else if(outcome == valid.outcomes[2]) {
                sorted <- byState[,c(1,4)]
                sorted <- sorted[order(sorted[,2], sorted[,1], na.last = NA), ]
                sorted <- sorted[1,1]
        } else if(outcome == valid.outcomes[3]) {
                sorted <- byState[,c(1,5)]
                sorted <- sorted[order(sorted[,2], sorted[,1], na.last = NA), ]
                sorted <- sorted[1,1]
        }
        
        # Returns top result
        sorted
}