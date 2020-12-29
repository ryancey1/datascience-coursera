rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- list.files("r-programming/week4/rprog_data_ProgAssignment3-data",
                           full.names = TRUE, pattern = ".csv")
        data <- read.csv(data[2])[c(2,7,11,17,23)]
        
        ## Coerces data columns to numeric, suppress warnings
        suppressWarnings(for(i in 3:5) {
                data[,i] <- as.numeric(data[,i])
        })   
        
        ## Check that state and outcome are valid
        valid.states <- state.abb
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!any(state == valid.states)) {
                stop("invalid state")
        }
        if(!any(outcome == valid.outcomes)) {
                stop("invalid outcome")
        }
        
        ## Splits and subsets
        byState <- split(data, data$State)
        byState <- byState[[state]]

        ## Subsets by outcome, sorts in ascending order, omit NAs
        if(outcome == valid.outcomes[1]) {
                sorted <- byState[,c(1,3)]
                sorted <- sorted[order(sorted[,2], sorted[,1], na.last = NA), ]
        } else if(outcome == valid.outcomes[2]) {
                sorted <- byState[,c(1,4)]
                sorted <- sorted[order(sorted[,2], sorted[,1], na.last = NA), ]
        } else if(outcome == valid.outcomes[3]) {
                sorted <- byState[,c(1,5)]
                sorted <- sorted[order(sorted[,2], sorted[,1], na.last = NA), ]
        }
        
        ## Set the value of num based on input. 
        ## Return NA if num > number of hosps.
        if(num == "best") {
                rank <- 1
        } else if(num == "worst") {
                rank <- nrow(sorted)
        } else if(num > nrow(sorted)) {
                return(NA)
        } else {
                rank <- num
        }
        
        ## Returns specified result
        sorted[rank,1]
}