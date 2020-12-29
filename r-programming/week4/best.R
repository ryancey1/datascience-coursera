best <- function(state, outcome) {
        ## Read and format outcome data, keep desired columns
        data <- list.files("r-programming/week4/rprog_data_ProgAssignment3-data",
                           full.names = TRUE, pattern = ".csv")
        data <- read.csv(data[2])[c(2,7,11,17,23)]
        
        ## Coerce data columns to numeric, suppress warnings
        suppressWarnings(for(i in 3:5) {
                data[,i] <- as.numeric(data[,i])
        })        
        
        ## Vectors of valid queries
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        valid.states <- state.abb
        
        ## Check that state and outcome are valid
        if(!any(state == valid.states)) {
                stop("invalid state")
        }
        if(!any(outcome == valid.outcomes)) {
                stop("invalid outcome")
        }
        
        ## Split and subset 
        byState <- split(data, data$State)
        byState <- byState[[state]]
        
        ## Subsets 
        if(outcome == valid.outcomes[1]) {
                sorted <- byState[,c(1,3)]
                sorted <- sorted[order(sorted[,2], sorted[,1]), ]
                sorted[1,1]
        } else if(outcome == valid.outcomes[2]) {
                sorted <- byState[,c(1,4)]
                sorted <- sorted[order(sorted[,2], sorted[,1]), ]
                sorted[1,1]
        } else if(outcome == valid.outcomes[3]) {
                sorted <- byState[,c(1,5)]
                sorted <- sorted[order(sorted[,2], sorted[,1]), ]
                sorted[1,1]
        }
}