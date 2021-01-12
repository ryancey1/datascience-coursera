run_scripts <- function(n) {
        
        for (i in n) {
                script <- paste0("plot", i, ".R")
                message(paste0("Sourcing script: \'", script, "\'."))
                source(script)
        }
}
