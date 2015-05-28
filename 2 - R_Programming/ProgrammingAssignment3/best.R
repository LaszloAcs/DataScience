best <- function(state, outcome) {
     ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state and outcome are valid
     if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
          stop("invalid outcome")
     }
     states <- unique(data$State)
     if(!state %in% states) stop("invalid state")
     
     ## Determine column to use for ranking
     switch(outcome, 
          "heart attack"  = (outCol <- 11),
          "heart failure" = (outCol <- 17),
          "pneumonia"     = (outCol <- 23))
     
     ## Filter on state
     output <- data[data$State==state,]
     
     ## Convert using as.numeric, supressing warnings
     suppressWarnings(output[, outCol] <- as.numeric(output[, outCol]))

     ## Set column index for output
     ## Order on outCol and then name
     output <- output[order(output[,outCol], output[,2]), ]

     ## Return hospital name in state with lowest 30-day death rate
     return(output[1,2])
}

## TESTS
# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")
# best("BB", "heart attack")
# best("NY", "hert attack")
