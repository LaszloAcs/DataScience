
rankhospital <- function(state, outcome, num) {
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state and outcome are valid
     if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
          stop("invalid outcome")
     }
     
     states <- unique(data$State)
     if(!state %in% states) stop("invalid state")
     
     if (class(num) != "numeric" & !num %in% c("best", "worst")) {
          stop('invalid "num" value')
     }
          

     ## Determine column to use for ranking
     switch(outcome, 
            "heart attack"  = (outCol <- 11),
            "heart failure" = (outCol <- 17),
            "pneumonia"     = (outCol <- 23))
     
     ## Filter on state
     output <- data[data$State==state,]
     
     ## Convert column using as.numeric, supressing warnings
     suppressWarnings(output[, outCol] <- as.numeric(output[, outCol]))
     
     ## Set column index for output
     ## Order on outCol and then name
     if (class(num) == "numeric" || num=="best") {
          ## Order on outCol, and then name (column 2)
          output <- output[order(output[,outCol], output[,2]), ]
     } else if (num=="worst") {
          ## Order on reverse of outCol, and then name
          output <- output[order(-output[,outCol], output[,2]), ]
     }

     ## check if not numeric rank and if so, set num as 1
     if (class(num) != "numeric") { num <- 1 }
     
     ## Return hospital name in that state with the given rank for
     ## 30-day death rate of specified outcome type
     return (output[num,2])
}

## TESTS
# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)
# rankhospital("AL", "pneumonia", "help")