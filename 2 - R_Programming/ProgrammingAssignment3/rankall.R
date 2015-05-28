rankall <- function(outcome, num = "best") {
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that num and outcome are valid
     if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
          stop("invalid outcome")
     }
     
     if (class(num) != "numeric" & !num %in% c("best", "worst")) {
          stop('invalid "num" value')
     }
     
     # create a sorted list of states from the read data
     states <- sort(unique(data$State))
     
     ## check if num is not numeric since already valid and if so, 
     ## set rank to 1, to be used later
     if (class(num) != "numeric") { 
          rank <- 1 
     } else {
          rank <- num
     }
     
     ## Determine column to use for ranking
     switch(outcome, 
            "heart attack"  = (outCol <- 11),
            "heart failure" = (outCol <- 17),
            "pneumonia"     = (outCol <- 23))
     
     ## Create a list of dataframes of what we need from the read-in data
     output_states <- split(data[,c("Hospital.Name", "State", names(data)[outCol])], data$State)
     
     ## Create our output dataframe and column names
     output_final <- data.frame(matrix(ncol=2, nrow = 0))
     names(output_final)<-c("hospital", "state")
     
     for(i in 1:length(states)) {
          ## Convert column using as.numeric, supressing warnings
          suppressWarnings(output_states[[i]][,3] <- as.numeric(output_states[[i]][,3]))
          ## Set column index for output
          ## Order on outCol and then name
          if (class(num) == "numeric" || num=="best") {
               ## Order on outcome, and then hospital name (column 1)
               output_states[[i]] <- output_states[[i]][order(output_states[[i]][,3], output_states[[i]][,1], na.last=NA), ]
          } else if (num=="worst") {
               ## Order on reverse of outcome, and then hospital name
               output_states[[i]] <- output_states[[i]][order(-output_states[[i]][,3], output_states[[i]][,1], na.last=NA), ]
          }
          output_final[i,] <- c(output_states[[i]][rank, 1], output_states[[i]][1, 2])
     }
     
     ## Return hospital name in that state with the given rank for
     ## 30-day death rate of specified outcome type
     return (output_final)
}


## tests
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)