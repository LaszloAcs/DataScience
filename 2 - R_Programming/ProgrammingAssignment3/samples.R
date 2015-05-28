## ==========================================================
## SAMPLE 1
## ==========================================================

num_helper <- function(data, col_num, state, num) {
    state_subset <- data[data[, 7]==state, ]
    # get "attack", "failure" and "pneumonia" vector
    outcome_arr <- state_subset[, col_num]
    len <- dim(state_subset[!is.na(outcome_arr), ])[1]
    if (num == "worst") {
        rank <- rank_helper(state_subset, outcome_arr, len)
    } else if (num > len) {
        rank <- NA
    } else {
        rank <- rank_helper(state_subset, outcome_arr, num)
    }
    result <- rank
    return(result)
}

rank_helper <- function(state_subset, outcome_arr, num) {
    result <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[num]]
    return(result)
}

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    # read the data file
    directory <- "./data/outcome-of-care-measures.csv"
    data <- read.csv(directory, colClasses="character")
    # change data type from character to numeric
    data[, 11] <- as.numeric(data[, 11]) # heart attack
    data[, 17] <- as.numeric(data[, 17]) # heart failure
    data[, 23] <- as.numeric(data[, 23]) # pneumonia
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!state %in% data$State) {
        stop("invalid state")
    } else if(!outcome %in% valid_outcomes) {
        stop("invalid outcome")
    } else {
        if (num == "best") {
            rank <- beast(state, outcome)
        } else {
            if(outcome == "heart attack") {
                rank <- num_helper(data, 11, state, num) 
            } else if(outcome == "heart failure") {
                rank <- num_helper(data, 17, state, num) 
            } else {
                rank <- num_helper(data, 23, state, num) 
            }
        }
        result <- rank
        return(result)
    }
}

# tests
rankhospital("MN", "heart attack", 5000)
rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart failure", 4)


## ==========================================================
## SAMPLE 2
## ==========================================================
Outcome <- read.csv("Outcome-of-care-measures.csv", colClasses = "character")
head(Outcome)

#PARTE 3
rankhospital <- function(stateChr, outcomeChr, rankObj) {
	outcomeDfr <- Init("Outcome-of-care-measures.csv")
	
	suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
	suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
	suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))
	
	tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
							length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
	rownames(tableDfr) <- NULL
	
	inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
			Col = c(11, 17, 23))
	
	if (nrow(tableDfr[tableDfr$State == stateChr, ]) == 0) 
		stop("invalid state")
	if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
		stop("invalid outcome")
	
	stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
	colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
	stateDfr <- stateDfr[complete.cases(stateDfr[, colNum]), ]
	stateDfr <- stateDfr[order(stateDfr[, colNum], stateDfr$Hospital.Name), 
	]
	
	if (rankObj == "best") 
		rankObj <- 1
	if (rankObj == "worst") 
		rankObj <- nrow(stateDfr)
	
	suppressWarnings(rankNum <- as.numeric(rankObj))
	
	return(stateDfr[rankNum, ]$Hospital.Name)
}

## ==========================================================
## SAMPLE 3
## ==========================================================
rankhospital <- function(state, outcome, num = "best") {
    full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
   	column <- if (outcome == "heart attack") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	} else if (outcome == "heart failure") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	} else if (outcome == "pneumonia") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	} else {
		stop("invalid outcome")
	}

	data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
	
	if (nrow(data_for_state) == 0) {
		stop("invalid state")	
	}

    data_for_state[,2] <- as.numeric(data_for_state[,2])
	ordered_data_for_state <- order(data_for_state[column], data_for_state$Hospital.Name, na.last=NA)
    
    if (num == "best") {
        as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
    } else if (num == "worst") {
       as.character(data_for_state$Hospital.Name[ordered_data_for_state[length(ordered_data_for_state)]])
    } else if (is.numeric(num)) {
       as.character(data_for_state$Hospital.Name[ordered_data_for_state[num]])
    } else {
        stop("invalid num")
    }
}

## ==========================================================
## SAMPLE 4
## ==========================================================
rankhospital <- function(state, outcome, num) {
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")) {
        stop("invalid outcome")
    }
    states <- unique(outcomeData$State)
    if(!state %in% states) stop("invalid state")
    ## Return hospital name in that state with lowest 30-day death
    ## Set col index for outcome
    if (outcome == "heart attack") outcomeCol <- 11
    if (outcome == "heart failure") outcomeCol <- 17
    if (outcome == "pneumonia") outcomeCol <- 23
    ## Filter DF on state
    outcomeData <- outcomeData[outcomeData$State==state,]
    ## Convert using as.numeric, supressing coercian warnings
    suppressWarnings(outcomeData[, outcomeCol] <- as.numeric(outcomeData[, outcomeCol]))
    if (class(num) == "numeric") {
        ## Order DF on outcomeCol, and then name
        outcomeData <- outcomeData[order(outcomeData[,outcomeCol], outcomeData[,2]), ]
        return (outcomeData[num,2])
    }
    if (num=="best") {
        ## Order DF on outcomeCol, and then name
        outcomeData <- outcomeData[order(outcomeData[,outcomeCol], outcomeData[,2]), ]
        return (outcomeData[1,2])
    }
    if (num=="worst") {
        ## Order DF on reverse of outcomeCol, and then name
        outcomeData <- outcomeData[order(-outcomeData[,outcomeCol], outcomeData[,2]), ]
        return (outcomeData[1,2])
    }
}

rankhospital <- function(state, outcome, num = "best") { 
  source('unique.R')
  outcomes = c('heart attack', 'heart failure', 'pneumonia')
  outcome1 = read.csv("outcome-of-care-measures.csv", 
                      colClasses="character")
  diseaseCol = 0
  outcome1[, 11] <- as.numeric(outcome1[, 11])
  outcome1[, 17] <- as.numeric(outcome1[, 17])
  outcome1[, 23] <- as.numeric(outcome1[, 23])
  if(outcome == 'heart attack')
  {
    diseaseCol = 11
  }
  if(outcome == 'heart failure')
  {
    diseaseCol = 17
  }
  if(outcome == 'pneumonia')
  {
    diseaseCol = 23
  }
  
  ## Check that state and outcome are valid
  if(!state %in% Unique(outcome1[, 7]))
  {
    stop("invalid state") 
  }
  if(!outcome %in% outcomes)
  {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank 
  ## 30-day death rate
  state1 = outcome1[which(outcome1$State == state),]
  
  state1 = state1[order(state1$Hospital.Name),]
  if(diseaseCol == 11)
  {
    state1 = state1[order(state1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    state1 = state1[,c(2,diseaseCol)]
  }
  if(diseaseCol == 17)
  {
    state1 = state1[order(state1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    state1 = state1[,c(2,diseaseCol)]
  }
  if(diseaseCol == 23)
  {
    state1 = state1[order(state1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    state1 = state1[,c(2,diseaseCol)]
  }
  state1=state1[complete.cases(state1),]
  if(num=='worst'){num = nrow(state1)}
  if(num=='best'){num = 1}
  hos = state1[num,]
  return(hos$Hospital.Name)
  
}

## TESTS
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)