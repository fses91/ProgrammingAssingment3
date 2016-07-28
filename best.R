best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        heartAttack <- "heart attack"
        heartFailure <- "heart failure"
        pneumonia <- "pneumonia"
        hostpitalNames <- NULL
        
        # Check if input is valid.
        if(!(state %in% data$State)) stop("invalid state")
        if(!(outcome == heartAttack || 
             outcome == heartFailure || 
             outcome == pneumonia)) stop("invalid outcome")
        
        allInState <- data[data$State == state,]
        
        # Heart Attack
        if(outcome == heartAttack) {
                allInState[, 11] <- as.numeric(allInState[, 11])
                allInState <- allInState[!is.na(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                
                hostpitalNames <-  allInState[(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==
                                                 min(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]["Hospital.Name"]
        }
        
        # Heart Failure
        if(outcome == heartFailure) {
                allInState[, 17] <- as.numeric(allInState[, 17])
                allInState <- allInState[!is.na(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                
                hostpitalNames <- allInState[(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==
                                                min(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]["Hospital.Name"]
        }
        
        # Pneumonia
        if(outcome == pneumonia) {
                allInState[, 23] <- as.numeric(allInState[, 23])
                allInState <- allInState[!is.na(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                
                hostpitalNames <- allInState[(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==
                                                min(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]["Hospital.Name"]
        }
        sort(hostpitalNames)[1]
}