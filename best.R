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
                hostpitalNames <- allInState[(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==
                                                     min(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),][["Hospital.Name"]]
        }
        
        # Heart Failure
        if(outcome == heartFailure) {
                hostpitalNames <- allInState[(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==
                                                     min(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),][["Hospital.Name"]] 
        }
        
        # Pneumonia
        if(outcome == pneumonia) {
                hostpitalNames <- allInState[(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==
                                                     min(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),][["Hospital.Name"]]
        }
        sort(hostpitalNames)[1]
}