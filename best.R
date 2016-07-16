best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        heartAttack <- "heart attack"
        heartFailure <- "heart failure"
        pneumonia <- "pneumonia"
        states <- NULL
        
        # Check if input is valid.
        if(!(state %in% data$State)) stop("invalid state")
        if(!(outcome == heartAttack || 
             outcome == heartFailure || 
             outcome == pneumonia)) stop("invalid outcome")
        
        # Heart Attack
        if(outcome == heartAttack) {
                states <- data[data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == 
                                       min(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) & data$State == outcome, ][["Hospital.Name"]] 
        }
        
        # Heart Failure
        if(outcome == heartFailure) {
                states <- data[data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == 
                                       min(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) & data$State == outcome, ][["Hospital.Name"]] 
        }
        
        # Pneumonia
        if(outcome == pneumonia) {
                states <- data[data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == 
                                       min(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) & data$State == outcome, ][["Hospital.Name"]]
        }
        sort(states)[1]
}