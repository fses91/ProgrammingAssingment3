rankhospital <- function(state, outcome, num = "best") {
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
        
        if(num == "best") {
                
        }
        
}