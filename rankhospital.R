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
        
        if(outcome == heartAttack) {
                allInState[, 11] <- as.numeric(allInState[, 11])
                allInState <- allInState[!is.na(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                
                myOrder <- order(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                
                if(is.numeric(num)) {
                        
                        
                        
                        
                } else if(num == "best") {
                        
                } else if(num == "worst") {
                        
                }
        }
        
}





































