rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        heartAttack <- "heart attack"
        heartFailure <- "heart failure"
        pneumonia <- "pneumonia"
        hostpitalNames <- NULL
        
        if(!(outcome == heartAttack || 
             outcome == heartFailure || 
             outcome == pneumonia)) stop("invalid outcome")
        
        allStates <- unique(data$State)
        
        if(outcome == heartAttack) {
                data[, 11] <- as.numeric(data[, 11])
                data <- data[!is.na(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
        
                for (state in allStates) {
                        allInState <- data[data$State == state]
                        myOrder <- allInState[order(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, allInState$Hospital.Name),]
                        
                }
                
                
                
                
                
        }
        
        
        
        
        
}





































