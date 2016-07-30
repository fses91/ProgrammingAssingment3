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
        
        if(is.numeric(num)) {
                if(num > nrow(allInState)) return(NA)
        }
        
        if(outcome == heartAttack) {
                allInState[, 11] <- as.numeric(allInState[, 11])
                allInState <- allInState[!is.na(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                
                myOrder <- allInState[order(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, allInState$Hospital.Name),]
                
                if(is.numeric(num)) {
                        hostpitalNames <- myOrder[num,]["Hospital.Name"]
                        
                } else if(num == "best") {
                        hostpitalNames <- myOrder[1,]["Hospital.Name"]
                        
                } else if(num == "worst") {
                        hostpitalNames <- myOrder[nrow(myOrder),]["Hospital.Name"]
                }
        } else if(outcome == heartFailure) {
                allInState[, 17] <- as.numeric(allInState[, 17])
                allInState <- allInState[!is.na(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                
                myOrder <- allInState[order(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, allInState$Hospital.Name),]
                
                if(is.numeric(num)) {
                        hostpitalNames <- myOrder[num,]["Hospital.Name"]
                        
                } else if(num == "best") {
                        hostpitalNames <- myOrder[1,]["Hospital.Name"]
                        
                } else if(num == "worst") {
                        hostpitalNames <- myOrder[nrow(myOrder),]["Hospital.Name"]
                }
        } else if(outcome == pneumonia) {
                allInState[, 23] <- as.numeric(allInState[, 23])
                allInState <- allInState[!is.na(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                
                myOrder <- allInState[order(allInState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, allInState$Hospital.Name),]
                
                if(is.numeric(num)) {
                        hostpitalNames <- myOrder[num,]["Hospital.Name"]
                        
                } else if(num == "best") {
                        hostpitalNames <- myOrder[1,]["Hospital.Name"]
                        
                } else if(num == "worst") {
                        hostpitalNames <- myOrder[nrow(myOrder),]["Hospital.Name"]
                }
        }
        hostpitalNames
}





































