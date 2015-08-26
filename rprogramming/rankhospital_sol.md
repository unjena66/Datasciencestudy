이 함수의 전략은...
spital name ranked in 'num' in terms of 'outcome' in the 'state'
## ranked by order of outcome(lowest mortality rate is the best)
## 'num' can take 'best' and 'worst' as well as integer
## if 'num' is larer than given states number, return NA
## NA value should be excluded when deciding ranking
## when several hopitals tie in the mortality rate, order by lexical order

rankhospital <- function(state, outcome, num = "best"){
        ## Read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## check if outcome is valid AND translate outcome into real value
        if(outcome == "heart attack"){
                outcome.real <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }else if(outcome == "heart failure"){
                outcome.real <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }else if(outcome == "pneumonia"){
                outcome.real <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }else{
                stop("invalid outcome")
        }
        
        ## check if state is valid
        stateSet <- unique(data$State)
        stateValid <- is.element(state, stateSet)
        
        if(stateValid != TRUE){
                stop("invalid state")
        }
        
        ## return hospital name in that state with the given rank
        ## 30-day mortality rate
        
        ## subset the dataframe - hospital name, state, mortality rate from HA, 
        ## mortality rate from HF, mortality rate form PN
        subData <- data[,c(2,7,11,17,23)]
        
        for(i in c(3,4,5)){
                subData[,i] <- as.numeric(subData[,i])
        }
        
        ## extract the state data 
        stateData <- split(subData, subData$State)[[state]]
        
        ## check if num is valid
        lastrank <- sum(!is.na(stateData[[outcome.real]]))
        if(num == "best"){num <- 1}
        else if(num == "worst"){num <-lastrank}
        else if(num > lastrank){return(NA)}
        
        ## order stateData by outcome order
        ordered <- stateData[order(stateData[[outcome.real]], stateData$Hospital.Name),]
        
        ## extract hospital name in the 'num' rank
        rankHosp <- ordered$Hospital.Name[[num]]

        rankHosp
}
