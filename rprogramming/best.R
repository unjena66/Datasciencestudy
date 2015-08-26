## the best 30-day mortality for the specified outcome in that state
## outcomes - "heart attack", "heart failure", "pneumonia"

best <- function(state, outcome){
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
        
            
        ## subset the entire dataframe - hospital name, state, mortality rate from HA, 
        ## mortality rate from HF, mortality rate form PN
        subData <- data[,c(2,7,11,17,23)]
        
        for(i in c(3,4,5)){
                subData[,i] <- as.numeric(subData[,i])
        }
        
        ## split states and extract the state
        stateData <- split(subData, subData$State)[[state]]
        bestOutVal <- min(stateData[outcome.real], na.rm = TRUE)
        bestHosp <- stateData[stateData[,outcome.real]==bestOutVal,]$Hospital.Name
        
        ##if extracted states are more than one, order by lexical order
        if(length(bestHosp)>1){
                bestHosp <- sort(bestHosp)[1]
        }
        
        bestHosp
}