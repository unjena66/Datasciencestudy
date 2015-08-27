## return 2-column dataframe - hospital/state of specified ranking
rankall <- function(outcome, num = "best"){
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
        
        ## for each state, find the hospital of the given ranking
        
        ## subset the dataframe - hospital name, state, mortality rate from HA, 
        ## mortality rate from HF, mortality rate form PN
        subData <- data[,c(2,7,11,17,23)]
        
        for(i in c(3,4,5)){
                subData[,i] <- as.numeric(subData[,i])
        }
        
        ## split the data into each state data 
        stateData <- split(subData, subData$State)
        
        ## build the function to extract the hospital fo the given ranking
        rankHosp <- function(data, outcomeReal, rankNum){     #data is a data.frame argument
                
                # check if rankNum is valid
                lastrank <- sum(!is.na(data[[outcomeReal]]))
                if(rankNum == "best"){rankNum <- 1}
                else if(rankNum == "worst"){rankNum <-lastrank}
                else if(rankNum > lastrank){return(NA)}
                
                # order data by outcome order and lexical order of the hospital name
                rankData <- data[order(data[[outcomeReal]], data$Hospital.Name),]
                
                rankData$Hospital.Name[[rankNum]]
        }
        
        ## apply rankHosp function to all state data
        hospital <- lapply(stateData, rankHosp, outcomeReal = outcome.real, rankNum = num)
        state <- names(hospital)
        rankallData <- data.frame(cbind(hospital, state))
        
        rankallData
}