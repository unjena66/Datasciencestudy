이 함수는 각 state, outcome, rank(num)을 지정해주면 각 주별로 해당 outcome에서 해당 rank의 좋은 결과를 보이는 병원 이름을 뽑아내라는 것. 간단하게 전략을 소개하자면, 전체 데이터를 각 주별로 나눈후, 각 주를 outcome이 좋은 순서대로 나열한 후, 각 주에서 rank에 맞는 순서를 뽑아서 hospital.name을 읽어오자는 것

함수는 다음과 같다.
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
        
여기까지는 이전의 함수와 같다. 

num이라는 새로운 변수가 추가되었기 때문에, num이 valid한지도 봐야 함. 문제에서 직접적으로 validation하라는 이야기는 없었으나, num이 실제 매길 수 있는 랭킹 수를 넘어가는 경우에는 NA를 반환하라는 것이 validation을 하라는 이야기. 
        
그래서 state의 outcome데이터를 세었다. NA가 아닌 것만. NA까지 포함해서 세려면  nrow등도 사용할 수 있겠으나 NA를 제끼느라 is.na함수와 sum을 사용하였다. sum을 사용하면 true들만 1로 치고 합산을 한다.

        lastrank <- sum(!is.na(stateData[[outcome.real]]))

num 값을 best나 worst로 넣을 수 있도록 하였으므로 다음과 같이 변환해준다. 위에서 서 세었던 ourcome데이터의 갯수는 즉 가장 꼴찌의 순위와 같으므로 worst를 찾을 때 num에 넣어준다. outcome데이터 갯수보다 많은 수를 num에 넣어주면 NA.

        if(num == "best"){num <- 1}
        else if(num == "worst"){num <-lastrank}
        else if(num > lastrank){return(NA)}
        
그리고 데이터를 outcome순서대로 정렬한다. 여기서 []을 현란하게 쓰니 잘 보자. order라는 함수는 argument로 들어간 벡터를 내림차순으로 정렬하는 역할을 한다. vector를 여러개 넣을 수 있는데 그러면 첫번쨰 벡터를 정렬한 후 tie를 이루는 것들에 대해서만 두번째를 적용하여 순서를 정렬한다. 여기서 사용된 것을 보면 outcome에 따라 일단 정렬하고, outcome상 결과가 같은것들은 hospital.name의 알파벳순으로 정렬. 이렇게 정렬한 것으로 subset을 시키면 이 순서에 따라 정렬이 됨. subset할때 []내부에 ,가 어디에 찍혀있는지 자세히 보자.

        ordered <- stateData[order(stateData[[outcome.real]], stateData$Hospital.Name),]
        
정렬도 다 되었으니, 순서에 맞는 hospital.name만 뽑아내면 됨.
        rankHosp <- ordered$Hospital.Name[[num]]

        rankHosp
}
