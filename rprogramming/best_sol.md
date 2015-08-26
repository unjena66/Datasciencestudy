    
    best <- function(state, outcome) {

        ## Read outcome data 
        ## Check that state and outcome are valid 
        ## Return hospital name in that state with lowest 30-day death rate
    }

위와 같이 함수의 구조가 제시됨. 하나씩 살펴보자면, 처음에 파일로부터 데이터를 불러옴

    best <- function(state, outcome){

        ## colClasses로 character를 지정하게 되는데 이는 read.csv로불러오면 각 column이 factor로 지정되기 때문. 
        ##모든 data type은 character로 변환될 수 있기 때문에 character로 불러오는 것이 가장 안전.

    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

각 변수에 대한 밸리데이션 진행. outcome부터 여기서 하는데, 편의상 validation과 함께 outcome에 대한 value transformation(?)도 같이 진행함.

    if(outcome == "heart attack"){
	outcome.real <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }else if(outcome == "heart failure"){
                outcome.real <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }else if(outcome == "pneumonia"){
                outcome.real <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }else{

        ## 위 세개 중 하나가 아니면 outcome변수로는 의미가 없다.
                stop("invalid outcome")
        }
      
state변수에 대한 validation도 진행. 미국에 몇개주가 있는지 모르기 때문에 state column에 있는 값들을 뽑아서 이중에 없으면 state가 아닌걸로...;; 여기서 사용된 unique함수는 복수의 중복된 값들을 하나로 만들어 원래 데이터타입 그대로 만들어주는함수. is.element함수는 내가 입력한 값이 정해진 범위 내에 들어있는지 보고자 하는 것으로, 포함되어 있으면 TRUE, 안포함되어있으면 FALSE를 반환. 그래서 여기서는 해당 값이 안들어가있으면 stop하도록 함.

    stateSet <- unique(data$State)
    stateValid <- is.element(state, stateSet)
       
    if(stateValid != TRUE){
            stop("invalid state")
    }
             
대략 변수에 대해 정리가 되었으니 원본 데이터를 정리하고자 함. 원본데이터는 본 함수에서 의도하는 바에 비해 쓸데없이 칼럼도 많고 로우도 많으니까 원하는 데이터만 정리해봅시다.
	아까 원본 데이터를 불러왔을 때 모든 칼럼은 character로 불러왔기 때문에 이를 숫자로 변환하고자 함. 어차피 숫자는 이후 랭킹 정할 때 사용할 것이므로 character라도 랭킹정하는 데는 문제가 없을 것이나...그냥 안전하게 바꾸어줬음.        

    for(i in c(3,4,5)){
        subData[,i] <- as.numeric(subData[,i])
    }
      
데이터를 간략하게 정리하는 작업은 여기서 이루어짐. split함수로 변수에서 입력한 state에 해당하는 row들만 남기고자 하는 것. split함수를 통해 각 element가 data.frame으로 구성된 list가 반환되고, 뒤에 [[state]]를 붙여서 원하는 state에 해당하는 data.frame만 뽑아내서 결과적으로 원하는 state해당하는 데이터셋만 뽑아짐.

    stateData <- split(subData, subData$State)[[state]]

뽑아낸 데이터셋에서 가장 낮은 mortality를 보이는 row에서 hospital.name을 뽑아내야하는데, 여기서는 일단 가장 낮은  mortality값을 찾고, 이 mortality값에 해당하는row를 뽑아서 그 row의 hospital.name을 뽑는 것이 전략.(비효율적인 전략인것 같은데...다른 방법이 있을 듯) 그래서 일단 여기서는 min함수를 써서 가장 낮은 mortality값을 찾아냄.

    bestOutVal <- min(stateData[outcome.real], na.rm = TRUE)
	
여기가 좀 복잡할 수 있음. 위에서 뽑아낸 min값인 bestOutVal에 해당하는 row를 뽑고자 하는 작업. stateData[, outcome.real]==bestOutVal을 통해서는 원하는 outcome값에 해당하는 column의 각 값이 bestOUtVal과 일치하는지 보고 각각에 맞는 TRUE/FALSE값이 반환되어 TRUE와 FALSE로 이루어진 data.frame이 반환될 것.stateData를 이렇게 얻어낸 dataframe으로 subset을 하면 전체 row에서 TRUE에 해당하는, 즉bestoutval값을 갖고 있는 row만 반환될 것. 여기서 subset만들때 ,를 찍어주었다는 것을 잊지 말 것. 이렇게 얻어낸 row에서 hospital name만 얻어내면 끝! 

    bestHosp <- stateData[stateData[,outcome.real]==bestOutVal,]$Hospital.Name

사실은 끝이 아니다. hospital name으로 여러개가 반환된 경우 처리르 해줘야 하는 것. sort함수는 알파벳순서에 따라 자동으로 배열해주는 함수. 여러개가 아닌 경우에는 그냥 반환해주면 됨.
    if(length(bestHosp)>1){
        bestHosp <- sort(bestHosp)[1]
    }
        
        bestHosp
}

