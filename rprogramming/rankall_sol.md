이 함수는 outcome과 rank(num)을 넣어주면 각 주에서 해당 outcome의 num번째 랭킹에 있는 병원을 뽑아서 보여주겠다는 것. 간단하게 전략을 소개하자면, 전체 데이터를 각 주별로 나눈 후, 각 주에서 outcome이 좋은 순서대로 나열한후 각 주에서 rank에 맞는 순서를 뽑아내어 dataframe으로 만들어 반환. 복잡한만큼 다양한 방식이 있을수 있는데 내방식은 다음과 같다.

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
여기까지 이전의 함수와 같다. 다음부터가 좀 다르다. 앞으로 나올 설명을 위해 여기를 '분기점'이라고 표현하자'
    
rankhospital함수에서는
    stateData <- split(subData, subData$State)[[state]]
라고 했었다. 그런데 여기서는 split이후에 subset을 안만든다. 왜냐면...각 주별로 값을 뽑아야 하니까.  
        stateData <- split(subData, subData$State)
여기서 '값을 뽑는다'는 행위를 split으로 나눈 모든 주에 하려면 반복작업을 해야 한다. 반복작업이라고 하면 떠오르는 명령어.. 바로for문이다. 그런데 for문보다는 lapply가 간지가 난다. 그래서 나는 lapply를 쓰련다.
그런데 막상  lapply를 쓰려니 '값을 뽑는다'라는 작업을 할만한 함수가 없다. 그래서 나는 함수를 만든다.
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
        
함수의 형태는 rankhospital에 나온 일부분과 거의 같다. num을 받아서 각 주별로 체크하려니 num의 체크포인트가 이 함수 안으로 들어가게 되었고, 이후 outcome에 따라 정렬하고 해당 num의 랭킹에 있는 병원이름을 뽑아내는 것이 이 함수의 역할

그래서, 해당 outcome의 해당 num에 랭킹된 병원들의 list를 아래와 같이 뽑게 된다. lapply의 반환값은 무조건 list라는 것 잊지말자.
        hospital <- lapply(stateData, rankHosp, outcomeReal = outcome.real, rankNum = num)
그러면 이 리스트는 각 state이름을 element들의 이름으로 가진 리스트가 된다. 이를 이용하여 각 hospital들의 소속 state를 뽑아낸다.
        state <- names(hospital)
둘을 cbind로 붙이면 matrix가 되고, 이를 결과에서 요구하는 data.frame으로 변환하면 끝!
        rankallData <- data.frame(cbind(hospital, state))
        
        rankallData
    }

그런데, for문으로 다시 만들어보는 것도 추천한다. 앞에 분기점에서 for문을 사용하여, subData를 split해내는 것부터 병원 이름을 뽑아내는 것을 한 구간으로 해서 전체를 for문으로 돌리면 그때 그때 state별로 병원 이름이 뽑아진다. 유의할 것은 loop마다 뽑힌 병원이름과 해당병원 소속 state이름을 저장할 그릇은 for문 밖에 있어야 한다는 것. 애초에 data.frame을 그릇으로 만들어 한줄한줄 붙여나가든가, 병원이름용과 state용 그릇을 따로 만들어서 각각 채운후 cbind로 붙이든가 하는 방법이 있을 것.
