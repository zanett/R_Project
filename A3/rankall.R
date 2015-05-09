
rankall<- function(outcome, num="best") {
  setwd("D:/R/R_PROG_PA3")
  read_data = read.csv("outcome-of-care-measures.csv", colClasses="character")
  State = read_data[,"State"]
  Hospital.Name = read_data[,"Hospital.Name"]
  
  read_data[,11] = as.numeric(read_data[,11])
  Heart.Attack=read_data[,11]
  
  read_data[,17] = as.numeric(read_data[,17])
  Heart.Failure=read_data[,17]
  
  read_data[,23] = as.numeric(read_data[,23])
  Pneumonia=read_data[,23]
  
  my_data = data.frame(State,Heart.Attack,Heart.Failure,Pneumonia,Hospital.Name)
  
  if(outcome=="heart attack") {
    sorted_my_data = my_data[order(State,Heart.Attack,Hospital.Name),]
  }else if(outcome=="heart failure") {
    my_data$Heart.Attack<- NULL
    sorted_my_data = my_data[order(State,Heart.Failure,Hospital.Name),]
  }else if(outcome=="pneumonia"){
    my_data$Heart.Attack<- NULL
    my_data$Heart.Failure<- NULL
    sorted_my_data = my_data[order(State,Pneumonia,Hospital.Name),]
  }else {
    stop('invalid outcome')
  }
  
  sorted_my_data = sorted_my_data[complete.cases(sorted_my_data),]
  split_data = split(sorted_my_data,sorted_my_data$State)
  
  hospital=rep(NA,54)
  state=rep(NA,54)
  
  for(i in 1:54) {
    state[i]=as.character(split_data[[i]][1,1])
  }
  
  if(num=="best") {
    for(i in 1:54) {
      hospital[i]=as.character(split_data[[i]][1,"Hospital.Name"])
    }
    } else if(num=="worst") {
      for(i in 1:54){
        worstRow = max(split_data[[i]][,2])
        worstHospital = as.character(split_data[[i]][which(split_data[[i]][,2]==worstRow),"Hospital.Name"])
        hospital[i]=worstHospital
      }
    } else {
      num=as.integer(num)
      for(i in 1:54){
        if(typeof(num)=="integer" && nrow(split_data[[i]])>=num){
          hospital[i]=as.character(split_data[[i]][num,"Hospital.Name"])
        } else{
          NA
        }
      }
    }
  
  rankall_data = data.frame(hospital,state)
  return(rankall_data)
}