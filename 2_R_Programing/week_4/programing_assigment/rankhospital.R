rankhospital <- function(state, outcome, num){

  pdata <- read.csv("2_R_Programing/week_4/programing_assigment/data/outcome-of-care-measures.csv", sep = ",")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  names(pdata)[c(11,17,23)] <- outcomes
  pdata[pdata == "Not Available"] <- NA
  pdata[,11] <- as.numeric(pdata[,11])
  pdata[,17] <- as.numeric(pdata[,17])
  pdata[,23] <- as.numeric(pdata[,23])
  
  if (state %in% unique(pdata$State) == FALSE){stop("invalid state")}
  if (outcome %in% outcomes == FALSE){stop("invalid state")}
  if (is.numeric(num) & num > length(unique(sdata$Hospital.Name))) {stop(NA)}
  
  sdata<- subset(pdata, State == state, c("Hospital.Name", outcome))
  sdata <- sdata[order(sdata[,2], sdata[,1]),]
  
  if (num == "best") {
    all_best <- sdata[1,1]
  } 
  if (num == "worst") {
    all_best <- tail(sdata[which(!is.na(sdata[,2])),],1)[1,1]
  } 
  if (is.numeric(num)) {
    all_best <- sdata[num, 1]
  }
all_best
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
