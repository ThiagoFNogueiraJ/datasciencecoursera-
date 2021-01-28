best <- function(state, outcome){
  pdata <- read.csv("2_R_Programing/week_4/programing_assigment/data/outcome-of-care-measures.csv", sep = ",")
  if (state %in% unique(pdata$State) == FALSE){
    stop("invalid state")
  }
  if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE){
    stop("invalid state")
  }
  sdata<- subset(pdata, State == state, c(2, 11,17,23))
  if (outcome == "heart attack"){
    sdata[,2] <- as.numeric(sdata[,2])
    all_best <- which(sdata[,2] == min(sdata[,2], na.rm = TRUE))
    best_h <- sdata[all_best, 1]
  } 
  if (outcome == "heart failure"){
    sdata[,3] <- as.numeric(sdata[,3])
    all_best <- which(sdata[,3] == min(sdata[,3], na.rm = TRUE))
    best_h <- sdata[all_best, 1]
  } 
  if (outcome == "pneumonia"){
    sdata[,4] <- as.numeric(sdata[,4])
    all_best <- which(sdata[,4] == min(sdata[,4], na.rm = TRUE))
    best_h <- sdata[all_best, 1]
  } 
if(length(best_h > 1)){
  best_h <- sort(best_h, decreasing = FALSE)
  best_h <- best_h[1]
}
best_h
}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
