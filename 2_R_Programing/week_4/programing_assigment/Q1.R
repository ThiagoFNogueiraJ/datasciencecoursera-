outcome <- read.csv("week_4/programing_assigment/data/outcome-of-care-measures.csv", sep = ",")
head(outcome)
str(outcome)
summary(outcome)
names(outcome)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

sdata <- outcome[outcome$State == state, c(2,11)]
all_best <- which(sdata[,2] == min(sdata[,2], na.rm = TRUE))
all_four <- which(outcome[,11] == 14)
sdata[all_best,1]
outcome[all_four,2]      
outcome[,7]


pdata <- read.csv("2_R_Programing/week_4/programing_assigment/data/outcome-of-care-measures.csv", sep = ",")
state <- "MN"
num  <- 5000
outcome <- "heart failure"
sdata<- subset(pdata, State == state, select = c(2,11, 17,23))
dim(sdata)
if (is.numeric(num) & num > length(unique(sdata$Hospital.Name))) {stop(NA)}
if (outcome == "heart attack"){
  sdata[,2] <- as.numeric(sdata[,2])
  if (num == "best") {
    all_best <- which(sdata[,2] == min(sdata[,2], na.rm = TRUE))
  } 
  if (num == "worst") {
    all_best <- which(sdata[,2] == max(sdata[,2], na.rm = TRUE))
  } 
  if (is.integer(num) == TRUE) {
    all_best <- which(sdata[,2] == sort(sdata[,2], na.last =TRUE)[num])
  }
  best_h <- sdata[all_best, 1]
} else  print("não é heart attack")   
if (outcome == "heart failure"){
  sdata[,3] <- as.numeric(sdata[,3])
  if (num == "best") {
    all_best <- which(sdata[,3] == min(sdata[,3], na.rm = TRUE))
  } 
  if (num == "worst") {
    all_best <- which(sdata[,3] == max(sdata[,3], na.rm = TRUE))
  } 
  if (is.integer(num) == TRUE) {
    all_best <- which(sdata[,3] == order(sdata, c(3,2))[num])
  }
  best_h <- sdata[all_best, 1]
} else  print("não é heart failure")   
if (outcome == "pneumonia"){
  sdata[,4] <- as.numeric(sdata[,4])
  if (num == "best") {
    all_best <- which(sdata[,4] == min(sdata[,4], na.rm = TRUE))
  } 
  if (num == "worst") {
    all_best <- which(sdata[,4] == max(sdata[,4], na.rm = TRUE))
  } 
  if (is.integer(num) == TRUE) {
    all_best <- which(sdata[,4] == sort(sdata[,4], na.last =TRUE)[num])
  }
  best_h <- sdata[all_best, 1]
} 
best_h

s