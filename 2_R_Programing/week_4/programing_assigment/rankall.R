rankall<- function(outcome, num = "best"){
  pdata <- read.csv("2_R_Programing/week_4/programing_assigment/data/outcome-of-care-measures.csv", sep = ",")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  names(pdata)[c(11,17,23)] <- outcomes
  pdata[pdata == "Not Available"] <- NA
  pdata[,11] <- as.numeric(pdata[,11])
  pdata[,17] <- as.numeric(pdata[,17])
  pdata[,23] <- as.numeric(pdata[,23])
  
  
  if (outcome %in% outcomes == FALSE){stop("invalid outcome")}
  
  
  filtro <- function(subset, snum) {
    subset <- subset[order(subset[,2], subset[,1]),]
    if (snum == "best") {
      all_best <- subset[1,1]
    } 
    if (snum == "worst") {
      all_best <- tail(sdata[which(!is.na(sdata[,2])),],1)[1,1]
    } 
    if (is.numeric(snum)) {
      all_best <- sdata[snum, 1]
    }
    all_best
  }
  
  states <- unique(sort(pdata$State))
  result <- data.frame(hosp.nome = "", sat = states)
  for (st in result$sat) {
    sdata <- subset(pdata, State == st, select = c("Hospital.Name", outcome, "State"))
    sdata <- sdata[order(sdata[,2], sdata[,1]),]
    tlist <- filtro(sdata, num)
    result[result$sat == st, 1] <- tlist
  }
  result
}


head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "HI")$hosp.nome)
subset(r, sat == "NV")$hosp.nome
