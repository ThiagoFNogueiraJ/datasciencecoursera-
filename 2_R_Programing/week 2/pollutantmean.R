pollutantmean <- function(directory = getwd(), pollutant, id = 1:332) {
  pdata <- data.frame(Date = "", sulfate = "", nitrate = "", ID = "")
  id <- as.numeric(id)
  for (i  in 1:length(id)) {
    if (id[i]<10) {
      x <- paste("00", id[i], sep = "")
    } 
    if (id[i]>= 10 & id[i] < 100 ) {
      x <- paste("0", id[i], sep = "")
    }
    if (id[i] >= 100) {
      x <- paste(id[i], sep = "")
    }
    filedir <- paste(directory,x,".csv", sep='') 
    sdata <- read.csv(filedir, sep = ",")
    pdata <- rbind(pdata, sdata)
  }
  pdata[, pollutant] <- as.numeric(pdata[, pollutant])
  good<- complete.cases(pdata)
  mean(pdata[good, pollutant])
}


