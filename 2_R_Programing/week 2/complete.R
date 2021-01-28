complete<- function(directory, id = 1:332){
  pdata <- data.frame(id = "", nobs = "")
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
    pdata[i,]$id <- id[i]
    good <- complete.cases(sdata)
    pdata[i, ]$nobs <- sum(good)
  }
  pdata
}


