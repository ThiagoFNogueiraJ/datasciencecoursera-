corr <- function(directory, threshold = 0) {
  source("complete.R")
  all_monitors <- complete(directory)
  all_monitors$nobs <- as.numeric(all_monitors$nobs)
  id <- as.numeric(all_monitors[all_monitors$nobs  > threshold, ]$id)
  if (length(id) < 1) {
    return(id)
  }
  csn <- c()
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
    good <- complete.cases(sdata)
    csn[i] <- cor(sdata[good,]$sulfate,sdata[good,]$nitrate)
  }
  csn
}
