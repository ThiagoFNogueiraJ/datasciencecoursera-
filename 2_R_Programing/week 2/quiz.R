#5
class(x<- c(4,"a", TRUE))

#9
x<- 1:4;y<-2:3
class(x+y)

#11
dataset_2 <- read.csv("2_R_Programing/data/hw1_data.csv", sep = ",")
colnames(dataset_2)

#12
dataset_2[1:2,]

#13
dim(dataset_2)

#14
dataset_2[152:153,]

#15
dataset_2[47,]

#16
 nteste <- is.na(dataset_2$Ozone) 
nteste[nteste  == TRUE]

#17
good<- complete.cases(dataset_2)
dataset_3 <- dataset_2[good, ]
mean(dataset_3$Ozone)

#18
subset1 <- dataset_3[dataset_3$Ozone > 31,]
subset1 <- subset1[subset1$Temp >90,]
mean(subset1$Solar.R)

#19
subset2 <- dataset_3[dataset_3$Month == 6,]
mean(subset2$Temp)

#20
max(dataset_3[dataset_3$Month ==5,]$Ozone)


