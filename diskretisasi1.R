setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
datascoring <-read.csv("datascoring.csv")
head(datascore1)

datascoring$status.bil<-ifelse(datascoring$status== "GOOD", 0, 1)
datascore1<-datascoring[c(2,7)]

library(discretization)
mdlp <-mdlp(datascore1)
mdlp$cutp
mdlp$Disc.data