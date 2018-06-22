setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
wine <-read.csv("white_wine2.csv")
colnames(wine)
data <-wine[,c("alcohol", "density", "quality")]
head(data)
data$kelas.kualitas<-ifelse(data$quality> 6, 1, 0)
model.logistik<-glm(kelas.kualitas~ alcohol+density,
                    data=data, family="binomial")
summary(model.logistik)
prob.prediksi<-predict(model.logistik, data, type="response")
prediksi<-ifelse(prob.prediksi>0.5, 1, 0)
library(caret)
confusionMatrix(prediksi, data$kelas.kualitas)