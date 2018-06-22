
setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
data <-read.csv("disk01.csv")
head(data)
summary(data)
dim(data)

acak <- sample(1:nrow(data), 1146)
data.training <- data[acak,]
data.testing <- data[-acak,]


model.logistik<-glm(class~ x,
                    data=data.training, family="binomial")
summary(model.logistik)

prob.prediksi<-predict(model.logistik, data.testing, type="response")
prediksi<-ifelse(prob.prediksi>0.5, 1, 0)
library(caret)
confusionMatrix(prediksi, data.testing$class)

library(woe)
woe<-woe(Data=data.training,"x",TRUE,"class",7,Bad=0,Good=1)
woe
data.training$x_bin <- ifelse(data.training$x < 27, 110.7, 
                              ifelse(data.training$x < 35, -62.3, 
                              ifelse(data.training$x < 44, -189.7, 
                              ifelse(data.training$x < 51, -97.7, 
                              ifelse(data.training$x < 59, 49.5,238.4))))) 

data.testing$x_bin <- ifelse(data.testing$x < 27, 110.7, 
                       ifelse(data.testing$x < 35, -62.3, 
                       ifelse(data.testing$x < 44, -189.7, 
                       ifelse(data.testing$x < 51, -97.7, 
                       ifelse(data.testing$x < 59, 49.5,238.4)))))


model.logistik<-glm(class~ x_bin,
                    data=data.training, family="binomial")
summary(model.logistik)

prob.prediksi<-predict(model.logistik, data.testing, type="response")
prediksi<-ifelse(prob.prediksi>0.5, 1, 0)
library(caret)
confusionMatrix(prediksi, data.testing$class)