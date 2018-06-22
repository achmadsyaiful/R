library(caret)
library(discretization)
library(rpart)
library(rpart.plot)

setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
wine <- read.csv("white_wine3.csv")
colnames(wine)

set.seed(1988)
data <- wine[,c("alcohol", "density", "quality")]
data$kelas.kualitas <- ifelse(data$quality > 6, 1, 0)

head(data)
dim(data)


acak <- sample(1:nrow(data), 3402)
data.training <- data[acak,]
data.testing <- data[-acak,]

#original
model.logistik<-glm(kelas.kualitas~ alcohol+density,
                    data=data.training, family="binomial")
summary(model.logistik)


prob.prediksi<-predict(model.logistik, data.testing, type="response")
prediksi<-ifelse(prob.prediksi>0.5, 1, 0)
confusionMatrix(prediksi, data.testing$kelas.kualitas)

library(woe)
woe<-woe(Data=data.training,"alcohol",TRUE,"kelas.kualitas",10,Bad=0,Good=1)
woe
data.training$alcohol_woe <-  ifelse(data.training$alcohol < 9.1, -47.8, 
                        ifelse(data.training$alcohol < 9.4, -152.3, 
                        ifelse(data.training$alcohol < 9.7, -181.4, 
                        ifelse(data.training$alcohol < 10.1, -123.8, 
                        ifelse(data.training$alcohol < 10.6, -69.3,
                        ifelse(data.training$alcohol < 11.0, 12.2,
                        ifelse(data.training$alcohol < 11.5, 37.5,
                        ifelse(data.training$alcohol < 12.3,  97.8,149.7))))))))

data.testing$alcohol_woe <-  ifelse(data.testing$alcohol < 9.1, -47.8, 
                              ifelse(data.testing$alcohol < 9.4, -152.3, 
                              ifelse(data.testing$alcohol < 9.7, -181.4, 
                              ifelse(data.testing$alcohol < 10.1, -123.8, 
                              ifelse(data.testing$alcohol < 10.6, -69.3,
                              ifelse(data.testing$alcohol < 11.0, 12.2,
                              ifelse(data.testing$alcohol < 11.5, 37.5,
                              ifelse(data.testing$alcohol < 12.3,  97.8,149.7))))))))

woe<-woe(Data=data.training,"density",TRUE,"kelas.kualitas",4,Bad=0,Good=1)
woe
data.training$density_woe <-  ifelse(data.training$density < 0.99362, -320.2, 
                              ifelse(data.training$density < 0.99628,  23.3,78.8))

data.testing$density_woe <-  ifelse(data.testing$density < 0.99362, -320.2, 
                              ifelse(data.testing$density < 0.99628,  23.3,78.8))



model.logistik<-glm(kelas.kualitas~ alcohol_woe+density_woe,
                    data=data.training, family="binomial")
summary(model.logistik)


prob.prediksi<-predict(model.logistik, data.testing, type="response")
prediksi<-ifelse(prob.prediksi>0.5, 1, 0)
confusionMatrix(prediksi, data.testing$kelas.kualitas)
