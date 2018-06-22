library(caret)
library(rpart)
library(randomForest)
library(discretization)
library(rpart.plot)
library(cvTools)

setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017/Tugas/data")

bankloan <- read.csv("bankloan.csv", header=TRUE, sep=",")
head(bankloan)

n<-50
data.accuracy<-cbind(bg=c(1:n),rf=c(1:n),pk=c(1:n))
data.Sensitivity<-cbind(bg=c(1:n),rf=c(1:n),pk=c(1:n))
data.Specificity<-cbind(bg=c(1:n),rf=c(1:n),pk=c(1:n))

for (j in 1:n)
{
  
  folds <- cvFolds(NROW(bankloan), K=10)
  bankloan$pred.bg <- rep(0,nrow(bankloan))
  bankloan$pred.rf <- rep(0,nrow(bankloan))
  bankloan$pred.pk <- rep(0,nrow(bankloan))
  
  #Perform 10 fold cross validation
  for(k in 1:10)
  {
    data.training <- bankloan[folds$subsets[folds$which != k], ] #Set the training set
    data.testing <- bankloan[folds$subsets[folds$which == k], ] #Set the validation set
    
    #Bagging
    t<-100
    prediksi<-matrix(NA,nrow(data.testing),t)
    for(i in 1:t)
    {
      resample <- sample(1:nrow(data.training), replace=TRUE)
      contoh.boot <- data.training[resample,]
      tree <-rpart(default ~ age + ed + employ + address
                   + income + debtinc + creddebt + othdebt, data=contoh.boot, method="class")
      prob <-predict(tree, data.testing)[,2]
      prediksi[,i] <-ifelse(prob<0.5, 0, 1)
    }
    
    vote1 <-apply(prediksi,1,sum)
    prediksi.bg<- ifelse(vote1 < t/2, 0, 1)
    bankloan[folds$subsets[folds$which == k], ]$pred.bg <- prediksi.bg
    
    #Random Forest
    model.forest <- randomForest(as.factor(default) ~ age + ed + employ + address
                                 + income + debtinc + creddebt + othdebt,
                                 data=data.training, importance=TRUE, ntree=2000, mtry=3)
    prediksi.rf <- predict(model.forest, data.testing)
    bankloan[folds$subsets[folds$which == k], ]$pred.rf <- prediksi.rf
    
    #Pohon Klasifikasi
    model.ph <- rpart(default ~ age + ed + employ + address
                      + income + debtinc + creddebt + othdebt,data = data.training, method="class",
                      control = rpart.control(minsplit = 50, cp = 0))
    
    prediksi.pk1<- predict(model.ph, data.testing)
    prediksi.pk <-ifelse(prediksi.pk1[,2] > 0.5, 1, 0)
    bankloan[folds$subsets[folds$which == k], ]$pred.pk <- prediksi.pk
  }
  
  
  data<-confusionMatrix(bankloan$default, bankloan$pred.bg, positive="1")
  test<-data$overall
  test2<-data.frame(test)
  data.accuracy[j,1]<-test2[1,1]
  test<-data$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,1]<-test2[1,1]
  data.Specificity[j,1]<-test2[2,1]
  
  bankloan$pred.rf1<-ifelse(bankloan$pred.rf==2,1,0)
  data<-confusionMatrix(bankloan$default, bankloan$pred.rf1, positive="1")
  test<-data$overall
  test2<-data.frame(test)
  data.accuracy[j,2]<-test2[1,1]
  test<-data$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,2]<-test2[1,1]
  data.Specificity[j,2]<-test2[2,1]
  
  data<-confusionMatrix(bankloan$default, bankloan$pred.pk, positive="1")
  test<-data$overall
  test2<-data.frame(test)
  data.accuracy[j,3]<-test2[1,1]
  test<-data$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,3]<-test2[1,1]
  data.Specificity[j,3]<-test2[2,1]
}

data.accuracy
data.Sensitivity
data.Specificity