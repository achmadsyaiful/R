library(caret)
library(rpart)
library(randomForest)
library(discretization)
library(rpart.plot)
library(cvTools)

setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017/Tugas/data")

data.tree <- read.csv("data tree.csv", header=TRUE, sep=",")
head(data.tree)
dim(data.tree)
n<-10
data.accuracy<-cbind(bg=c(1:n),rf=c(1:n),pk=c(1:n))
data.Sensitivity<-cbind(bg=c(1:n),rf=c(1:n),pk=c(1:n))
data.Specificity<-cbind(bg=c(1:n),rf=c(1:n),pk=c(1:n))

for (j in 1:n)
{
  
  folds <- cvFolds(NROW(data.tree), K=10)
  data.tree$pred.bg <- rep(0,nrow(data.tree))
  data.tree$pred.rf <- rep(0,nrow(data.tree))
  data.tree$pred.pk <- rep(0,nrow(data.tree))
  
  #Perform 10 fold cross validation
  for(k in 1:10)
 {
    data.training <- data.tree[folds$subsets[folds$which != k], ] #Set the training set
    data.testing <- data.tree[folds$subsets[folds$which == k], ] #Set the validation set
    
  #Bagging
  t<-100
  prediksi<-matrix(NA,nrow(data.testing),t)
  for(i in 1:t)
  {
    resample <- sample(1:nrow(data.training), replace=TRUE)
    contoh.boot <- data.training[resample,]
    tree <-rpart(Tertarik.Beli~Jenis.Kelamin+Single+Tinggal.di.Kota+usia+Perokok
                 +Budget+Kesukaan, data=contoh.boot, method="class")
    prob <-predict(tree, data.testing)[,2]
    prediksi[,i] <-ifelse(prob<0.5, 0, 1)
  }
  
  vote1 <-apply(prediksi,1,sum)
  prediksi.bg<- ifelse(vote1 < t/2, 0, 1)
  data.tree[folds$subsets[folds$which == k], ]$pred.bg <- prediksi.bg
  
  #Random Forest
  model.forest <- randomForest(as.factor(Tertarik.Beli) ~ Jenis.Kelamin+Single+Tinggal.di.Kota+usia+Perokok
                               +Budget+Kesukaan,
                               data=data.training, importance=TRUE, ntree=2000, mtry=3)
  prediksi.rf <- predict(model.forest, data.testing)
  data.tree[folds$subsets[folds$which == k], ]$pred.rf <- prediksi.rf
  
  #Pohon Klasifikasi
  model.ph <- rpart(Tertarik.Beli~Jenis.Kelamin+Single+Tinggal.di.Kota+usia+Perokok
                     +Budget+Kesukaan,data = data.training, method="class",
                    control = rpart.control(minsplit = 50, cp = 0))
  
  prediksi.pk1<- predict(model.ph, data.testing)
  prediksi.pk <-ifelse(prediksi.pk1[,2] > 0.5, 1, 0)
  data.tree[folds$subsets[folds$which == k], ]$pred.pk <- prediksi.pk
  }
  
  
  data<-confusionMatrix(data.tree$Tertarik.Beli, data.tree$pred.bg, positive="1")
  test<-data$overall
  test2<-data.frame(test)
  data.accuracy[j,1]<-test2[1,1]
  test<-data$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,1]<-test2[1,1]
  data.Specificity[j,1]<-test2[2,1]
  
  data.tree$pred.rf1<-ifelse(data.tree$pred.rf==2,1,0)
  data<-confusionMatrix(data.tree$Tertarik.Beli, data.tree$pred.rf1, positive="1")
  test<-data$overall
  test2<-data.frame(test)
  data.accuracy[j,2]<-test2[1,1]
  test<-data$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,2]<-test2[1,1]
  data.Specificity[j,2]<-test2[2,1]
  
  data<-confusionMatrix(data.tree$Tertarik.Beli, data.tree$pred.pk, positive="1")
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