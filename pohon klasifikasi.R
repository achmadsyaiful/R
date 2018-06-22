library(caret)
library(discretization)
library(rpart)
library(rpart.plot)

setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
data <-read.csv("data tree.csv")
data$tertarik<-factor(data$Tertarik.Beli., levels = c(0, 1), labels=c("tidak", "tertarik"))
data$jk<-factor(data$Jenis.Kelamin, levels=c(0,1), labels=c("p", "l"))
data$tempattinggal<-factor(data$Tinggal.di.Kota, levels = c(0,1), labels = c("desa", "kota"))
data$single<-factor(data$Single,levels= c(0,1), labels = c("Menikah", "Single"))
data$merokok<-factor(data$Perokok, levels = 0:1, labels = c("Tidak", "Ya"))

library(discretization)
entropy_total<-ent(data$tertarik)
entropy_lakilaki<-ent(data$tertarik[data$jk== "l"])
entropy_perempuan<-ent(data$tertarik[data$jk== "p"])
IG_jk<-entropy_total-length(data$tertarik[data$jk== "l"])*entropy_lakilaki/ nrow(data) -length(data$tertarik[data$jk== "p"])*entropy_perempuan/ nrow(data)
IG_jk

entropy_merokok<-ent(data$tertarik[data$merokok== "Ya"])
entropy_tidakmerokok<-ent(data$tertarik[data$merokok== "Tidak"])
IG_merokok<-entropy_total-length(data$tertarik[data$merokok== "Ya"])*entropy_merokok/ nrow(data) -length(data$tertarik[data$merokok== "Tidak"])*entropy_tidakmerokok/ nrow(data)

IG_merokok
head(data)
entropy_single<-ent(data$tertarik[data$single== "Single"])
entropy_menikah<-ent(data$tertarik[data$single== "Menikah"])
IG_single<-entropy_total-length(data$tertarik[data$single== "Single"])*entropy_single/ nrow(data) -length(data$tertarik[data$single== "Menikah"])*entropy_menikah/ nrow(data)
IG_single

library(rpart)
library(rpart.plot)
model = rpart(tertarik~ jk+ tempattinggal+ single + usia+ merokok+ Budget,
              data = data, method="class",
              control = rpart.control(minsplit= 100, cp= 0))
print(model)
rpart.plot(model, extra=4)


model = rpart(tertarik~ jk+ tempattinggal+ single + usia+ merokok+ Budget,
              data = data, method="class",
              control = rpart.control(minsplit= 50, cp= 0))
print(model)
rpart.plot(model, extra=4)


model = rpart(tertarik~ jk+ tempattinggal+ single + usia+ merokok+ Budget,
              data = data, method="class",
              control = rpart.control(minsplit= 30, cp= 0))
print(model)
rpart.plot(model, extra=4)

model = rpart(tertarik~ jk+ kota+ single + usia+ merokok + Budget,
              data = a.data, method="class",
              control = rpart.control(minsplit = 50, cp = 0))
prob_prediksi <-predict(model, newdata=data, type = 'prob')
head(prob_prediksi, n=10)

prediksi<-ifelse(prob_prediksi[,2] > 0.5, "tertarik", "tidak")
table(data$tertarik, prediksi)

library(caret)
confusionMatrix(prediksi, data$tertarik, positive="tertarik")