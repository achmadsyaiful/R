setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
data.wine<-read.csv("white_wine3.csv")

#melihatnama-namakolompadadata.wine
names(data.wine)
#membuatkelasbaru
#yang skorquality lebihdari6 dikelaskanmenjadikelas=1
#yang skorquality tidaklebihdari6 dikelaskanmenjadikelas=0
quality <-as.factor(ifelse(data.wine$quality>6,1,0))
alcohol <-data.wine$alcohol
density <-data.wine$density
plot (density, alcohol,
      cex=ifelse(quality == 1, 1, 0.3),
      col=ifelse(quality == 1, 3, 6), pch=ifelse(quality == 1, 6, 3))
library(MASS)
ldafit<-lda(quality ~ density + alcohol)
ldafit
plot(ldafit)
prediksi<-predict(ldafit,data.wine)
prediksi
class(prediksi)
library(caret)
prediksi$class
quality
confusionMatrix(as.matrix(prediksi$class),as.matrix(quality))