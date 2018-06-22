#Baca Data
setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
datacluster <-read.csv("white_wine2.csv")
PredCluster <- datacluster[,c(8,11,12)]
PredCluster$QualityClass <-ifelse(datacluster$quality>6,1,2)
PredCluster

plot(PredCluster$density, PredCluster$alcohol, col=PredCluster$QualityClass)
plot(PredCluster$density, PredCluster$alcohol, col=PredCluster$QualityClass,
     pch=ifelse(PredCluster$QualityClass>1,17,12))
points(x=15, y=19, pch = 13, cex=1.5)

aggregate.data.frame(PredCluster[,c(1,2)], by=list(cluster=PredCluster$QualityClass), mean)
summary.data.frame(PredCluster[,c(1,2)])
PredCluster$std_density<-with(PredCluster,(density-0.9872)/(1.0056-0.9872))
PredCluster$std_alcohol<-with(PredCluster,(alcohol-8)/(14.2-8))

plot(PredCluster$std_density, PredCluster$std_alcohol, col=PredCluster$QualityClass)
plot(PredCluster$std_density, PredCluster$std_alcohol, col=PredCluster$QualityClass,
     pch=ifelse(PredCluster$QualityClass>1,17,12))
points(x=15, y=19, pch = 13, cex=1.5)

#Batas testing
m <-NULL
a <-seq(0, 1, by = 0.005)
b <-seq(0, 1, by = 0.005)
for (i in a){
  for (j in b) {
    m <-rbind(m, c(i, j))
  }
}

m

training <-PredCluster[,5:6]
kelas <-as.factor(PredCluster[,4])

library(class)
prediksi<-knn(training, m, kelas, k = 15)
points(m[,1], m[,2],
       col=ifelse(prediksi=="1",
                  "cyan","yellow"),
       pch=ifelse(prediksi=="2",17,12))
plot(PredCluster$density, PredCluster$alcohol, col=PredCluster$QualityClass)
