#Baca Data
setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017/Tugas/data")
data <-read.delim("ilustrasiknn.txt")
#datacluster

#data <-read.table("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017/ilustrasiknn.txt", header=TRUE)
plot(data$x1, data$x2, col=data$class)
plot(data$x1, data$x2, col=data$class,
     pch=ifelse(data$class>1,17,12))
points(x=15, y=19, pch = 13, cex=1.5)

training <-data[,1:2]
kelas <-as.factor(data[,3])
maudiprediksi <-c(15,19)
library(class)
prediksi <-knn(training, maudiprediksi, kelas, k = 5)
prediksi

points(x=20, y=19, pch = 13, cex=1.5)
maudiprediksi <-c(20,19)
library(class)
prediksi <-knn(training, maudiprediksi,
               kelas, k = 5)
prediksi

m <-NULL
a <-seq(8, 26, by = 0.5)
b <-seq(14, 25, by = 0.5)
for (i in a){
  for (j in b) {
    m <-rbind(m, c(i, j))
  }
}
prediksi<-knn(training, m, kelas, k = 12)
plot(m[,1], m[,2],
     col=ifelse(prediksi=="1",
                "cyan","yellow"),
     pch=ifelse(prediksi=="2",17,12))

points(data$x1, data$x2, col=data$class,
       pch=ifelse(data$class>1,17,12), cex=2)

prediksi<-knn(training, m, kelas, k = 3)
head(prediksi)
plot(m[,1], m[,2],
     col=ifelse(prediksi=="1",
                "cyan","yellow"),
     pch=ifelse(prediksi=="2",17,12))

points(data$x1, data$x2, col=data$class,
       pch=ifelse(data$class>1,17,12), cex=2)


prediksi<-knn(training, m, kelas, k = 1)

plot(m[,1], m[,2],
     col=ifelse(prediksi=="1",
                "cyan","yellow"),
     pch=ifelse(prediksi=="2",17,12))

points(data$x1, data$x2, col=data$class,
       pch=ifelse(data$class>1,17,12), cex=2)