setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
wine <- read.csv("white_wine3.csv")
colnames(wine)
contoh<-head(wine)
write.csv(contoh,"E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017/contoh.csv")
data <- wine[,c("alcohol", "density", "quality")]
head(data)
data$kelas.kualitas <- ifelse(data$quality > 6, 1, 2)
plot(data$density, data$alcohol, col=data$kelas.kualitas)


#membagi data menjadi dua bagian K=2
library(data.table)
acak <- sample(1:nrow(data), 2449)
data.ori<- data[,c(1,2)]

data.1 <- data[acak,]
data.2 <- data[-acak,]
data.2
x.1 <- data.1[,c(1,2)]
y.1 <- data.1[,c(4)]
x.2 <- data.2[,c(1,2)]
y.2 <- data.2[,c(4)]
data.testing<-rbind(data.frame(kualitas=data.1[,c(4)]),data.frame(kualitas=data.2[,c(4)]))
#menghitung ratarata dan dibagi stdev
rata <- apply(data.ori, 2, mean)
sb <- apply(data.ori, 2, sd)

#membakukan data 1: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.1),2, byrow=TRUE)
simpangan.baku <- matrix(sb, nrow(x.1),2, byrow=TRUE)
x.1.baku <- (x.1 - rata.rata)/simpangan.baku

#membakukan data 2: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.2),2, byrow=TRUE)
simpangan.baku <- matrix(sb, nrow(x.2),2, byrow=TRUE)
x.2.baku <- (x.2 - rata.rata)/simpangan.baku
x.1.baku
x.2.baku

#Validasi silang
library(class)
library(caret)
nilai.k <- seq(1, 100, by=2)
akurasi <- NULL


for (k in nilai.k) {
  prediksi.1 <- knn(x.1.baku, x.2.baku, y.1, k=k)
  prediksi.2 <- knn(x.2.baku, x.1.baku, y.2, k=k)
  prediksi <- rbind(data.frame(prediksi=prediksi.1),data.frame(prediksi=prediksi.2))
  
  kinerja <- confusionMatrix(as.matrix(prediksi), as.matrix(data.testing))
  akurasi <- c(akurasi, kinerja$overall[1])
}
plot(nilai.k, akurasi,type="b")
data.frame(cbind(nilai.k,akurasi))


#K=4
#membagi data menjadi dua bagian K=4
acak <- sample(1:nrow(data), 2449)

data.one <- data[acak,]
data.two <- data[-acak,]
acak.1<- sample(1:nrow(data.one), 1224)
data.1<- data.one[acak.1,]
data.2<- data.one[-acak.1,]

acak.2<- sample(1:nrow(data.two), 1224)
data.3<- data.two[acak.2,]
data.4<- data.two[-acak.2,]

data.ori<- data.1[,c(1,2)]
x.1 <- data.1[,c(1,2)]
y.1 <- data.1[,4]

x.2 <- data.2[,c(1,2)]
y.2 <- data.2[,4]

x.3 <- data.3[,c(1,2)]
y.3 <- data.3[,4]

x.4 <- data.4[,c(1,2)]
y.4 <- data.4[,4]

data.testing<-rbind(data.frame(kualitas=y.4),data.frame(kualitas=y.3),
                    data.frame(kualitas=y.2),data.frame(kualitas=y.1))

#menghitung ratarata dan dibagi stdev
rata <- apply(data.ori, 2, mean)
sb <- apply(data.ori, 2, sd)

#membakukan data 1: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.1),2, byrow=TRUE)
simpangan.baku <- matrix(sb, nrow(x.1),2, byrow=TRUE)
x.1.baku <- (x.1 - rata.rata)/simpangan.baku

#membakukan data 2: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.2),2, byrow=TRUE)
simpangan.baku <- matrix(sb, nrow(x.2),2, byrow=TRUE)
x.2.baku <- (x.2 - rata.rata)/simpangan.baku

#membakukan data 3: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.3),2, byrow=TRUE)
simpangan.baku <- matrix(sb, nrow(x.3),2, byrow=TRUE)
x.3.baku <- (x.3 - rata.rata)/simpangan.baku

#membakukan data 4: dikurangi ratarata dan dibagi stdev
rata.rata <- matrix(rata,nrow(x.4),2, byrow=TRUE)
simpangan.baku <- matrix(sb, nrow(x.4),2, byrow=TRUE)
x.4.baku <- (x.4 - rata.rata)/simpangan.baku

y.one<-rbind(data.frame(kualitas=y.3),data.frame(kualitas=y.2)
             ,data.frame(kualitas=y.1))
x.one.baku<-rbind(x.3.baku,x.2.baku,x.1.baku)

y.two<-rbind(data.frame(kualitas=y.4),data.frame(kualitas=y.2)
             ,data.frame(kualitas=y.1))
x.two.baku<-rbind(x.4.baku,x.2.baku,x.1.baku)

y.three<-rbind(data.frame(kualitas=y.4),data.frame(kualitas=y.3)
             ,data.frame(kualitas=y.1))
x.three.baku<-rbind(x.4.baku,x.3.baku,x.1.baku)

y.four<-rbind(data.frame(kualitas=y.4),data.frame(kualitas=y.3)
             ,data.frame(kualitas=y.2))
x.four.baku<-rbind(x.4.baku,x.3.baku,x.2.baku)

#Validasi silang
library(class)
library(caret)
nilai.k <- seq(1, 100, by=2)
akurasi <- NULL


for (k in nilai.k) {
  prediksi.1 <- knn(x.one.baku, x.4.baku, as.matrix(y.one), k=k)
  prediksi.2 <- knn(x.two.baku, x.3.baku, as.matrix(y.two), k=k)
  prediksi.3 <- knn(x.three.baku, x.2.baku, as.matrix(y.three), k=k)
  prediksi.4 <- knn(x.four.baku, x.1.baku, as.matrix(y.four), k=k)
  prediksi <- rbind(data.frame(prediksi=prediksi.1),data.frame(prediksi=prediksi.2)
                    ,data.frame(prediksi=prediksi.3),data.frame(prediksi=prediksi.4))
  kinerja <- confusionMatrix(as.matrix(prediksi), as.matrix(data.testing))
  akurasi <- c(akurasi, kinerja$overall[1])
}
plot(nilai.k, akurasi,type="b")
data.frame(cbind(nilai.k,akurasi))