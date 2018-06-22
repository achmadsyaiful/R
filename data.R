setwd("C:/Users/USER/Downloads/Data")

data <-read.csv("ilustrasikm.csv")
cluster <-kmeans(data, 3)
plot(data[,1], data[,2], col=cluster$cluster)
points(cluster$centers, pch=9)


plot(data[,1], data[,2], col=cluster$cluster)
points(cluster$centers, pch=19)

cluster$centers


plot(data[,1], data[,2], col=cluster$cluster,cex=2,pch=19)
points(cluster$centers, pch=9)

#clustering#
ilustrasi<-read.csv("C:/Users/USER/Downloads/Data/ilustrasi2a.csv", header=T, sep=";")
head(ilustrasi)

hasilgerombol<-kmeans(ilustrasi, centers=3, iter.max=10)
hasilgerombol$cluster

hasilgerombol$tot.withinss

wssplot<-function(data, nc=15, seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <-sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(ilustrasi, nc=10)

library("cluster")
jarak<-as.matrix(dist(ilustrasi))
hasilgerombol<-kmeans(ilustrasi, centers=3, iter.max=10)
sil.3 <-mean(silhouette(hasilgerombol$cluster,dmatrix=jarak)[,3])
hasilgerombol<-kmeans(ilustrasi, centers=4, iter.max=10)
sil.4 <-mean(silhouette(hasilgerombol$cluster,dmatrix=jarak)[,3])

hasilgerombol<-kmeans(ilustrasi, centers=5, iter.max=10)
sil.5 <-mean(silhouette(hasilgerombol$cluster,dmatrix=jarak)[,3])
c(sil.3, sil.4,sil.5)

#Coba clustering#
setwd("C:/Users/USER/Downloads/Data")

datacluster <-read.csv("cobacluster.csv")
newcluster <- datacluster[,c(14,3,5,7,10)]
Ratio <- datacluster[,c(4,13,18)]

Ratio$Total <- with(Ratio, REV_VOICE+REV_SMS+REV_DATA)
Ratio$PCT_REV_VOICE <- with(Ratio, REV_VOICE/Total )
Ratio$PCT_REV_SMS <- with(Ratio, REV_SMS/Total )
Ratio$PCT_REV_DATA <- with(Ratio, REV_DATA/Total )
PCT_Ratio <- Ratio[,c(5,6,7)]

Cobacluster <- pd.concat(newcluster, PCT_Ratio, axis=1)

#baca data
#pilih 5 variable
#hitung 3 variable
#standarisasi
#hitung within sum square
#tentukan jumlah berdasarkan plot wss
  

