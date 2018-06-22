
#Coba clustering#
#Baca Data
setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")

#Pilih Data
datacluster <-read.csv("cobacluster.csv")
newcluster <- datacluster[,c(14,3,5,7,10)]

#Hitung Ratio
Ratio <- datacluster[,c(4,13,18)]
Ratio$Total <- with(Ratio, REV_VOICE+REV_SMS+REV_DATA)
Ratio$PCT_REV_VOICE <- with(Ratio,REV_VOICE/Total )
Ratio$PCT_REV_SMS <- with(Ratio, REV_SMS/Total )
Ratio$PCT_REV_DATA <- with(Ratio, REV_DATA/Total )
Ratio[is.na(Ratio)] <- 0
PCT_Ratio <- Ratio[,c(5,6,7)]


#Standarisasi
library(dplyr)
Vcluster <- data.frame(newcluster, PCT_Ratio)
SCluster <- Vcluster %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("CDR_TOTAL_DATA_MB","CDR_VOICE_DRTN_MIN","CDR_TXN_VOICE","CDR_RATIO_VOICE_ONNET","CDR_RATIO_SMS_ONNET","PCT_REV_VOICE","PCT_REV_SMS","PCT_REV_DATA"))
SCluster

#Eksplorasi
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

colMin(Vcluster)
colMeans(Vcluster)
colMax(Vcluster)

#Explorasi
boxplot(SCluster$CDR_TOTAL_DATA_MB,xlab="CDR_TOTAL_DATA_MB")
boxplot(SCluster$CDR_VOICE_DRTN_MIN,xlab="CDR_VOICE_DRTN_MIN")
boxplot(SCluster$CDR_TXN_VOICE,xlab="CDR_TXN_VOICE")
boxplot(SCluster$CDR_RATIO_VOICE_ONNET,xlab="CDR_RATIO_VOICE_ONNET")
boxplot(SCluster$CDR_RATIO_SMS_ONNET,xlab="CDR_RATIO_SMS_ONNET")
boxplot(SCluster$PCT_REV_VOICE,xlab="PCT_REV_VOICE")
boxplot(SCluster$PCT_REV_SMS,xlab="PCT_REV_SMS")
boxplot(SCluster$PCT_REV_DATA,xlab="PCT_REV_DATA")

#Hitung within sum square
head(SCluster)
wssplot<-function(SCluster, nc=15, seed=1234){
  wss<-(nrow(SCluster)-1)*sum(apply(SCluster,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <-sum(kmeans(SCluster, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#Tentukan jumlah plot
wssplot(SCluster, nc=15)

#Buat K-Mean
set.seed(1333)
Cluster <- kmeans(SCluster, 5)
Clustering<-Cluster$cluster
count(Datacluster, vars = Clustering)

#Hitung Rataan Variable dr K-Mean
Datacluster<-data.frame(Vcluster,Clustering)
aggregate(Datacluster, by=list(cluster=Clustering), mean)
count(Datacluster, vars = Clustering)

