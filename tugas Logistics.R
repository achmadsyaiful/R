setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
datascoring <-read.csv("datascoring.csv")
colnames(datascoring)
head(datascoring)
dim(datascoring)

datascoring$status.bil<-ifelse(datascoring$status== "GOOD", 0, 1)
head(datascoring)

#Explorasi Data

counts <-table(datascoring$status.bil)
Status.ord <- c("Bad", "Good")
pct <- round(counts/sum(counts)*100) 
Status.ord <- paste(Status.ord, pct) 
Status.ord <- paste(Status.ord,"%",sep="") 
pie(counts,
    labels = Status.ord,main="Data Asal")

library(ggplot2)
counts.1 <- table(datascoring$status, 
                datascoring$Gender) 

barplot(counts.1, 
        main="Sebaran Gender berdasarkan Status", 
        xlab="Gender",
        col=c("darkblue","Blue"), 
        legend = rownames(counts.1), 
        beside=TRUE)

counts.2 <- table(datascoring$status, 
                datascoring$Residence.Ownership) 

barplot(counts.2, 
        main="Sebaran Residence Ownership berdasarkan Status", 
        xlab="Residence Ownership", 
        col=c("darkblue","Blue"), 
        legend = rownames(counts.2), 
        beside=TRUE)


counts.3 <- table(datascoring$status, 
                  datascoring$Age) 

ggplot(datascoring, aes(x=datascoring$Age, fill=datascoring$status)) +
  geom_histogram()


counts.4 <- table(datascoring$status, 
                  datascoring$number.of.dependants) 
ggplot(datascoring, aes(x=datascoring$number.of.dependants, fill=datascoring$status)) +
  geom_histogram()


datascoring$age.cat<-ifelse(datascoring$Age< 20, 1, 
                            ifelse(datascoring$Age< 30, 2, 
                                   ifelse(datascoring$Age< 40, 3, 
                                          ifelse(datascoring$Age< 50, 4, 5))))
counts.5 <- table(datascoring$status, 
                  datascoring$age.cat) 

#Resiko Relatif
prop.table(counts.1,2)

prop.table(counts.2,2)

prop.table(counts.3,2)

prop.table(counts.5,2)

prop.table(counts.4,2)


#Pembagian Data
acak <- sample(1:nrow(datascoring), 1832)
data.training <- datascoring[acak,]
data.testing <- datascoring[-acak,]

counts <-table(data.training$status.bil)
Status.ord <- c("Good", "Bad")
pct <- round(counts/sum(counts)*100) 
Status.ord <- paste(Status.ord, pct) 
Status.ord <- paste(Status.ord,"%",sep="") 
pie(counts,
    labels = Status.ord,main="Data Training")


counts <-table(data.testing$status.bil)
Status.ord2 <- c("Bad", "Good")
pct <- round(counts/sum(counts)*100) 
Status.ord2 <- paste(Status.ord2, pct) 
Status.ord2 <- paste(Status.ord2,"%",sep="") 
pie(counts,
    labels = Status.ord2,main="Data Testing")

(data.training$status.bil,)
head(data.training)
dim(data.training)

#Reglog
model.logistik<-glm(status.bil~ age.cat+number.of.dependants+Gender+Residence.Ownership,
                    data=data.training, family="binomial")
summary(model.logistik)

prob.prediksi<-predict(model.logistik, data.testing, type="response")
prediksi<-ifelse(prob.prediksi>0.5, 1, 0)
library(caret)
confusionMatrix(prediksi, data.testing$status.bil)
