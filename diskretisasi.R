x <-c(15, 4, 21, 11, 16, 18, 24, 26, 28)
library(classInt)
#equal width
eqwid<-classIntervals(x, 4, style = 'equal')
eqwid$brks

x.eqwid<-cut(x, breaks=eqwid$brks, include.lowest=TRUE)
cbind(x, x.eqwid)

#equal freq
eqfreq<-classIntervals(x, 4, style = 'quantile')
eqfreq$brks

x.eqfreq<-cut(x, breaks=eqfreq$brks, include.lowest=TRUE)
cbind(x, x.eqwid, x.eqfreq)

setwd("E:/Kuliah/Semester 2/Pemodelan Klasifikasi/2017")
data <-read.csv("disk01.csv")
head(data)
install.packages("classInt")

library(classInt)
model.asli <-glm(class ~ x, data=data, family="binomial")
maudiprediksi <-data.frame(data$x)
colnames(maudiprediksi) <-c("x")
prediksi.prob.asli <-predict(model.asli, newdata=maudiprediksi, type="response")
prediksi.asli <-ifelse(prediksi.prob.asli > 0.5, 1, 0)
table(data$class, prediksi.asli)
mean(data$class == prediksi.asli)

eqwid <-classIntervals(data$x, 10, style = 'equal')
x.eqwid <-cut(data$x, breaks=eqwid$brks, include.lowest=TRUE)
model.disk <-glm(data$class ~ x.eqwid, family="binomial")
prediksi.prob.disk <-predict(model.disk, newdata=x.eqwid, type="response")
prediksi.disk <-ifelse(prediksi.prob.disk> 0.5, 1, 0)
table(data$class, prediksi.disk)
mean(data$class == prediksi.disk)

table(x.eqwid, data$class)
prop.table(table(x.eqwid, data$class), margin=1)
proporsi <-prop.table(table(x.eqwid, data$class), margin=1)
barplot(t(proporsi))

summary(model.disk)
summary(model.asli)

library(klaR)
train <- sample(nrow(data), round(0.75*nrow(data)))
woemodel <- woe(class~., data = data[train,], zeroadj=0.5, applyontrain = TRUE)
woemodel

## plot variable information values and woes
plot(woemodel)
plot(woemodel, type = "woes")
## apply woes
traindata <- predict(woemodel, GermanCredit[train,], replace = TRUE)
str(traindata)
## fit logistic regression model
glmodel     <- glm(credit_risk~., traindata, family=binomial)