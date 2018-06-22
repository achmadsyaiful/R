library(readxl)

# membaca data langsung dari website
gdp<-read.csv("https://raw.githubusercontent.com/bagusco/metodegrafik/master/gdp.csv")
life<-read.csv("https://raw.githubusercontent.com/bagusco/metodegrafik/master/lifeexp.csv")
pop<-read.csv("https://raw.githubusercontent.com/bagusco/metodegrafik/master/pop.csv")

# menyimpan data hasil baca ke directory local
write.csv(gdp,"gdp.csv")
write.csv(life,"life.csv")
write.csv(pop,"pop.csv")

# membaca data dari direktori lokal
gdp<-read.csv("gdp.csv")
life<-read.csv("life.csv")
pop<-read.csv("pop.csv")

region<-read.delim("clipboard")

# menambahkan region atau benua
gdp<-data.frame(gdp,region)
pop<-data.frame(pop,region)
life<-data.frame(life,region)

plot(gdp$X2000,life$X2000,
     xlab = "Pendapatan per Kapita",
     ylab = "Angka Harapan Hidup",
     log = "x",cex=log(pop$X2000/8000000+3),
     pch = 19, col = rainbow(30))

plot(gdp$X2000,life$X2000,
     xlab = "Pendapatan per Kapita",
     ylab = "Angka Harapan Hidup",
     log = "x",cex=log(pop$X2000/8000000+3),
     pch = 19, col = "blue")

plot(gdp$X2000,life$X2000,
     xlab = "Pendapatan per Kapita",
     ylab = "Angka Harapan Hidup",
     log = "x",cex=log(pop$X2000/8000000+3),
     pch = 19, #col = ifelse(pop$negara=="Indonesia","green","coral"),
     col = pop$Region,
     ylim = c(15,85),xlim = c(100,100000))
text(40000,30,"2000",cex=5)

a<-names(pop)[-1]
b<-as.numeric(substr(a,2,5))

for (i in b) {
  x<-paste0("X",i)
  l<-life[[x]]
  g<-gdp[[x]]
  p<-pop[[x]]
  jpeg(paste0("D:/Google Drive/Dian/S2/Statistika Terapan/Semester 2/Metode Grafik untuk Analisis dan Penyajian Data/Interactive graph/",i,".jpg"))
  plot(g,l,
       xlab = "Pendapatan per Kapita",
       ylab = "Angka Harapan Hidup",
       log = "x", cex=log(p/8000000+3),
       pch = 19, 
       #col = ifelse(pop$negara=="Indonesia","blue","red"), 
       col = pop$Region,
       ylim = c(15,90), xlim = c(100,100000))
  text(40000,30,i,cex=5,col="coral")
  dev.off()
}



plot(gdp$X1900,life$X1900,
     xlab = "Pendapatan per Kapita",
     ylab = "Angka Harapan Hidup",
     log = "x",cex=log(pop$X1900/8000000+3),
     pch = 19, col = ifelse(pop$negara=="Indonesia","blue","coral"),
     ylim = c(15,85))


library(ggplot2)
p <- ggplot(data=pop, aes(x=Total.population, y=Total.population,
                        size=Total.population))
p + geom_jitter(position=
                  position_jitter(w=.2, h=.1),
                shape=21, fill="gray") +
  scale_y_continuous(breaks=
                       c(1,2,3,4,5,6,7)) +
  scale_size_area(breaks=
                    c(50,100,200,300,1000),
                  max_size=18) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(angle=0),
        legend.key=element_blank(),
        legend.text.align=1)
