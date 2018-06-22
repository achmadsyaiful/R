x<-seq(3,11,by=0.1)
fx<-dnorm(x,7,1)
plot(x,fx,type="l")

x1<-seq(6,24,by=0.2)
x2<-seq(6,24,by=0.2)

grid <- NULL
for (i in x1) {
  for (j in x2) {
    grid <-rbind(grid, c(i, j))
  }
}

mean1<-c(15,15)
mean2 <-c(22, 22)
sigma <-matrix(c(5, 3, 3, 5), 2, 2)
sigma <-matrix(c(5, 4, 4, 5), 2, 2)
sigma <-matrix(c(5, 4.5, 4.5, 5), 2, 2)

require(mvtnorm)
y <- dmvnorm(grid, mean1, sigma, log=FALSE) 
require(mvtnorm)
y <-0.5*dmvnorm(grid, mean1, sigma, log=FALSE) + 0.5*dmvnorm(grid, mean2, sigma, log=FALSE)
z <-matrix(y, length(x1), length(x2), byrow=TRUE)

persp(x1, x2, z, phi=5, theta=25)
contour(x1, x2, z)

a <-seq(10, 30,length.out=50)
b <-seq(10, 30,length.out=50)
grid <-NULL
for (i in a) {
  for (j in b) {
    grid <-rbind(grid, c(i, j))
  }
}
mean1 <-c(15, 15)
mean2 <-c(25, 25)
sigma <-matrix(c(5, 3, 3, 5), 2, 2)
require(mvtnorm)
y = 0.5*dmvnorm(grid, mean1, sigma, log=FALSE) + 0.5*dmvnorm(grid, mean2, sigma, log=FALSE)
z <-matrix(y, length(a), length(b), byrow=TRUE)
persp(a, b, z, phi=5, theta=25)
contour(a, b, z)