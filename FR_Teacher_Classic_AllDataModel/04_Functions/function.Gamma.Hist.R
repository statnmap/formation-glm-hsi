Gamma.Hist <- function(y) {
m <- mean(y)
v <-var(y)
shape <-m*m/v
rate <- m/v
# scale = rate
x <- qgamma(seq(.01,.999,len=200),shape=shape,rate=rate)
yl <- dgamma(x,shape=shape,rate=rate)
hi <- max(x,y)
hist(y,xlim=c(0,hi),probability=T)
lines(x,yl,col="red",lwd=2)
}