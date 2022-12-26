df <- read.delim("CAN_Unempl.TSV")
View(df)

y <- c(df$unempl)

plot.ts(y, type="o", pch=18, cex=0.5)
grid()

mod1 <- arima(y, c(1,1,1), list(order=c(1,1,1), period=12))

#amod1 <- forecast::auto.arima(ts(y, frequency = 12))
#помогает автоматически подобрать порядок модели

print(mod1)

lmtest::coeftest(mod1)

acf(resid(mod1))

h <- 24
TT <- length(y) - h
mod1 <- arima(y[1:TT], c(2,1,1), list(order=c(2,1,1), period=12))

print(mod1)
acf(resid(mod1))

pr <- predict(mod1, n.ahead = h)
yp <- pr$pred
str(yp)

plot(yp)
plot(y, type="o", pch=18, cex=0.5)
grid()
lines(yp, col="blue", type="o", pch=20, cex=0.5)

ep <- y[(TT+1):(TT+h)] - yp
plot(ep, type="o", pch=18, ylim=range(ep,0))
abline(h=0, col=6)

qnorm(1-0.1/2) #вероятность левого хвоста
L <- pr$pred - pr$se * qnorm(1-0.1/2)
U <- pr$pred + pr$se * qnorm(1-0.1/2)

str(L)
plot(y, type="o", pch=18, cex=1,xlim=c(160, TT+h))
grid()
lines(yp, col="blue",type="o", pch=20,cex=1, lty=2)
lines(L, col="red", lty=3, lwd=2)
lines(U, col="green",lty=3, lwd=2)


#-------------------------------------------------
df <- read.delim("Turkey_XU100.TSV")
View(df)

r <- diff(log(df$Close))*100
plot.ts(r)
grid()
abline(h=0, col=4)

acf(r^2)
acf(abs(r))

TT <- length(r)
lambda <- 0.95
s2 <- c(1)
for (t in 2:TT){
  s2[t] <- lambda * s2[t-1] + (1 - lambda) * r[t-1]^2
}
vol.rm <- sqrt(s2)

plot.ts(vol.rm, ylim=range(vol.rm,0))
abline(h=0, col=6, lwd=2)

plot.ts(r)
lines(vol.rm, col="blue", lwd=2)
lines(-vol.rm, col="blue", lwd=2)
abline(h=0, col=4, lwd=2)


library(fGarch)
ga <- fGarch::garchFit(~ garch(1,1),r,
                       include.mean = FALSE,
                       cond.dist = "std",
                       trace=FALSE)
print(ga)

vol.ga <- volatility(ga)

plot.ts(vol.ga, ylim=range(vol.ga,0))
abline(h=0,col=6, lwd=2)

plot.ts(r)
lines(vol.ga, col="blue", lwd=2)
lines(-vol.ga, col="blue", lwd=2)
abline(h=0, col=4,lwd=2)

pr <- predict(ga, n.ahead=100)
plot(pr$standardDeviation)

volp <- c(vol.ga, pr$standardDeviation)

plot.ts(volp, xlim = c(1600,TT+100), col = 6,ylim=range(volp,0))
abline(v=TT, lty=3)
lines(vol.ga)
abline(h=0, col=4, lwd=2)