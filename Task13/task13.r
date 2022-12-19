setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("GBR_Dairy.tsv")
View(df)
h <- 24
train_len <- length(df$dairy) - h
data <- c(df$dairy[1:train_len])
test <- c(df$dairy[(train_len + 1): length(df$dairy)])
plot.ts(data, type = "o", pch = 18, cex = 0.5)
grid()

library(forecast)
train_ts <- ts(data = data, start = c(1990, 1), frequency = 12)

mod1 <- auto.arima(train$Sales, seasonal = TRUE)
summary(m1)
print(m1)

acf(resid(m1))

pr <- predict(mod1, n.ahead = h)
yp <- pr$pred
str(yp)

plot(yp)
plot(y, type = "o", pch = 18, cex = 0.5)
grid()
lines(yp, col = "blue", type = "o", pch = 20, cex = 0.5)

ep <- y[(TT+1):(TT+h)] - yp
plot(ep, type="o", pch=18, ylim=range(ep,0))
abline(h = 0, col = 6)

qnorm(1 - 0.1 / 2) #вероятность левого хвоста
L <- pr$pred - pr$se * qnorm(1 - 0.1 / 2)
U <- pr$pred + pr$se * qnorm(1 - 0.1 / 2)

str(L)
plot(y, type = "o", pch = 18, cex = 1, xlim = c(160, TT + h))
grid()
lines(yp, col = "blue", type = "o", pch = 20, cex = 1, lty = 2)
lines(L, col = "red", lty = 3, lwd = 2)
lines(U, col = "green", lty = 3, lwd = 2)


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

#--
library(psych)
library(lmtest)
setwd("C:/Users/Legion/Desktop/шлак/R_datas")
df = read.csv('Chile_IPSA.csv')
View(df)
yield <- diff(log(df$Close))*100

plot.ts(yield)
grid()
abline(h=0,col=2)

acf(yield^2)
acf(abs(yield))

TT <- length(yield)
#Riskmetrics
s2 <- c(1)
lam <- 0.95
for(i in 2:TT)
  s2[i] <- lam*s2[i-1]+yield[i-1]^2*(1-lam)
  
vol.rm <- sqrt(s2)

plot.ts(vol.rm, ylim=range(vol.rm,0))  
abline(h=0,col=2)
grid()
lines(vol.rm,col="green2")


plot.ts(yield)  
abline(h=0,col=2)
grid()
lines(vol.rm,col="green2")
lines(-vol.rm,col="green2")

library(fGarch)
g_mod <- garchFit(~garch(1,1),yield,trace=F,include.mean = F,cond.dist = "std")#std - normalized Student
print(g_mod)

vol.g_mod <- volatility(g_mod)

plot.ts(yield)  
abline(h=0,col=2)
grid()
lines(vol.g_mod,col="green2")
lines(-vol.g_mod,col="green2")

pred <- predict(g_mod,n.ahed=100)

plot(pred$standardDeviation)#стратовали из периода высокой волатильности

volp=c(vol.g_mod,pred$standardDeviation)
length(vol.g_mod)
TT
plot.ts(volp)
lines(volp[TT:TT+100],col=2,pch=18)
abline(v=TT,col=2)

