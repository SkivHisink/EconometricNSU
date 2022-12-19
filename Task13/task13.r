setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("GBR_Dairy.tsv")
View(df)
h <- 24
train_len <- length(df$dairy) - h
data <- c(df$dairy[1:train_len])
test <- c(df$dairy[(train_len): length(df$dairy)])
plot.ts(data, type = "o", pch = 18, col = "#0000ff")
grid()

library(forecast)
train_ts <- ts(data = data, start = c(1990, 1), frequency = 12)
test_ts <- ts(data = test, start = c(2008, 1), frequency = 12)
model1 <- auto.arima(train_ts, seasonal = TRUE)
summary(model1)
print(model1)
acf(resid(model1))

model2 <- arima(train_ts, c(2, 1, 1), list(order = c(2, 1, 1)))
summary(model2)
print(model2)
acf(resid(model2))

pr <- predict(model1, n.ahead = h)
yp <- pr$pred
str(yp)

plot(test_ts, type = "o", pch = 18)
grid()
lines(yp, col = "#ff0000", type = "o", pch = 18)

ep <- test_ts - yp
plot(ep, type = "o", pch = 18, ylim = range(ep, 0))
abline(h = 0, col = "#00FF00")

bottom_boundary <- pr$pred - pr$se * qnorm(1 - 0.1 / 2)
top_boundary <- pr$pred + pr$se * qnorm(1 - 0.1 / 2)

str(bottom_boundary)
str(top_boundary)
str(yp)
full_ts <- ts(data= df$dairy, start = c(1990, 1), frequency = 12)
plot(full_ts, type = "o", pch = 18, xlim = c(2005, 2010))
grid()
lines(yp, col = "blue", type = "o", pch = 18, lty = 2)
lines(bottom_boundary, col = "#FF0000", lty = 2, lwd = 2)
lines(top_boundary, col = "#00FF00", lty = 2, lwd = 2)

#-------------------------------------------------
df <- read.delim("Turkey_XU100.TSV")
View(df)

r <- diff(log(df$Close)) * 100
plot.ts(r)
grid()
abline(h = 0, col = 4)

acf(r^2)
acf(abs(r))

TT <- length(r)
lambda <- 0.95
s2 <- c(1)
for (t in 2:TT){
  s2[t] <- lambda * s2[t-1] + (1 - lambda) * r[t-1]^2
}
vol.rm <- sqrt(s2)

plot.ts(vol.rm, ylim = range(vol.rm, 0))
abline(h=0, col = 6, lwd = 2)

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

