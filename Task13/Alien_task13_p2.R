library(psych)
library(lmtest)
setwd("C:/Users/Legion/Desktop/шлак/R_datas")
df = read.csv("StockIndices/Chile_IPSA.csv")
View(df)
yield <- diff(log(df$Close)) * 100

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
abline(h = 0,col = 2)
grid()
lines(vol.rm, col = "#00ff00")


plot.ts(yield)  
abline(h=0,col=2)
grid()
lines(vol.rm,col="#00ff00")
lines(-vol.rm,col="#00ff00")

library(fGarch)
g_mod <- garchFit(~garch(1,1),yield,trace=F,include.mean = F,cond.dist = "std")#std - normalized Student
print(g_mod)

vol.g_mod <- volatility(g_mod)

plot.ts(yield)
abline(h = 0, col = 2)
grid()
lines(vol.g_mod, col = "#00ff00")
lines(-vol.g_mod, col = "#00ff00")

pred <- predict(g_mod, n.ahed = 100)

plot(pred$standardDeviation)#стратовали из периода высокой волатильности

volp=c(vol.g_mod,pred$standardDeviation)
length(vol.g_mod)
TT
plot.ts(volp)
lines(volp[TT:TT+100],col=2,pch=18)
abline(v=TT,col=2)

