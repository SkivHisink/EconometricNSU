setwd("C:/Users/bazof/OneDrive/Martins Drop/Ёконометри€/12")

HSI = read.csv("Hong_Kong_HSI.csv")
HSI$Date = as.Date(HSI$Date)
HSI = HSI[c((length(HSI$Date)):1),]
plot(data = HSI, Close ~ Date, type = "l")
plot(data = HSI, Volume ~ Date, type = "l")

r = diff(log(HSI$Close)) * 100

plot(r ~ HSI$Date[2:(length(HSI$Date))], type = "l")
abline(h = mean(r), col = "red")

acf(r)

#Riskmetrics

TT = length(r)
lambda = 0.95
s2 = c(1)

for (t in 2:TT) {
  s2[t] = lambda * s2[t-1] + (1-lambda) * r[t-1]^2
}
vol_rm = sqrt(s2)

plot(vol_rm ~ HSI$Date[2:(TT+1)], type = "l", xlab = "Date")

plot.ts(r)
lines(vol_rm, col = "red", lwd = 2)
lines(-vol_rm, col = "red", lwd = 2)

library("fGarch")

ga = fGarch::garchFit(~garch(1,1),r,
                      include.mean = FALSE,
                      cond.dist = "std",
                      trace = FALSE)
print(ga)
vol_ga = volatility(ga)

plot.ts(r)
lines(vol_ga, col = "red", lwd = 2)
lines(-vol_ga, col = "red", lwd = 2)

pr = predict(ga, n.ahead=100)
plot(pr$standardDeviation)

volp = c(vol_ga, pr$standardDeviation)
plot(volp, xlim = c(5500, TT+100),
     ylim = c(0,4),type = "l", col = "red")
lines(vol_ga)
abline(v = TT, lty = 3)

