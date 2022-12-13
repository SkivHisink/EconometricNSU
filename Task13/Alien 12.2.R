setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df  <- read.delim(file = "Germany_dax30.tsv")
View(df)

r <-diff(log(df$C))*100


plot.ts(r); grid()
abline(h = 0, col = 4)

acf(r^2)
acf(abs(r))

TT <-  length(r)

# Rismetrics

lambda = 0.95
s2 <- c(1)
for (t in 2:TT) {
  s2[t] <- lambda * s2[t-1] + (1-lambda) * r[t-1]^2
}
vol.rm <- sqrt(s2)

plot.ts(vol.rm, ylim = range(vol.rm, 0))
abline(h = 0, col = 6)

#acf(vol.rm)


plot.ts(r)
lines(vol.rm, col = 'red', lwd = 2)
lines(-vol.rm, col = 'red', lwd = 2)
abline(h = 0, col = 6)

#garch

library(fGarch)
ga <- fGarch::garchFit(~ garch(1,1), r, 
                       include.mean = FALSE, 
                       cond.dist = "std",
                       trace = FALSE)
print(ga)
vol.ga <- volatility(ga)

plot.ts(vol.ga, ylim = range(vol.ga, 0))
abline(h = 0, col = 6)

plot.ts(r)
lines(vol.ga, col = 'red', lwd = 2)
lines(-vol.ga, col = 'red', lwd = 2)
abline(h = 0, col = 6)

pr <-  predict(ga, n.ahead = 100)
plot(pr$standardDeviation)

volp <- c(vol.ga, pr$standardDeviation)
plot(volp, xlim = c(4500, TT +100), col = 1,
     ylim = range(volp, 0))
abline(v = TT, lty = 3)
lines(vol.ga)
abline(h = 0, col = 4, lwd = 2)







