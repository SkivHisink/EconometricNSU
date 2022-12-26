df <- read.delim("Exchange_Rate_Report.TSV")
View(df)
df$X <- NULL
df$X.1 <- NULL
names(df) <- c("Date", "MXN")

lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
df$Date <- as.Date(df$Date, "%d-%b-%Y")
Sys.setlocale("LC_TIME", lct)

x <- log(df$MXN)
xx <- matrix(x, nrow=55)

jj <- sample(1:ncol(xx), 10)
jj <- 1:10

Cor.xx <- cor(xx)
cor.xx <- c(Cor.xx[lower.tri(Cor.xx)])
r <- c(0, diff(x)) * 100
rr <- matrix(r, nrow=55)

ii <- sample(1:ncol(rr), 10)

Cor.rr <- cor(rr)
cor.rr <- c(Cor.rr[lower.tri(Cor.rr)])
tr <- 1:length(x) #тренд
tr2 <- tr^2
train <-df$Date <= "2020-11-30"
test <- !train #тестовый период (логическая переменная)
train.2y <- train & (df$Date > "2018-11-28")
reg <- lm(x ~ tr+tr2, subset = train.2y)
xp <- predict(reg, newdata = list(tr = tr[test], tr2 = tr2[test]))
ep <- x[test] - xp #ошибка

#задание 1

x1 <- c(x[1], x[-length(x)])
plot(x~ x1)

T0 <- length(x)
x2 <- c(x1[1], x1[-T0])

xp.RW <- x1[test] #случайное блуждание

AR1 <- lm(x~x1, subset = train) #авторегрессия x от своего лага 
summary(AR1) 
cf.AR1 <- coef(AR1)
xp.AR1 <- cf.AR1[1] + cf.AR1[2] * x1[test]

xpxp <- cbind(xp.RW, xp.AR1) #склеиваем прогнозы

ts.plot(xpxp, col=1:2) #прогнозы близки, две модели похожи
grid()
lines(x[test], col = "blue")

ts.plot(xpxp[1:20,], col=1:2) #прогнозы близки, две модели похожи
grid()
lines(x[test][1:20], col = "blue") #фактически сдвинутый на 1 прогноз

ts.plot(epep, col = 1:2)
grid()
abline(h=0, col = "blue")

AR2 <- lm(x~x1+x2, subset = train)
cf.AR2 <- coef(AR2)
xp.AR2 <- cf.AR2[1] + cf.AR2[2] * x1[test]
AR02 <- lm(x~x2, subset = train)
cf.AR02 <- coef(AR02)
xp.AR02 <- cf.AR02[1] + cf.AR02[2] * x1[test]
xp.M2 <- (x1[test] + x2[test])/2

xpxp <- cbind(xp.RW, xp.AR1, xp.AR2, xp.AR02,xp.M2)
epep <- x[test] - xpxp

reg.DM <- lm(epep[,2]^2 - epep[,1]^2 ~ 1) #тест Д-М
summary(reg.DM)

library(lmtest)
library(sandwich)
coeftest(reg.DM)
coeftest(reg.DM, vcov=vcovHAC) 
#точность двух прогнозов примерно одинаковое, т.е. статистически незначимое различие

coeftest(lm(epep[,2]^2 - epep[,1]^2 ~ 1), vcov=vcovHAC) 
coeftest(lm(epep[,3]^2 - epep[,1]^2 ~ 1), vcov=vcovHAC) 
coeftest(lm(epep[,4]^2 - epep[,1]^2 ~ 1), vcov=vcovHAC) 
coeftest(lm(epep[,5]^2 - epep[,1]^2 ~ 1), vcov=vcovHAC) 

MSE <- colMeans(epep^2) #средний квадрат 
RMSE <- sqrt(MSE)
View(RMSE)
RMSE * 100

#задание 2

df <- read.delim("CAN_Unempl.TSV")
View(df)
plot.ts(df$unempl)
grid()

df2 <- read.delim("ISL_AirPass.TSV")
View(df2)
plot.ts(df2$airpass)
grid()

cor(df$unempl[1:130], df2$airpass)
cor.test(df$unempl[1:130], df2$airpass)

#задание 3

ts.plot(epep[,2]*100)
abline(h=0, col = "blue")
