setwd("C:/Users/anast/Desktop/Rprojects")
library(readxl)
df <- read_excel("Report.xlsx")

df$KRW = log(as.numeric(df$KRW))
df$t = c(1:length(df$KRW))

table = df[c(3:(length(df$KRW))),]
table
table$x1 = df$KRW[c(2:((length(df$KRW))-1))]
table$x1
table$x2 = df$KRW[c(1:((length(df$KRW))-2))]
table$x2

table = table[,c(2,1,3,4)]
table

train = table[c(1:(6688-365*2)),]

test = table[c(5959:6688),]

test <- !train

forecast = data.frame(test$x1)
RW <- x1[test]
names(forecast) = c("RW")

AR1 = lm(data = train, KRW ~ x1)
summary(AR1)
sample = data.frame(test$x1)
names(sample) = c("x1")
forecast$AR1 =  predict(AR1, newdata = sample)
rm(sample, AR1)

AR2 = lm(data = train, KRW ~ x1 + x2)
summary(AR2)
sample = data.frame(test$x1, test$x2)
names(sample) = c("x1", "x2")
forecast$AR2 =  predict(AR2, newdata = sample)
rm(sample, AR2)

AR02 = lm(data = train, KRW ~ + x2)
summary(AR02)
sample = data.frame(test$x1, test$x2)
names(sample) = c("x1", "x2")
forecast$AR02 =  predict(AR02, newdata = sample)
rm(sample, AR02)

forecast$M2 = (test$x1 + test$x2)/2

plot(data = test[c(700:730),], KRW ~ t, type = "l", xlab = "RW")
lines(forecast$RW[c(700:730)] ~ test$t[c(700:730)], col = "red")

plot(data = test[c(700:730),], KRW ~ t, type = "l", xlab = "AR1")
lines(forecast$AR1[c(700:730)] ~ test$t[c(700:730)], col = "red")

plot(data = test[c(700:730),], KRW ~ t, type = "l", xlab = "AR2")
lines(forecast$AR2[c(700:730)] ~ test$t[c(700:730)], col = "red")

plot(data = test[c(700:730),], KRW ~ t, type = "l", xlab = "AR01")
lines(forecast$AR02[c(700:730)] ~ test$t[c(700:730)], col = "red")

plot(data = test[c(700:730),], KRW ~ t, type = "l", xlab = "M2")
lines(forecast$M2[c(700:730)] ~ test$t[c(700:730)], col = "red")

RMSE = matrix(nrow = 1, ncol = 5)
colnames(RMSE) = c("RW","AR1","AR2","AR02","M2")
rownames(RMSE) = c("RMSE")

RMSE[1,1] = round(sqrt(mean((test$KRW - forecast$RW)^2)) * 100,2)
RMSE[1,1]
RMSE[1,2] = round(sqrt(mean((test$KRW - forecast$AR1)^2)) * 100,2)
RMSE[1,3] = round(sqrt(mean((test$KRW - forecast$AR2)^2)) * 100,2)
RMSE[1,4] = round(sqrt(mean((test$KRW - forecast$AR02)^2)) * 100,2)
RMSE[1,5] = round(sqrt(mean((test$KRW - forecast$M2)^2)) * 100,2)

RMSE

RW_AR1 = lm((test$KRW - forecast$RW)^2 - (test$KRW - forecast$AR1)^2 ~ 1)
summary(RW_AR1)

RW_AR2 = lm((test$KRW - forecast$RW)^2 - (test$KRW - forecast$AR2)^2 ~ 1)
summary(RW_AR2)

RW_AR02 = lm((test$KRW - forecast$RW)^2 - (test$KRW - forecast$AR02)^2 ~ 1)
summary(RW_AR02)

RW_M2 = lm((test$KRW - forecast$RW)^2 - (test$KRW - forecast$M2)^2 ~ 1)
summary(RW_M2)