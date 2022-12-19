crypto = read.csv("crypto.csv")[c("BTC","BCH")]

BTC = (crypto[c(2:495),c("BTC")] / crypto[c(1:494),c("BTC")])*100 - 100
BCH = (crypto[c(2:495),c("BCH")] / crypto[c(1:494),c("BCH")])*100 - 100

change = data.frame(BTC,BCH)
rm(BTC,BCH)

change$BTC_pos = (change$BTC > 0)*1
change$BCH_pos = (change$BCH > 0)*1

table = table(change[,c("BTC_pos","BCH_pos")])

round(prop.table(table(change[,c("BTC_pos","BCH_pos")]))*100,1)
round(prop.table(table(change[,c("BTC_pos","BCH_pos")]),1)*100,1)
round(prop.table(table(change[,c("BTC_pos","BCH_pos")]),2)*100,1)

hist(change$BTC,breaks = seq(-20,30,1), freq = F)
curve(dnorm(x,mean = mean(change$BTC),sd = sd(change$BTC)),add = T,col="red")

hist(change$BCH,breaks = seq(-40,60,1), freq = F)
curve(dnorm(x,mean = mean(change$BCH),sd = sd(change$BCH)),add = T,col="red")

model1 = lm(data = change, BCH ~ BTC_pos)
summary(model1)


library(lmtest)
library(sandwich)
coefci(model1, vcov = vcovHC)

