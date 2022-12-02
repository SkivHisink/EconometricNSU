setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("StreetLight2020.tsv", skip = 5)
head(df)
reg1 <- lm(df$actual ~ df$estimated)

plot(df$actual ~ df$estimated)
abline(c(0, 1), lwd = 2, col = "red")
grid()

plot(df$actual ~ fitted(reg1))
abline(c(0, 1), lwd = 2, col = "red")
grid()

plot(resid(reg1) ~ fitted(reg1), data = df)
points(sqrt(fitted(reg.bp)) ~ rank(df$estimated))
grid()
abline(h = 0, col = "red")

reg.bp <- lm(resid(reg1)^2 ~ df$estimated)

plot(resid(reg1) ~ rank(df$estimated))
points(sqrt(fitted(reg.bp)) ~ rank(df$estimated), col = "blue", pch = "*")
points(-sqrt(fitted(reg.bp)) ~ rank(df$estimated), col = "blue", pch = "*")
grid()
abline(h = 0, col = "red")

summary(reg.bp)
install.packages("lmtest")
library(lmtest)

lmtest::coeftest(reg.bp)
reg1 <- lm(df$actual - df$estimated ~ df$estimated)
reg0 <- lm(df$actual - df$estimated ~ 0)
lmtest::waldtest(reg1, reg0)
anova(reg0)
install.packages("sandwich")
library(sandwich)
waldtest(reg1, reg0, vcov = sandwich::vcovHC)

lmtest::coeftest(reg1)
lmtest::coeftest(reg1, vcov = vcovHC)

lmtest::coefci(reg1)
lmtest::coefci(reg1, vcov = vcovHC)
# second part
install.packages("quantreg")
library(quantreg)

data("engel")
df <- engel
rm(engel)


plot(df$foodexp ~ df$income)
grid()
reg1 <- lm(df$foodexp ~ df$income)
abline(reg1, col = "red", lwd = 2)

plot(resid(reg1) ~ df$income)
abline(h = 0, col = "red", lwd = 2)
grid()

plot(resid(reg1) ~ rank(df$income))
abline(h = 0, col = "red", lwd = 2)
grid()

lmtest::bptest(reg1)

reg.bp <- lm(resid(reg1)^2 ~ df$income)
summary(reg.bp)

plot(resid(reg1) ~ rank(df$income))
points(sqrt(fitted(reg.bp)) ~ rank(income),
data = df, col = "blue", pch = "*")
points(-sqrt(fitted(reg.bp)) ~ rank(income),
data = df, col = "blue", pch = "*")
abline(h = 0, col = "red", lwd = 2)
grid()

plot(log(df$foodexp) ~ log(df$income))
reg.log <- lm(log(df$foodexp) ~ log(df$income))
abline(reg.log, col = "red")

summary(reg.log)
lmtest::bptest(reg.log)

reg.bp <- lm(resid(reg.log)^2 ~ log(df$income))
summary(reg.bp)

plot(resid(reg.log) ~ rank(df$income))
points(sqrt(fitted(reg.bp)) ~ rank(df$income), col = "blue", pch= "*")
points(-sqrt(fitted(reg.bp)) ~ rank(df$income), col = "blue", pch = "*")
abline(h = 0, col = "red", lwd = 2)
grid()

reg.w <- lm(df$foodexp ~ df$income, weights = 1 / df$income^2)
summary(reg.w)

plot(df$foodexp ~ df$income)
abline(reg.w, col = "red")

lmtest::coefci(reg1, vcov = sandwich::vcovHC)
lmtest::coefci(reg.w, vcov = sandwich::vcovHC)

install.packages("gamlss")
library(gamlss)

model3 <- gamlss(df$foodexp ~ df$income, ~ log(df$income))
summary(model3)

plot(df$foodexp - model3$mu.fv ~ rank(df$income))
abline(h = 0, col = "red")
points(model3$sigma.fv ~ rank(df$income), col = "blue", pch = "*")
points(-model3$sigma.fv ~ rank(df$income), col = "blue", pch = "*")

plot(resid(model3) ~ rank(df$income))
abline(h = 0, col = "red")

# quantile regression

qreg <- quantreg::rq(df$foodexp ~ df$income) # median regression
summary(qreg)
plot(df$foodexp ~ df$income)
abline(qreg, col = "blue")
abline(reg1, col = "red")
grid()

qreg10 <- quantreg::rq(df$foodexp ~ df$income, tau = seq(0.1, 0.9, 0.1))
print(qreg10)
plot(qreg10)

plot(df$foodexp ~ df$income)
grid()
arr <- c(1:9)
for(x in arr){
  abline(coef(qreg10)[, x], col = x, lwd = 2)
}
