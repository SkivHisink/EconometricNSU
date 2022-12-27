#Task 1


sigma_eps_sq <- 6400
N <- 100
sd <- sqrt(sigma_eps_sq)
#1а
y[1] <- 0
y[2] <- 1
for(t in 3:N){
    eps_1 <- rnorm(1, 0, sd)
    y[t] <- 1.7 * y[t-1] - 0.8 * y[t - 2] + eps_1
}
#1б
corr_vector <- c()
    first <- c(1, 0, 2)
    second <- c(1, 0, 2)
for (i in 1:N){
  for (t in 3:N){
    first[t] <- 1.7 * first[t - 1] - 0.8 * first[t - 2] + rnorm(1, 0, sd)
    second[t] <- 1.7 * second[t - 1] - 0.8 * second[t - 2] + rnorm(1, 0, sd)
  }
  corr_vector[i] <- cor(first, second)
}
hist(corr_vector)
summary(corr_vector)
#1в
intervals <- t.test(corr_vector)
print(intervals$conf.int[1])
print(intervals$conf.int[2])

#y[1] <- 0
#y[2] <- 1
#for(t in 3:N){
#    eps_1 <- rnorm(1, 0, 80)
#    y[t] <- y[t-1] - 0.8 * y[t - 2] + eps_1
#}
#z <- c()
#z[1] <- 0
#z[2] <- 1
#for(t in 3:N){
#    eps_2 <- rnorm(1, 0, 80)
#    z[t] <- z[t-1] - 0.8 * z[t - 2] + eps_2
#}
#corr_vector <-c()
#for(t in 1:length(y)){
#    corr_vector <- cor
#}
#1a
#ar_1 <- lm(y ~ x1)
#1б

#1c

#Task 2
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim(file = "J_and_J.csv", sep = ",",
                 stringsAsFactors = TRUE, skip = 3) # читаем данные

df$log <- log(df$jj)

plot(df$jj)

plot(df$log)
#2a
facf <- acf(df$log)

first_diff <- diff(df$log)

fdiff_acf <- acf(first_diff)

plot(facf)
plot(fdiff_acf)
# лаги стали чередоваться знаком, некоторые лаги перестали "зашкаливать"
#2б
data_len <- length(df$jj)
train_data <- df$log[1:(data_len - 13)]
train_time <- df$Date[1:(data_len - 13)]
test_data <- df$log[(data_len - 12):(data_len)]
test_time <- df$Date[(data_len - 12):(data_len)]
index_vector <- 1 : (length(df$log) - 13)
train_subset <-  c(1:(length(df$log)  - 13))
length(index_vector)
length(train_data)
model <- lm(train_data ~ index_vector, subset = train_subset)

predicted_data <- summary(model)$coefficients[2, 1] * ((length(df$log) - 11):length(df$log)) +
summary(model)$coefficients[1,1]
# Смотрим на предсказанные данные и наши данные
plot(predict, col = "#00FF00")
points(df$log, col = "#FF0000")
# Смотрим на ошибку
plot(predict$pred - test)
abline(h = 0, col = "#0000FF")
#2в
model <- arima(train_data, order = c(1, 1, 1),
seasonal = c(1, 1, 1), period = 4)
pred_2 <- forecast(model, h = 12)
plot(pred_2)
points(df$log, col = "#FF0000")
#2г

#3 Task
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim(file = "crypto.csv", sep = ",",
                 stringsAsFactors = TRUE) # читаем данные
my_data <- data.frame(VEN = df$VEN)
my_data$Date <- as.Date(df$date)
#3a
data_len <- length(my_data$VEN)
r <- diff(log(my_data$VEN)) * 100
lambda <- 0.95
s2 <- c(1)
s2_max <- -10000000
index = 2
for (t in 2:data_len){
  s2[t] <- lambda * s2[t - 1] + (1 - lambda) * r[t - 1]^2
  if (s2[t] > s2_max) {
    s2_max <- s2[t]
    index <- t
  }
}
print(my_data$Date[index]) # самая большая волатильность дневная
plot(sqrt(s2))
abline(v = 31)
abline(v = 61)
abline(v = 92)
abline(v = 123)
#3б
sign <- c(1)
plus <- as.numeric(diff(my_data$VEN))
s2_len <- length(s2)
vol.rm <- sqrt(s2)
sign <- ifelse(plus > 0, 1, 0)
neededFrame <- data.frame( vol.rm = vol.rm, sign = c(0, sign))
train_subset <- 1:60
reg <- glm(sign ~ vol.rm, data = neededFrame,
subset = train_subset, family = "binomial")
summary(reg)
#согласно значению p-value и самой статистики можно сказать что не является статистически значимой
#3в
predict_data <- predict(reg, 
newdata = neededFrame[c(61:144), ], type = 'response')
print(predict_data)
install.packages("pROC")
test_subset <- c(61:length(neededFrame$sign))
pROC::plot.roc(neededFrame$sign[test_subset], predict_data)
pROC::roc(sign[test_subset], predict_data)
#3г
spec_diff <- neededFrame$sign[test_subset] - predict_data
brier_test <- mean((spec_diff^2))
print(brier_test)
#3д 
support_vector <- rep((1.0 / 2.0), length(predicted_data))
binded <- cbind(predict_data, support_vector)
epep <- sign[test_subset] - binded
epep_1 <- epep[,1]^2
epep_2 <- epep[,2]^2
reg.DM <- lm(epep_2 - epep_1 ~ 1)
summary(reg.DM)

library(lmtest)
library(sandwich)
coeftest(reg.DM)
coeftest(reg.DM, vcov = vcovHAC) 

coeftest(lm(epep_2 - epep_1 ~ 1), vcov = vcovHAC) 