# To compare a pair of random walk (RW) predictions with other Diebold-Mariano (DM) predictions using a heteroscedasticity and autocorrelation consistent (HAC) covariance matrix in R,
# you can use the DM.test 
install.packages("forecast")
library(forecast)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim(file = "datatask11.csv", sep = ",",
                 stringsAsFactors = TRUE) # читаем данные

names(df) <- c("Date", "MXN")
df$MXN <- as.numeric(df$MXN)
df$Date <- as.Date(df$Date)
# Добавим логарифм наших данных
df$log <- log(df$MXN)
# Проверим наши данные
head(df)
data_length <- length(df$MXN)
train_length <- 365 * 2
x1 <- df$MXN[c(2:((data_length) - 1))]
x2 <- df$MXN[c(1:((data_length) - 2))]
altdf <- df[c(3:data_length), ]
altdf$x1 <- x1
altdf$x2 <- x2
train_len <- data_length - train_length
train_data <- altdf[c(1:(train_len)), ]
test_data <- altdf[c((train_len + 1):data_length), ]
predicted_data <- data.frame(RW = test_data$x1)

ar_1 <- lm(data = train_data, MXN ~ x1)
summary(ar_1)
predicted_data$ar_1 <-  predict(ar_1,
newdata = data.frame(x1 = test_data$x1))
install.packages("sandwich")
library(sandwich)
rw_errors <- predicted_data$RW - test_data$MXN
ar1_errors <- predicted_data$ar_1 - test_data$MXN
df_errors <- data.frame(rw_errors)
df_errors$ar1_errors <- ar1_errors
df_errors$Date <- as.Date(test_data$Date)
cov_matrix <- NeweyWest(df_errors, lag = 2)
hac_cov <- sandwich::vcovHAC(x = rw_errors, lag = 2)
result <- dm.test(predicted_data$RW, predicted_data$ar_1, hac_cov)
coeftest(df_errors, vcov = vcovHAC)
