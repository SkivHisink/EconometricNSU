# Task1 Валютный курс: нестационарные ряды и ложная корреляция
#Ссылка для скачивания данных
#Для рассчёта был Взят мексиканский песо
#https://www.imf.org/external/np/fin/ert/GUI/Pages/Report.aspx?CU=%27MXN%27&EX=REP&P=DateRange&Fr=628929792000000000&To=638051904000000000&CF=Compressed&CUF=Period&DS=Ascending&DT=Blank

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim(file = "datatask11.csv", skip = 2, sep = ",",
stringsAsFactors = TRUE) # читаем данные, скипаем шапку

names(df) <- c("Date", "MXN")
df$MXN <- as.numeric(df$MXN)
df$Date <- as.Date(df$Date, "%Y-%m-%d")
# Добавим логарифм наших данных
df$log <- log(df$MXN)
# Проверим наши данные
head(df)

acf(df$MXN, main = "MNX/USD", col = "#0008ff")
plot(df$log ~ df$Date, col = "#0008ff", ylab = "log(USD/MNX)", type = "l")

mat <- matrix(df$log, nrow = 60)
iter <- sample(1:80, 10)
plot.ts(mat[, iter])

install.packages("corrplot")

cor_1 <- cor(mat[, iter])
corrplot::corrplot(cor_1, method = "number")
cor_2 <- cor(mat[, sample(1:80, 50)])
corrplot::corrplot(cor_2)

cor_3 <- cor(mat)
cor_3 <- c(cor_3[lower.tri(cor_3)])
hist(cor_3, breaks = 27)

# Task2 Многошаговое прогнозирование валютного курса с помощью тренда

data_len <- length(df$log)
train_supp <- 1:data_len
train_supp_pow2 <- train_supp^2
tr_len <- length(train_supp)
predict_len <- 365 * 2
train_data_len <- tr_len - predict_len

data <- data.frame(df$log[1:tr_len], train_supp[1:tr_len],
train_supp_pow2[1:tr_len])
colnames(data) <- c('log', 'train_supp', 'train_supp_pow2')
reg <- lm(df$log ~ train_supp + train_supp_pow2, data)

summary(reg)

plot(df$log ~ df$Date, col = "#0008ff", ylab = "log(USD/MNX)", type = "l")
lines(fitted(reg) ~ df$Date, col = "#000000")


predicted_data <- predict(reg,
newdata = list(train_supp = train_supp[train_data_len+1:tr_len],
train_supp_pow2 = train_supp_pow2[train_data_len + 1:tr_len]))
temp <- df$log[train_data_len + 1:data_len]
temp <- temp[1:predict_len]
plot.ts(temp)
lines(predicted_data, col = 2)

plot.ts(df$log)
vec <- c(fitted(reg), predicted_data)
lines(vec, col = 2)
lines(fitted(reg), col = 3)


prediction_diff <- df$log[train_data_len + 1:data_len] - predicted_data
prediction_diff <- prediction_diff[1:predict_len]
plot(prediction_diff, type = 'l')
abline(h = 0, col = 2)

MSE <- mean(prediction_diff^2)
RMSE <- sqrt(MSE)
temp_arr <- df$log[train_data_len + 1:data_len]
temp_arr <- temp_arr[1: predict_len]
temp <- var(temp_arr)
R2 <- 1 - MSE / temp

print(MSE)
print(RMSE)
print(R2)
