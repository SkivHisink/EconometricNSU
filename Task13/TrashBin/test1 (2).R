setwd("C:/Users/bazof/OneDrive/Martins Drop/Ёконометри€/12")

sale = read.csv("monthly_champagne_sales.csv", skip = 5)

train = sale[c(1:81),]

library(forecast)
train$Sales = ts(data = train$Sales, start = c(1964,1), frequency = 12)


m1 = auto.arima(train$Sales, seasonal = TRUE)
summary(m1)
print(m1)

forecast = predict(m1,24)

plot(forecast(m1,24))
lines(ts(data = sale$Sales, start = c(1964,1), frequency = 12), col = "red", lwd =2)


plot(forecast$pred - sale$Sales[82:105])

