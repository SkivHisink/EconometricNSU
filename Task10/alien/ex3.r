setwd("C:/Users/anast/Desktop/Rprojects")

sale = read.csv("monthly-car-sales.csv", skip = 2)        

n = length(sale$Sales) 
ts1 = rnorm(n, mean = 10, sd = 2)+sin(c(3:(n+2))*pi/6)*rnorm(n, mean = 5, sd = 2)
plot(ts1 ~ c(1:n), type = "l")
plot(sale$Sales ~ c(1:n), type = "l")

cor(sale$Sales,ts1)            

rm(n,ts1)

sale$Month = c(1:12,1:12,1:12,1:12,1:12,1:12,1:12,1:12,1:12)
sale$t = c(1:108)
sale = sale[,c(2,1,3)]

train = sale[c(1:81),]
test  = sale[c(82:108),]

m1 = lm(data = train, Sales ~ factor(Month) + t)
summary(m1)

forecast = predict(m1, newdata = test)

plot(data = test, Sales ~ t, type = "l")
lines(forecast ~ test$t, col = "red")

plot(test$Sales - forecast ~ test$t)
