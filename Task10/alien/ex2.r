setwd("C:/Users/anast/Desktop/Rprojects")
df <- read.csv("monthly-car-sales.csv",skip=2)
plot.ts(df$Sales);
grid()

df2 <- read.csv("monthly-mean-temp.csv",skip=2)
plot.ts(df2$Temperature);
grid()

cor(df$Sales,df2$Temperature[1:108])
cor.test(df$Sales,df2$Temperature[1:108])

m <- factor(rep(1:12,14))
