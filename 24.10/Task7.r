install.packages(c('rstudioapi'),repos='https://cran.r-project.org/') # загружаем либу
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # cтавим чтение в папку где расположен наш файл
df <- read.delim(file='kuiper.tsv', skip= 4,
stringsAsFactors=TRUE) # читаем данные, скипаем шапку
head(df)
plot(df) # эксперементируем
# Первоначалный анализ
plot(df$Price)
hist(df$Price)
summary(df)
table(df$Make)
unique(df$Make)
table(df$Trim)
table(df$Type)
table(df$Cylinder)
table(df$Liter)
table(df$Leather)
table(df$Doors)
table(df$Cruise)
table(df$Sound)

table(df$Make, df$Model) # неоднозначно
table(df$Liter, df$Cylinder) #однозначно
table(df$Type, df$Doors) #однозначно
table(df$Type, df$Liter) #неоднозначно
table(df$Make, df$Trim) #неоднозначно
table(df$Make, df$Type) #неоднозначно
table(df$Make, df$Doors) #неоднозначно
table(df$Make, df$Sound) #неоднозначно
table(df$Cylinder, df$Doors) #неоднозначно
table(df$Cylinder, df$Type) #неоднозначно
table(df$Doors, df$Liter) #неоднозначно

reg <- lm(log(Price) ~ Liter, data = df)
summary(reg)
plot(reg)
sigma(reg)
sigma(lm(log(Price) ~ 1, data = df))
deviance(reg)
deviance(lm(log(Price) ~ 1, data = df))

r_quad <- 1 - deviance(reg) / deviance(lm(log(Price) ~ 1, data=df))
plot(log(Price) ~ Liter, data = df, asp = 1)
abline(reg, col=6, lwd = 2)
abline(c(0,1), col=4, lwd = 4)
plot(resid(reg) ~ fitted(reg), data=df, asp = 1)
abline(h = 0, col = 4, lwd = 4)
grid()
# Задание
df.num <- df[c("Price", "Mileage", "Cylinder", "Liter",
"Doors", "Cruise", "Sound", "Leather")]
names(df)
round(cor(df.num), 3)

psych::pairs.panels(df.num)
regno <- lm(Price ~ Liter, data = df)
summary(regno)
deviance(regno)

reg_big <- lm(log(Price) ~ Mileage + Cylinder +
Liter + Doors + Cruise + Sound + Liter, data = df.num)
summary(reg_big)

plot(log(df$Price) ~ fitted(reg_big))
abline(c(0, 1), col = 2, lwd = 2)

confint(reg_big)
plot(resid(reg_big) ~ fitted(reg_big), col = df$Cylinder)
legend("bottomright", lty = 1, col = df$Cylinder, legend = unique(df$Cylinder))
abline(h=0, col =2, lwd = 2)

plot(resid(reg_big) ~ fitted(reg_big), col = df$Make)
legend("bottomright", lty = 1, col = df$Make, legend = unique(df$Make))
abline(h = 0, col = 2, lwd = 2)

plot(resid(reg_big) ~ fitted(reg_big), col = df$Type)
legend("bottomright", lty = 1, col = df$Type, legend = unique(df$Type))
abline(h = 0, col = 2, lwd = 2)

reg_big <- lm(log(df.num$Price) ~ df.num$Mileage + df.num$Cylinder +
df.num$Doors + df.num$Cruise +
df.num$Sound + df.num$Liter + df.num$Leather)
summary(reg_big)
#-doors
reg_big <- lm(log(df.num$Price) ~ df.num$Mileage + df.num$Cylinder +
df.num$Cruise + df.num$Sound + df.num$Liter + df.num$Leather)
summary(reg_big)
#-doors-cylinder
reg_big <- lm(log(df.num$Price) ~ df.num$Mileage + df.num$Cruise +
df.num$Sound + df.num$Liter + df.num$Leather)
summary(reg_big)
#-doors-cylinder-sound
reg_big <- lm(log(df.num$Price) ~ df.num$Mileage + df.num$Cruise +
df.num$Liter + df.num$Leather)
summary(reg_big)
