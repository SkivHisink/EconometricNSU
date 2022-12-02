# загружаем либу
install.packages(c("rstudioapi"), repos = "https://cran.r-project.org/")
# cтавим чтение в папку где расположен наш файл
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# грузим данные
df <- read.delim("kuiper.tsv", stringsAsFactors = TRUE, skip = 4)
head(df) # проверяем корректность загруженных данных
reg <- lm(log(df$Price) ~ df$Mileage + df$Cylinder + df$Liter +
I(df$Doors == 2) + df$Cruise + df$Sound + df$Leather) # строим регрессию
summary(reg) # смотрим основные статистические характеристики
new_reg <- lm(log(df$Price) ~ df$Mileage + df$Cylinder + df$Liter + df$Type +
df$Cruise + df$Sound + df$Leather) # стриоим другую регрессию
summary(new_reg) # смотрим основные статистические характеристики
table(df$Type, df$Doors)
anova(reg, new_reg) # Проводим тест на добовление пременной
# Находим разность расчётных значений для двух регрессий
dif <- fitted(new_reg) - fitted(reg)
plot(dif, col = "#ff0000") # Анализируем различие
abline(h = 0, col = "#0000ff", lwd = 2) # Добавляем линию нуля
###
# грузим данные
df <- read.delim("StreetLight2020.tsv", stringsAsFactors = TRUE, skip = 5)
head(df)
plot(df$actual ~ df$estimated)
abline(c(0, 1), col = "#0000ff", lwd = 1)
reg <- lm(df$actual ~ df$estimated)
summary(reg)
confint(reg)
reg_1 <- lm(df$actual - df$estimated ~ df$estimated)
summary(reg_1)
confint(reg_1)
reg_2 <- lm(df$actual - df$estimated ~ 1)
summary(reg_2)
reg_3 <- lm(df$actual ~ df$estimated + I(df$estimated^2))
summary(reg_3)
reg_4 <- lm(df$actual - df$estimated ~ df$estimated + I(df$estimated^2))
summary(reg_4)
reg_5 <- lm(df$actual - df$estimated ~ 1)
anova(reg_5, reg_4)
reg_5 <- lm(df$actual - df$estimated ~ 0)
anova(reg_5, reg_4)
