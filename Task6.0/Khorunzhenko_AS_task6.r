setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("temp_wage.tsv", skip = 3)
plot(y = df$wage, x = df$temp, xlab = "Temp", ylab = "Wage", type = "p",
main = "График зарплаты от температуры", pch = 19, col = "#000000")
#график зарплаты от температуры
cor_xy <- cor(df$temp, df$wage) # считаем корреляцию температуры и зарплаты
reg_xy <- lm(df$wage ~ df$temp) # строим регрессию
reg_yx <- lm(df$temp ~ df$wage) # снова строим регрессию
summary_1 <- summary(reg_xy) # коэф r^2 будет сейм для них
summary_2 <- summary(reg_yx)
plot(y = df$temp, x = reg_xy$fitted.values)
r_quad <- 0.2591 # Достали ручками r^2
is_true <- round(summary_$r.squared, digits = 7) ==
           round(cor_xy * cor_xy, digits = 7)
# проверили что r^2 == cor_xy^2
abline(reg_xy, col = "red")
plot(x = df$wage, y = reg_xy$fitted.values,
xlab = "Wage", ylab = "Regression", type = "p",
main = "Фактическая зарплата от расчётных значений зарплаты",
pch = 19, col = "#000000")
abline(c(0, 1), col = "red")
is_true <- round(summary_1$r.squared, digits = 7) ==
           round(summary_2$r.squared, digits = 7)
a1 <- summary_1$coefficients[1]
b1 <- summary_1$coefficients[2]
b2 <- summary_2$coefficients[2]
a2 <- -summary_2$coefficients[1] / b2
b2 <- 1 / b2
abline(c(a1, b1), col = "red")
abline(c(a2, b2), col = "green")
abline(v = mean(df$temp), col = "purple")
abline(h = mean(df$wage), col = "purple")
