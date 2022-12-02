# Генерируем исходные данные
n <- 15 + 70
e <- rnorm(n, 0, 1)
x <- rnorm(n, 3, 4)
y <- -5 + 2 * x + e
# Тестируем
t.test(y)
cor.test(x, y)
t.test(scale(x)*scale(y))
t.test(x, y, paired = TRUE, var.equal = FALSE)
t.test(x, y, paired = FALSE, var.equal = FALSE)
psych::describe(x)
psych::describe(y)
t.test(scale(y)^3)
t.test(scale(y)^4 - 3)
# Строим гистограмму и график
hist(y, breaks = seq(round(min(y)) - 1, round(max(y)) + 1, 0.5), freq = FALSE)
curve(dnorm(x, mean(y), sd(y)), add = TRUE,
 col = 3, lwd = 3)
qqnorm(y)
qqline(y, col = "red")
shapiro.test(y)
# Доп
hist(x, breaks = seq(round(min(x)) - 1, round(max(x)) + 1, 0.5), freq = FALSE)
curve(dnorm(y, mean(x), sd(x)), add = TRUE,
 col = 3, lwd = 3)
qqnorm(x)
qqline(x, col = "red")
shapiro.test(x)
