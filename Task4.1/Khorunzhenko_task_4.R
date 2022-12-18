install.packages("psych")
n <- 15 + 70
e <- rnorm(n, 0, 1)
x <- rnorm(n, 3, 4)
y <- -5 + 2 * x + e
mean(x)
mean(y)
# несмещенная
var(x)
var(y)
# смещенная
var(x) * (n - 1) / (n)
var(y) * (n - 1) / (n)
#cov and cor
cov(x, y)
cor(x, y)
plot(x, y, pch = '*')
rug(x, ticksize = 0.03, side = 1, lwd = 0.5, col = 6,
    quiet = getOption("warn") < 0)
rug(y, ticksize = 0.03, side = 2, lwd = 0.5, col = 4,
    quiet = getOption("warn") < 0)
grid()
abline(h = mean(x), col = "#911a1a", lty = 3)
abline(v = mean(y), col = "#911a1a", lty = 3)
legend(-5, 10, paste0("r=", cor(x, y)))
# creating selective cor matrix
mat <- cbind(x, y, e)
smat <- cor(mat)
library(psych)
pairs.panels(mat)
# creating theoretical cor matrix
# theoretical var
t_var_x <- 4
t_var_y <- 4 * t_var_x + 1
t_var_e <- 1
# theoretical cor
cor_s_x <- c(1, 2 * t_var_x / ((t_var_x * t_var_y)^(1 / 2)), 0)
cor_s_y <- c(2 * t_var_x / ((t_var_x * t_var_y)^(1 / 2)), 1,
    t_var_e / ((t_var_e * t_var_y)^(1 / 2)))
cor_s_e <- c(0, t_var_e / ((t_var_e * t_var_y)^(1 / 2)), 1)
tmat <- rbind(cor_s_x, cor_s_y, cor_s_e)
colnames(tmat) <- c("x", "y", "e")
rownames(tmat) <- c("x", "y", "e")
# result
tmat
smat
