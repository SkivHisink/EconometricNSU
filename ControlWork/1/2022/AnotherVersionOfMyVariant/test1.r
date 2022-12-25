# E[x] = E[a0 + a1 * eps + a2 * psi]
# E[x] = E[a0] + E[a1 * eps] + E[a2 * psi]
# E[x] = a0 + 0 + 0 = a0
# Var[x] = E[(x - E[x])^2]
# Var[x] = E[(a0 + a1 * eps + a2 * psi - a0)^2]
# Var[x] = E[a1^2 * eps^2] + E[a2^2 * psi^2] + 2 * E[a1 * a2 * eps * psi]
# Var[x] = a1^2 * Var[eps] + a2^2 * Var[psi] + 2 * a1 * a2 * E[eps] * E[psi]
# Cov[eps, psi] = E[eps * psi] - E[eps] * E[psi] = 0 - 0 * 0 = 0
# Var[eps] = 4
# Var[psi] = 9
# Var[x] = a1^2 * Var[eps] + a2^2 * Var[psi] + 2 * a1 * a2 * Cov[eps, psi]
# Var[x] = 1^2 * 4 + (-6)^2 * 9 + 2 * 1 * -6 * 0
# Var[x] = 328
# E[x] = 3
# sd[x] = 18.11
# E[y] = 5
# Var[y] = b1^2 * Var[eps] + b2^2 * Var[psi] + 2 * b1 * b2 * Cov[eps, psi]
# Var[y] = 7^2 * Var[eps] + (-2)^2 * Var[psi]
# Var[y] = 49 * 4 + 4 * 9 = 232
# Sd[y] = 15.232
# Cov[x, y] = E[(a0 + a1 * eps + a2 * psi - a0)(b0 + b1 * eps + b2 * psi - b0)]
# Cov[x, y] = E[a1 * b1 * eps^2] + E[a2 * b2 * psi^2] + E[(a1 * b2 + a2 * b1) * eps * psi]
# Corr[x, y] = Cov[x, y] / (Std[x] * Std[y])

# Set the seed of the random number generator
set.seed(123)

# Set the number of samples
N <- 160

# Generate samples of the error variables
eps <- rnorm(N, mean = 0, sd = 2)
psi <- rnorm(N, mean = 0, sd = 3)

# Calculate the values of x and y
x <- 3 + 1 * eps - 6 * psi
y <- 5 + 7 * eps - 2 * psi

# Calculate the sample means of x and y
mean_x <- mean(x)
mean_y <- mean(y)

# Calculate the sample variances of x and y
var_x <- var(x)
var_y <- var(y)

# Calculate the sample standard deviations of x and y
sd_x <- sd(x)
sd_y <- sd(y)

# Calculate the sample correlation coefficient of x and y
r <- cor(x, y)

# Perform a hypothesis test to determine whether there is a significant correlation between x and y
cor.test.result <- cor.test(x, y, conf.level = 0.95)

# Extract the confidence interval from the test result
ci <- cor.test.result$conf.int

# Plot the confidence interval
plot(ci, type = "l", xlab = "", ylab = "Correlation coefficient", main = "95% Confidence Interval for the Correlation Coefficient")
abline(h = 0, col = "#ff0000")

# Perform a hypothesis test to determine whether the theoretical correlation coefficient is equal to the calculated value
cor.test.result <- cor.test(x, y, r = r, alternative = "two.sided", conf.level = 0.99)

# Extract the p-value from the test result
p.value <- cor.test.result$p.value

# Test the null hypothesis at the 1% level
if (p.value < 0.01) {
  print("Reject the null hypothesis at the 1% level.")
} else {
  print("Fail to reject the null hypothesis at the 1% level.")
}

# Fit a linear regression model to the data
model <- lm(y ~ x)

# Extract the residuals from the model
residuals <- model$residuals

# Extract the fitted values from the model
fitted.values <- model$fitted.values

# Plot the residuals against the fitted values
plot(fitted.values, residuals, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "#ff0000")

# Load the ggplot2 package
library(ggplot2)

# Fit a linear regression model to the data
model <- lm(y ~ x)

# Calculate the residuals
residuals <- residuals(model)

df <- data.frame(x = x, y = y)
# Add the residuals to the data frame
df$residuals <- residuals
# Plot the residuals
ggplot(df, aes(x = x, y = residuals)) +
  geom_point() +
  labs(x = "x", y = "Residuals")


