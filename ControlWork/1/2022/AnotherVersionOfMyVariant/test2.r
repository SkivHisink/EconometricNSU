# Install the ccxtr package if it is not already installed
install.packages("ccxtr")
# Load the ccxtr package
library(ccxtr)

# Set the start and end dates for the data
start <- "2017-07-23"
end <- "2018-11-29"

# Download the daily data for USDT and XRP
usdt <- ccxt_ohlcv(exchange = "bitfinex", symbol = "USDT/USD", timeframe = "1d", since = start, until = end)
xrp <- ccxt_ohlcv(exchange = "bitfinex", symbol = "XRP/USD", timeframe = "1d", since = start, until = end)

# Extract the close data from the USDT and XRP data frames
close.usdt <- usdt$close
close.xrp <- xrp$close

# Calculate the returns in percent for USDT and XRP
returns.usdt <- (close.usdt - lag(close.usdt)) / lag(close.usdt)
returns.xrp <- (close.xrp - lag(close.xrp)) / lag(close.xrp)

# Calculate the signs of the returns for USDT and XRP
signs.usdt <- ifelse(returns.usdt > 0, "+", "-")
signs.xrp <- ifelse(returns.xrp > 0, "+", "-")

# Create a data frame with the signs of the returns for USDT and XRP
df <- data.frame(usdt = signs.usdt, xrp = signs.xrp)

# Calculate the frequency table for the entire data frame
freq.table <- table(df)

# Calculate the frequency tables for the rows and columns of the data frame
freq.table.rows <- prop.table(freq.table, 1)
freq.table.cols <- prop.table(freq.table, 2)

# Calculate the proportion of "+" signs in each row of the frequency table
prop.rows <- prop.table(freq.table, 1)

# Extract the proportions for the "+" signs
prop.plus <- prop.rows[2,]

# Calculate the percentage of "+" signs that matched the two rows
percentage <- sum(prop.plus) * 100

# Calculate the counts of the signs in each row and column of the frequency table
counts.rows <- table(df, 1)
counts.cols <- table(df, 2)

# Extract the counts for the "+" signs in the first row and the "-" signs in the second column
counts.plus <- counts.rows[2,1]
counts.minus <- counts.cols[1,2]

# Calculate the percentage of "+" signs in the first row among the "-" signs in the second column
percentage <- counts.plus / counts.minus * 100

# Calculate the proportions of the signs in each row and column of the frequency table
prop.rows <- prop.table(freq.table, 1)
prop.cols <- prop.table(freq.table, 2)

# Extract the proportions for the "+" signs in the first row and the "-" signs in the second column
prop.plus <- prop.rows[2,1]

# Plot the distribution of returns for USDT
hist(returns.usdt, main = "Distribution of Returns for USDT", xlab = "Returns", ylab = "Frequency")

# Plot the distribution of returns for XRP
hist(returns.xrp, main = "Distribution of Returns for XRP", xlab = "Returns", ylab = "Frequency")

# Install the ggplot2 package if it is not already installed
install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)

# Create a data frame with the returns of the second currency and the signs of the returns of the first currency
df <- data.frame(xrp = returns.xrp, usdt = signs.usdt)

# Create a scatterplot of the returns of the second currency against the signs of the returns of the first currency
plot <- ggplot(df, aes(x = usdt, y = xrp)) + geom_point()

# Add a linear regression line to the scatterplot
plot <- plot + geom_smooth(method = "lm")

# Display the plot
plot

# Install the broom and ggplot2 packages if they are not already installed
install.packages(c("broom", "ggplot2"))

# Load the broom and ggplot2 packages
library(broom)
library(ggplot2)

# Fit a linear regression model to the data
model <- lm(xrp ~ usdt, data = df)

# Extract the confidence intervals for the coefficients from the model
ci <- augment(model)

# Extract the confidence intervals for the coefficient at the sign variable
sign.ci <- ci[2, c("lower", "upper")]

# Rename the columns of the confidence interval data frame
names(sign.ci) <- c("Lower Bound", "Upper Bound")

# Add the coefficient estimate to the confidence interval data frame
sign.ci$Estimate <- coef(model)[2]

# Convert the confidence interval data frame to a long format
sign.ci.long <- melt(sign.ci, id.vars = "Estimate")

# Create a bar plot of the confidence intervals using the ggplot2 package
plot <- ggplot(sign.ci.long, aes(x = variable, y = value)) + geom_bar(stat = "identity")

# Add a horizontal line to the plot showing the estimate of the coefficient
plot <- plot + geom_hline(aes(yintercept = Estimate), lty = 2)

# Display the plot
plot
