# Assume you have a vector of predicted values and a vector of actual values
predicted <- c(1, 0, 1, 1, 0, 0)
actual <- c(1, 0, 1, 0, 0, 1)

# Create a contingency table using the table function
contingency_table <- table(predicted, actual)

# Extract the values for TP, FP, TN, and FN from the contingency table
TP <- contingency_table[1,1]
FP <- contingency_table[2,1]
TN <- contingency_table[2,2]
FN <- contingency_table[1,2]

# Bind the values together into a single data frame
results_table <- data.frame(TP, FP, TN, FN)

# Print the results table
print(results_table)
