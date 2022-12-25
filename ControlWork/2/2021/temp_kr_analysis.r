# Load the necessary packages
library(stats)
library(ggplot2)
library(fpc)
library(clusterCrit)

# Load the iris dataset
data(iris)

# Perform k-means clustering with k = 3
result <- kmeans(iris[, 1:4], 3)

# Add the cluster assignments as a column to the iris data frame
iris$cluster <- result$cluster

# Visualize the data using a scatter plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = factor(cluster))) +
  geom_point() +
  labs(title = "Iris Data Clustered with k-Means", x = "Sepal Length", y = "Sepal Width")

# Calculate the sum of squared distances (within-cluster sum of squares)
ssplot(iris[, 1:4], result$cluster)

# Calculate the silhouette coefficient
silhouette(iris[, 1:4], result$cluster)

# Calculate the Calinski-Harabasz index
chindex(iris[, 1:4], result$cluster)

# If you have a labeled dataset, you can also calculate external evaluation measures
# For example, if you have a column in the iris data frame called "species" with the true cluster assignments:

# Calculate the adjusted Rand index
ari(iris$species, result$cluster)

# Calculate the adjusted mutual information
ami(iris$species, result$cluster)

# Calculate the Fowlkes-Mallows index
fm(iris$species, result$cluster)