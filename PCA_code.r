# Principal Component Analysis (PCA) in R

# This R script demonstrates how to perform PCA on a dataset for dimensionality reduction.
# It includes detailed comments to guide you through each step.

# Steps:
# 1. Load the necessary library for PCA.
# 2. Load your dataset and inspect its structure.
# 3. Separate features from the target variable if applicable.
# 4. Standardize features to have zero mean and unit variance.
# 5. Apply PCA using the 'prcomp' function.
# 6. Explore and visualize the variance explained by each principal component.
# 7. Choose the number of principal components based on cumulative variance.
# 8. Retain selected components and create a new dataset.
# 9. Save PCA results or use them for further analysis.

# Note: Replace "your_dataset.csv" with the actual dataset file path.
# Adjustments



# Load required library for PCA
library(stats)

# Load your dataset (replace 'your_dataset.csv' with your actual dataset file)
data <- read.csv("your_dataset.csv")

# Check the structure of the dataset
str(data)

# Separate the features (variables) from the target variable (if applicable)
# For example, assuming the target variable is in the last column:
target_column <- ncol(data)
features <- data[, -target_column]

# Standardize the features to have zero mean and unit variance
standardized_features <- scale(features)

# Apply PCA
pca_result <- prcomp(standardized_features, center = TRUE, scale. = TRUE)

# Extract and print the proportion of variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cat("Proportion of Variance Explained by Each Principal Component:\n")
print(variance_explained)

# Plot the cumulative proportion of variance explained
plot(cumsum(variance_explained), xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     main = "Cumulative Proportion of Variance Explained by Principal Components")

# Choose the number of principal components based on the cumulative proportion
# For example, choose components explaining 95% of the variance
num_components <- which(cumsum(variance_explained) >= 0.95)[1]
cat("\nNumber of Principal Components Chosen:", num_components, "\n")

# Retain only the selected number of principal components
pca_features <- as.data.frame(pca_result$x[, 1:num_components])

# Optional: Add the target variable back if separated
# For example, assuming the target variable is in the last column
if (exists("target_column")) {
  target_variable <- data[, target_column, drop = FALSE]
  pca_dataset <- cbind(pca_features, target_variable)
} else {
  pca_dataset <- pca_features
}

# Save the PCA results or use them for further analysis
# For example, save the results to a new CSV file
write.csv(pca_dataset, "pca_results.csv", row.names = FALSE)
