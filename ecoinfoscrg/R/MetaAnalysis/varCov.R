# Generate covariance matrix
cov_matrix <- cov(combined_betas_only, use = "pairwise.complete.obs")

# Find where there's missing values
missing_values <- is.na(cov_matrix)

# Set missing off-diagonals to zero
cov_matrix[missing_values & !row(cov_matrix) == col(cov_matrix)] <- 0

# Set missing variances to a very large value
large_value <- 1000000 # arbitrary; might need to adjust later
diag(cov_matrix)[missing_values[diag(TRUE, nrow(cov_matrix))]] <- large_value

###

# x <- c(2, 6 ,23, 2)
# y <- c(1,2,5,8)
# cov(x,y)

