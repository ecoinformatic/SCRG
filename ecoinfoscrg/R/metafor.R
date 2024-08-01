library(metafor)
library(Matrix)

source("R/getBetas.R")
source("R/varCov.R")

# Model output for each study can be found here:
chocBetas <- readRDS("../output/chocContinuous_average_betas.rds")
pensBetas <- readRDS("../output/pensContinuous_average_betas.rds")
IRLBetas <- readRDS("../output/IRLContinuous_average_betas.rds")
tampaBetas <- readRDS("../output/tampaContinuous_average_betas.rds")

###############
# Define predictor columns
predictor_columns <- colnames(combined_betas_only)

# Create the formula string dynamically
formula_string <- paste("~", paste(predictor_columns, collapse = " + "))
formula <- as.formula(formula_string)

# Prepare the beta estimates
betas <- combined_betas_only


###########
eigen_decomp <- eigen(cov_matrix)
eigenvalues <- eigen_decomp$values
eigenvectors <- eigen_decomp$vectors

# Find the smallest positive eigenvalue
smallest_eigenvalue <- min(eigenvalues[eigenvalues > 0])

# # Define the maximum allowed variance
# max_allowed_variance <- sqrt(1 / .Machine$double.eps) * smallest_eigenvalue

# Adjust eigenvalues
adjusted_eigenvalues <- pmax(eigenvalues, (.Machine$double.eps)^(1/3))
adjusted_eigenvalues <- pmin(eigenvalues, (.Machine$double.eps)^(-1/3))
# adjusted_eigenvalues <- pmin(eigenvalues, max_allowed_variance)

# adjusted_eigenvalues <- pmax(adjusted_eigenvalues, (.Machine$double.eps)^(1/3))
# adjusted_eigenvalues <- pmin(adjusted_eigenvalues, (.Machine$double.eps)^(-1/3))
adjusted_cov_matrix <- eigenvectors %*% diag(adjusted_eigenvalues) %*% t(eigenvectors)

# another check
smallest_eigenvalue <- min(adjusted_eigenvalues[adjusted_eigenvalues > 0])
smallest_eigenvalue

#####
# Recommendation: Add a small jitter to the diagonal to ensure positive definiteness
epsilon <- 1e-6
adjusted_cov_matrix <- adjusted_cov_matrix + diag(epsilon, nrow(adjusted_cov_matrix))
#####

##############################
# Meta-Analytic Regression
##############################
# Note: Testing with betas but need effect sizes (e.g. odd ratios) from BUPD.R model output

# Flatten beta coefficients
betas_vector <- as.vector(t(combined_betas_only))

# Get study vector corresponding to each beta (required for study random effect)
study_labels <- c("choc", "pens", "tampa", "IRL") # will need to change labels as more data is available
study_vector <- rep(study_labels, each = ncol(combined_betas_only))

# specify input data
beta = betas_vector
study = factor(study_vector)
predictor = rep(colnames(combined_betas_only), times = length(study_labels))
variance <- diag(cov_matrix)

# run metafor meta-analytic regression
result <- rma.mv(
  yi = beta, # Vector of all beta coefficients (may need effect size e.g. odds ratios from BUPD.R output instead)
  V = variance, # vector of length k with the corresponding sampling variances or a k x k variance-covariance matrix of the sampling errors
  method = "REML", # default
  random = ~ 1 | study, # Random effects for studies
  mods = ~ 0 + predictor # Including predictors as fixed effects without an intercept (AKA is predictors in formula)
)

summary(result)
