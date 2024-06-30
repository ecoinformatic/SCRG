library(metafor)
library(Matrix)

source("ecoinfoscrg/R/MetaAnalysis/wranglingCleaning.R")
source("ecoinfoscrg/R/MetaAnalysis/standardize.R")
source("ecoinfoscrg/R/MetaAnalysis/getBetas.R")
source("ecoinfoscrg/R/MetaAnalysis/varCov.R")

# Model output for each study can be found here:
chocBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/chocContinuous_average_betas.rds")
pensBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/pensContinuous_average_betas.rds")
IRLBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/IRLContinuous_average_betas.rds")
tampaBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/tampaContinuous_average_betas.rds")


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

# Define the maximum allowed variance
max_allowed_variance <- (1 / .Machine$double.eps) * smallest_eigenvalue

# Adjust eigenvalues
adjusted_eigenvalues <- pmin(eigenvalues, max_allowed_variance)
adjusted_cov_matrix <- eigenvectors %*% diag(adjusted_eigenvalues) %*% t(eigenvectors)

# View(adjusted_cov_matrix)

# ##### testing #####
yi <- as.numeric(unlist(combined_betas_only[1,]))
# ##############

# Fit the meta-analytic model using rma.mv from the metafor package
meta_result <- rma.mv(
  yi = yi,
  V = adjusted_cov_matrix
)

summary(meta_result)










