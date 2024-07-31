library(metafor)
library(Matrix)

source("ecoinfoscrg/R/getBetas.R")
source("ecoinfoscrg/R/varCov.R")
source("ecoinfoscrg/R/effectSize.R")

# Model output for each study can be found here:
chocBetas <- readRDS("output/chocContinuous_average_betas.rds")
pensBetas <- readRDS("output/pensContinuous_average_betas.rds")
IRLBetas <- readRDS("output/IRLContinuous_average_betas.rds")
tampaBetas <- readRDS("output/tampaContinuous_average_betas.rds")

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

# Fit the meta-analytic model using rma.mv from the metafor package
pred$study <- NULL
pred$SMMv5Def <- NULL
formula <- as.formula(paste("~", paste(colnames(pred)[-1], collapse = " + ")))

pred_t <- t(pred)
pred_t <- as.data.frame(t(pred_t))
colnames(pred_t) <- colnames(pred)


# If it's a dataframe and should be a vector
# overall_effect <- overall_effect$overall_effect
# overall_effect <- as.data.frame(t(overall_effect))
# rownames(overall_effect) <- colnames(pred)



 
meta_result <- rma.mv( 
  yi = overall_effect, # need to fix issues here
  V = adjusted_cov_matrix,
  mods = formula,
  data = pred_t
)

summary(meta_result)

##############################
# SANDBOX 
##############################
# meta_result <- rma.mv( 
#   yi = overall_effect, # need to fix issues here
#   V = adjusted_cov_matrix,
#   mods = formula,
#   method = "REML"
# )


# pred$study <- as.vector(study$study) # column in data containing study info
pred$study <- as.factor(study$study)
pred$overall_effect <- overall_effect
pred$variances <- variances

variances <- overall_standard_error^2
result <- rma.mv(yi = overall_effect, V = variances, method = "REML", random = ~ 1 | study, data = pred) # note, if have "data", then variable should match column names??? Otherwise can stand alone?

result <- rma.mv(yi = overall_effect, V = variances, method = "REML", random = ~ 1 | study)











# Assume combined_betas_only is your current 4 x 426 dataframe
# Convert it from wide to long format
library(tidyr)
library(dplyr)

data_long <- combined_betas_only %>%
  mutate(study = rownames(.)) %>%
  pivot_longer(cols = -study, names_to = "predictor", values_to = "beta")

# Assuming you have the variances for each beta in a similar matrix:
variance_matrix <- cov_matrix

# Convert variance matrix to a long format that matches data_long
variance_long <- as.data.frame(variance_matrix) %>%
  mutate(predictor = rownames(.)) %>%
  pivot_longer(cols = -predictor, names_to = "study", values_to = "variance")

# Merge variances back into your long data
data_long <- left_join(data_long, variance_long, by = c("study", "predictor"))

# Example: Meta-analysis for one predictor, say 'angle'
angle_data <- data_long %>% 
  filter(predictor == "angle")

# Run the meta-analysis
result_angle <- rma.mv(yi = beta, V = variance, method = "REML", random = ~ 1 | study, data = angle_data)
summary(result_angle)


# angle_data$variance <- rep(0.01, nrow(angle_data))  # Example constant variance






# Preparing the data for meta-analysis
meta_data <- data.frame(
  beta = as.vector(t(combined_betas_only)),  # Converting beta coefficients to a vector
  study = rep(study_labels, each = ncol(combined_betas_only))  # Repeating study labels
)


# Extracting the variance-covariance matrix for each predictor from 'cov_matrix'
# This step assumes that each row in 'combined_betas_only' corresponds to a predictor in 'cov_matrix'
variance_matrices <- lapply(1:nrow(combined_betas_only), function(i) {
  matrix(cov_matrix[i, i, drop = FALSE], nrow = 1, ncol = 1)
})









# Assuming 'combined_betas_only' has your beta coefficients with predictors in columns and studies as rows
# and 'cov_matrix' is the complete variance-covariance matrix for these betas

# Flatten the betas into a single vector and prepare corresponding study identifiers
betas_vector <- as.vector(t(combined_betas_only)) 
study_vector <- rep(study, each = ncol(combined_betas_only))

# Running the combined meta-analysis model
result <- rma.mv(
  yi = betas_vector,                     # Vector of all beta coefficients
  V = cov_matrix,                        # Complete variance-covariance matrix
  method = "REML",
  random = ~ 1 | factor(study_vector),   # Random effects for studies
  mods = ~ 0 + rownames(cov_matrix)      # Including predictors as fixed effects without an intercept
)











# Flatten the beta coefficients
betas_vector <- as.vector(t(combined_betas_only))

# Create a study vector corresponding to each beta
study_labels <- c("choc", "pens", "tampa", "IRL") # Assuming these are the labels
study_vector <- rep(study_labels, each = ncol(combined_betas_only))

# Extract variances (diagonal elements) from the covariance matrix
variances_vector <- diag(cov_matrix)

# Repeat the variances for each study
variances_vector <- rep(variances_vector, times = length(study_labels))

# Combine all relevant data into a single data frame
meta_data <- data.frame(
  study = factor(study_vector),
  beta = betas_vector,
  variance = variances_vector,
  predictor = rep(colnames(combined_betas_only), times = length(study_labels))
)

# Generate a block diagonal variance-covariance matrix
block_cov_matrix <- bdiag(replicate(length(study_labels), cov_matrix, simplify = FALSE))

# Convert to a dense matrix if necessary
block_cov_matrix <- as.matrix(block_cov_matrix)

# Running the combined meta-analysis model
result <- rma.mv(
  yi = beta,                               # Vector of all beta coefficients
  V = block_cov_matrix,                    # Complete variance-covariance matrix
  method = "REML",
  random = ~ 1 | study,          # Random effects for studies
  mods = ~ 0 + predictor,         # Including predictors as fixed effects without an intercept
  data = meta_data
) 
# Gets:
## Error: Final variance-covariance matrix not positive definite.
## In addition: Warning message:
## 'V' appears to be not positive definite. 

# Check the results
summary(result)
