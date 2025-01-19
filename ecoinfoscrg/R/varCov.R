# Source getBetas.R script for predictors
source("R/getBetas.R")

# Load final models from model selection
choc_mod_new <- readRDS("data/choc_non_parallel_new_final_model.rds")
pens_mod_new <- readRDS("data/pens_non_parallel_new_final_model.rds")
tampa_mod_new <- readRDS("data/tampa_non_parallel_new_final_model.rds")
IRL_mod_new <- readRDS("data/IRL_non_parallel_new_final_model.rds")

# Only using variances for now
cov_matrix <- list(choc = ctmm::pd.solve(choc_mod_new$Hessian),
                   pens = ctmm::pd.solve(pens_mod_new$Hessian),
                   tampa = ctmm::pd.solve(tampa_mod_new$Hessian),
                   IRL = ctmm::pd.solve(IRL_mod_new$Hessian))

for (i in 1:length(cov_matrix)) {

  # # Symmetrization of matrices
  # cov_matrix[[i]] <- (cov_matrix[[i]] + t(cov_matrix[[i]]))/2

  # Remove intercepts
  cov_matrix[[i]] <- cov_matrix[[i]][1:(dim(cov_matrix[[i]])[[1]]-2),
                                     1:(dim(cov_matrix[[i]])[[2]]-2)]
}

# Find where there's missing values
missing_values <- list()
for (i in 1:length(cov_matrix)) {

  # Find non-selected predictors
  selected <- dimnames(cov_matrix[[i]])
  missing_preds <- setdiff(numeric_pred_cols, selected[[1]])
  missing_col <- matrix(NA, length(selected[[1]]), length(missing_preds))
  missing_row <- matrix(NA, length(missing_preds), length(c(selected[[1]], missing_preds)))
  cov_matrix[[i]] <- cbind(cov_matrix[[i]], missing_col)
  cov_matrix[[i]] <- rbind(cov_matrix[[i]], missing_row)
  dimnames(cov_matrix[[i]]) <- list(c(selected[[1]], missing_preds),
                                    c(selected[[1]], missing_preds))

  # Predictors with missing values
  missing_values[[i]] <- is.na(cov_matrix[[i]])

  # Set missing off-diagonals to zero
  cov_matrix[[i]][missing_values[[i]] & !row(cov_matrix[[i]]) == col(cov_matrix[[i]])] <- 0

  # Set missing variances to very large value
  # large_value <- 10000
  large_value <- (.Machine$double.eps)^(-1/3)
  diag(cov_matrix[[i]])[missing_values[[i]][diag(TRUE, nrow(cov_matrix[[i]]))]] <- large_value
  # Set infinite variances to very large value
  diag(cov_matrix[[i]])[which(is.infinite(diag(cov_matrix[[i]])))] <- large_value
  # Set very large variances to very large value
  diag(cov_matrix[[i]])[which(diag(cov_matrix[[i]])>large_value)] <- large_value

}
# View(cov_matrix)

# save(cov_matrix, file = "../output/cov_matrices.rda")


# Grab variances from each matrix
VAR <- combined_betas_only  # simulate structure of combined betas dataframe
for (i in 1:nrow(VAR)) {
  for (j in 1:ncol(VAR)) {

    # Add variances to corresponding predictor and study
    VAR[i,j] <- diag(cov_matrix[[i]])[which(names(diag(cov_matrix[[i]])) == colnames(VAR)[j])]
  }
}

# View(VAR)

# save(VAR, file = "../output/variances.rda")





## OLD CODE ##

# # Generate covariance matrix
# # cov_matrix <- cov(combined_betas_only, use = "pairwise.complete.obs") # calculates the correlation between each pair of variables using all complete pairs of observations for those variables
#
# # Predictors with missing values
# missing_values <- is.na(cov_matrix)
#
# # Set missing off-diagonals to zero
# cov_matrix[missing_values & !row(cov_matrix) == col(cov_matrix)] <- 0
#
# # Set missing variances to very large value
# # large_value <- 10000
# large_value <- (.Machine$double.eps)^(-1/3)
# diag(cov_matrix)[missing_values[diag(TRUE, nrow(cov_matrix))]] <- large_value
# # Set infinite variances to very large value
# # diag(cov_matrix)[which(is.infinite(diag(cov_matrix[[3]])))] <- large_value

# # Retrieve covariance matrices from model outputs
# cov_matrix <- list(choc = vcov(choc_mod_new), pens = vcov(pens_mod_new),
#                    tampa = vcov(tampa_mod_new), IRL = vcov(IRL_mod_new))



