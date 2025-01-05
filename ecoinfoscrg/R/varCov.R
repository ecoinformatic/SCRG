# Load final models from model selection
choc_mod_fix <- readRDS("data/choc_non_parallel_fix_final_model.rds")
pens_mod_fix <- readRDS("data/pens_non_parallel_fix_final_model.rds")
tampa_mod_fix <- readRDS("data/tampa_non_parallel_fix_final_model.rds")
IRL_mod_fix <- readRDS("data/IRL_non_parallel_fix_final_model.rds")

# Generate covariance matrix
# cov_matrix <- cov(combined_betas_only, use = "pairwise.complete.obs") # calculates the correlation between each pair of variables using all complete pairs of observations for those variables

# Retrieve covariance matrices from model outputs
cov_matrix <- list(choc = choc_mod_fix$Hessian, pens = pens_mod_fix$Hessian,
                   tampa = tampa_mod_fix$Hessian, IRL = IRL_mod_fix$Hessian)


# Find where there's missing values
missing_values <- list()
for (i in 1:length(cov_matrix)) {
missing_values[[i]] <- is.na(cov_matrix[[i]])
}

# Set missing off-diagonals to zero
for (i in 1:length(cov_matrix)) {
cov_matrix[[i]][missing_values[[i]] & !row(cov_matrix[[i]]) == col(cov_matrix[[i]])] <- 0
}

# Set missing variances to very large value
large_value <- 10000
for (i in 1:length(cov_matrix)) {
diag(cov_matrix[[i]])[missing_values[[i]][diag(TRUE, nrow(cov_matrix[[i]]))]] <- large_value
}
# View(cov_matrix)


