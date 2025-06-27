library(dplyr, quietly = TRUE)

############################
# GRAB MODEL OUTPUT
############################
# Original
# source("inst/scripts/wranglingCleaning.R")
# source("inst/scripts/standardize.R")
# Updated with Kriging
# source("inst/scripts/wranglingCleaning_kriging.R")
# source("inst/scripts/standardize_kriging.R")

# Retrieve cleaned predictor data
# pred <- readRDS("data/predictors_kriged_standardized.rds")
pred <- data$predictors
# pred <- as.data.frame(pred)  # convert to dataframe so "geometry" is not selected
studies <- unique(data$state$study)

# Check if `Betas` exists
if(!exists("Betas")) {
  stop("No betas found. Please assign beta coefficient estimates from selected models to a list named `Betas`.")
  } else { message(paste("Identified beta coefficient estimates from", length(Betas), "studies.")) }

# # Retrieve beta estimates
# chocBetas <- Betas[[1]]
# pensBetas <- Betas[[2]]
# IRLBetas <- Betas[[3]]
# tampaBetas <- Betas[[4]]

# chocBetas <- readRDS("data/choc_non_parallel_new_average_betas.rds")
# pensBetas <- readRDS("data/pens_non_parallel_new_average_betas.rds")
# IRLBetas <- readRDS("data/IRL_non_parallel_new_average_betas.rds")
# tampaBetas <- readRDS("data/tampa_non_parallel_new_average_betas.rds")

# Standardize betas (Z-score)
Betas[[1]][,1] <- (Betas[[1]][,1] + mean(Betas[[1]][,1]))/sd(Betas[[1]][,1])
Betas[[2]][,1] <- (Betas[[2]][,1] + mean(Betas[[2]][,1]))/sd(Betas[[2]][,1])
Betas[[3]][,1] <- (Betas[[3]][,1] + mean(Betas[[3]][,1]))/sd(Betas[[3]][,1])
Betas[[4]][,1] <- (Betas[[4]][,1] + mean(Betas[[4]][,1]))/sd(Betas[[4]][,1])

# chocBetas[,1] <- (chocBetas[,1] + mean(chocBetas[,1]))/sd(chocBetas[,1])
# pensBetas[,1] <- (pensBetas[,1] + mean(pensBetas[,1]))/sd(pensBetas[,1])
# tampaBetas[,1] <- (tampaBetas[,1] + mean(tampaBetas[,1]))/sd(tampaBetas[,1])
# IRLBetas[,1] <- (IRLBetas[,1] + mean(IRLBetas[,1]))/sd(IRLBetas[,1])

# Get predictors (excluding study and definitions)
pred <- pred %>%
  mutate(across(c(OBJECTID, ID, bmpCountv5, n, distance, X, Y), as.character))
## keep non predictor data as characters to avoid being selected as predictors
numeric_pred <- pred %>%
  select_if(is.numeric)
factor_pred <- pred %>%
  select_if(is.factor)
numeric_pred_cols <- colnames(cbind(factor_pred, numeric_pred))
# numeric_pred_cols <- numeric_pred_cols[-5]

# Function to convert model output to dataframe (only keep "Estimate")
prepare_df <- function(matrix, source, stat = c("beta", "se")) {
    df <- as.data.frame(t(matrix))
    if(stat == "beta") {
      df <- df[1, , drop = FALSE] # Keep "Estimate"
    } else if (stat == "se") {
      df <- df[2, , drop = FALSE] # Keep "Estimate"
    }
    colnames(df) <- rownames(matrix)
    missing_cols <- setdiff(numeric_pred_cols, rownames(matrix)) # missing predictors as NA
    df[missing_cols] <- NA
    df <- df[, numeric_pred_cols] # order columns
    df$study <- source
    return(df)
}

DF <- list()  # list to store dataframes of betas from each study
DF[[1]] <- prepare_df(Betas[[1]], studies[1], stat = "beta")
DF[[2]] <- prepare_df(Betas[[2]], studies[2], stat = "beta")
DF[[3]] <- prepare_df(Betas[[3]], studies[3], stat = "beta")
DF[[4]] <- prepare_df(Betas[[4]], studies[4], stat = "beta")


# Combine all dataframes
combined_betas <- do.call(rbind, DF)
combined_betas[] <- lapply(combined_betas, function(x) as.numeric(as.character(x))) # will probably get NAs

# Get column names from pred, set any other missing columns as NA
missing_cols <- setdiff(numeric_pred_cols, colnames(combined_betas))
combined_betas[missing_cols] <- NA


############################
# GENERATE AVERAGES AND REPLACE NA's WITH THEM
############################
# Get average for each row ignore NA
# combined_betas[] <- lapply(combined_betas, function(x) as.numeric(as.character(x)))
row_averages <- apply(combined_betas, 1, function(row) mean(row, na.rm = TRUE))

# choc_avg <- row_averages[1]
# pens_avg <- row_averages[2]
# tampa_avg <- row_averages[3]
# IRL_avg <- row_averages[4]

### WHY REPLACE WITH SITEWIDE AVG AND NOT VARIABLE AVG ACROSS SITES??? ###

# Replace NAs
combined_betas[1, ][is.na(combined_betas[1, ])] <- row_averages[1]
combined_betas[2, ][is.na(combined_betas[2, ])] <- row_averages[2]
combined_betas[3, ][is.na(combined_betas[3, ])] <- row_averages[3]
combined_betas[4, ][is.na(combined_betas[4, ])] <- row_averages[4]

# Add study column (optional)
combined_betas$study <- studies
# Remove study column
combined_betas_only <- combined_betas[, !colnames(combined_betas) %in% "study"]

message("Betas estimates combined. See objects `combined_betas` and `combined_betas_only`.")


################################
# SCALE BETAS TO REFERENCE STUDY
################################
# # Tampa as reference
# reference_study <- combined_betas[3, ]
# # str(reference_study, list.len=ncol(reference_study))
#
# for (i in 2:(ncol(combined_betas))) { # the last column is the study
#   reference_value <- as.numeric(reference_study[i])
#
#   # Scale columns by reference study's beta value
#   combined_betas[, i] <- combined_betas[, i] / reference_value
# }
#
# # add study column (optional)
# combined_betas$study <- c("choc", "pens", "tampa", "IRL")
# # Remove study column
# combined_betas_only <- combined_betas[, !colnames(combined_betas) %in% "study"]
# # View(combined_betas_only)

################################
# EXTRA: SE (for effect size)
################################

# # Standardize SE (Z-score)
# chocBetas[,2] <- (chocBetas[,2] + mean(chocBetas[,2]))/sd(chocBetas[,2])
# pensBetas[,2] <- (pensBetas[,2] + mean(pensBetas[,2]))/sd(pensBetas[,2])
# tampaBetas[,2] <- (tampaBetas[,2] + mean(tampaBetas[,2]))/sd(tampaBetas[,2])
# IRLBetas[,2] <- (IRLBetas[,2] + mean(IRLBetas[,2]))/sd(IRLBetas[,2])

# # Function to convert model out put to dataframe (only keep "Estimate")
# prepare_se_df <- function(matrix, source) {
#   df <- as.data.frame(t(matrix))
#   df <- df[2, , drop = FALSE] # Keep "Estimate"
#   colnames(df) <- rownames(matrix)
#   missing_cols <- setdiff(numeric_pred_cols, rownames(matrix)) # missing predictors as NA
#   df[missing_cols] <- NA
#   df <- df[, numeric_pred_cols] # order columns
#   df$study <- source
#   return(df)
# }

DF_se <- list()
DF_se[[1]] <- prepare_df(Betas[[1]], studies[1], stat = "se")
DF_se[[2]] <- prepare_df(Betas[[2]], studies[2], stat = "se")
DF_se[[3]] <- prepare_df(Betas[[3]], studies[3], stat = "se")
DF_se[[4]] <- prepare_df(Betas[[4]], studies[4], stat = "se")

combined_se <- do.call(rbind, DF_se)
combined_se[] <- lapply(combined_se, function(x) as.numeric(as.character(x)))

row_averages_se <- apply(combined_se, 1, function(row) mean(row, na.rm = TRUE))

# choc_avg_se <- row_averages_se[1]
# pens_avg_se <- row_averages_se[2]
# tampa_avg_se <- row_averages_se[3]
# IRL_avg_se <- row_averages_se[4]

# Replace NAs
combined_se[1, ][is.na(combined_se[1, ])] <- row_averages_se[1]
combined_se[2, ][is.na(combined_se[2, ])] <- row_averages_se[2]
combined_se[3, ][is.na(combined_se[3, ])] <- row_averages_se[3]
combined_se[4, ][is.na(combined_se[4, ])] <- row_averages_se[4]

combined_betas$study <- studies
# View(combined_se)

# remove study column
combined_se_only <- combined_se[, !colnames(combined_se) %in% "study"]

message("Standard error estimates combined. See objects `combined_se` and `combined_se_only`.")

