library(dplyr)

############################
# GRAB MODEL OUTPUT
############################
source("scripts/wranglingCleaning.R")
source("scripts/standardize.R")

chocBetas <- readRDS("output/chocContinuous_average_betas.rds")
pensBetas <- readRDS("output/pensContinuous_average_betas.rds")
IRLBetas <- readRDS("output/IRLContinuous_average_betas.rds")
tampaBetas <- readRDS("output/tampaContinuous_average_betas.rds")

# Get predictors (excluding study and definitions)
numeric_pred <- pred %>%
  select_if(is.numeric)
numeric_pred_cols <- colnames(numeric_pred)

# Function to convert model out put to dataframe (only keep "Estimate")
prepare_df <- function(matrix, source) {
    df <- as.data.frame(t(matrix))
    df <- df[1, , drop = FALSE] # Keep "Estimate"
    colnames(df) <- rownames(matrix)
    missing_cols <- setdiff(numeric_pred_cols, rownames(matrix)) # missing predictors as NA
    df[missing_cols] <- NA
    df <- df[, numeric_pred_cols] # order columns
    df$study <- source
    return(df)
}

chocDF <- prepare_df(chocBetas, "choc")
pensDF <- prepare_df(pensBetas, "pens")
tampaDF <- prepare_df(tampaBetas, "tampa")
IRLDF <- prepare_df(IRLBetas, "IRL")

# Combine all dataframes
combined_betas <- rbind(chocDF, pensDF, tampaDF, IRLDF)
combined_betas[] <- lapply(combined_betas, function(x) as.numeric(as.character(x))) # will probably get NAs

# Get column names from pred
missing_cols <- setdiff(numeric_pred_cols, colnames(combined_betas))
combined_betas[missing_cols] <- NA


############################
# GENERATE AVERAGES AND REPLACE NA's WITH THEM
############################
# Get average for each row ignore NA
combined_betas[] <- lapply(combined_betas, function(x) as.numeric(as.character(x)))
row_averages <- apply(combined_betas, 1, function(row) mean(row, na.rm = TRUE))

choc_avg <- row_averages[1]
pens_avg <- row_averages[2]
tampa_avg <- row_averages[3]
IRL_avg <- row_averages[4]

choc_avg
pens_avg
tampa_avg
IRL_avg

# Replace NAs
combined_betas[1, ][is.na(combined_betas[1, ])] <- choc_avg
combined_betas[2, ][is.na(combined_betas[2, ])] <- pens_avg
combined_betas[3, ][is.na(combined_betas[3, ])] <- tampa_avg
combined_betas[4, ][is.na(combined_betas[4, ])] <- IRL_avg

################################
# SCALE BETAS TO REFERENCE STUDY
################################
# Tampa as reference
reference_study <- combined_betas[3, ]
# str(reference_study, list.len=ncol(reference_study))

for (i in 2:(ncol(combined_betas))) { # the last column is the study
  reference_value <- as.numeric(reference_study[i])

  # Scale columns by reference study's beta value
  combined_betas[, i] <- combined_betas[, i] / reference_value
}

# add study column (optional)
combined_betas$study <- c("choc", "pens", "tampa", "IRL")
# Remove study columned
combined_betas_only <- combined_betas[, !colnames(combined_betas) %in% "study"]
# View(combined_betas_only)

################################
# EXTRA: SE (for effect size)
################################
# Function to convert model out put to dataframe (only keep "Estimate")
prepare_se_df <- function(matrix, source) {
    df <- as.data.frame(t(matrix))
    df <- df[2, , drop = FALSE] # Keep "Estimate"
    colnames(df) <- rownames(matrix)
    missing_cols <- setdiff(numeric_pred_cols, rownames(matrix)) # missing predictors as NA
    df[missing_cols] <- NA
    df <- df[, numeric_pred_cols] # order columns
    df$study <- source
    return(df)
}

chocDF_se <- prepare_df(chocBetas, "choc")
pensDF_se <- prepare_df(pensBetas, "pens")
tampaDF_se <- prepare_df(tampaBetas, "tampa")
IRLDF_se <- prepare_df(IRLBetas, "IRL")

combined_se <- rbind(chocDF_se, pensDF_se, tampaDF_se, IRLDF_se)
combined_se[] <- lapply(combined_se, function(x) as.numeric(as.character(x)))

row_averages_se <- apply(combined_se, 1, function(row) mean(row, na.rm = TRUE))

choc_avg_se <- row_averages_se[1]
pens_avg_se <- row_averages_se[2]
tampa_avg_se <- row_averages_se[3]
IRL_avg_se <- row_averages_se[4]

# Replace NAs
combined_se[1, ][is.na(combined_se[1, ])] <- choc_avg_se
combined_se[2, ][is.na(combined_se[2, ])] <- pens_avg_se
combined_se[3, ][is.na(combined_se[3, ])] <- tampa_avg_se
combined_se[4, ][is.na(combined_se[4, ])] <- IRL_avg_se

combined_betas$study <- c("choc", "pens", "tampa", "IRL")
# View(combined_se)

# remove study column
combined_se_only <- combined_se[, !colnames(combined_se) %in% "study"]
