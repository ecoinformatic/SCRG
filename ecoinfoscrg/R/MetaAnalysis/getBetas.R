library(dplyr)

############################
# GRAB MODEL OUTPUT
############################
source("ecoinfoscrg/R/MetaAnalysis/wranglingCleaning.R")
source("ecoinfoscrg/R/MetaAnalysis/standardize.R")

chocBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/chocContinuous_average_betas.rds")
pensBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/pensContinuous_average_betas.rds")
IRLBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/IRLContinuous_average_betas.rds")
tampaBetas <- readRDS("ecoinfoscrg/R/MetaAnalysis/Routput/tampaContinuous_average_betas.rds")

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
combined_betas[] <- lapply(combined_betas, function(x) as.numeric(as.character(x)))

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

combined_betas$study <- c("choc", "pens", "tampa", "IRL")

View(combined_betas)

# Remove study columned
combined_betas_only <- combined_betas[, !colnames(combined_betas) %in% "study"]

################################
# SE(for effect size)
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
View(combined_se)

# remove study column
combined_se_only <- combined_se[, !colnames(combined_se) %in% "study"]
