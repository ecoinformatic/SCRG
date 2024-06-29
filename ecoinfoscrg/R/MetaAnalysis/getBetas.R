
library(nnet)
library(dplyr)

# Get models from .rds
model_paths <- c("ecoinfoscrg/R/MetaAnalysis/Routput/chocTest_final_model.rds",
                 "ecoinfoscrg/R/MetaAnalysis/Routput/pensTest_final_model.rds",
                 "ecoinfoscrg/R/MetaAnalysis/Routput/tampaTest_final_model.rds",
                 "ecoinfoscrg/R/MetaAnalysis/Routput/IRLTest_final_model.rds")
model_names <- c("choc", "pens", "tampa", "IRL")

# Load and prep model data
prepare_df <- function(model_path, model_name) {
  model <- readRDS(model_path)
  coef_df <- as.data.frame(coef(model))
  coef_df$response_category <- rownames(coef_df)
  coef_df$model <- model_name
  return(coef_df)
}

# Grab and combine betas
model_data_frames <- Map(prepare_df, model_paths, model_names)

# Create data frames for each model
choc_betas <- model_data_frames[[1]]
pens_betas <- model_data_frames[[2]]
tampa_betas <- model_data_frames[[3]]
IRL_betas <- model_data_frames[[4]]

# Combine all individual data frames
combined_betas <- bind_rows(model_data_frames)
combined_betas <- combined_betas %>%
  arrange(response_category, model) # Rearrange/move response to the left 

# check
choc_betas
pens_betas
tampa_betas
IRL_betas
combined_betas

#######################
# GET COLUMNS FOR EACH STUDY
#######################
# Generate dummy variable names function
generate_dummy_colnames <- function(base_cols, pred_cols) {
  dummy_colnames <- pred_cols[grepl(paste(base_cols, collapse = "|"), pred_cols)]
  return(dummy_colnames)
}

# Extract "base" (original) column names from dummy variables in `pred`
base_pred_cols <- unique(sub("_.*", "", colnames(pred)))

# Match study columns to dummy variable format in pred
match_study_to_pred <- function(study_cols, pred_cols) {
  base_study_cols <- intersect(study_cols, base_pred_cols)
  dummy_colnames <- generate_dummy_colnames(base_study_cols, pred_cols)
  return(dummy_colnames)
}

# Get dummy variable names for studies
choc_dcolnames <- match_study_to_pred(colnames(choc), colnames(pred))
pens_dcolnames <- match_study_to_pred(colnames(pens), colnames(pred))
tampa_dcolnames <- match_study_to_pred(colnames(tampa), colnames(pred))
IRL_dcolnames <- match_study_to_pred(colnames(IRL), colnames(pred))
choc_dcolnames
pens_dcolnames
tampa_dcolnames
IRL_dcolnames

#######################
# ENSURE STUDY BETAS CONTAIN NECESSARY COLUMNS
#######################
# Function to add missing columns as numeric
add_missing_columns <- function(df, colnames_vec) {
  missing_cols <- setdiff(colnames_vec, colnames(df))
  for (col in missing_cols) {
    df[[col]] <- as.numeric(NA)
  }
  return(df)
}

# Add missing columns to each study's betas dataframe
choc_betas <- add_missing_columns(choc_betas, choc_dcolnames)
pens_betas <- add_missing_columns(pens_betas, pens_dcolnames)
tampa_betas <- add_missing_columns(tampa_betas, tampa_dcolnames)
IRL_betas <- add_missing_columns(IRL_betas, IRL_dcolnames)
convert_to_numeric <- function(df) {
  df[] <- lapply(df, function(col) if(is.logical(col)) as.numeric(col) else col)
  return(df)
}
choc_betas <- convert_to_numeric(choc_betas)
pens_betas <- convert_to_numeric(pens_betas)
tampa_betas <- convert_to_numeric(tampa_betas)
IRL_betas <- convert_to_numeric(IRL_betas)

#######################
# CALCULATE AND FILL STUDY AVERAGES
#######################
# Helpful for averaging:
calculate_study_average <- function(df) {
  numeric_cols <- df %>%
    select(-`(Intercept)`, where(is.numeric)) %>%
    select_if(~ is.numeric(.) && !all(is.na(.))) # make sure they're numeric!!!
  avg <- mean(unlist(lapply(numeric_cols, as.numeric)), na.rm = TRUE)
  return(avg)
}

# Calculate study beta averages
choc_avg <- calculate_study_average(choc_betas)
pens_avg <- calculate_study_average(pens_betas)
tampa_avg <- calculate_study_average(tampa_betas)
IRL_avg <- calculate_study_average(IRL_betas)
choc_avg
pens_avg
tampa_avg
IRL_avg

# Fill missing values with the study average
fill_missing_with_average <- function(df, average) {
  numeric_cols <- df %>%
    select(-`(Intercept)`, where(is.numeric))
  df[names(numeric_cols)] <- lapply(numeric_cols, function(col) {
    ifelse(is.na(col), average, col)
  })
  return(df)
}

# Relace missing values with averages
choc_betas <- fill_missing_with_average(choc_betas, choc_avg)
pens_betas <- fill_missing_with_average(pens_betas, pens_avg)
tampa_betas <- fill_missing_with_average(tampa_betas, tampa_avg)
IRL_betas <- fill_missing_with_average(IRL_betas, IRL_avg)

# New column for "response_category"
choc_betas$response_category <- rownames(choc_betas)
pens_betas$response_category <- rownames(pens_betas)
tampa_betas$response_category <- rownames(tampa_betas)
IRL_betas$response_category <- rownames(IRL_betas)

# Remove row names
rownames(choc_betas) <- NULL
rownames(pens_betas) <- NULL
rownames(tampa_betas) <- NULL
rownames(IRL_betas) <- NULL
print(head(choc_betas))
print(head(pens_betas))
print(head(tampa_betas))
print(head(IRL_betas))

######### YOU ARE HERE #########
# Combine datasets
combined_betas <- bind_rows(choc_betas, pens_betas, tampa_betas, IRL_betas)

# Ignoring character columns, study column, (Intercept) re and other character columns and intercept
combined_betas_only <- combined_betas %>%
  select(-response_category, -model, -study, -`(Intercept)`, where(is.numeric))
View(combined_betas_only)
## Note: Both "NA" and "Invalid Number" are present
