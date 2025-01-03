#############################################
# GET RESPONSE AND STANDARDIZED PREDICTORS
#############################################
resp <- as.data.frame(cbind(state$Response, state$study))
colnames(resp) <- c("Response", "study")

# Replace NAs with means for numerical variables
pred <- pred %>%
  mutate(across(all_of(numerical_vars), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Standardize numeric vars
pred <- pred %>%
  mutate(across(all_of(numerical_vars), ~ (.-mean(., na.rm = TRUE))/sd(., na.rm = TRUE)))

resp_choc <- resp %>% filter(study == "choc")
resp_pens <- resp %>% filter(study == "pens")
resp_tampa <- resp %>% filter(study == "tampa")
resp_IRL <- resp %>% filter(study == "IRL")

pred_choc <- pred %>% filter(study == "choc")
pred_pens <- pred %>% filter(study == "pens")
pred_tampa <- pred %>% filter(study == "tampa")
pred_IRL <- pred %>% filter(study == "IRL")

##### CHOSE STUDY HERE #####
# combine response and pred
data <- cbind(resp_choc, pred_choc) # choc example

# Specify a short name of the model
name <- "choc_non_parallel_fix"
############################

# Grab categorical variables (dummyvars has the separated out names/dummy variables)
dummyvars <- colnames(pred)[grepl("_", colnames(pred))]

# List predictors (AKA column names of known predictors)
predictors <- c(setdiff(numerical_vars, dummyvars), dummyvars)

# Define the response variable
response_var <- "Response" 

# NEW! 
# resp <- data.frame(Response = state$Response)
resp <- data.frame(Response = factor(state$Response, ordered = TRUE))


study <- data.frame(study = state$study)
input <- cbind(resp, pred)
input$SMMv5Def <- NULL
input$study <- as.factor(input$study)

# Run build-up/pair-down R scripts
start_time <- Sys.time()
source("scripts/BUPD_nonparallel.R")
end_time <- Sys.time()



#######################
# COMPARE MODELS
#######################

# choc (~17 mins, ~17 mins)
choc_mod_old <- readRDS("output/choc_parallel_test_final_model.rds")
choc_mod_new <- readRDS("output/choc_non_parallel_final_model.rds")
choc_mod_fix <- readRDS("output/choc_non_parallel_fix_final_model.rds")

# pens (~31 mins, ~18 mins)
pens_mod_old <- readRDS("output/pensTest_final_model.rds")
pens_mod_new <- readRDS("output/pens_non_parallel_final_model.rds")
pens_mod_fix <- readRDS("output/pens_non_parallel_fix_final_model.rds")

# tampa (~25 mins, ~18 mins)
tampa_mod_old <- readRDS("output/tampaTest_final_model.rds")
tampa_mod_new <- readRDS("output/tampa_non_parallel_final_model.rds")
tampa_mod_fix <- readRDS("output/tampa_non_parallel_fix_final_model.rds")

# IRL (~34 mins, ~18 mins)
IRL_mod_old <- readRDS("output/IRLTestNonParallel_final_model.rds")
IRL_mod_new <- readRDS("output/IRL_non_parallel_final_model.rds")
IRL_mod_fix <- readRDS("output/IRL_non_parallel_fix_final_model.rds")

# Full (~41 mins, ~24 mins)
full_mod_new <- readRDS("output/full_non_parallel_final_model.rds")
full_mod_fix <- readRDS("output/full_non_parallel_ang_final_model.rds")

# Response ~ Hardened_1 + WTLD_VEG_3_2 + Slope_4 + City_5 + Erosion_1_2 + Adj_LU_7 + Rest_Opp + Adj_H1_6 + City_6
## AIC: 54.28808
## AIC2: 54.28279




# NEW average betas???
choc_betas <- as.data.frame(summary(choc_mod_fix)[1])
colnames(choc_betas) <- c("Estimate", "Std. Error", "t value")
choc_betas <- as.matrix(choc_betas)

pens_betas <- as.data.frame(summary(pens_mod_fix)[1])
colnames(pens_betas) <- c("Estimate", "Std. Error", "t value")
pens_betas <- as.matrix(pens_betas)

tampa_betas <- as.data.frame(summary(tampa_mod_fix)[1])
colnames(tampa_betas) <- c("Estimate", "Std. Error", "t value")
tampa_betas <- as.matrix(tampa_betas)

IRL_betas <- as.data.frame(summary(IRL_mod_fix)[1])
colnames(IRL_betas) <- c("Estimate", "Std. Error", "t value")
IRL_betas <- as.matrix(IRL_betas)

## SHOULD THEY ALL BE IDENTICAL???




# FROM OLD BUPD.R SCRIPT

# Get a list of all predictor names from each matrix
all_predictors <- unique(unlist(lapply(results, function(x) if (!is.null(x)) rownames(x))))
# Create a template matrix with all predictors and zeros
template <- matrix(0, nrow = length(all_predictors), ncol = 3, dimnames = list(all_predictors, c("Estimate", "Std. Error", "t value")))
standardized_results <- lapply(results, function(x) {
  if (is.null(x)) {
    return(template)
  } else {
    # Create a copy of the template
    standardized_matrix <- template
    # Update the values for predictors present in this subset's result
    intersecting_predictors <- intersect(rownames(x), rownames(template))
    standardized_matrix[intersecting_predictors, ] <- x[intersecting_predictors, ]
    return(standardized_matrix)
  }
})
# Sum the standardized matrices
total_sum <- Reduce("+", standardized_results)
# Calculate the average
average_betas <- total_sum / length(standardized_results)
# Print the average results
print(average_betas)


# Save output
output_directory <- "output"
saveRDS(average_betas, file = file.path(output_directory, paste0(name_prefix, "_average_betas.rds")))

chocBetas <- readRDS("output/chocContinuous_average_betas.rds")
pensBetas <- readRDS("output/pensContinuous_average_betas.rds")
IRLBetas <- readRDS("output/IRLContinuous_average_betas.rds")
tampaBetas <- readRDS("output/tampaContinuous_average_betas.rds") 


