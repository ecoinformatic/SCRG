#############################################
# GET RESPONSE AND STANDARDIZED PREDICTORS
#############################################
library(dplyr)
resp <- as.data.frame(cbind(state$Response, state$study))
colnames(resp) <- c("Response", "study")
# str(resp)
# str(pred)
# View(resp)
# View(pred)

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
data <- cbind(resp_IRL, pred_IRL) # choc example

# Specify a short name of the model
name <- "IRLTestNonParallel"
############################

# Grab categorical variables (dummyvars has the separated out names/dummy variables)
dummyvars <- colnames(pred)[grepl("_", colnames(pred))]

# List predictors (AKA column names of known predictors)
predictors <- c(numerical_vars, dummyvars)

# Define the response variable
response_var <- "Response" 
# response_var <- "BMPallSMM" 

# # Run build-up/pair-down R script
start_time <- Sys.time()
source("MetaAnalysis/BUPD.R")
end_time <- Sys.time()

# output_formula <- readRDS("../../../Data/Routput/chocTest_final_form.rds")
# output_model <- readRDS("../../../Data/Routput/chocTest_final_model.rds")
# output_OR <- readRDS("../../../Data/Routput/chocTest_odds_ratios.rds")
