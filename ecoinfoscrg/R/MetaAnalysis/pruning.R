#############################################
# GET RESPONSE AND STANDARDIZED PREDICTORS
#############################################
library(dplyr)
resp <- as.data.frame(cbind(state$Response, state$study))
colnames(resp) <- c("Response", "study")

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
name <- "chocContinuous"
############################

# Grab categorical variables (dummyvars has the separated out names/dummy variables)
dummyvars <- colnames(pred)[grepl("_", colnames(pred))]

# List predictors (AKA column names of known predictors)
predictors <- c(numerical_vars, dummyvars)

# Define the response variable
response_var <- "Response" 

# NEW! 
resp <- data.frame(Response = state$Response)
study <- data.frame(study = state$study)
input <- cbind(resp, pred)
input$SMMv5Def <- NULL
input$study <- as.factor(input$study)

# # Run build-up/pair-down R script
start_time <- Sys.time()
source("ecoinfoscrg/R/MetaAnalysis/BUPD.R")
end_time <- Sys.time()

