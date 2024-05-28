############################################
# PREP DATA
############################################
rm(list=ls())
library(nnet)  # easy for using multinomial logistic regression

data <- read.csv("Tampa_Bay_Living_Shoreline_Suitability_Model_Results.csv")
# Replace NULL or missing values with appropriate data
data$canal[data$canal == ""] <- "None"
data$SandSpit[data$SandSpit == ""] <- "No"
data$forestshl[data$forestshl == ""] <- "No"
data$Structure[data$Structure == ""] <- "None"
data$offshorest[data$offshorest == ""] <- "None"
data$defended[data$defended == ""] <- "No"
data$roads[data$roads == ""] <- "No"
data$PermStruc[data$PermStruc == ""] <- "None"
data$WideBeach[data$WideBeach == ""] <- "No"
data$tribs[data$tribs == ""] <- "None"
data$SAV[data$SAV == ""] <- "No"
data$PublicRamp[data$PublicRamp == ""] <- "No"

# Convert all predictors and the response variable to factors
predictors <- c("Exposure", "RiparianLU", "bathymetry", "marsh_all", "bnk_height", 
                "canal", "SandSpit", "forestshl", "Structure", "offshorest", 
                "defended", "roads", "PermStruc", "Beach", "WideBeach", "tribs", 
                "SAV", "PublicRamp")
data[predictors] <- lapply(data[predictors], as.factor)
data$BMPallSMM <- factor(data$BMPallSMM)


############################################
# BUILD UP PHASE
############################################
# Function to fit and evaluate models (similar to Chris' only with a multinomial logistic regression for this specific dataset)
fit_and_evaluate <- function(formula, data) {
  model <- multinom(formula, data = data, MaxNWts = 5000, trace = FALSE) # Note: Using multinomial logistic regression
  AIC(model)  # Using AIC for simplicity, but you can choose other criteria
}

# Initial empty model
# Note that `best_formula` starts with `initial_formula` as baseline
initial_formula <- BMPallSMM ~ 1
best_formula <- initial_formula 
best_model <- multinom(best_formula, data = data, MaxNWts = 5000)
best_aic <- AIC(best_model)

#####
# Build models iteratively
for (i in 1:length(predictors)) { # begin outer loop by adding predictors one at a time
  candidate_models <- list() # empty list to store candidate models and their AICs
  for (j in i:length(predictors)) { # begine inner loop to test adding each predictor not included in current best model
    new_formula <- update(best_formula, paste(". ~ . +", predictors[j]))
    formula_str <- paste(deparse(new_formula, width.cutoff = 500), collapse = "") # converts model formulat to string. Note that the width.cutoff and collapse are EXTREMELY important or deparse will split your string into two lines by default
    candidate_aic <- fit_and_evaluate(new_formula, data) # using the `fit_and_evaluate` function from above to fit new model and calculate AIC
    candidate_models[[formula_str]] <- candidate_aic # store the new model and AIC in cadidate model list
    print(paste("Testing formula:", formula_str, "with AIC:", candidate_aic)) # helpful output
  }
  
  # ID the best model
  best_candidate <- which.min(sapply(candidate_models, identity)) # ID the model with lowest AIC among candidate model list
  best_candidate_formula <- names(candidate_models)[best_candidate] # Get formula of best candidate model (string)
  
  # Compare with the current best model
  if (candidate_models[[best_candidate_formula]] < best_aic) { # If the AIC of the best candidate model is lower than the current best model's AIC, it replaces the model with the new model
    best_formula <- as.formula(best_candidate_formula) # Converts the model formula string back into a regular formula object
    best_aic <- candidate_models[[best_candidate_formula]] # Updates best_aic to the AIC of the new best model
    print(paste("New best model:", best_candidate_formula, "AIC:", best_aic))  # helpful output
  } else {
    print("No further improvement, stopping.")
    break  # Stop if the AIC of the best candidate model is not lower than the current best AIC
  }
}
#####

# Final model
best_model <- multinom(best_formula, data = data, MaxNWts = 5000, trace = FALSE)
summary(best_model)


############################################
# PAIR-DOWN PHASE
############################################
# TBD