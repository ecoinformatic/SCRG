
############################################
# INITIALIZE
############################################
# library(nnet)
library(lme4)
# Initial empty model
# Note that `best_formula` starts with `initial_formula` as baseline
initial_formula <- as.formula(paste(response_var, "~ 1 + (1|study)"))
best_formula <- initial_formula
# best_model <- multinom(best_formula, data = data, MaxNWts = 5000)
best_model <- lmer(best_formula, data = input)
best_aic <- AIC(best_model)
name_prefix <- gsub(" ", "_", name) # add underscores
############################################
# BUILD-UP PHASE
############################################
# Function to fit and evaluate models (similar to Chris' only with a multinomial logistic regression for this specific dataset)
fit_model <- function(formula, data) {
  model <- lmer(formula, data = data)
  list(model = model, aic = AIC(model))
}
# Similar to Chris' only with a multinomial logistic regression (which can be switched out)
for (i in 1:length(predictors)) {
    current_predictors <- all.vars(best_formula)[-1]
    remaining_predictors <- setdiff(predictors, current_predictors) 
    candidate_models <- list()  # for storing model and their AIC
    # Build models iteratively based on current best model
    for (predictor in remaining_predictors) {
        new_formula <- update(best_formula, paste(". ~ . +", predictor))
        formula_str <- paste(deparse(new_formula, width.cutoff = 500), collapse = "") # converts model formulat to string. Note that the width.cutoff and collapse are EXTREMELY important or deparse will split your string into two lines by default
        fit <- fit_model(new_formula, input)        
        candidate_models[[formula_str]] <- fit$aic # store the new model and AIC in cadidate model list
        print(paste("Testing build-up formula:", formula_str, "with AIC:", fit$aic)) # helpful output
    }
    # ID the best model
    if (length(candidate_models) > 0) {
        best_candidate <- which.min(unlist(candidate_models)) # ID model with lowest AIC
        best_candidate_formula <- names(candidate_models)[best_candidate] # Get formula of best candidate model (string)
        best_candidate_aic <- unlist(candidate_models[best_candidate_formula])
        # Update to the new best model if it improves AIC
        if (best_candidate_aic < best_aic) { # If the AIC of the best candidate model is lower than the current best model's AIC, it replaces the model with the new model
            best_formula <- as.formula(best_candidate_formula) # Converts the model formula string back into a regular formula object
            best_aic <- best_candidate_aic # Updates best_aic to the AIC of the new best model
            print(paste("New best model:", best_candidate_formula, "AIC:", best_aic)) # helpful output
        } else {
            print("No further improvement, stopping build-up.")
            break  
        }
    } else {
        print("No more predictors to test, stopping build-up.")
        break
    }
}
#####
# Final model
# best_model <- multinom(best_formula, data = data, MaxNWts = 5000, trace = FALSE)
best_model <- lmer(best_formula, data = input)
# summary(best_model)
############################################
# PAIR-DOWN PHASE
############################################
current_formula <- best_formula  # start with best FORMULA from build-up phase
# Iteratively remove predictors
repeat {
  predictors_in_model <- all.vars(current_formula)[-1]  # get all predictors currently in the best model
  candidate_models <- list() # list to store models and AIC
  current_model <- lmer(current_formula, data = input)
  # current_aic <- AIC(multinom(current_formula, data = data, MaxNWts = 5000, trace = FALSE)) # calculate AIC of current best model
  current_aic <- AIC(current_model)
  
  for (predictor in predictors_in_model) { # Loop through each predictors to test their removal
    pairdown_formula <- as.formula(paste(response_var, "~", paste(setdiff(predictors_in_model, predictor), collapse = "+"))) # New formula without current predictor
    if (length(all.vars(pairdown_formula)[-1]) == 0) { # check if model is empty (no predictors)
      next  # skip iteration if no predictors are left
    }
    # pairdown_aic <- AIC(multinom(pairdown_formula, data = data, MaxNWts = 5000, trace = FALSE)) # fit pairdown model, get AIC
    fit <- fit_model(pairdown_formula, input)
    formula_str <- paste(deparse(pairdown_formula, width.cutoff = 500), collapse = "") # store the name/string of the model properly
    candidate_models[formula_str] <- fit$aic # Store AIC and formula of pairdown model
    print(paste("Testing pairdown formula:", deparse(pairdown_formula), "with AIC:", pairdown_aic)) # Helpful output
  }
  
  # See if any pairdown model is better than current best model
  if (length(candidate_models) > 0) { 
    best_pairdown_aic <- min(sapply(candidate_models, identity)) # find smallest AIC among pairdown models
    if (best_pairdown_aic < current_aic) { # If a pairdown model has lower AIC, update current best model
      best_pairdown_formula <- names(candidate_models)[which.min(sapply(candidate_models, identity))]
      current_formula <- as.formula(best_pairdown_formula)
      current_aic <- best_pairdown_aic
      print(paste("New best pairdown model:", best_pairdown_formula, "AIC:", best_pairdown_aic)) # Output new model's formula and AIC
    } else {
      print("No further improvement, final model selected.")
      break  # Exit loop if no improvement
    }
  } else {
    print("No improvement, stopping pair-down phase.") # If no pairdown models found with lower AIC, then stops the process
    break
  }
}
# Final pairdown model
# final_model <- multinom(current_formula, data = data, MaxNWts = 5000, trace = FALSE)
final_model <- lmer(current_formula, data = input)
# summary(final_model)
# final formula
final_form <- formula(final_model)
# print(final_form)
# # Useful info for meta-analysis
coeff <- coef(final_model) # grab coefficients
# standard_err <- sqrt(diag(vcov(final_model))) # Calculate SE (method OK?)
# confidence_intervals <- confint(final_model, level = 0.95) # Calculate CI
odds_ratios <- exp(coeff) # Calculate OR
# `assign` to new variables based on name prefix chosen
assign(paste0(name_prefix, "_final_model"), final_model)
assign(paste0(name_prefix, "_final_form"), final_form)
assign(paste0(name_prefix, "_odds_ratios"), odds_ratios)
# save for later
output_directory <- "ecoinfoscrg/R/MetaAnalysis/Routput"
saveRDS(get(paste0(name_prefix, "_final_model")), file = file.path(output_directory, paste0(name_prefix, "_final_model.rds")))
saveRDS(get(paste0(name_prefix, "_final_form")), file = file.path(output_directory, paste0(name_prefix, "_final_form.rds")))
saveRDS(get(paste0(name_prefix, "_odds_ratios")), file = file.path(output_directory, paste0(name_prefix, "_odds_ratios.rds")))
