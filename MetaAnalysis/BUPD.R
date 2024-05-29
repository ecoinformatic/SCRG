############################################
# HOUSEKEEPING
############################################
data[predictors] <- lapply(data[predictors], as.factor) # convert to factor
data[[response_var]] <- factor(data[[response_var]]) # convert to factor
name_prefix <- gsub(" ", "_", name) # add underscores


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
initial_formula <- as.formula(paste(response_var, "~ 1"))
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
    print(paste("Testing build-up formula:", formula_str, "with AIC:", candidate_aic)) # helpful output
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
    print("No further improvement, stopping build-up.")
    break  # Stop if the AIC of the best candidate model is not lower than the current best AIC
  }
}
#####

# Final model
best_model <- multinom(best_formula, data = data, MaxNWts = 5000, trace = FALSE)
# summary(best_model)


############################################
# PAIR-DOWN PHASE
############################################
current_formula <- best_formula  # start with best FORMULA from build-up phase

# Iteratively remove predictors
repeat {
  predictors_in_model <- all.vars(current_formula)[-1]  # get all predictors currently in the best model
  candidate_models <- list() # list to store models and AIC
  current_aic <- AIC(multinom(current_formula, data = data, MaxNWts = 5000, trace = FALSE)) # calculate AIC of current best model
  
  for (predictor in predictors_in_model) { # Loop through each predictors to test their removal
    pairdown_formula <- as.formula(paste("BMPallSMM ~", paste(setdiff(predictors_in_model, predictor), collapse = "+"))) # New formula without current predictor
    if (length(all.vars(pairdown_formula)[-1]) == 0) { # check if model is empty (no predictors)
      next  # skip iteration if no predictors are left
    }
    pairdown_aic <- AIC(multinom(pairdown_formula, data = data, MaxNWts = 5000, trace = FALSE)) # fit pairdown model, get AIC
    formula_str <- paste(deparse(pairdown_formula, width.cutoff = 500), collapse = "") # store the name/string of the model properly
    candidate_models[formula_str] <- pairdown_aic # Store AIC and formula of pairdown model
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
final_model <- multinom(current_formula, data = data, MaxNWts = 5000, trace = FALSE)
# summary(final_model)

# final formula
final_form <- formula(final_model)
# print(final_form)

# # Useful info for meta-analysis
coeff <- coef(final_model) # grab coefficients
# standard_err <- sqrt(diag(vcov(final_model))) # Calculate SE (method OK?)
# confidence_intervals <- confint(final_model, level = 0.95) # Calculate CI
odds_ratios <- exp(coeff) # Calculate OR

######################
# `assign` to new variables based on name prefix chosen
assign(paste0(name_prefix, "_final_model"), final_model)
assign(paste0(name_prefix, "_final_form"), final_form)
assign(paste0(name_prefix, "_odds_ratios"), odds_ratios)

# save for later
saveRDS(get(paste0(name_prefix, "_final_model")), paste0(name_prefix, "_final_model.rds"))
saveRDS(get(paste0(name_prefix, "_final_form")), paste0(name_prefix, "_final_form.rds"))
saveRDS(get(paste0(name_prefix, "_odds_ratios")), paste0(name_prefix, "_odds_ratios.rds"))
