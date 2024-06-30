# Note: GPT-4o helped subset and parallelize the original version of this script
## Generally looks ok but may want to carefully check later since not much checks/output
library(lme4)
library(parallel)
library(doParallel)
name_prefix <- gsub(" ", "_", name)

# Setup parallel backend to use multiple cores
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Ensure all necessary packages are loaded on each worker node
clusterEvalQ(cl, {
    library(lme4)
})

# Subset
subsets <- split(input, sample(1:4, nrow(input), replace = TRUE))

# Function to apply model building phases on a subset of data
process_subset <- function(subset_data, response_var, predictors) {
    initial_formula <- reformulate(c("(1|study)"), response = response_var)
    best_model <- lmer(initial_formula, data = subset_data)
    best_aic <- AIC(best_model)
    
    # Build-Up Phase
    for (predictor in predictors) {
        new_formula <- update(initial_formula, paste(". ~ . +", predictor))
        model <- lmer(new_formula, data = subset_data)
        current_aic <- AIC(model)
        if (current_aic < best_aic) {
            initial_formula <- new_formula
            best_model <- model
            best_aic <- current_aic
        }
    }

    # Pair-Down Phase
    while (length(all.vars(initial_formula)[-1]) > 0) {
        current_aic <- best_aic
        improvements <- FALSE

        predictors_in_model <- all.vars(initial_formula)[-1]
        for (predictor in predictors_in_model) {
            reduced_formula <- update(initial_formula, paste(". ~ . -", predictor))
            model <- lmer(reduced_formula, data = subset_data)
            reduced_aic <- AIC(model)

            if (reduced_aic < current_aic) {
                initial_formula <- reduced_formula
                best_model <- model
                best_aic <- reduced_aic
                improvements <- TRUE
            }
        }

        if (!improvements) break
    }

    return(coef(summary(best_model)))
}

# Run models on each subset in parallel
results <- parLapply(cl, subsets, process_subset, response_var = response_var, predictors = predictors)

# Stop and deregister parallel backend
stopCluster(cl)






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
output_directory <- "ecoinfoscrg/R/MetaAnalysis/Routput"
saveRDS(average_betas, file = file.path(output_directory, paste0(name_prefix, "_average_betas.rds")))



