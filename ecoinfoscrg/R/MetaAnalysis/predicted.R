average_betas <- data.frame(t(colMeans(combined_betas_only, na.rm = TRUE)))
# View(average_betas)
# str(average_betas)
# max_index <- which.max(average_betas)
# max_predictor <- predictor_columns[max_index]
# max_predictor

pred$study <- NULL
pred$SMMv5Def <- NULL
predictor_data <- pred

# convert to right format for matix multiplicatrion
average_betas_vector <- as.numeric(average_betas[1, ]) 
predictor_matrix <- as.matrix(predictor_data)

# Calc expected outcome
# expected_outcome <- predictor_matrix %*% average_betas_vector
expected_outcome <- as.matrix(predictor_data) %*% average_betas_vector

# Print the expected outcomes
print(expected_outcome)

############### TESTING #############
state$predicted <- expected_outcome
tampa_test <- subset(state, study == "tampa")
tampa <- st_transform(st_read("/home/gzaragosa/data/Tampa_Bay_Living_Shoreline_Suitability_Model_Results/Tampa_Bay_Living_Shoreline_Suitability_Model_Results_POINTS_0.001deg.shp"), crs = 6346) # Response: 
coo <- c("OBJECTID", "ID", "geometry", "feature_x", "feature_y", "nearest_x", "nearest_y", "shape__len", "Shape__Len", "distance", "distance_2", "n", "x", "y", "X", "Y")
available_columns <- coo[coo %in% colnames(tampa)]
tampa_coo <- tampa %>% select(all_of(available_columns))
write.csv(tampa_coo, "/home/gzaragosa/data/Tampa_Bay_Living_Shoreline_Suitability_Model_Results/tampa_predicted.csv", row.names = FALSE)
