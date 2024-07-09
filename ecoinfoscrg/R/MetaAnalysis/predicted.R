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
# print(expected_outcome)

############### TESTING #############
coo_state <- c("OBJECTID", "ID", "geometry", "feature_x", "feature_y", "nearest_x", "nearest_y", "shape__len", "Shape__Len", "distance", "distance_2", "n", "x", "y", "X", "Y")
state$predicted <- expected_outcome

##### TAMPA #####
tampa_test <- subset(state, study == "tampa")
tampa <- st_transform(st_read("/home/gzaragosa/data/Tampa_Bay_Living_Shoreline_Suitability_Model_Results/Tampa_Bay_Living_Shoreline_Suitability_Model_Results_POINTS_0.001deg.shp"), crs = 6346) # Response: 
tampa_available_columns <- coo[coo %in% colnames(tampa)]
tampa_coo <- tampa %>% select(all_of(tampa_available_columns))
tampa_predicted <- cbind(tampa_coo, tampa_test)
write.csv(tampa_predicted, "/home/gzaragosa/data/Tampa_Bay_Living_Shoreline_Suitability_Model_Results/tampa_predicted.csv", row.names = FALSE)

##### CHOC #####
choc_test <- subset(state, study == "choc")
choc <- st_transform(st_read("/home/gzaragosa/data/choctawatchee_bay_lssm/choctawatchee_bay_lssm_POINTS_0.001deg.shp"), crs = 6346) # Response: 
choc_available_columns <- coo[coo %in% colnames(choc)]
choc_coo <- choc %>% select(all_of(choc_available_columns))
choc_predicted <- cbind(choc_coo, choc_test)
write.csv(choc_predicted, "/home/gzaragosa/data/choctawatchee_bay_lssm/choc_predicted.csv", row.names = FALSE)

##### PENS #####
pens_test <- subset(state, study == "pens")
pens <- st_transform(st_read("/home/gzaragosa/data/pensacola_lssm/Santa_Rosa_Bay_Living_Shoreline_POINTS_0.001deg.shp"), crs = 6346) # Response: 
pens_available_columns <- coo[coo %in% colnames(pens)]
pens_coo <- pens %>% select(all_of(pens_available_columns))
pens_predicted <- cbind(pens_coo, pens_test)
write.csv(pens_predicted, "/home/gzaragosa/data/pensacola_lssm/pens_predicted.csv", row.names = FALSE)

##### IRL #####
IRL_test <- subset(state, study == "IRL")
IRL <- st_transform(st_read("/home/gzaragosa/data/Final NIRL Shapefile_all data/Final Shapefile_all data/UCF_livingshorelinemodels_MosquitoNorthIRL_111m.shp"), crs = 6346) # Response: 
IRL_available_columns <- coo[coo %in% colnames(IRL)]
IRL_coo <- IRL %>% select(all_of(IRL_available_columns))
IRL_predicted <- cbind(IRL_coo, IRL_test)
IRL_predicted$feature_x <- IRL$feature_x
IRL_predicted$feature_y <- IRL$feature_y
# note that coordinates need to be adjusted at some point before mapping (either in R or GIS)
## note that exports at Florida 17N
write.csv(IRL_predicted, "/home/gzaragosa/data/Final NIRL Shapefile_all data/Final Shapefile_all data/IRL_predicted.csv", row.names = FALSE)

# ##### LSSM4 #####
# # All 4 LSSMs together
# # LSSM4_predicted <- rbind(choc_predicted, pens_predicted, tampa_predicted, IRL_predicted)
# LSSM4_predicted <- bind_rows(choc_predicted, pens_predicted, tampa_predicted, IRL_predicted)
# write.csv(LSSM4_predicted, "/home/gzaragosa/data/Created/LSSM4_predicted.csv", row.names = FALSE)

