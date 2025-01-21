# average_betas <- data.frame(t(colMeans(combined_betas_only, na.rm = TRUE)))
# # View(average_betas)
# # str(average_betas)
# # max_index <- which.max(average_betas)
# # max_predictor <- predictor_columns[max_index]
# # max_predictor
#
# pred$study <- NULL
# pred$SMMv5Def <- NULL
# predictor_data <- pred
#
# # convert to right format for matrix multiplication
# average_betas_vector <- as.numeric(average_betas[1, ])
# predictor_matrix <- as.matrix(predictor_data)
#
# # Calc expected outcome
# # expected_outcome <- predictor_matrix %*% average_betas_vector
# expected_outcome <- as.matrix(predictor_data) %*% average_betas_vector
#
# # Print the expected outcomes
# # print(expected_outcome)


## NEW MODEL ##

# For planar data
sf::sf_use_s2(FALSE)

# Load requisite packages
library(dplyr)
library(sf)

# Import meta-analytic regression model
meta_analysis <- readRDS("../output/scaled_meta.rds")  # scaled model
meta_analysis <- readRDS("../output/unscaled_meta.rds")  # unscaled model

# Retrieve studies for predictions
pred <- readRDS("data/predictors_kriged_standardized.rds")
colnames(pred)[41] <- "predicted_OLD"  # rename to compare to new predictions
predictor_data <- pred
predictors <- meta_analysis$data$predictor[1:248]  # retrieve predictors

# Remove non-predictor columns
predictor_data <- predictor_data %>%
  select(all_of(predictors)) %>%
  select(order(colnames(.)))
# Reformat predictor columns
predictor_data <- predictor_data %>%
  mutate(across(all_of(predictors), as.character)) %>% # convert predictors to character first (needed for factorized columns)
  mutate(across(all_of(predictors), as.numeric)) %>% # convert all predictors to numeric
  mutate(across(all_of(predictors), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))  # sub NAs for statewide means
## NAs in the data can result in predicted values to be NA

## CALCULATE AND STORE STATEWIDE MEANS FOR PREDICTORS AS FILL-INS FOR EMPTY/MISSING PREDICTORS

# Extract coefficients and intercept from final meta-analysis model
betas <- meta_analysis$beta
# Remove "predictor" from predictor names
rownames(betas) <- gsub("predictor", "", rownames(betas))  # match predictor names
setdiff(colnames(predictor_data), rownames(betas))  # which predictor used as the intercept?
rownames(betas)[1] <- setdiff(colnames(predictor_data), rownames(betas))  # replace with predictor name
betas <- as.vector(betas)

# # Create the new model matrix and remove the intercept
# predgrid <- model.matrix(~predictors, data = predictor_data)[,-1]  ## TEST ##
# colnames(predgrid) <- gsub("predictors", "predictor", colnames(predgrid))
#
# test <- metafor::predict.rma(meta_analysis, newmods = predictor_data)

# Multiply predictors by the meta-analysis model betas
expected_outcome <- as.matrix(predictor_data) %*% betas

# Inverse probit for predicted values
predicted <- pnorm(expected_outcome)

# Add predictions to predictors data
pred$predicted <- predicted

# predicted3 <- predict(meta_analysis, transf=pnorm)  ## ????
#
# predicted <- mcp::iprobit(expected_outcome)  # Error: 'iprobit' is not an exported object from 'namespace:mcp'


# Load in original predictions and data for each site
choc <- st_transform(st_read("../Data/choctawatchee_bay/choctawatchee_bay_lssm_POINTS_0.001deg.shp"), crs = 6346) # Response:
pens <- st_transform(st_read("../Data/santa_rosa_bay/Santa_Rosa_Bay_Living_Shoreline_POINTS_0.001deg.shp"), crs = 6346) # Response:
tampa <- st_transform(st_read("../Data/tampa_bay/Tampa_Bay_Living_Shoreline_Suitability_Model_Results_POINTS_0.001deg.shp"), crs = 6346) # Response:
IRL <- st_transform(st_read("../Data/indian_river_lagoon/UCF_livingshorelinemodels_MosquitoNorthIRL_111m.shp"), crs = 6346) # Response:

coo <- c("OBJECTID", "ID", "geometry", "feature_x", "feature_y", "nearest_x", "nearest_y", "shape__len", "Shape__Len", "distance", "distance_2", "n", "x", "y", "X", "Y")
pred_dat <- as.data.frame(pred)

# Add predictions to each site
## Choctawatchee
choc_available_columns <- coo[coo %in% colnames(choc)]
choc_coo <- choc %>% select(all_of(choc_available_columns))
choc_pred <- pred_dat[pred_dat$study == "choc",]
choc_pred$predicted <- pred_dat[pred_dat$study == "choc", "predicted"]
choc_cols <- intersect(colnames(choc_pred), colnames(choc_coo))
choc_pred <- choc_pred %>%
  select(!all_of(choc_cols))

choc_predicted <- cbind(choc_coo, choc_pred)
write.csv(choc_predicted, "../output/Final_Shapefile_all_data/Choctawatchee Bay/choc_predicted_new.csv", row.names = FALSE)

## Pensacola Bay
pens_available_columns <- coo[coo %in% colnames(pens)]
pens_coo <- pens %>% select(all_of(pens_available_columns))
pens_pred <- pred_dat[pred_dat$study == "pens",]
pens_pred$predicted <- pred_dat[pred_dat$study == "pens", "predicted"]
pens_cols <- intersect(colnames(pens_pred), colnames(pens_coo))
pens_pred <- pens_pred %>%
  select(!all_of(pens_cols))

pens_predicted <- cbind(pens_coo, pens_pred)
write.csv(pens_predicted, "../output/Final_Shapefile_all_data/Pensacola Bay/pens_predicted_new.csv", row.names = FALSE)

# Tampa Bay
tampa_available_columns <- coo[coo %in% colnames(tampa)]
tampa_coo <- tampa %>% select(all_of(tampa_available_columns))
tampa_pred <- pred_dat[pred_dat$study == "tampa",]
tampa_pred$predicted <- pred_dat[pred_dat$study == "tampa", "predicted"]
tampa_cols <- intersect(colnames(tampa_pred), colnames(tampa_coo))
tampa_pred <- tampa_pred %>%
  select(!all_of(tampa_cols))

tampa_predicted <- cbind(tampa_coo, tampa_pred)
write.csv(tampa_predicted, "../output/Final_Shapefile_all_data/Tampa Bay/tampa_predicted_new.csv", row.names = FALSE)

# IRL
IRL_available_columns <- coo[coo %in% colnames(IRL)]
IRL_coo <- IRL %>% select(all_of(IRL_available_columns))
IRL_pred <- pred_dat[pred_dat$study == "IRL",]
IRL_pred$predicted <- pred_dat[pred_dat$study == "IRL", "predicted"]
IRL_cols <- intersect(colnames(IRL_pred), colnames(IRL_coo))
IRL_pred <- IRL_pred %>%
  select(!all_of(IRL_cols))

IRL_predicted <- cbind(IRL_coo, IRL_pred)
write.csv(IRL_predicted, "../output/Final_Shapefile_all_data/Indian River Lagoon/IRL_predicted_new.csv", row.names = FALSE)


# ## Test for plotting ##
#
# test <- st_transform(st_read("C:/Users/erika/OneDrive - University of Central Florida/General - SCRG/choctawatchee_bay_lssm/choctawatchee_bay_lssm.shp"), crs = 6346)
# IRL_test <- st_as_sf(IRL_predicted)
# plot_sf(IRL_test$predicted)
#
# choc_test <- choc_predicted[, c("feature_x", "feature_y", "predicted")]
# st_geometry(choc_test) <- choc_full$geometry
# st_crs(choc_test)
# plot_sf(choc_test$predicted)
# write.csv(choc_test, "../output/Final_Shapefile_all_data/Choctawatchee Bay/choc_predicted_test.csv", row.names = FALSE)
# write_sf(choc_test, "../output/Final_Shapefile_all_data/Choctawatchee Bay/choc_predicted_test.shp")


# choc_full <- st_transform(st_read("data/choctawatchee_bay/choctawatchee_bay_lssm_POINTS_0.001deg.shp"))
# IRL_full <- st_transform(st_read("../output/Final_Shapefile_all_data/Indian River Lagoon/IRL_predicted.shp"))
# pens_full <- st_transform(st_read("../output/Final_Shapefile_all_data/Pensacola Bay/pens_predicted.shp"))
# tampa_full <- st_transform(st_read("../output/Final_Shapefile_all_data/Tampa Bay/tampa_predicted.shp"))
#
# # Rename Erosion column due to duplicate naming
# colnames(choc)[45] <- "Erosion_1"
# colnames(IRL)[45] <- "Erosion_1"
# colnames(pens)[45] <- "Erosion_1"
# colnames(tampa)[44] <- "Erosion_1"
#
# # Keep full data set for convenient access
# choc_full <- choc
# IRL_full <- IRL
# pens_full <- pens
# tampa_full <- tampa
#
# # List numerical vars
# numerical_vars <- c("angle", "IT_Width", "Hab_W1",
#                     "Hab_W2", "Hab_W3", "Hab_W4", "Slope", "X3_m_depth", "X5_m_depth", "Slope_4",
#                     "X10th", "X20th", "X30th", "X40th", "X50th", "X60th", "X70th", "X80th",
#                     "X90th", "X99th", "MANGROVE")
#
# # List categorical vars
# categorical_vars <- c("bnk_height", "Beach", "WideBeach", "Exposure", "bathymetry",
#                       "roads", "PermStruc", "PublicRamp", "RiparianLU", "canal",
#                       "SandSpit", "Structure", "offshorest", "SAV", "marsh_all",
#                       "tribs", "defended", "rd_pstruc", "lowBnkStrc", "ShlType",
#                       "Fetch_", "selectThis", "StrucList", "forestshl",
#                       "City", "Point_Type", "Edge_Type", "Hard_Mater", "Adj_LU",
#                       "Erosion_1", "Erosion_2", "Owner", "Adj_H1", "Adj_H2", "Adj_H3",
#                       "Adj_H4", "V_Type1", "V_Type2", "V_Type3", "V_Type4",
#                       "Rest_Opp", "X0yster_Pr", "Seagrass_P", "Hardened_1", "WTLD_VEG_3")
# # note that study column is excluded here for easier processing later
#
# # List binary variables
# binary_vars <- c("Beach", "WideBeach", "PublicRamp", "canal", "SandSpit", "SAV",
#                  "defended", "selectThis", "Erosion_2", "Rest_Opp", "X0yster_Pr", "Seagrass_P", "Hardened_1")
#
# # Update list of categorical variables (no binary)
# categorical_vars2 <- setdiff(categorical_vars, binary_vars)
#
# # Combine data into list
# pred <- list(choc = choc, IRL = IRL, pens = pens, tampa = tampa)
#
# # Factorize categorical variables
# for (i in 1:length(pred)) {
#   for (j in 1:ncol(pred[[i]])) {
#     pred[[i]][[j]] <- gsub("NA", NA, pred[[i]][[j]])
#   }
# }
#
# # Reconvert to site-specific sf objects
# choc <- pred$choc %>%
#   st_set_geometry(choc_full$geometry) %>%
#   st_as_sf()
# IRL <- pred$IRL %>%
#   st_set_geometry(IRL_full$geometry) %>%
#   st_as_sf()
# pens <- pred$pens %>%
#   st_set_geometry(pens_full$geometry) %>%
#   st_as_sf()
# tampa <- pred$tampa %>%
#   st_set_geometry(tampa_full$geometry) %>%
#   st_as_sf()
#
# # Combine Data
# state <- dplyr::bind_rows(choc, pens, tampa, IRL)








## PLOTTING
# QGIS: ESRI World Imagery layer





############### TESTING #############
# coo <- c("OBJECTID", "ID", "geometry", "feature_x", "feature_y", "nearest_x", "nearest_y", "shape__len", "Shape__Len", "distance", "distance_2", "n", "x", "y", "X", "Y")
# state$predicted <- expected_outcome
#
# ##### TAMPA #####
# tampa_test <- subset(state, study == "tampa")
# tampa <- st_transform(st_read("../Data/tampa_bay/Tampa_Bay_Living_Shoreline_Suitability_Model_Results_POINTS_0.001deg.shp"), crs = 6346) # Response:
# tampa_available_columns <- coo[coo %in% colnames(tampa)]
# tampa_coo <- tampa %>% select(all_of(tampa_available_columns))
# tampa_predicted <- cbind(tampa_coo, tampa_test)
# write.csv(tampa_predicted, "../output/tampa_predicted.csv", row.names = FALSE)
#
# ##### CHOC #####
# choc_test <- subset(state, study == "choc")
# choc <- st_transform(st_read("../Data/choctawatchee_bay/choctawatchee_bay_lssm_POINTS_0.001deg.shp"), crs = 6346) # Response:
# choc_available_columns <- coo[coo %in% colnames(choc)]
# choc_coo <- choc %>% select(all_of(choc_available_columns))
# choc_predicted <- cbind(choc_coo, choc_test)
# write.csv(choc_predicted, "../output/choc_predicted.csv", row.names = FALSE)
#
# ##### PENS #####
# pens_test <- subset(state, study == "pens")
# pens <- st_transform(st_read("../Data/santa_rosa_bay/Santa_Rosa_Bay_Living_Shoreline_POINTS_0.001deg.shp"), crs = 6346) # Response:
# pens_available_columns <- coo[coo %in% colnames(pens)]
# pens_coo <- pens %>% select(all_of(pens_available_columns))
# pens_predicted <- cbind(pens_coo, pens_test)
# write.csv(pens_predicted, "../output/pens_predicted.csv", row.names = FALSE)
#
# ##### IRL #####
# IRL_test <- subset(state, study == "IRL")
# IRL <- st_transform(st_read("../Data/indian_river_lagoon/UCF_livingshorelinemodels_MosquitoNorthIRL_111m.shp"), crs = 6346) # Response:
# IRL_available_columns <- coo[coo %in% colnames(IRL)]
# IRL_coo <- IRL %>% select(all_of(IRL_available_columns))
# IRL_predicted <- cbind(IRL_coo, IRL_test)
# IRL_predicted$feature_x <- IRL$feature_x
# IRL_predicted$feature_y <- IRL$feature_y
# # note that coordinates need to be adjusted at some point before mapping (either in R or GIS)
# ## note that exports at Florida 17N
# write.csv(IRL_predicted, "../output/Final_Shapefile_all_data/IRL_predicted.csv", row.names = FALSE)
#
# # ##### LSSM4 #####
# # # All 4 LSSMs together
# # # LSSM4_predicted <- rbind(choc_predicted, pens_predicted, tampa_predicted, IRL_predicted)
# # LSSM4_predicted <- bind_rows(choc_predicted, pens_predicted, tampa_predicted, IRL_predicted)
# # write.csv(LSSM4_predicted, "output/LSSM4_predicted.csv", row.names = FALSE)
#
