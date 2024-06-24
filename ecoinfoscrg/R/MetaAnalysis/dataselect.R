############################################
# PREP DATA
############################################
# GZ Note: If allowing users to set vars, force them to do clean up manually (i.e., edit this prep data chunk)
rm(list=ls())
library(nnet)  # easy for using multinomial logistic regression

# Grab data [ ]
# data <- read.csv("Sandbox/Tampa_Bay_Living_Shoreline_Suitability_Model_Results.csv")
data <- st_read("/home/gzaragosa/data/Created/Tampa_LSSM.shp")

# Remove geometry (for modeling)
data <- st_set_geometry(data, NULL)

# Replace NULL or missing values with appropriate data (may not need to do this) [ ]
data$SEAGRASS[is.na(data$SEAGRASS)] <- "None"
data$DESCRIPT[is.na(data$DESCRIPT)] <- "None" # Mangroves
data$WETLAND_TY[is.na(data$WETLAND_TY)] <- "None"
data$LANDUSE_DE[is.na(data$LANDUSE_DE)] <- "None"

# Convert all categorical variables to factors
data$SEAGRASS <- factor(data$SEAGRASS)
data$DESCRIPT <- factor(data$DESCRIPT)
data$WETLAND_TY <- factor(data$WETLAND_TY)
data$LANDUSE_DE <- factor(data$LANDUSE_DE)
data$BMPallSMM <- factor(data$BMPallSMM)

# List predictors (AKA column names of known predictors) [ ]
predictors <- c("SEAGRASS", "DESCRIPT", "WETLAND_TY", "LANDUSE_DE") # no data in WMO_WIND for now

# Define the response variable [ ]
response_var <- "BMPallSMM" 

# Specify a short name of the model [x]
name <- "TampaJoinTest"

# # Run build-up/pair-down R script [x]
source("/home/gzaragosa/Documents/SCRG/MetaAnalysis/BUPD.R")

# # Test model
# library(nnet)
# model <- multinom(BMPallSMM ~ SEAGRASS + DESCRIPT + WETLAND_TY + LANDUSE_DE, data = data, MaxNWts = 5000, trace = TRUE)

output_form <- readRDS("/home/gzaragosa/Documents/SCRG/TampaJoinTest_final_form.rds")
head(output_form)
# output_model <- readRDS("/home/gzaragosa/Documents/SCRG/TampaJoinTest_final_model.rds")
output_OR <- readRDS("/home/gzaragosa/Documents/SCRG/TampaJoinTest_odds_ratios.rds")
