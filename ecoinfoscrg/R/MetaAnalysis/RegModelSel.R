############################################
# PREP DATA
############################################
# GZ Note: If allowing users to set vars, force them to do clean up manually (i.e., edit this prep data chunk)
rm(list=ls())
library(nnet)  # easy for using multinomial logistic regression

# Grab data
data <- read.csv("Sandbox/Tampa_Bay_Living_Shoreline_Suitability_Model_Results.csv")

#Replace NULL or missing values with appropriate data
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

# List predictors (AKA column names of known predictors)
predictors <- c("Exposure", "RiparianLU", "bathymetry", "marsh_all", "bnk_height",
                "canal", "SandSpit", "forestshl", "Structure", "offshorest",
                "defended", "roads", "PermStruc", "Beach", "WideBeach", "tribs",
                "SAV", "PublicRamp")

# Define the response variable
response_var <- "BMPallSMM"

# Specify a short name of the model
name <- "Tampa Bay"

# Run build-up/pair-down R script
source("BUPD.R")
