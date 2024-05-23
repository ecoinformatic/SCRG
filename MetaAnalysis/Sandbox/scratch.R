library(nnet) # for multinomial logistic regressions

data <- read.csv("Tampa_Bay_Living_Shoreline_Suitability_Model_Results.csv")

# Convert predictors to factors
predictors <- c("Exposure", "RiparianLU", "bathymetry", "marsh_all", "bnk_height", "canal", "SandSpit", "forestshl", "Structure", "offshorest", "defended", "roads", "PermStruc", "Beach", "WideBeach", "tribs", "SAV", "PublicRamp")
data[predictors] <- lapply(data[predictors], as.factor)

# Convert response to factor
data$BMPallSMM <- factor(data$BMPallSMM)

# Multinomial logistic regression (simple additive for now)
## Needed to arbitrarily add `MaxNWts = 5000`` since model is quite complex
model <- multinom(BMPallSMM ~ Exposure + RiparianLU + bathymetry + marsh_all + bnk_height + canal + SandSpit + forestshl + Structure + offshorest + defended + roads + PermStruc + Beach + WideBeach + tribs + SAV + PublicRamp, data = data, MaxNWts = 5000)

# Model output
summary(model)
coeff <- summary(model)$coeff
oddsratio <- exp(coeff)


# NOTES
# Dependencies ‘googledrive’, ‘googlesheets4’, ‘httr’, ‘ragg’, ‘rvest’ are not available for package ‘tidyverse’