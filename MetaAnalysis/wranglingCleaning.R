rm(list=ls())
library(metafor)
library(sf)
library(dplyr)

# Choctawatchee data (transformed 0.001deg.shp from WGS 84 to 6346 17N )
choc <- st_transform(st_read("/home/gzaragosa/data/choctawatchee_bay_lssm/choctawatchee_bay_lssm_POINTS_0.001deg.shp"), crs = 6346) # Reponse: SMMv5Class

# Pensacola data (transformed 0.001deg.shp from WGS 84 to 6346 17N )
pens <- st_transform(st_read("/home/gzaragosa/data/pensacola_lssm/Santa_Rosa_Bay_Living_Shoreline_POINTS_0.001deg.shp"), crs = 6346) # Reponse: SMMv5Class

# Tampa data (transformed 0.001deg.shp from WGS 84 to 6346 17N )
tampa <- st_transform(st_read("/home/gzaragosa/data/Tampa_Bay_Living_Shoreline_Suitability_Model_Results/Tampa_Bay_Living_Shoreline_Suitability_Model_Results_POINTS_0.001deg.shp"), crs = 6346) # Response: 

# IRL data (transformed 0.001deg.shp from WGS 84 to 6346 17N )
IRL <- st_transform(st_read("/home/gzaragosa/data/Final NIRL Shapefile_all data/Final Shapefile_all data/UCF_livingshorelinemodels_MosquitoNorthIRL_111m.shp"), crs = 6346)

# colnames(choc)
# colnames(pens)
# colnames(tampa)
# colnames(IRL)
# View(choc)
# View(pens)
# View(tampa)
# View(IRL)

####################################
# RESPONSE VARIABLES
####################################
# choc$SMMv5Class, pens$SMMv5Class, tampa$BMPallSMM # Reponse variables for VIMS
# IRL$Priority $ Response variable for IRL study

####################################
# PREDICTOR DATA
####################################
# Columns that are geodata/metadata and can be removed across studies
choc <- st_drop_geometry(choc)
pens <- st_drop_geometry(pens)
tampa <- st_drop_geometry(tampa)
IRL <- st_drop_geometry(IRL)
drop <- c("OBJECTID", "ID", "geometry", "feature_x", "feature_y", "nearest_x", "nearest_y", "shape__len", "Shape__Len", "distance", "distance_2", "n", "x", "y", "X", "Y")
choc <- choc[, !(colnames(choc) %in% drop)]
pens <- pens[, !(colnames(pens) %in% drop)]
tampa <- tampa[, !(colnames(tampa) %in% drop)]
IRL <- IRL[, !(colnames(IRL) %in% drop)]

# Columns that can be removed from individual studies 
# Remove metadata columns and other shapefile stuff
choc <- choc[, !(colnames(choc) %in% c("DefDate", "Needs_QC", "bmpCountv5", "SMMv5Def"))] # DefDate is just a date of data entry
pens <- pens[, !(colnames(pens) %in% c("Additional", "Permitting"))] # "Additional" and "Permitting" are links in the pens data
tampa <- tampa[, !(colnames(tampa) %in% c("Source"))]
IRL <- IRL[, !(colnames(IRL) %in% c("Comments"))]

# Rename columns to be consisent
## Columns that should be the same:
### choc$MxQExpCode, pens$exposure, tampa$Exposure
### choc$Beach, pens$beach, tampa$Beach
### choc$WideBeach, pens$widebeach, tampa$WideBeach
choc <- choc %>% rename(
  Exposure = MxQExpCode,
  Beach = Beach,
  WideBeach = WideBeach,
  Response = SMMv5Class
)
pens <- pens %>% rename(
  Exposure = exposure,
  Beach = beach,
  WideBeach = widebeach,
  Response = SMMv5Class
)
tampa <- tampa %>% rename(
  Exposure = Exposure,
  Beach = Beach,
  WideBeach = WideBeach,
  Response = BMPallSMM
)
IRL$Priority <- as.character(IRL$Priority)
IRL <- IRL %>% rename(
  Response = Priority
)

# Add a 'study' column
choc$study <- 'choc'
pens$study <- 'pens'
tampa$study <- 'tampa'
IRL$study <- 'IRL'

# Combine Data
state <- dplyr::bind_rows(choc, pens, tampa, IRL) 
pred <- state %>% 
  select(-"Response") # Remove response variables

####################################
# SPELL CHECK
####################################
library(hunspell)
library(dplyr)
library(stringr)

# Review misspelled/suggestions
words_to_check <- unique(unlist(pred))
misspelled_info <- hunspell_check(words_to_check)
misspelled_words <- words_to_check[!misspelled_info]
suggestions <- hunspell_suggest(misspelled_words)
print(data.frame(misspelled = misspelled_words, suggestions = I(suggestions)))

# Spelling corrections (words needs to be chosen manually)
corrections <- data.frame(
  incorrect = c("YEs", "RIprap", "riprap", "Permament", "Permanenet", "Bulkead", "Bulkhea", "BUlkhead"),
  correct = c("Yes", "Riprap", "Riprap", "Permanent", "Permanent", "Bulkhead", "Bulkhead", "Bulkhead")
)

pred <- pred %>%
  mutate(across(where(is.character), ~{
    column <- .
    for (i in seq_along(corrections$incorrect)) {
      # Use regex to match case-insensitively
      pattern <- str_c("(?i)\\b", corrections$incorrect[i], "\\b")
      column <- str_replace_all(column, regex(pattern), corrections$correct[i])
    }
    column
  }))


# ################### UTILS/UNIQUE VALUES ####################
# # Get observed states for each column:
# table(tolower(pens$exposure), useNA = "ifany") # Count info
# table(tolower(choc$MxQExpCode), useNA = "ifany")
# table(tolower(tampa$Exposure), useNA = "ifany")
# table(tolower(pens$exposure), useNA = "ifany")
# ############################################################


