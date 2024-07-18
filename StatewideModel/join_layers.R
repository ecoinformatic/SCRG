rm(list=ls())
library(sf)
library(raster)

##### GRAB LSSM DATA (All in WGS84) #####
Tampa_LSSM <- st_transform(st_read("../Data/join_layersr/Tampa_Bay_Living_Shoreline_Suitability_Model_Results_POINTS_0.001deg.shp"), crs = 6346) #BMPallSMM
Choc_LSSM <- st_transform(st_read("../Data/join_layersr/choctawatchee_bay_lssm_POINTS_0.001deg.shp"), crs = 6346) # SMMv5Class
Pens_LSSM <- st_transform(st_read("../Data/join_layersr/Santa_Rosa_Bay_Living_Shoreline_POINTS_0.001deg.shp"), crs = 6346) # SMMv5Class
IRL_LSSM <- st_transform(st_read("/home/gzaragosa/data/UCF_living_shoreline/Shoreline_Characterization_N_IRL/Shoreline_Characterization_N_IRL_POINTS_111m.shp"), crs = 6346) # Priority

#####
# Subsets
Tampa_LSSM = Tampa_LSSM[, c("BMPallSMM")] 
Choc_LSSM = Choc_LSSM[, c("SMMv5Class")]
Pens_LSSM = Pens_LSSM[, c("SMMv5Class")]
IRL_LSSM = IRL_LSSM[, c("Priority")]

##### GRAB RASTER DATA #####
# WorldClim Data
WC_wind <- raster("/home/gzaragosa/data/WorldClim/wc2.1_30s_wind/wc2.1_30s_wind_01.tif")
# WC_wind <- projectRaster(WC_wind, crs = CRS("EPSG:6346"))

WC_elev <- raster("/home/gzaragosa/data/WorldClim/wc2.1_30s_elev/wc2.1_30s_elev.tif")
# WC_elev <- projectRaster(WC_elev, crs = CRS("EPSG:6346"))

WC_temp <- raster("/home/gzaragosa/data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_1.tif") # Mean Temp (BIO1)
# WC_temp <- projectRaster(WC_temp, crs = CRS("EPSG:6346"))

WC_precip <- raster("/home/gzaragosa/data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_12.tif") # Annual Precipitation (BIO12)
# WC_precip <- projectRaster(WC_precip, crs = CRS("EPSG:6346"))

# Mangrove Above Ground Biomass (Raster)
# mangrove_abv <- raster("../Data/Mangrove_Above_Ground_Biomass/Mangrove_agb_UnitedStates.tif")


##### GRAB SHAPEFILE DATA #####
# Seagrass Integrated Mapping and Monitoring
seagrass <- st_transform(st_read("/home/gzaragosa/data/SIMM/Seagrass_Habitat_in_Florida.shp"), crs = 6346)
seagrass = seagrass[, c("SEAGRASS")] # States: Continuous, Discontinuous

# NOAA Mangrove Habitat Shapefile
mangrove <- st_transform(st_read("../Data/NOAA_data/Mangrove_Habitat_in_Florida.shp"), crs = 6346)
mangrove = mangrove[, c("DESCRIPT")] # States: Mangrove Swamp

# NOAA Wetlands Data - NAD_1983_Albers, EPSG 5070?
wetlands <- st_transform(st_read("/home/gzaragosa/data/NOAA/Biotic/Wetlands/FL_shapefile_wetlands/FL_Wetlands.shp"), crs = 6346)
wetlands = wetlands[, c("WETLAND_TY")] # States: Estuarine and Marine Deepwater, Estuarine and Marine Wetland, Lake, Freshwater Pond, Freshwater Emergent Wetland, Freshwater Forested/Shrub Wetland, Riverine, Other

# Bathymetry Data Countours
# bath_con <- st_read("../Data/Bathymetry/Bathymetry_Contours_Southeast_United_States.shp")
# Need to get additional contour data for the NW edge of the state

# FL Land Use and Land Cover Data - NAD83(2011) / Florida GDL Albers, EPSG 6439
FL_LULC <- st_transform(st_read("/home/gzaragosa/data/Statewide_Land_Use_Land_Cover/Statewide_Land_Use_Land_Cover.shp"), crs = 6346)
FL_LULC = FL_LULC[, c("LANDUSE_DE")] # States: Many Land Use Descriptions

# IBTrACS Hurrican Data
hurricane <- st_transform(st_read("../Data/IBTrACS/IBTrACS.NA.list.v04r00.lines.shp"), crs = 6346)
hurricane = hurricane[, c("WMO_WIND")]










# Testing with joined IRL_LSSM (still need to fix raster stuff)
IRL_LSSM <- st_join(IRL_LSSM, seagrass, left = TRUE, join = st_intersects)
IRL_LSSM <- st_join(IRL_LSSM, mangrove, left = TRUE, join = st_intersects)
IRL_LSSM <- st_join(IRL_LSSM, wetlands, left = TRUE, join = st_intersects)
IRL_LSSM <- st_join(IRL_LSSM, FL_LULC, left = TRUE, join = st_intersects)
IRL_LSSM <- st_join(IRL_LSSM, hurricane, left = TRUE, join = st_intersects)

# Save (need to check output projection)
st_write(IRL_LSSM, "/home/gzaragosa/data/Created/IRL_LSSM.shp", delete_layer = FALSE) 
