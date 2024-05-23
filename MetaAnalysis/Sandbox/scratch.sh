...
#####
# Note: The attribute table is contained in the .dbf file
## Can view the .dbf file viewer with: sudo apt-get install dbview
dbview Shoreline_Characterization_N_IRL.dbf | less

#####
# Convert .dbf to .csv
import geopandas as gpd
# Load the DBF file
file_path = '~/data/Tampa_Bay_Living_Shoreline_Suitability_Model_Results/Tampa_Bay_Living_Shoreline_Suitability_Model_Results.dbf'
gdf = gpd.read_file(file_path)
# Save to CSV
output_path = '/home/gzaragosa/Documents/SCRG/MetaAnalysis/Sandbox/Tampa_Bay_Living_Shoreline_Suitability_Model_Results.csv'
gdf.to_csv(output_path, index=False)


# #####
# # Convert .dbf to .csv
# import geopandas as gpd
# # Load the DBF file
# file_path = '~/data/UCF_living_shoreline/Shoreline_Characterization_N_IRL/Shoreline_Characterization_N_IRL.dbf'
# gdf = gpd.read_file(file_path)
# # Save to CSV
# output_path = '/home/gzaragosa/Documents/SCRG/MetaAnalysis/Sandbox/Shoreline_Characterization_N_IRL.csv'
# gdf.to_csv(output_path, index=False)


# #####
# # Convert .dbf to .csv
# import geopandas as gpd
# # Load the DBF file
# file_path = '~/data/UCF_living_shoreline/Shoreline_Characterization_Docs/Volusia_Profile_Point.dbf'
# gdf = gpd.read_file(file_path)
# # Save to CSV
# output_path = '/home/gzaragosa/Documents/SCRG/MetaAnalysis/Sandbox/Volusia_Profile_Point.csv'
# gdf.to_csv(output_path, index=False)

# # Explore .xlsx files
# ## pip install visidata
# vd UCF_DeSoto_Monitoring_Data_2017_2018.xlsx




