######## SMAP Data Extraction ##############################################
##### get soil moisture info from SMAP #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 2/6/25 #######
############ last modified: 2/19/25 ########################


#### load packages #####
library(terra)  # For raster processing
library(rhdf5)  # For reading HDF5 files
  

##### Processing SMAP data ####

# Inspect a specific file to list datasets
h5_file <- "/Users/Becca/SMAP_data/SMAP_L2_SM_SP_1AIWDV_20200323T142815_20200324T015116_118W37N_R17000_001.h5"
h5_contents <- h5ls(h5_file)

# Print the names of all datasets in the HDF5 file
print(h5_contents)


# Set working directory where the SMAP data is stored
smap_dir <- "/Users/Becca/SMAP_data/"  

# List all HDF5 files in the directory
h5_files <- list.files(smap_dir, pattern = "\\.h5$", full.names = TRUE)

# Function to process a single SMAP HDF5 file
process_smap_h5 <- function(h5_file) {
  # Open the HDF5 file and check available datasets
  h5_contents <- h5ls(h5_file)
  
  # Define the dataset path for soil moisture
  dataset_path <- "/Soil_Moisture_Retrieval_Data_3km/soil_moisture"
  
  # Check if the dataset exists in this file
  if (!dataset_path %in% h5_contents$name) {
    message("Dataset not found in file: ", h5_file)
    return(NULL)
  }
  
  # Read soil moisture data
  soil_moisture <- h5read(h5_file, dataset_path)
  
  # Read latitude and longitude (ensure paths exist)
  lat_path <- "/Soil_Moisture_Retrieval_Data_AM/latitude"
  lon_path <- "/Soil_Moisture_Retrieval_Data_AM/longitude"
  
  if (!(lat_path %in% h5_contents$name) || !(lon_path %in% h5_contents$name)) {
    message("Latitude or longitude missing in file: ", h5_file)
    return(NULL)
  }
  
  lat <- h5read(h5_file, lat_path)
  lon <- h5read(h5_file, lon_path)
  
  # Ensure the data dimensions match (SMAP data is usually in a grid)
  if (!all(dim(soil_moisture) == dim(lat), dim(soil_moisture) == dim(lon))) {
    message("Dimension mismatch in file: ", h5_file)
    return(NULL)
  }
  
  # Create raster
  r <- rast(soil_moisture)
  
  # Set spatial extent (bounding box)
  ext(r) <- ext(min(lon), max(lon), min(lat), max(lat))  
  crs(r) <- "EPSG:4326"  # WGS84 projection
  
  # Save as GeoTIFF
  output_tif <- file.path(smap_dir, paste0(tools::file_path_sans_ext(basename(h5_file)), ".tif"))
  writeRaster(r, output_tif, format = "GTiff", overwrite = TRUE)
  
  message("Processed and saved: ", output_tif)
  return(output_tif)
}

# Loop through all files and process them
tif_files <- lapply(h5_files, process_smap_h5)

# Print summary
message("Finished processing ", length(na.omit(tif_files)), " files.")
