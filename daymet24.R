###### Pull 2024 daymet data #########
###### code by Becca Nelson #######
######## created 4/3/25 ########
### last modfied 4/4/25 by Becca ##########

###### load required packages #######
library(ncdf4)
library(raster)  # terra also works
library(dplyr)
library(purrr)
library(daymetr)

library(terra)

# Set the path to your NetCDF files
base_path <- "/Users/Becca/Desktop/daymet2024/11551_2024"

# List of Daymet variables and corresponding file names
daymet_files <- c(
  dayl = "dayl.nc",
  precip = "prcp.nc",
  srad = "srad.nc",
  swe = "swe.nc",
  tmax = "tmax.nc",
  tmin = "tmin.nc",
  vp = "vp.nc"
)

# Read each NetCDF file into a SpatRaster and store in a list
daymet_data <- lapply(daymet_files, function(file) {
  nc_path <- file.path(base_path, file)
  rast(nc_path)
})

# Assign names to the list elements
names(daymet_data) <- names(daymet_files)

# Example: access the tmax raster
tmax_raster <- daymet_data$tmax




########## get climate info ######

# Define the base directory where the tile folders are stored
base_dir <- "/Users/Becca/Desktop/daymet2024"

# Define locations and their corresponding tiles
locations <- data.frame(
  name = c("dino", "GoeblS1", "Pearlwise", "Peavine", "Plymouth", "SouthEden", "SSHigh", "SSHQ"),
  tile = c(11736, 11558, 11911, 11551, 11551, 11735, 12094, 12094),
  lon = c(-109.249642, -105.095917, -118.80428, -119.878406, -119.783711, -111.275659, -112.096947, -112.213263),
  lat = c(40.422908, 39.550182, 42.33939, 39.591003, 39.072101, 41.926717, 44.295988, 44.246799)
)


library(ncdf4)
library(dplyr)

# Define the function to read data from local Daymet .nc files
 read_local_daymet <- function(site, lat, lon, start_year, end_year, base_dir) {
  # Find the corresponding tile for the site (adjust according to your tiles)
  # You may need to adjust how tiles are mapped based on your data
  # In this example, we use tile 11558 (for demonstration)
  tile <- "11558"  
  file_path <- file.path(base_dir, paste0(tile, "_2024"), paste0("dayl.nc"))
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    message("File not found: ", file_path)
    return(NULL)
  }
  
  # Open the NetCDF file
  nc <- nc_open(file_path)
  
  # Extract latitude and longitude
  lon_vals <- ncvar_get(nc, "lon")
  lat_vals <- ncvar_get(nc, "lat")
  
  # Find the nearest lat and lon indices
  idx_lon <- which.min(abs(lon_vals - lon))
  idx_lat <- which.min(abs(lat_vals - lat))
  
  # Extract the time dimension (assuming time is the third dimension)
  time_vals <- ncvar_get(nc, "time_bnds")
  time_dim <- length(time_vals)
  
  # Extract the variable data (daylight hours in this case)
  dayl_data <- ncvar_get(nc, "dayl", 
                         start = c(idx_lon, idx_lat, 1), 
                         count = c(1, 1, time_dim))
  
  # Convert the extracted data to a data frame
  data_df <- data.frame(
    site = site,
    date = as.Date(time_vals, origin = "1970-01-01"),  # Assuming 'time_bnds' is a date range
    dayl = dayl_data
  )
  
  # Close the NetCDF file
  nc_close(nc)
  
  return(data_df)
}

# Example usage: Create a data frame with sites and their coordinates
siteD <- data.frame(
  site = c("GoeblS1", "Peavine", "Plymouth"),
  Lat = c(39.550182, 39.591003, 39.072101),
  Lon = c(-105.095917, -119.878406, -119.783711)
)

# Apply the function for all sites
climD <- bind_rows(lapply(1:nrow(siteD), function(i) {
  tmp <- read_local_daymet(site = siteD$site[i],
                           lat = siteD$Lat[i],
                           lon = siteD$Lon[i],
                           start_year = 2020,
                           end_year = 2023,
                           base_dir = "/Users/Becca/Desktop/daymet2024")
  tmp$SiteCode <- siteD$site[i]
  return(tmp)
}))

# View the final data
head(climD)
