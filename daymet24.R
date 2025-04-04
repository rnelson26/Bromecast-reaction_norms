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





########## get climate info ######



# Define the base directory and the subfolders (tiles)
base_dir <- "/Users/Becca/Desktop/daymet2024"
folders <- c("11551_2024", "11558_2024", "11735_2024", "11911_2024", "12094_2024", "11736_2024")

# Corrected file names
daymet_files <- c(
  dayl = "dayl.nc",
  prcp = "prcp.nc",   # corrected from "precip.nc"
  srad = "srad.nc",
  swe = "swe.nc",
  tmax = "tmax.nc",
  tmin = "tmin.nc",
  vp = "vp.nc"
)

# Initialize an empty list to store all the data
all_daymet_data <- list()

# Loop through each folder and read the NetCDF files
for (folder in folders) {
  folder_path <- file.path(base_dir, folder)
  
  # Read each variable and store in a list
  var_data <- lapply(daymet_files, function(file) {
    nc_path <- file.path(folder_path, file)
    rast(nc_path)
  })
  
  names(var_data) <- names(daymet_files)
  
  # Store in main list
  all_daymet_data[[folder]] <- var_data
}

# Example: access prcp from tile 11911_2024
prcp_raster_example <- all_daymet_data[["11911_2024"]]$prcp

######## Link to location ######

# Define locations and their corresponding tiles
locations <- data.frame(
  name = c("dino", "GoeblS1", "Pearlwise", "Peavine", "Plymouth", "SouthEden", "SSHigh", "SSHQ"),
  tile = c(11736, 11558, 11911, 11551, 11551, 11735, 12094, 12094),
  lon = c(-109.249642, -105.095917, -118.80428, -119.878406, -119.783711, -111.275659, -112.096947, -112.213263),
  lat = c(40.422908, 39.550182, 42.33939, 39.591003, 39.072101, 41.926717, 44.295988, 44.246799)
)



# Make a column to match your folder names
locations <- locations %>%
  mutate(folder = paste0(tile, "_2024"))

# Select variables to extract
variables <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")

# Create a list to store results
location_climate <- list()


# Loop through locations

# Your list of rasters, already loaded
# all_daymet_data <- list(tile = list(dayl = ..., prcp = ..., etc.))

# Your locations (already defined)
locations <- data.frame(
  name = c("dino", "GoeblS1", "Pearlwise", "Peavine", "Plymouth", "SouthEden", "SSHigh", "SSHQ"),
  tile = c(11736, 11558, 11911, 11551, 11551, 11735, 12094, 12094),
  lon = c(-109.249642, -105.095917, -118.80428, -119.878406, -119.783711, -111.275659, -112.096947, -112.213263),
  lat = c(40.422908, 39.550182, 42.33939, 39.591003, 39.072101, 41.926717, 44.295988, 44.246799)
)

# Convert locations to spatial points (WGS84)
location_vect_wgs84 <- vect(locations, geom = c("lon", "lat"), crs = "EPSG:4326")


# Extract data
location_climate <- list()

for (i in 1:nrow(locations)) {
  loc <- locations[i, ]
  tile_id <- paste0(as.character(loc$tile), "_2024")  # Add "2024" suffix to tile_id
  
  # Check if the tile exists in the data
  if (is.null(all_daymet_data[[tile_id]])) {
    cat("Tile not found:", tile_id, "\n")
    next  # Skip to the next iteration if the tile is not found
  }
  
  rasters <- all_daymet_data[[tile_id]]
  
  # Check if 'tmax' exists in rasters
  if (is.null(rasters$tmax)) {
    cat("No 'tmax' raster found for tile", tile_id, "\n")
    next  # Skip this iteration if 'tmax' raster is missing
  }
  
  # Manually define the CRS for the raster if necessary
  crs(rasters$tmax) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  # Reproject the point to match the raster CRS
  loc_vect <- vect(loc, geom = c("lon", "lat"), crs = "EPSG:4326")
  loc_proj <- project(loc_vect, crs(rasters$tmax))  # Reproject to match raster CRS
  
  # Extract values for each raster variable
  loc_values <- map(rasters, ~ extract(.x, loc_proj)[1, -1])  # Drop the ID column
  
  # Combine extracted values into a data frame with each variable's data as columns
  loc_df <- as.data.frame(loc_values)
  
  # Ensure we have one row for each day (365 rows)
  loc_df <- t(loc_df)  # Transpose so each row corresponds to a day
  loc_df <- as.data.frame(loc_df)
  
  # Add day column (1 to 365) and location name
  loc_df$day <- 1:365
  loc_df$name <- loc$name
  
  # Store the result in the list
  location_climate[[loc$name]] <- loc_df
}


# Combine all data frames in location_climate into one
combined_data <- bind_rows(location_climate, .id = "SiteCode")

# The .id argument adds a "SiteCode" column indicating which site each row belongs to
# Write the combined data frame to a CSV file
write.csv(combined_data, "location_climate_data.csv", row.names = FALSE)


###### make into df format######
# Assuming 'combined_data' has row names in the form of 'variable_name_variable_value'

final_data <- data.frame()

# Loop through each site in location_climate
for (site_name in names(location_climate)) {
  
  # Extract the site data for this particular site
  site_data <- location_climate[[site_name]]
  
  # Create a new dataframe to store the structured data for this site
  site_final <- data.frame()
  
  # Extract variables from row names using regular expressions
  # For example, extract 'srad' and '267' from 'srad.srad_267'
  rownames_split <- strsplit(rownames(site_data), "\\.")  # Split by dot
  
  # Extract variable names and values from rownames
  variables <- sapply(rownames_split, function(x) x[1])  # Variable name (e.g., 'srad')
  values <- sapply(rownames_split, function(x) gsub("[^0-9]", "", x[2]))  # Extract numeric part from 'srad_267'
  
  # Combine variables and values into a dataframe
  variable_data <- data.frame(variable = variables, value = as.numeric(values))
  
  # Spread the variable data into columns
  site_final <- reshape(variable_data, timevar = "variable", idvar = "rowname", direction = "wide")
  
  # Add necessary columns like 'year', 'yday', 'climYr', 'climDay', etc.
  site_final$year <- rep(2024, nrow(site_final))  # Assuming data is for the year 2024
  site_final$yday <- 1:nrow(site_final)  # Assuming day of year is from 1 to nrow (change if needed)
  site_final$climYr <- site_final$year
  site_final$climDay <- site_final$yday
  
  # Season classification (you can adjust this as needed)
  site_final$season <- ifelse(site_final$yday <= 80 | site_final$yday >= 355, "Winter", 
                              ifelse(site_final$yday <= 171, "Spring", 
                                     ifelse(site_final$yday <= 264, "Summer", "Fall")))
  
  # Compute tavg (assuming tmax and tmin exist)
  site_final$tavg <- (site_final$tmax + site_final$tmin) / 2
  
  # Add site name as SiteCode
  site_final$SiteCode <- site_name
  
  # Reorder columns to match the required format
  site_final <- site_final[, c("year", "yday", "daylength", "prcp", "radiation", "swe", "tmax", "tmin", 
                               "vp", "SiteCode", "climYr", "climDay", "season", "tavg")]
  
  # Combine the site data into the final dataframe
  final_data <- rbind(final_data, site_final)
}

# Write the final data to CSV
write.csv(final_data, "final_climate_data.csv", row.names = FALSE)





# Create a new data frame with the required columns
site_data <- data.frame(
  year = rep(2024, length(tmax)),  # Assuming the year is 2024; modify as needed
  yday = loc_df$day,               # Day of the year (1 to 365)
  daylength = loc_df$dayl.dayl_1,  # Use the available daylength column
  prcp = loc_df$prcp,              # Assuming prcp column exists
  radiation = loc_df$radiation,    # Assuming radiation column exists
  swe = loc_df$swe,                # Assuming swe column exists
  tmax = loc_df$tmax,              # Assuming tmax column exists
  tmin = loc_df$tmin,              # Assuming tmin column exists
  vp = loc_df$vp,                  # Assuming vp column exists
  SiteCode = rep(site_name, length(tmax)),  # Assign site name as SiteCode
  climYr = rep(2024, length(tmax)),         # Assuming climYr is 2024
  climDay = loc_df$day,                    # Same as yday or day of year
  season = rep("Spring", length(tmax)),    # Assuming season is Spring; modify based on logic
  tavg = (loc_df$tmax + loc_df$tmin) / 2  # Calculate average temperature
)






