##########################################
### Extract Daymet daily data for seed source sites ###########
### Date created: 9/24/25 #########
## last modified: 9/24/25 ###########
##########################################
## does not work, need to get correct tiles 


library(terra)
library(dplyr)
library(purrr)
library(readr)

### INPUTS ----
base_dir <- "/Users/Becca/Desktop/daymet2024"
seed_sites <- read_csv("seed_sites.csv")  # columns: site_code, lon, lat

# Tiles covering western US + southern Canada
folders <- c("11551_2024", "11558_2024", "11735_2024", "11911_2024", "12094_2024", "11736_2024")

daymet_files <- c(
  dayl = "dayl.nc",
  prcp = "prcp.nc",
  srad = "srad.nc",
  swe  = "swe.nc",
  tmax = "tmax.nc",
  tmin = "tmin.nc",
  vp   = "vp.nc"
)

### LOAD ALL DAYMET TILES ----
all_daymet_data <- list()
tile_extents <- list()
for (folder in folders) {
  folder_path <- file.path(base_dir, folder)
  var_data <- lapply(daymet_files, function(file) rast(file.path(folder_path, file)))
  names(var_data) <- names(daymet_files)
  
  all_daymet_data[[folder]] <- var_data
  tile_extents[[folder]] <- ext(var_data[[1]])  # store extent for tile selection
}

### EXTRACT FOR SITES ----
site_data_list <- list()

for (i in 1:nrow(seed_sites)) {
  site <- seed_sites[i, ]
  
  # Find which tile contains the site
  tile_id <- NULL
  for (folder in folders) {
    e <- tile_extents[[folder]]
    if (site$lon >= e$xmin & site$lon <= e$xmax &
        site$lat >= e$ymin & site$lat <= e$ymax) {
      tile_id <- folder
      break
    }
  }
  
  if (is.null(tile_id)) {
    warning("Site outside all tile extents: ", site$site_code)
    next
  }
  
  rasters <- all_daymet_data[[tile_id]]
  
  # Set CRS if necessary
  crs(rasters$tmax) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  # Reproject site
  site_vect_proj <- project(vect(site, geom = c("lon", "lat"), crs = "EPSG:4326"), crs(rasters$tmax))
  
  # Extract values
  loc_values <- map(rasters, ~ extract(.x, site_vect_proj)[1, -1])
  
  # Check extraction worked
  if (any(sapply(loc_values, is.null)) | all(is.na(unlist(loc_values)))) {
    warning("Extraction failed for site: ", site$site_code)
    next
  }
  
  # Convert to data.frame
  loc_df <- as.data.frame(do.call(cbind, loc_values))
  loc_df$yday <- 1:nrow(loc_df)
  loc_df$year <- 2024
  loc_df$site_code <- site$site_code
  
  # Climate year/day
  loc_df$climYr <- ifelse(loc_df$yday > 273, loc_df$year + 1, loc_df$year)
  loc_df$climDay <- ifelse(loc_df$yday > 273, loc_df$yday - 273, loc_df$yday + (365 - 273))
  
  # Season
  loc_df$season <- "Win"
  loc_df$season[loc_df$climDay < 92] <- "Fall"
  loc_df$season[loc_df$climDay > 184 & loc_df$climDay < 276] <- "Spr"
  loc_df$season[loc_df$climDay >= 276] <- "Sum"
  
  # Daily mean temp
  loc_df$tavg <- (loc_df$tmax + loc_df$tmin)/2
  
  site_data_list[[site$site_code]] <- loc_df
}

### COMBINE AND SAVE ----
daymet_seed_sites <- bind_rows(site_data_list)
write_csv(daymet_seed_sites, "daymet_seed_sites_daily_2024.csv")

