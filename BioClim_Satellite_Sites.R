######## Extract BioClim Data for Satellite sites #########
########## code by Becca Nelson ####################
############ created on 8-20-25 ##############
############# Last modified 9-4-25 ############
rm(list = ls())

## note need to fix errors with chelsea tifs and elevation extractions 

# -------------------------
# 0. Original setup
# -------------------------
source("scripts/00_setup.R")

library(terra)
library(dplyr)
library(geodata)  # For WorldClim
library(elevatr)  # For elevation

# -------------------------
# 1. Subset your satellite sites (unchanged)
# -------------------------
sites <- data %>%
  filter(Type == "Satellite") %>%
  dplyr::select(Lat, Lon, site) %>%
  distinct()

sites_vect <- vect(sites, geom = c("Lon", "Lat"), crs = "EPSG:4326")

# -------------------------
# 2. Load WorldClim v2 bioclim rasters (BIO1–BIO19) – unchanged
# -------------------------
bioclim_rasters <- geodata::worldclim_global(var = "bio", res = 10, path = "data")

site_bioclim <- terra::extract(bioclim_rasters, sites_vect)

new_sites <- cbind(sites, site_bioclim[,-1])  # remove ID column

# Rename columns to match your previous workflow
colnames(new_sites)[4:22] <- c(
  "ann.mean.tmp",   # BIO1
  "mean.diurn.rng", # BIO2
  "isotherm",       # BIO3
  "tmp.seas",       # BIO4
  "max.tmp.wrm.m",  # BIO5
  "min.tmp.cld.m",  # BIO6
  "tmp.ann.rng",    # BIO7
  "mean.tmp.wet.q", # BIO8
  "mean.tmp.dry.q", # BIO9
  "mean.tmp.wrm.q", # BIO10
  "mean.tmp.cld.q", # BIO11
  "ann.prc",        # BIO12
  "prc.wet.m",      # BIO13
  "prc.dry.m",      # BIO14
  "prc.seas",       # BIO15
  "prc.wet.q",      # BIO16
  "prc.dry.q",      # BIO17
  "prc.wrm.q",      # BIO18
  "prc.cld.q"       # BIO19
)

# -------------------------
# 3. CHELSA bioclim rasters
# -------------------------
dir.create("data/CHELSA", showWarnings = FALSE)
chelsa_files <- paste0(
  "https://envidat.ch/storage/f/2019-10-11T00%3A00%3A00.000Z/CHELSA_bio_", 1:19, ".tif"
)
chelsa_paths <- file.path("data/CHELSA", paste0("bio_", 1:19, ".tif"))

for(i in 1:19){
  if(!file.exists(chelsa_paths[i])){
    download.file(chelsa_files[i], chelsa_paths[i], mode = "wb")
  }
}
##wrong URL
chelsa_rasters <- rast(chelsa_paths)
chelsa_values <- terra::extract(chelsa_rasters, sites_vect)
new_sites_chelsa <- cbind(sites, chelsa_values[,-1])  # remove ID column

# -------------------------
# 4. CHELSA metadata table
# -------------------------
chelsa_metadata <- tibble(
  variable = paste0("bio", 1:19),
  description = c(
    "Annual Mean Temperature", "Mean Diurnal Range", "Isothermality",
    "Temperature Seasonality", "Max Temperature of Warmest Month",
    "Min Temperature of Coldest Month", "Temperature Annual Range",
    "Mean Temperature of Wettest Quarter", "Mean Temperature of Driest Quarter",
    "Mean Temperature of Warmest Quarter", "Mean Temperature of Coldest Quarter",
    "Annual Precipitation", "Precipitation of Wettest Month",
    "Precipitation of Driest Month", "Precipitation Seasonality",
    "Precipitation of Wettest Quarter", "Precipitation of Driest Quarter",
    "Precipitation of Warmest Quarter", "Precipitation of Coldest Quarter"
  )
)

# -------------------------
# 5. Elevation extraction
# -------------------------
elev_rast <- get_elev_raster(locations = sites[,c("Lon","Lat")], z = 9)
site_elev <- terra::extract(elev_rast, sites_vect)
new_sites$elev <- site_elev[,2]  # second column has elevation

# -------------------------
# 6. Preview
# -------------------------
head(new_sites)          # WorldClim + elevation
head(new_sites_chelsa)   # CHELSA
chelsa_metadata
