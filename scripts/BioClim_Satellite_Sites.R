######## Extract BioClim Data for Satellite sites #########
########## code by Becca Nelson ####################
############ created on 8-20-25 ##############
############# Last modified 8-20-25 ############

rm(list = ls())

source("scripts/00_setup.R")

library(terra)
library(dplyr)
library(geodata)  # for worldclim_global()

# -------------------------
# 1. Subset your sites
# -------------------------
sites <- data %>%
  filter(Type == "Satellite") %>%
  dplyr::select(Lat, Lon, site) %>%
  distinct()

# -------------------------
# 2. Load WorldClim bioclim rasters
# -------------------------
# WorldClim v2.1, ~10 arcmin resolution (~18 km)
bioclim_rasters <- geodata::worldclim_global(var = "bio", res = 10, path = "data")

# -------------------------
# 3. Extract values at sites
# -------------------------
sites_vect <- vect(sites, geom = c("Lon", "Lat"), crs = "EPSG:4326")

site_bioclim <- terra::extract(bioclim_rasters, sites_vect)

# -------------------------
# 4. Combine with sites
# -------------------------
new_sites <- cbind(sites, site_bioclim[,-1])  # drop ID column

# -------------------------
# 5. Rename BIO1–BIO19 to match your other bioclim dataframe
# -------------------------
colnames(new_sites) <- c(
  "lat", "lon", "site_code",
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
# 6. Preview
# -------------------------
head(new_sites)

