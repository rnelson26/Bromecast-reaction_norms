#### Bromecast Soil Data #######
######## code by Becca Nelson ###############################
############# created 4-24-25 ######################
############# Last modified: 8-19-25 ##########################

rm(list = ls())

##### Load required packages #####
library(tidyverse)

##### Load Data #########
world_soil <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/soil_data.csv", header = TRUE)

soil <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soils.csv", header = TRUE)

textures <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/texture_8_19_25.csv", header = TRUE)


soil_info <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soil_site_info.csv", header = TRUE)

######## Combine data in format for integration with model #####

soil_clean <- left_join(soil, soil_info, by = "SiteID")

soil_clean <- left_join(soil_clean, textures, by = "SiteCode")

soil_clean <- soil_clean %>% dplyr::select(SiteCode, UniqueID, SampleDescription, pH, EC, OMpercent, Protein_g.kg, SiteCode, X..Sand, X..Clay, X..Silt)


## pull any missing texture information from world soil horizon

missing_texture <- world_soil %>% filter(hzdept == 5) %>%  filter(id %in% c("FtK_Cottonwood_Coulee", "FtK_Lone_Pine", "CPER- Far north", "HardwareRanch", "MPG_IR", "MPG_TH", "Peavine",   "Plymouth")) %>% dplyr::select(id, siltmean, claymean, sandmean)
## missing Goebl and Redbluff

missing_texture$SiteCode <- missing_texture$id
missing_texture$SiteCode[missing_texture$SiteCode == "CPER- Far north"] <- "FAR NORTH CPER"

missing_texture$X..Sand <- missing_texture$sandmean
missing_texture$X..Silt <- missing_texture$siltmean
missing_texture$X..Clay <- missing_texture$claymean

missing_texture_clean <- missing_texture %>% dplyr::select(X..Sand, X..Silt, X..Clay, SiteCode)

soil_clean <- soil_clean %>%
  left_join(missing_texture_clean, by = "SiteCode", suffix = c("", ".new")) %>%
  mutate(
    X..Sand = coalesce(X..Sand, X..Sand.new),
    X..Silt = coalesce(X..Silt, X..Silt.new),
    X..Clay = coalesce(X..Clay, X..Clay.new)
  ) %>%
  dplyr::select(-X..Sand.new, -X..Silt.new, -X..Clay.new)

soil_clean$site_old <- soil_clean$SiteCode



write.csv(soil_clean,"/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soil_clean.csv",row.names=F)

###### visualize soil texture ########
library(ggtern)

ggtern(data = soil_clean, 
       aes(x = X..Sand, y = X..Silt, z = X..Clay, color = SiteCode)) +
  geom_point() +
  theme_bw() +
  labs(title = "Soil Texture Triangle by Site",
       x = "Sand (%)", y = "Silt (%)", z = "Clay (%)")

## without legend
ggtern(data = soil_clean, 
       aes(x = X..Sand, y = X..Silt, z = X..Clay, color = SiteCode)) +
  geom_point() +
  theme_bw() +
  labs(title = "Soil Texture Triangle by Site",
       x = "Sand (%)", y = "Silt (%)", z = "Clay (%)") + theme(legend.position = "none")

## sand and silt might be more meaningful to put in a PCA than clay since they all seem to have not much clay so far...
## world soil data for our missing sites seem to be on a different order of magnitude than the sites for which we did the measurements 

####### world soil extraction ##########
###### Retreive soil texture #########
# Install and load necessary packages
#library(httr)
library(soilDB)
library(soiltexture)
library(aqp)
library(sf)

## how the ML mapping of soil works: https://www.isric.org/explore/soilgrids/faq-soilgrids#What_is_SoilGrids
### the soil database: https://www.isric.org/explore/wosis

# Ensure site_list has correct column names
#x <- site_list %>% 
# rename(id = ID, lat = Latitude, lon = Longitude) %>% 
#  select(id, lat, lon)

# Fetch SoilGrids data
#soil_data <- fetchSoilGrids(
# x = x,
#loc.names = c("id", "lat", "lon"),
#depth_intervals = c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"),
#variables = c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt",
#  "soc", "ocd", "wv0010", "wv0033", "wv1500"),
#grid = FALSE,  # Ensures point data retrieval instead of grid-based data
#target_resolution = c(250, 250),  # Resolution in meters
#summary_type = c("Q0.05", "Q0.5", "Q0.95", "mean"),  # Include multiple statistics
#verbose = TRUE,  # Show progress messages
#progress = TRUE  # Show download progress
#)

# Check structure of the returned data
#str(soil_data)

# Print a preview of the soil data
#head(soil_data)

## extract info
#horizon_data <- horizons(soil_data)

## explore info
library(ggplot2)

# Example: Assuming your data is in a dataframe called `soil_data`
ggplot(horizon_data, aes(x = id, y = clayQ50)) +
  geom_boxplot() + 
  theme_classic()
labs(title = "Median Clay Fraction (clayQ50) by Site",
     x = "Site",
     y = "Clay Fraction (Median)") 

ggplot(horizon_data, aes(x = id, y = claymean)) +
  geom_boxplot() +
  theme_classic()
labs(title = "Median Clay Fraction (clayQ50) by Site",
     x = "Site",
     y = "Clay Fraction (Median)") 

ggplot(horizon_data, aes(x = id, y = siltQ50)) +
  geom_boxplot() + 
  theme_classic()
labs(title = "Median Clay Fraction (clayQ50) by Site",
     x = "Site",
     y = "Clay Fraction (Median)") 

ggplot(horizon_data, aes(x = id, y = siltmean)) +
  geom_boxplot() +
  theme_classic()
labs(title = "Median Clay Fraction (clayQ50) by Site",
     x = "Site",
     y = "Clay Fraction (Median)") 

ggplot(horizon_data, aes(x = id, y = sandQ50)) +
  geom_boxplot() + 
  theme_classic()
labs(title = "Median Clay Fraction (clayQ50) by Site",
     x = "Site",
     y = "Clay Fraction (Median)") 

ggplot(horizon_data, aes(x = id, y = sandmean)) +
  geom_boxplot() +
  theme_classic()
labs(title = "Median Clay Fraction (clayQ50) by Site",
     x = "Site",
     y = "Clay Fraction (Median)") 

#write.csv(horizon_data, "soil_data.csv", row.names = FALSE)
