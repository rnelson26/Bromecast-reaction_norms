#### Bromecast Soil Data #######
######## code by Becca Nelson ###############################
############# created 4-24-25 ######################
############# Last modified: 9-22-25 ##########################

rm(list = ls())

##### Load required packages #####
library(tidyverse)

##### Load Data #########
world_soil <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/soil_data_updated.csv", header = TRUE)

soil <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soils.csv", header = TRUE)

textures <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/texture_8_25_25.csv", header = TRUE)


soil_info <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soil_site_info.csv", header = TRUE)

list <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/list.csv", header = TRUE)
site_list <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/site_list.csv", header = TRUE)

######## Combine data in format for integration with model #####

soil_clean <- left_join(soil, soil_info, by = "SiteID")

soil_clean <- left_join(soil_clean, textures, by = "SiteCode")

soil_clean <- soil_clean %>% dplyr::select(SiteCode, UniqueID, SampleDescription, pH, EC, OMpercent, Protein_g.kg, SiteCode, X..Sand, X..Clay, X..Silt)


###### Compare measured vs database values ##########
database_textures <- world_soil %>% filter(hzdept == 5) %>% dplyr::select(id, siltmean, claymean, sandmean)


database_textures$SiteCode <- database_textures$id
database_textures$SiteCode[database_textures$SiteCode == "CPER- Far north"] <- "FAR NORTH CPER"

database_textures$SiteCode[database_textures$SiteCode == "EnsingS1_SuRDC" ] <- "EnsingS1 SuRDC"
database_textures$SiteCode[database_textures$SiteCode == "EnsingS2_SumPrinceRd"] <- "EnsingS2 Summerland-Princeton"
database_textures$SiteCode[database_textures$SiteCode == "EnsingS3_BearCreek"]  <- "EnsingS3 Bear Creek"
database_textures$SiteCode[database_textures$SiteCode == "EnsingS4_LDBM"]  <- "EnsingS4 Lundbom"
database_textures$SiteCode[database_textures$SiteCode == "CPER-4-Way"] <- "NEAR 4WAY CPER"
database_textures$SiteCode[database_textures$SiteCode == "CPER- Near NutNet"] <- "NEAR NUTNET"

unique(database_textures$SiteCode)
unique(textures$SiteCode)

compare_textures <- left_join(textures, database_textures, by = "SiteCode")

library(ggplot2)

#Silt
lims_silt <- range(c(compare_textures$X..Silt, compare_textures$siltmean), na.rm = TRUE)
ggplot(compare_textures, aes(x = X..Silt, y = siltmean, color = SiteCode)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(lims_silt) + ylim(lims_silt) +
  theme_classic()

#Sand
lims_sand <- range(c(compare_textures$X..Sand, compare_textures$sandmean), na.rm = TRUE)
ggplot(compare_textures, aes(x = X..Sand, y = sandmean, color = SiteCode)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(lims_sand) + ylim(lims_sand) +
  theme_classic()

#Clay
lims_clay <- range(c(compare_textures$X..Clay, compare_textures$claymean), na.rm = TRUE)
ggplot(compare_textures, aes(x = X..Clay, y = claymean, color = SiteCode)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  xlim(lims_clay) + ylim(lims_clay) +
  theme_classic()

######## Database vs Sample Offset Regression ######
library(DirichletReg)


compare_textures$comp_obs <- DR_data(compare_textures[, c("X..Clay", "X..Silt", "X..Sand")] / 100)
compare_textures$comp_means <- DR_data(compare_textures[, c("claymean", "siltmean", "sandmean")] / 100)

fit <- DirichReg(comp_obs ~ claymean + siltmean + sandmean, data = compare_textures)

summary(fit)

missing_texture <- world_soil %>% filter(hzdept == 5) %>%  filter(id %in% c("FtK_Cottonwood_Coulee", "FtK_Lone_Pine", "CPER- Far north", "HardwareRanch", "MPG_IR", "MPG_TH", "Peavine",   "Plymouth", "GoeblS1", "RedBluff" )) %>% dplyr::select(id, siltmean, claymean, sandmean)



pred <- predict(fit, newdata = missing_texture, type = "response")
pred * 100  

pred_df <- as.data.frame(pred * 100)
colnames(pred_df) <- c("X..Clay", "X..Silt", "X..Sand")

missing_texture_pred <- cbind(missing_texture, pred_df)

missing_texture_pred

## add to existing dataframe with soil values

missing_texture_pred$SiteCode <- missing_texture_pred$id
missing_texture_pred$SiteCode[missing_texture_pred$SiteCode == "CPER- Far north"] <- "FAR NORTH CPER"

missing_texture_clean <- missing_texture_pred %>% dplyr::select(X..Sand, X..Silt, X..Clay, SiteCode)

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
 #rename(id = ID, lat = Latitude, lon = Longitude) %>% 
  #select(id, lat, lon)

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

### get two sites missing from original extraction:
library(soilDB)
library(aqp)
library(dplyr)
library(sf)

# Define the two new sites
new_sites <- data.frame(
  id  = c("GoeblS1", "RedBluff"),
  lat = c(39.550182, 45.579969),
  lon = c(-105.095917, -111.664617)
)

# Fetch SoilGrids data for new sites
soil_data_new <- fetchSoilGrids(
  x = new_sites,
  loc.names = c("id", "lat", "lon"),
  depth_intervals = c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"),
  variables = c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt",
                "soc", "ocd", "wv0010", "wv0033", "wv1500"),
  grid = FALSE,
  target_resolution = c(250, 250),
  summary_type = c("Q0.05", "Q0.5", "Q0.95", "mean"),
  verbose = TRUE,
  progress = TRUE
)

# Extract horizon-level data
horizon_data_new <- horizons(soil_data_new)

# Optional: save as CSV
write.csv(horizon_data_new, "soil_data_new_sites.csv", row.names = FALSE)

# Load existing soil data
world_soil <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/soil_data.csv", header = TRUE)

# Merge new soil data with existing
horizon_data_new <- horizon_data_new %>%
  mutate(
    hzID = as.integer(hzID)  
  )
world_soil_combined <- bind_rows(world_soil, horizon_data_new)
# Check
head(world_soil_combined)

write.csv(world_soil_combined, "soil_data_updated.csv", row.names = FALSE)

