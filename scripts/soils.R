#### Bromecast Soil Data #######
######## code by Becca Nelson ###############################
############# created 4-24-25 ######################
############# Last modified: 4-24-25 ##########################
######## modifies RMD file to pull from one integrated df ########

rm(list = ls())

##### Load required packages #####
library(tidyverse)

##### Load Data #########
soil <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soils.csv", header = TRUE)

soil_info <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soil_site_info.csv", header = TRUE)

######## Combine data in format for integration with model #####

soil_clean <- left_join(soil, soil_info, by = "SiteID")

soil_clean <- soil_clean %>% dplyr::select(SiteCode, UniqueID, SampleDescription, pH, EC, OMpercent, Protein_g.kg, SiteCode)

soil_clean$site_old <- soil_clean$SiteCode

write.csv(soil_clean,"/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soil_clean.csv",row.names=F)
