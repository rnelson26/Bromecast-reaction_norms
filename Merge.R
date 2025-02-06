######## Data Merging ##############################################
##### Merge common garden and satellite site data #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 1/28/25 #######
############ last modified: 2/6/25 ########################

### outstanding questions ##########

# 1. Is there a way to account for if plants emerged but were not alive at
# harvest to make these columns more equivalent to each other? (for merging live
# harvest and emerged together)

# MLV: Yes! I can add emergence data. I'll put it on my to-do list for this
# week. The reason for the "alive at last phenology check" is a holdover from a
# QA/QC data check.


### load required packages ########
rm(list = ls())


#library(tidyverse) 
library(dplyr)

##### load data #########
sat <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/all_plants_ftypes1.csv", header = TRUE)

cg <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/cg_fullData_withFlags.csv", header = TRUE)

##### create a new column for type ######
sat$Type <- "Satellite"

cg$Type <- "Common_Garden"

## allows us to filter by type of experiment once the dataframes are stacked together 

###### standardize similar columns #######
## site 
colnames(sat)[colnames(sat) == "SiteCode"] <- "site"
## year
colnames(sat)[colnames(sat) == "Year"] <- "year"

## plantID
sat$plantID <- 1:nrow(sat) ## assigns a unique number to each row (ind plant)

#density
colnames(sat)[colnames(sat) == "BRTE.neighbors"] <- "density"
## to do: add neighbor selection for cg

# transect
cg$merged_block_plot <- paste(cg$block, cg$plot)
colnames(cg)[colnames(cg) == "merged_block_plot"] <- "Transect" 
 

## Emerged
colnames(cg)[colnames(cg) == "live_harvest"] <- "Emerged"


## Reproduced

cg <- cg %>%
  mutate(Reproduced = case_when(
    is.na(first_flower) ~ "No",
    TRUE ~ "Yes"
  ))

## Fecundity
colnames(cg)[colnames(cg) == "seed_count_total"] <- "Fecundity"

#Fecundity in the satellite sites was always counted by exact
# number of seeds (because plants didn't get that big), where for most of the
# common garden plants, seeds were subsampled, counted, and then total seed
# count was estimated given the known inflorescence weights of the whole sample
# and subsample. so although we combined these two columns they were not identical in how they were measured.

## Biomass
cg <- cg %>%
  mutate(Biomass = veg_mass + inflor_mass)
#need to add seed mass to sat site Biomass

## add columns unique to common garden as blanks
sat$albedo <- NA
sat$x <- NA
sat$y <- NA
sat$genotype <- NA
sat$block <- NA
sat$plot <- NA
sat$tillers <- NA
sat$note_standard_harvest <- NA
sat$inflor_mass <- NA
sat$veg_mass <- NA
sat$first_flower <- NA
sat$v_phen <- NA
sat$last_phen_status <- NA
sat$note_standard_phen <- NA
sat$v_harvest <- NA
sat$note_standard_harvest <- NA

cg$Distance <- NA
cg$Lat <- NA
cg$Lon <- NA
cg$prcp.Spr <- NA #can cut climate columns 
cg$tmean.Spr <- NA
cg$swe_mean.Spr <- NA
cg$prcp.Sum <- NA
cg$tmean.Sum <- NA
cg$swe_mean.Sum <- NA
cg$prcp.Win <- NA
cg$tmean.Win <- NA
cg$swe_mean.Win <- NA
cg$prcp.Fall <- NA
cg$tmean.Fall <- NA
cg$swe_mean.Fall <- NA
cg$annual <- 0
cg$unknown <- 0
cg$perennial <- 0
cg$shrub <- 0
cg$groundcover <- NA
cg$biocrust <- NA
cg$fecundityflag <- NA
cg$notesFlag <- NA
cg$Treatment <- NA

### check dataframes to make sure they match
colnames(cg)
colnames(sat)

# Assuming colnames1 and colnames2 represent your two lists
cg_list <- c("plantID", "site", "year", "density", "albedo", "block", "plot", 
               "x", "y", "genotype", "first_flower", "v_phen", "last_phen_status", 
               "note_standard_phen", "Emerged", "v_harvest", "tillers", "veg_mass", 
               "inflor_mass", "Fecundity", "note_standard_harvest", "Type", "Transect", 
               "Reproduced", "Biomass", "Distance", "Lat", "Lon", "prcp.Spr", 
               "tmean.Spr", "swe_mean.Spr", "prcp.Sum", "tmean.Sum", "swe_mean.Sum", 
               "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", "tmean.Fall", 
               "swe_mean.Fall", "annual", "unknown", "perennial", "shrub", 
               "groundcover", "biocrust", "fecundityflag", "notesFlag", "Treatment")

sat_list <- c("site", "year", "Treatment", "Transect", "Distance", "Emerged", 
               "Reproduced", "density", "Fecundity", "Biomass", "fecundityflag", 
               "notesFlag", "Lat", "Lon", "prcp.Spr", "tmean.Spr", "swe_mean.Spr", 
               "prcp.Sum", "tmean.Sum", "swe_mean.Sum", "prcp.Win", "tmean.Win", 
               "swe_mean.Win", "prcp.Fall", "tmean.Fall", "swe_mean.Fall", "annual", 
               "unknown", "perennial", "shrub", "groundcover", "biocrust", "Type", 
               "plantID", "albedo", "x", "y", "genotype", "block", "plot", 
               "tillers", "note_standard_harvest", "inflor_mass", "veg_mass", 
               "first_flower", "v_phen", "last_phen_status", "note_standard_phen", "v_harvest")

# Check if they contain the same elements
identical <- setequal(cg_list, sat_list)
print(identical) 

# Find differences
diff1 <- setdiff(cg_list, sat_list)
diff2 <- setdiff(cg_list, sat_list)

print(diff1) # In colnames1 but not in colnames2
print(diff2) # In colnames2 but not in colnames1


#### merge the two dfs############

combined <- rbind(sat, cg)

str(combined) #inspect our merged data frame

colnames(combined)

#### Remove columns not relevant to this project #######

#select columns to retain from merged dataset
combined_clean <- combined %>% dplyr::select(site, year, Treatment, Transect, Distance, Emerged, Reproduced, density, Fecundity, Biomass, fecundityflag, notesFlag, Lat, Lon, annual, unknown, perennial, shrub, Type, plantID, albedo, x, y, genotype, block, plot, note_standard_harvest, note_standard_phen)

## make merged column for site, plot, and year
combined_clean$Transect_Site_Year <- paste(combined_clean$Transect, combined_clean$site, combined_clean$year, sep = " - ")



