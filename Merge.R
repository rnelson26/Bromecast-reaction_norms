######## Data Merging ##############################################
##### Merge common garden and satellite site data #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 1/28/25 #######
############ last modified: 1/28/25 ########################

### outstanding questions ##########

# 1. Is there a way to account for if plants emerged but were not alive at
# harvest to make these columns more equivalent to each other? (for merging live
# harvest and emerged together)

# MLV: Yes! I can add emergence data. I'll put it on my to-do list for this
# week. The reason for the "alive at last phenology check" is a holdover from a
# QA/QC data check.

# 2. Does biomass for the satellite sites include vegetative and inflorensce
# biomass combined?

# MLV: Yes (?). I'm pretty sure that's correct, but let's double-check with
# Peter.

# 3. Is there a way to convert the phenology statuses of the common garden
# plants into a simple yes vs no on whether they reproduced?

# MLV: Yes. The column "first_flower" has the first date a flower was recorded.
# If it is NA, then it didn't flower. So you could just make a binary column
# based on that info.

# 4. Was fecundity in the satellite sites measured differently than seed count
# in the common garden?

# MLV: Yes and no. Fecundity in the satellite sites was always counted by exact
# number of seeds (because plants didn't get that big), where for most of the
# common garden plants, seeds were subsampled, counted, and then total seed
# count was estimated given the known inflorescence weights of the whole sample
# and subsample.

# 5. Is there a way to add lat/long & climate variables for the common garden
# sites that match those used for the satellite sites?

# MLV: We can add the lat/long, but I would hesitate to include those climate
# variables. I don't think we will need them for the reaction norm analysis. The
# lat/lon for the gardens are in the Bromecast-data Github >
# rawdata/garden_info.csv

# 6. Is it better to code NAs as character "not applicable" as I've currently
# done or simply have them as NAs?

# MLV: I would do "NA" preferably to be consistent. R treats "NA"s as different
# than a character string that says "not applicable".

### load required packages ########
library(tidyverse)

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
colnames(sat)[colnames(sat) == "Treatment"] <- "density" #combine trt and density 

# transect
cg$merged_block_plot <- paste(cg$block, cg$plot)
colnames(cg)[colnames(cg) == "merged_block_plot"] <- "Transect" #combine trt and density 

## Emerged
colnames(cg)[colnames(cg) == "live_harvest"] <- "Emerged"
## is there a way to account for if plants emerged but were not alive at harvest to make these columns more equivalent to each other 

## Reproduced

## not sure if there is a way to link phenology statuses to whether or not they reproduced??

## Fecundity
colnames(cg)[colnames(cg) == "seed_count_total"] <- "Fecundity"

## Biomass
colnames(sat)[colnames(sat) == "Biomass"] <- "veg_mass"
## need to check is the satellite biomass variable also includes infloresence mass 

## add columns unique to common garden as blanks
sat$albedo <- "Not_Applicable"
sat$x <- "Not_Applicable"
sat$y <- "Not_Applicable"
sat$genotype <- "Not_Applicable"
sat$block <- "Not_Applicable"
sat$plot <- "Not_Applicable"
sat$tillers <- "Not_Applicable"
sat$note_standard_harvest <- "Not_Applicable"
sat$inflor_mass <- "Not_Applicable"
sat$first_flower <- "Not_Applicable"
sat$v_phen <- "Not_Applicable"
sat$last_phen_status <- "Not_Applicable"
sat$note_standard_phen <- "Not_Applicable"
sat$v_harvest <- "Not_Applicable"
sat$note_standard_harvest <- "Not_Applicable"

cg$Distance <- "Not_Applicable"
cg$Lat <- "Not_Applicable"
cg$Lon <- "Not_Applicable"
cg$prcp.Spr <- "Not_Applicable"
cg$tmean.Spr <- "Not_Applicable"
cg$swe_mean.Spr <- "Not_Applicable"
cg$prcp.Sum <- "Not_Applicable"
cg$tmean.Sum <- "Not_Applicable"
cg$swe_mean.Sum <- "Not_Applicable"
cg$prcp.Win <- "Not_Applicable"
cg$tmean.Win <- "Not_Applicable"
cg$swe_mean.Win <- "Not_Applicable"
cg$prcp.Fall <- "Not_Applicable"
cg$tmean.Fall <- "Not_Applicable"
cg$swe_mean.Fall <- "Not_Applicable"
cg$annual <- "Not_Applicable"
cg$unknown <- "Not_Applicable"
cg$perennial <- "Not_Applicable"
cg$shrub <- "Not_Applicable"
cg$groundcover <- "Not_Applicable"
cg$biocrust <- "Not_Applicable"
cg$fecundityflag <- "Not_Applicable"
cg$notesFlag <- "Not_Applicable"
cg$Reproduced <- "Not_Applicable"

### check dataframes to make sure they match
colnames(cg)
colnames(sat)

listcg <- c("plantID", "site", "year", "density", "albedo", "block", "plot", "x", 
           "y", "genotype", "first_flower", "v_phen", "last_phen_status", 
           "note_standard_phen", "Emerged", "v_harvest", "tillers", "veg_mass", 
           "inflor_mass", "Fecundity", "note_standard_harvest", "Type", "Transect", 
           "Distance", "Lat", "Lon", "prcp.Spr", "tmean.Spr", "swe_mean.Spr", 
           "prcp.Sum", "tmean.Sum", "swe_mean.Sum", "prcp.Win", "tmean.Win", 
           "swe_mean.Win", "prcp.Fall", "tmean.Fall", "swe_mean.Fall", "annual", 
           "unknown", "perennial", "shrub", "groundcover", "biocrust", "fecundityflag", 
           "notesFlag", "Reproduced", "Type")

listsat <- c("site", "year", "density", "Transect", "Distance", "Emerged", "Reproduced", 
           "Fecundity", "veg_mass", "fecundityflag", "notesFlag", "Lat", "Lon", 
           "prcp.Spr", "tmean.Spr", "swe_mean.Spr", "prcp.Sum", "tmean.Sum", "swe_mean.Sum", 
           "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", "tmean.Fall", "swe_mean.Fall", 
           "annual", "unknown", "perennial", "shrub", "groundcover", "biocrust", "Type", 
           "plantID", "albedo", "x", "y", "genotype", "block", "plot", "tillers", 
           "note_standard_harvest", "inflor_mass", "first_flower", "v_phen", "last_phen_status", 
           "note_standard_phen", "v_harvest")


# Check if the lists are identical
setequal(listcg, listsat)

#### merge the two dfs############

combined <- rbind(sat, cg)

str(combined) #inspect our merged data frame
