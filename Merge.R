######## Data Merging ##############################################
##### Merge common garden and satellite site data #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 1/28/25 #######
############ last modified: 2/20/25 ########################

### outstanding questions ##########
## whether approach to zero neighbors makes sense 


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

## Reproduced

cg <- cg %>%
  mutate(Reproduced = case_when(
    is.na(first_flower) ~ "N",
    TRUE ~ "Y"
  ))


##### neighbor density calculations for common garden data ######
# Set possible number of neighbors for each location in high density
cg %>%
  mutate(plot_unique = paste(site, block, plot, sep = "_")) -> cg

cg$possible_neighbors <- NULL
cg$neighbors <- NULL
cg$prop_neighbors <- NULL

for(i in 1:nrow(cg)){
  
  if(cg$density[i] == "lo"){
    cg[i,] %>% 
      dplyr::select(x, y) %>% 
      mutate(x_new = x + 1,       #change to zero for low (see below)
             x_new2 = x - 1,
             y_new = y + 1,
             y_new2 = y - 1) -> search_coords
    
    cg %>% 
      filter(plot_unique == cg$plot_unique[i]) %>% 
      filter(x == search_coords$x_new & y == search_coords$y |
               x == search_coords$x_new2 & y == search_coords$y |
               x == search_coords$x & y == search_coords$y_new  |
               x == search_coords$x & y == search_coords$y_new2 ) -> possible_neighbors
  }else{
    expand.grid(x = cg[i,]$x + -5:5, y = cg[i,]$y + -5:5) -> search_coords
    
    # Filter out search coords that are not within circle using distance matrix
    distances <- as.matrix(dist(cbind(search_coords$x, search_coords$y)))
    focal_coords <- which(search_coords$x == cg$x[i] & search_coords$y == cg$y[i])
    search_coords <- search_coords %>% 
      mutate(dist = distances[focal_coords,]) %>% 
      filter(dist <= 2.5)
    
    cg %>% 
      filter(plot_unique == cg$plot_unique[i]) %>% 
      filter(x %in% search_coords$x & y %in% search_coords$y) %>% 
      filter(x != cg$x[i] | y != cg$y[i]) -> possible_neighbors
  }
  
  cg[i, "possible_neighbors"] <- nrow(possible_neighbors)
  cg[i, "neighbors"] <- nrow(possible_neighbors %>% filter(Reproduced == "Yes"))
  
}

## Adjust for edge effects ####

# Get proportion that survived for each plot
cg %>% 
  mutate(w = ifelse(Reproduced == "Yes", 1, 0)) %>% 
  group_by(plot_unique) %>% 
  summarize(prop_survived = sum(w)/n()) %>% 
  ungroup() -> plot_survival

merge(cg, plot_survival) -> cg

cg %>% 
  mutate(new_neighbors = case_when(density == "lo" & possible_neighbors == 3 ~ prop_survived + neighbors,
                                   density == "lo" & possible_neighbors == 2 ~ prop_survived * 2 + neighbors,
                                   density == "lo" & possible_neighbors == 1 ~ prop_survived * 3 + neighbors,
                                   # for 2023 there were less possible neighbors because there were less plants (WI had up to 90, all other sites up to 80)
                                   density == "hi" & site != "WI" & possible_neighbors < 80 ~ prop_survived * (80-possible_neighbors) + neighbors,
                                   density == "hi" & site == "WI" & possible_neighbors < 90 ~ prop_survived * (90-possible_neighbors) + neighbors,
                                   density == "lo" & possible_neighbors > 3 ~ neighbors)) -> cg


## change low to zero have zero neighbors 
cg <- cg %>%
  mutate(neighbors = ifelse(density == "lo", 0, neighbors))



###### standardize similar columns #######
## site 
colnames(sat)[colnames(sat) == "SiteCode"] <- "site"
## year
colnames(sat)[colnames(sat) == "Year"] <- "year"

## plantID
sat$plantID <- 1:nrow(sat) ## assigns a unique number to each row (ind plant)

#density
colnames(sat)[colnames(sat) == "BRTE.neighbors"] <- "neighbors"
## should I include new_neighbors as well? 

# transect
cg$merged_block_plot <- paste(cg$block, cg$plot)
colnames(cg)[colnames(cg) == "merged_block_plot"] <- "Transect" 
 

## Emerged
colnames(cg)[colnames(cg) == "emergence"] <- "Emerged"


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
sat$density <- NA
sat$plot_unique <- NA
sat$prop_survived <- NA
sat$new_neighbors <- NA
sat$density <- NA
sat$possible_neighbors <- NA
sat$live_harvest <- NA

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

#### merge the two dfs############

combined <- rbind(sat, cg)

str(combined) #inspect our merged data frame

colnames(combined)

#### Remove columns not relevant to this project #######

#select columns to retain from merged dataset
combined_clean <- combined %>% dplyr::select(site, year, Treatment, Transect, Distance, Emerged, Reproduced, neighbors, Fecundity, Biomass, fecundityflag, notesFlag, Lat, Lon, annual, unknown, perennial, shrub, Type, plantID, albedo, x, y, genotype, block, plot, note_standard_harvest, note_standard_phen, prcp.Win, tmean.Win)

## make merged column for site, plot, and year
combined_clean$Transect_Site_Year <- paste(combined_clean$Transect, combined_clean$site, combined_clean$year, sep = " - ")

#inspect results
str(combined_clean)

## save as .csv 
write.csv(combined_clean, "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv", row.names = FALSE)

