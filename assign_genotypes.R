######## Assign Genotypes ##############################################
##### Match genotypes to satellite sites #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 4/1/25 #######
############ last modified: 4/1/25 ########################

###### Load packages #####
library(dplyr)
library(sf)

#### Load in data ########
rm(list = ls())

BRTE <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/BRTE_NorthAmerica.csv", header = TRUE)
seed <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/SeedCollectionData.csv", header = TRUE)
sites <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE)
kinshipIDs <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/93cg_genotypes.csv")

tips <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/307tips.csv")

####### match data to assign sat site genotypes #######

BRTE <- left_join(BRTE, tips, by = "PopNum")

sites <- sites %>% filter(Type == "Satellite") %>% select(site, Lon, Lat) %>% distinct()

sites_sf <- st_as_sf(sites, coords = c("Lon", "Lat"), crs = 4326)
BRTE_sf <- st_as_sf(BRTE, coords = c("Longitude", "Latitude"), crs = 4326)


find_nearest_brte <- function(sites_sf, BRTE_sf) {
  # Locate and extract the nearest BRTE point for each site
  nearest_indices <- st_nearest_feature(sites_sf, BRTE_sf)
  nearest_brte <- BRTE_sf[nearest_indices, ]
  nearest_brte_info <- BRTE_sf[nearest_indices, c("SeedSource", "NewSiteCode")]
  # Calculate the differences in Latitude and Longitude 
  lat_diff <- st_coordinates(sites_sf)[, 2] - st_coordinates(nearest_brte)[, 2]
  lon_diff <- st_coordinates(sites_sf)[, 1] - st_coordinates(nearest_brte)[, 1]
  # Combine and store info
  result <- sites_sf %>%
    mutate(
      Nearest_BRTE_Lat = st_coordinates(nearest_brte)[, 2],  # Latitude of nearest BRTE
      Nearest_BRTE_Lon = st_coordinates(nearest_brte)[, 1],  # Longitude of nearest BRTE
      SeedSource = nearest_brte_info$SeedSource,
      NewSiteCode = nearest_brte_info$NewSiteCode,
      Lat_Diff = lat_diff,  # Latitude difference
      Lon_Diff = lon_diff   # Longitude difference
    )
  
  return(result)
}

nearest_matches <- find_nearest_brte(sites_sf, BRTE_sf)

assigned_genoyptes <- left_join(nearest_matches, kinshipIDs, by = "NewSiteCode")


st_write(assigned_genoyptes, "assigned_genotypes.csv", layer_options = "GEOMETRY=AS_XY", delete_dsn = TRUE)

