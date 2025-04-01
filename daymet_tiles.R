#### Code for linking daymet shapefile to 2024 satellite locations #######
######## Becca Nelson ###############################
############# created 4-1-25 ######################
############# Last modified: 4-1-25 ##########################

rm(list = ls())

######## Load packages ######
library(sf)
library(dplyr)

####### Load spatial info ########
data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE)

shapefile_path <- "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/DaymetV4_Tiles_Continental_HI_PR.shp"
sf_object <- st_read(shapefile_path)


### Get list of relevant coordinates for 2024 ########

spatial <- data %>% filter(Type == "Satellite") %>% filter(year == 2024) %>% select(site, Lat, Lon) %>% distinct()

#### make a map ######

plot(sf_object)

# Define Western U.S. bounding box
west_bbox <- st_bbox(c(xmin = -130, xmax = -100, ymin = 30, ymax = 50), crs = st_crs(sf_object))

# Crop shapefile to this region
western_us <- st_crop(sf_object, west_bbox)
plot(western_us)

###### locate tiles #######
sites_sf <- st_as_sf(spatial, coords = c("Lon", "Lat"), crs = 4326)

sites_with_tiles <- st_join(sites_sf, sf_object, left = FALSE)  # left = FALSE removes 
print(sites_with_tiles)

write.csv(sites_with_tiles, "2024_tiles.csv", row.names = FALSE)
