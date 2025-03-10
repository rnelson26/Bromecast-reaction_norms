######## SMAP Data Extraction ##############################################
##### get soil moisture info from SMAP #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 2/6/25 #######
############ last modified: 3/10/25 ########################
rm(list = ls())

### read in file 
SMAP <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/SMAP.csv", header = TRUE)
site_list <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/site_list.csv", header = TRUE)
horizon_data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/soil_data.csv", header = TRUE)

### organize data #####
library(dplyr)
library(ggplot2)

### shrink file down 
## Code to generate SMAP.csv

# Select only the specified columns
#SMAP <- SMAP %>%
 # select(
  #  Category,
   # ID,
  #  Latitude,
   # Longitude,
    #Date,
    #SMAP_Tile,
    #SPL3SMP_E_006_Line_Y_9km,
    #SPL3SMP_E_006_Sample_X_9km,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_scah,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_scav,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_surface_temperature,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_clay_fraction_pm,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_error_pm,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_scah_pm,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_scav_pm,
    #SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_surface_temperature_pm,
  #  SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_vegetation_water_content_pm
 # )

#write.csv(SMAP, "/Users/Becca/Desktop/Adler Lab/SMAP/SMAP.csv", row.names = FALSE)


## create columns to filter by month, day, and year
library(lubridate)

# Add new columns for year, month, and Julian day
SMAP <- SMAP %>%
  mutate(Year = year(Date), 
         Month = month(Date), 
         Julian_day = yday(Date))

### Replace -9999 with NAS so that they don't get counted as actual  numbers
# Replace -9999 with NA in the entire dataframe
SMAP[SMAP == -9999] <- NA

library(dplyr)



YearlySummary <- SMAP %>%
  group_by(Category, ID, Latitude, Longitude, Year) %>%
  summarize(
    soil_moisture_AM = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE),
    soil_moisture_PM = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_total = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE) + 
      mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_AM_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE),
    soil_moisture_PM_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE),
    soil_moisture_total_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE) + 
      mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE)
  ) %>%
  ungroup()



MonthlySummary = SMAP %>%
  group_by(Category, ID, Latitude, Longitude, Year, Month) %>%
  summarize(
    soil_moisture_AM = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE),
    soil_moisture_PM = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_total = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE) + 
      mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_AM_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE),
    soil_moisture_PM_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE),
    soil_moisture_total_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE) + 
      mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE)
  )


DailySummary = SMAP %>%
  group_by(Category, ID, Latitude, Longitude, Year, Month, Julian_day) %>%
  summarize(
    soil_moisture_AM = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE),
    soil_moisture_PM = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_total = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE) + 
      mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_AM_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE),
    soil_moisture_PM_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE),
    soil_moisture_total_dca = mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE) + 
      mean(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE)
  )


# Make individual plots for each site

YearlySummary %>% 
  ggplot(aes(x = Year, y = soil_moisture_PM_dca, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

DailySummary %>% 
  filter(Year == 2022) %>% 
  ggplot(aes(x = Julian_day, y = soil_moisture_PM_dca, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  ggplot(aes(x = Month, y = soil_moisture_PM_dca, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = soil_moisture_PM_dca, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = soil_moisture_AM_dca, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x =soil_moisture_AM, y = soil_moisture_AM_dca, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = soil_moisture_PM, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = soil_moisture_AM, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


### make a map
library(maps)

#map of sat sites
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
points(x=MonthlySummary$Longitude,y=MonthlySummary$Latitude, pch=1, cex=1.5,col="purple")

#with color ramp
library("fields")

color_palette <- colorRampPalette(c("blue", "yellow", "red"))
colors <- color_palette(100)  

### by year
#check why one point is floating off the map

# Convert state map data
state_map <- map_data("state")
state_map_filtered <- state_map[state_map$long >= -128 & state_map$long <= -95 &
                                  state_map$lat >= 30 & state_map$lat <= 52, ]


YearlySummary$color <- colors[cut(YearlySummary$soil_moisture_PM, breaks = 100)]

ggplot() +
  geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill="gray100", color="black") +
  geom_point(data=YearlySummary, aes(x=Longitude, y=Latitude, color=soil_moisture_PM)) +
  scale_color_gradientn(colors=colors) +
  coord_cartesian(xlim=c(-128,-95), ylim=c(30,52)) +  # Set zoom area
  facet_wrap(~Year) +
  labs(title="Soil Moisture PM by Year", color="Soil Moisture PM") +
  theme_classic()

#### by variable 

MonthlySummary$color <- colors[cut(MonthlySummary$soil_moisture_PM, breaks = 100)]  
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Soil Moisture PM")
points(MonthlySummary$Longitude, MonthlySummary$Latitude, col = MonthlySummary$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$soil_moisture_PM), col = colors, legend.lab = "Soil Moisture PM")

MonthlySummary$color <- colors[cut(MonthlySummary$soil_moisture_AM, breaks = 100)]  
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Soil Moisture AM")
points(MonthlySummary$Longitude, MonthlySummary$Latitude, col = MonthlySummary$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$soil_moisture_AM), col = colors, legend.lab = "Soil Moisture AM")


MonthlySummary$color <- colors[cut(MonthlySummary$soil_moisture_PM_dca, breaks = 100)]  
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Soil Moisture PM")
points(MonthlySummary$Longitude, MonthlySummary$Latitude, col = MonthlySummary$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$soil_moisture_PM_dca), col = colors, legend.lab = "Soil Moisture PM DCA")

MonthlySummary$color <- colors[cut(MonthlySummary$soil_moisture_AM_dca, breaks = 100)]  
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Soil Moisture AM DCA")
points(MonthlySummary$Longitude, MonthlySummary$Latitude, col = MonthlySummary$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$soil_moisture_AM_dca), col = colors, legend.lab = "Soil Moisture AM DCA")


###### Retreive soil texture #########
# Install and load necessary packages
#library(httr)
library(soilDB)
library(soiltexture)
library(aqp)
library(sf)



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
                "soc", "ocd", "wv0010", "wv0033", "wv1500"),
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
  
 
######## Use a Van Genuchten Model to calculate water potential ######
#https://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.html
  

  # call ROSETTA API 
  vars <- c('claymean', 'siltmean', 'sandmean')
  r <- ROSETTA(horizon_data, vars)  

r$ID <- r$id  

 
r_summary <- r %>%
  select(ID, theta_r, claymean, siltmean, sandmean, alpha, npar, ksat, theta_s) %>% 
  group_by(ID) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

### fix slight differences in site names between the two datasets before merging
     
# Create a mapping for inconsistent names
name_mapping <- data.frame(
  old_name = c("CG_BoiseHigh", "CG_BoiseLow", "Boise_High", "Boise_Low", "CPER-NEAR HQ\xca", "CG_Cheyenne", "CG_SheepStation", "Rush_Valley", "MPG_TH", "MPG_SR", "MPG_IR", "L1_vanDiepen", "K1_vanDiepen" , "FtK_Lone_Pine", "FtK_Cottonwood_Coulee", "EnsingS1_SuRDC", "EnsingS2_SumPrinceRd" , "EnsingS3_BearCreek", "EnsingS4_LDBM" ), 
  new_name = c("CGBoiseHigh", "CGBoiseLow", "BoiseHigh", "BoiseLow", "CPER-NEAR HQ", "CGCheyenne", "CGSheepStation", "RushValley", "MPGTH", "MPGSR", "MPGIR", "L1vanDiepen", "K1vanDiepen", "FtKLonePine" , "FtKCottonwoodCoulee", "EnsingS1SuRDC", "EnsingS2SumPrinceRd", "EnsingS3BearCreek", "EnsingS4LDBM" )
)

# Replace values in the r_summary and YearlySummary dataframes
r_summary$ID <- sapply(r_summary$ID, function(x) {
  match <- name_mapping$new_name[name_mapping$old_name == x]
  if (length(match) > 0) match else x
})

YearlySummary$ID <- sapply(YearlySummary$ID, function(x) {
  match <- name_mapping$new_name[name_mapping$old_name == x]
  if (length(match) > 0) match else x
})



MonthlySummary_r <- left_join(MonthlySummary, r_summary, by= "ID")
YearlySummary_r <- left_join(YearlySummary, r_summary, by= "ID")

# Define a function to calculate water potential using Rosetta model
# Define the function for calculating water potential
calculate_water_potential <- function(claymean, siltmean, sandmean, theta_s, theta_r, alpha, soil_moisture, npar) {
 # # Convert log-transformed parameters to linear scale -- check is this needs to happen
  alpha <- 10^alpha  # Converts log10(1/cm) to 1/cm
  npar <- 10^npar    # Converts log10(n) to n
  
  # Compute water potential in cm using Van Genuchten equation
  water_potential_cm <- (1 / alpha) * (((soil_moisture / theta_s) ^ (-1 / (1 - 1 / npar))) - 1) ^ (1 / npar)
  
  # Convert to kPa
  return(water_potential_cm * -0.0981)
}



YearlySummary_r <- YearlySummary_r %>%
  mutate(
    water_potential = mapply(
      calculate_water_potential,
      clay = claymean,
      silt = siltmean,
      sand = sandmean,
      theta_s = theta_s,
      theta_r = theta_r,
      alpha = alpha,
      npar = npar,
      soil_moisture = soil_moisture_AM
    )
  )

MonthlySummary_r <- MonthlySummary_r %>%
  mutate(
    water_potential = mapply(
      calculate_water_potential,
      clay = claymean,
      silt = siltmean,
      sand = sandmean,
      theta_s = theta_s,
      theta_r = theta_r,
      alpha = alpha,
      npar = npar,
      soil_moisture = soil_moisture_AM
    )
  )


#theta_r : residual volumetric water content
#theta_s : saturated volumetric water content
#log10(alpha) : retention shape parameter ⁠[log10(1/cm)]⁠
#log10(npar) : retention shape parameter
#log10(ksat) : saturated hydraulic conductivity ⁠[log10(cm/d)]⁠

YearlySummary_r %>% 
  ggplot(aes(x = reorder(ID, water_potential), y = water_potential, color = Category)) + 
  geom_boxplot() +
  theme_bw(base_size = 16) + theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate text 90 degrees (vertical)
  ) 

## explore extreme values
YearlySummary_r %>% 
  ggplot(aes(x = reorder(ID, siltmean), y = siltmean, color = Category)) + 
  geom_boxplot() +
  theme_bw(base_size = 16) + theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate text 90 degrees (vertical)
  ) 




p1 <- MonthlySummary_r %>% 
  ggplot(aes(x = Month, y = water_potential, color = Year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category, scales = "free") +
  theme_bw(base_size = 16) 
ggsave(p1, filename = "wp1.png",height = 15, width = 15)

p2 <- MonthlySummary_r %>% 
  ggplot(aes(x = Month, y = water_potential, color = Year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) 
ggsave(p2, filename = "wp2.png",height = 15, width = 15)


color_palette <- colorRampPalette(c("blue", "yellow", "red"))
colors <- color_palette(100)  

YearlySummary_r$color <- colors[cut(YearlySummary_r$water_potential, breaks = 100)]  
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Water Potential")
points(YearlySummary_r$Longitude, YearlySummary_r$Latitude, col = YearlySummary_r$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(YearlySummary_r$water_potential), col = colors, legend.lab = "Water Potential")

state_map <- map_data("state")
state_map_filtered <- state_map[state_map$long >= -128 & state_map$long <= -95 &
                                  state_map$lat >= 30 & state_map$lat <= 52, ]


YearlySummary_r$color <- colors[cut(YearlySummary_r$water_potential, breaks = 100)]

ggplot() +
  geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill="gray100", color="black") +
  geom_point(data=YearlySummary_r, aes(x=Longitude, y=Latitude, color=water_potential)) +
  scale_color_gradientn(colors=colors) +
  coord_cartesian(xlim=c(-128,-95), ylim=c(30,52)) +  # Set zoom area
  facet_wrap(~Year) +
  labs(title="Water Potential by Year", color="Water Potential") +
  theme_classic()


# Create a custom color palette with warm and cool colors for different ranges
custom_colors <- c("blue", "cyan", "green", "yellow", "orange", "red")

map_wp <- ggplot() +
  geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill="gray100", color="black") +
  geom_point(data=YearlySummary_r, aes(x=Longitude, y=Latitude, color=water_potential)) +
  scale_color_gradientn(
    colors = c("blue", "cyan", "green", "yellow", "orange", "red"),  # Define your color scale from blue (for extreme negative) to red (for positive)
    values = scales::rescale(c(-7217, -2000, -500, -100, 0)),  # Customize where the breaks happen for more color variation
    limits = c(-7217, 0),  # Use the full range from -7217 to 0
    breaks = c(-7217, -2000, -1000, 0),  # Choose specific breaks for better contrast
    labels = c("-7217", "-2000", "-1000", "0")  # Label breaks
  ) +
  coord_cartesian(xlim=c(-128, -95), ylim=c(30, 52)) +
  facet_wrap(~Year) +
  labs(title="Water Potential by Year", color="Water Potential") +
  theme_classic()
ggsave(map_wp, filename = "wp.map.png",height = 15, width = 15)

## summary
site_wp_info <- YearlySummary_r %>% 
  group_by(ID) %>% 
  summarise(
    mean_wp = mean(water_potential, na.rm = TRUE),
    min_wp = min(water_potential, na.rm = TRUE),
    max_wp = max(water_potential, na.rm = TRUE),
    sd_wp = sd(water_potential, na.rm = TRUE),
    n = n()
  )


###### Merge with combined dataframe #####

combined_clean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv")

## rename columns accordingly
MonthlySummary$year <- MonthlySummary$Year 
MonthlySummary$site <- MonthlySummary$ID
MonthlySummary$Type <- MonthlySummary$Category
MonthlySummary$Lat <- MonthlySummary$Latitude
MonthlySummary$Lon <- MonthlySummary$Longitude

MonthlySummary <- MonthlySummary%>%
  mutate(Type = recode(Type, "CommonGarden" = "Common_Garden"))

MonthlySummary_clean <- MonthlySummary %>% select(year, site, Type, Lat, Lon, soil_moisture_PM)


