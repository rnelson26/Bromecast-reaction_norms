######## SMAP Data Extraction ##############################################
##### get soil moisture info from SMAP #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 2/6/25 #######
############ last modified: 3/3/25 ########################

### read in file 
SMAP <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/SMAP/Bromecast-Soil-Moisture-SPL3SMP-E-006-results.csv", header = TRUE)

### organize data #####
library(dplyr)
library(ggplot2)

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


MonthlySummary = SMAP %>%
  group_by(Category, ID, Latitude, Longitude, Year, Month) %>%
  summarize(
    soil_moisture_AM = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE),
    soil_moisture_PM = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_total = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE) + 
      sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_AM_dca = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE),
    soil_moisture_PM_dca = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE),
    soil_moisture_total_dca = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE) + 
      sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE)
  )


DailySummary = SMAP %>%
  group_by(Category, ID, Latitude, Longitude, Year, Month, Julian_day) %>%
  summarize(
    soil_moisture_AM = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE),
    soil_moisture_PM = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_total = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture, na.rm = TRUE) + 
      sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_pm, na.rm = TRUE),
    soil_moisture_AM_dca = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE),
    soil_moisture_PM_dca = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE),
    soil_moisture_total_dca = sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_AM_soil_moisture_dca, na.rm = TRUE) + 
      sum(SPL3SMP_E_006_Soil_Moisture_Retrieval_Data_PM_soil_moisture_dca_pm, na.rm = TRUE)
  )


# Make individual plots for each site
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


######## Use a Van Genuchten Model to calculate water potential ######
# Define soil parameters (example for sandy loam)
theta_s <- 8.1 #estimate based on being slightly bigger than max observed value
alpha <- 0.075   # Air entry parameter (1/cm)
n <- 1.89        # Shape parameter
m <- 1 - 1/n     # Derived parameter
## can modify these more based on soil type, but started with middle of the road values 
## would need more detailed info about soil types to do this in a way that gets realistic water potential values...

MonthlySummary <- MonthlySummary %>% mutate(water_potential =  ((1/alpha) * (((soil_moisture_AM/theta_s)^(-1/m)) - 1)^(1/n)) * -0.00981 )

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = water_potential, color = ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ID*Category) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary <- MonthlySummary %>%
  filter(!is.na(water_potential) & is.finite(water_potential))


color_palette <- colorRampPalette(c("blue", "yellow", "red"))
colors <- color_palette(100)  

MonthlySummary$color <- colors[cut(MonthlySummary$water_potential, breaks = 100)]  
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Water Potential")
points(MonthlySummary$Longitude, MonthlySummary$Latitude, col = MonthlySummary$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$water_potential), col = colors, legend.lab = "Water Potential")

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


