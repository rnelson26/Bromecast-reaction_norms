######## Data Merging ##############################################
##### Merge common garden and satellite site data #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 1/28/25 #######
############ last modified: 6/3/25 ########################

### outstanding questions ##########
## whether approach to zero neighbors makes sense 


rm(list = ls())


#library(tidyverse) 
library(dplyr)

##### load data #########
sat <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/all_plants_ftypes1.csv", header = TRUE)

cg <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/cg_fullData_withFlags.csv", header = TRUE)


##### create a new column for type ######
sat$Type <- "Satellite"

cg$Type <- "Common_Garden"

cg <- cg %>%
  rename(site_old = site) %>%
  mutate(site = paste(site_old, albedo, sep = "_"))


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
  cg[i, "neighbors"] <- nrow(possible_neighbors %>% filter(Reproduced == "Y"))
  
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
sat$site_old <- sat$site

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
combined_clean <- combined %>% dplyr::select(site, site_old, year, Treatment, Transect, Distance, Emerged, Reproduced, neighbors, Fecundity, Biomass, fecundityflag, notesFlag, Lat, Lon, annual, unknown, perennial, shrub, Type, plantID, albedo, x, y, genotype, block, plot, note_standard_harvest, note_standard_phen)

## make merged column for site, plot, and year
combined_clean$Transect_Site_Year <- paste(combined_clean$Transect, combined_clean$site, combined_clean$year, sep = " - ")

#inspect results
str(combined_clean)

## save as .csv 
write.csv(combined_clean, "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv", row.names = FALSE)


###### CG Lat Lon ######
combined_clean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv")

garden_info <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/garden_info.csv")

daymean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/daymet_season_means.csv")

climD <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/daymet_daily.csv")

### add a lat and long for common gardens
combined_clean <- combined_clean %>%
  mutate(
    Lat = ifelse(site_old == "BA" & Type == "Common_Garden", 43.208482, Lat),
    Lon = ifelse(site_old == "BA" & Type == "Common_Garden", -116.995198, Lon),
    Lat = ifelse(site_old == "CH" & Type == "Common_Garden", 41.212078, Lat),
    Lon = ifelse(site_old == "CH" & Type == "Common_Garden", -104.852543, Lon),
    Lat = ifelse(site_old == "SS" & Type == "Common_Garden", 44.245590, Lat),
    Lon = ifelse(site_old == "SS" & Type == "Common_Garden", -112.214337, Lon),
    Lat = ifelse(site_old == "WI" & Type == "Common_Garden", 43.474370, Lat),
    Lon = ifelse(site_old == "WI" & Type == "Common_Garden", -116.901770, Lon)
  )

write.csv(combined_clean, "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv", row.names = FALSE)


#### MAP/MAT with summary data ##########



#You can sum those seasons to get the annual totals (precip) and means (temperature). 

#combined_clean <- combined_clean %>%
 # mutate(
  #  MAP = prcp.Win + prcp.Spr + prcp.Sum + prcp.Fall,
   # MAT = tmean.Win + tmean.Spr + tmean.Sum + tmean.Fall
  #)

#summary <- combined_clean %>%
 # select(site, year, MAP, MAT, Lon, Lat) %>% distinct()

### MAP
#summary %>% filter(year < 2024) %>% ggplot(aes(x = year, y = MAP)) +
 # geom_point() + 
  #theme_classic() + facet_wrap(~site) + theme(
   # axis.text.x = element_text(angle = 90, hjust = 1)  
 # ) 

#custom_colors <- c("blue", "cyan", "green", "yellow", "orange", "red")

#state_map <- map_data("state")
#state_map_filtered <- state_map[state_map$long >= -128 & state_map$long <= -95 &
                                  state_map$lat >= 30 & state_map$lat <= 52, ]
# ggplot() +
 # geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill="gray100", color="black") +
  #geom_point(data=summary, aes(x=Lon, y=Lat, color=MAP)) +
  #scale_color_gradientn(
   # colors = c("blue", "cyan", "green", "yellow", "orange", "red"))  +
  #coord_cartesian(xlim=c(-128, -95), ylim=c(30, 52)) +
  #facet_wrap(~year) +
  #theme_classic() +
 #labs(title="MAP", color="MAP") 
 
 ### MAT
 #summary %>% filter(year < 2024) %>% ggplot(aes(x = year, y = MAT)) +
  # geom_point() + 
   #theme_classic() + facet_wrap(~site) + theme(
    # axis.text.x = element_text(angle = 90, hjust = 1)  
   #) 
 
 #custom_colors <- c("blue", "cyan", "green", "yellow", "orange", "red")
 
 #state_map <- map_data("state")
 #state_map_filtered <- state_map[state_map$long >= -128 & state_map$long <= -95 &
#                                   state_map$lat >= 30 & state_map$lat <= 52, ]
 #ggplot() +
  # geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill#="gray100", color="black") +
#   geom_point(data=summary, aes(x=Lon, y=Lat, color=MAT)) +
 #  scale_color_gradientn(
  #   colors = c("blue", "cyan", "green", "yellow", "orange", "red"))  +
   #coord_cartesian(xlim=c(-128, -95), ylim=c(30, 52)) +
   #facet_wrap(~year) +
   #theme_classic() +
   #labs(title="MAT", color="MAT") 
 
  
##### MAT/MAP/seasonal variability with daymet ########
 combined_clean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv")
 
 #climD <- read.csv("/Users/Becca/Desktop/Satellites_daymet_daily.csv", header = TRUE)
 
 # set up climate seasons
 climD$season <- "Win"
 climD$season[climD$climDay < 92] <- "Fall"
 climD$season[climD$climDay > 184 & climD$climDay < 276] <- "Spr"
 climD$season[climD$climDay >= 276] <- "Sum"
 
 ## For seasonality, I suggest identifying the day of the climate year (Oct 1 - Sept 30) when 50% of the total annual precip has been received. Eventually I'd like to calculate the analogous metric for WDD.
 
 
 climD_clean <- climD %>%
   group_by(climYr, SiteCode) %>%
   mutate(
     total_precip = sum(prcp),  # Total annual precipitation
     MAT = mean((tmax + tmin) / 2)  # Mean annual temperature
   ) %>%
   mutate(
     cumulative_prcp = cumsum(prcp)  # Cumulative precipitation over the year
   ) %>%
   filter(cumulative_prcp >= 0.5 * total_precip) %>%  # Keep only rows where cumulative_prcp reaches 50% of total
   slice_min(climDay, with_ties = FALSE) %>%  # Select the first occurrence 
   summarise(
     total_precip = first(total_precip),
     MAT = first(MAT),
     seasonality = first(climDay)  # The first day reaching 50% MAP
   ) %>%
   ungroup()  %>%  filter(climYr > 2020 & climYr < 2024)
 

 
 

 
site_list <- combined_clean %>% select(site_old, Lat, Lon) %>% distinct()
site_list$SiteCode <- site_list$site

name_mapping <- data.frame(
  old_name = c("EnsingS1 SuRDC", "EnsingS2 Summerland-Princeton", "EnsingS3 Bear Creek", "EnsingS4 Lundbom", "Symstad1", "Symstad2"  ), 
  new_name = c("EnsingS1_SuRDC","EnsingS2_SumPrinceRd","EnsingS3_BearCreek", "EnsingS4_LDBM", "SymstadS1", "SymstadS2"  )
)

# Replace values in the r_summary and YearlySummary dataframes
site_list$SiteCode <- sapply(site_list$SiteCode, function(x) {
  match <- name_mapping$new_name[name_mapping$old_name == x]
  if (length(match) > 0) match else x
})

climD_clean$SiteCode <- sapply(climD_clean$SiteCode, function(x) {
  match <- name_mapping$new_name[name_mapping$old_name == x]
  if (length(match) > 0) match else x
}) 

climD_clean_full <- left_join(climD_clean, site_list, by= "SiteCode")


### MAP
climD_clean_full %>%  filter(year > 2020 & year < 2024) %>% ggplot(aes(x = year, y = MAP)) +
  geom_point() + 
  theme_classic() + facet_wrap(~SiteCode) + theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  
  ) 

custom_colors <- c("blue", "cyan", "green", "yellow", "orange", "red")

state_map <- map_data("state")
state_map_filtered <- state_map[state_map$long >= -128 & state_map$long <= -95 &
                                  state_map$lat >= 30 & state_map$lat <= 52, ]
recent <- climD_clean_full

 ggplot() +
  geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill="gray100", color="black") +
  geom_point(data=recent, aes(x=Lon, y=Lat, color=total_precip)) +
  scale_color_gradientn(
    colors = c("blue", "cyan", "green", "yellow", "orange", "red"))  +
  coord_cartesian(xlim=c(-128, -95), ylim=c(30, 52)) +
  facet_wrap(~climYr) +
  theme_classic() +
  labs(title="MAP", color="MAP") 

### MAT
 recent %>%  filter(year > 2020 & year < 2024) %>% ggplot(aes(x = year, y = MAT)) +
   geom_point() + 
   theme_classic() + facet_wrap(~SiteCode) + theme(
     axis.text.x = element_text(angle = 90, hjust = 1)  
   ) 
 
 custom_colors <- c("blue", "cyan", "green", "yellow", "orange", "red")
 
 state_map <- map_data("state")
 state_map_filtered <- state_map[state_map$long >= -128 & state_map$long <= -95 &
                                   state_map$lat >= 30 & state_map$lat <= 52, ]

 
 ggplot() +
   geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill="gray100", color="black") +
   geom_point(data=recent, aes(x=Lon, y=Lat, color=MAT)) +
   scale_color_gradientn(
     colors = c("blue", "cyan", "green", "yellow", "orange", "red"))  +
   coord_cartesian(xlim=c(-128, -95), ylim=c(30, 52)) +
   facet_wrap(~climYr) +
   theme_classic() +
   labs(title="MAT", color="MAT") 
 
 
 ### Seasonality
recent %>%  filter(year > 2020 & year < 2024) %>% ggplot(aes(x = year, y = seasonality)) +
   geom_point() + 
   theme_classic() + facet_wrap(~SiteCode) + theme(
     axis.text.x = element_text(angle = 90, hjust = 1)  
   ) 
 
 custom_colors <- c("blue", "cyan", "green", "yellow", "orange", "red")
 
 state_map <- map_data("state")
 state_map_filtered <- state_map[state_map$long >= -128 & state_map$long <= -95 &
                                   state_map$lat >= 30 & state_map$lat <= 52, ]

## think more about why you would need to take the mean
 
  ggplot() +
   geom_polygon(data=state_map_filtered, aes(x=long, y=lat, group=group), fill="gray100", color="black") +
   geom_point(data=recent, aes(x=Lon, y=Lat, color=seasonality)) +
   scale_color_gradientn(
     colors = c("blue", "cyan", "green", "yellow", "orange", "red"),
   ) + 
   coord_cartesian(xlim=c(-128, -95), ylim=c(30, 52)) +
   facet_wrap(~climYr) +
   theme_classic() +
   labs(title="Seasonality", color="Seasonality")
 
 
###### combine climate summary variables with with merged data ######
  combined_clean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv")
  
  garden_info <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/garden_info.csv")
  
  daymean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/daymet_season_means.csv")
  
  climD <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/daymet_daily.csv")
  
  climD_24 <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/daymet_daily_24.csv")
  
  climD_full <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/daymet_daily_full.csv")
  
  climD_24 <- climD_24 %>% dplyr::select(-SiteCode_numeric, -season) %>% filter(climYr == 2024)
  climD <- climD %>% dplyr::select(-season)
  
  climD_full <- rbind(climD, climD_24)
  
  climD_full$season <- "Win"
  climD_full$season[climD_full$climDay < 92] <- "Fall"
  climD_full$season[climD_full$climDay > 184 & climD_full$climDay < 276] <- "Spr"
  climD_full$season[climD_full$climDay >= 276] <- "Sum"
  
write.csv(climD_full, "daymet_daily_full.csv", row.names = FALSE)

  annD <- climD_full %>% group_by(SiteCode,climYr,season) %>%
    summarise(prcp=sum(prcp),
              tmean=mean(tavg),
              swe_mean=mean(swe)) #,
  #swe_days=sum(swe>0))
  
  annD_wide <- annD %>%
    pivot_wider(
      names_from = season,
      values_from = c(prcp, tmean, swe_mean),
      names_sep = "."
    )
  

  
  climD_clean <- climD_full %>%
    group_by(climYr, SiteCode) %>%
    mutate(
      total_precip = sum(prcp, na.rm = TRUE),
      MAT = mean((tmax + tmin) / 2, na.rm = TRUE),
      cumulative_prcp = cumsum(prcp)
    ) %>%
    filter(cumulative_prcp >= 0.5 * total_precip) %>%
    slice_min(climDay, with_ties = FALSE) %>%
    summarise(
      total_precip = first(total_precip),
      MAT = first(MAT),
      seasonality = first(climDay)
    ) %>%
    ungroup() %>%
    filter(climYr > 2020 & climYr < 2025)
  

recent <- left_join(climD_clean, annD_wide, by = c("SiteCode", "climYr"))
  
  
colnames(recent)[colnames(recent) == "SiteCode"] <- "site_old"
colnames(recent)[colnames(recent) == "climYr"] <- "year" 


combined_clean_climate <- left_join(combined_clean, recent, by = c("site_old", "year"))


write.csv(combined_clean_climate, "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", row.names = FALSE)

########## Start of Season Variables ##################
data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE) 

sat <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/list_plots_sos.csv", header = TRUE) 

cg <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/garden_plots_sos.csv", header = TRUE) 

climD <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/daymet_daily.csv")

sat_clean  <- sat %>% dplyr::select(site_old, Year, SOS_doy, SOS_date)

cg_clean  <- cg %>% dplyr::select(site_old, Year, SOS_doy, SOS_date)

 
sos <- rbind(sat_clean, cg_clean)

## check first that none of the SOS dates for a given year are from Oct-Dec
names(sos)[names(sos) == "Year"] <- "year"

climD_short <-climD %>% dplyr::select(yday, prcp, tmax, tmin, SiteCode, climDay, tavg, climYr)
## climate year and year the same in all of these 

sos_climate <- left_join(
  sos,
  climD_short,
  by = c("site_old" = "SiteCode", "year" = "climYr", "SOS_doy" = "yday")
)

## remove duplicates 
sos_climate <- sos_climate %>% distinct()

library(dplyr)
library(purrr)

windows <- c(15, 30, 60)

# extract climate info function
extract_window_stats <- function(site_old, year, SOS_doy, var, direction, window) {
  if (direction == "before") {
    climD_window <- climD %>%
      filter(
        SiteCode == site_old,
        climYr == year,
        yday >= SOS_doy - window,
        yday < SOS_doy
      )
  } else {
    climD_window <- climD %>%
      filter(
        SiteCode == site_old,
        climYr == year,
        yday > SOS_doy,
        yday <= SOS_doy + window
      )
  }
  climD_window[[var]]
}

# Create summaries for climate windows
climate_summaries <- pmap_dfr(
  list(site_old = sos$site_old, year = sos$year, SOS_doy = sos$SOS_doy),
  function(site_old, year, SOS_doy) {
    vals <- list()
    for (w in windows) {
      for (direction in c("before", "after")) {
        vals[[paste0("prcp_", direction, w, "d_sum")]] <- sum(
          extract_window_stats(site_old, year, SOS_doy, "prcp", direction, w), na.rm = TRUE)
        vals[[paste0("tmin_", direction, w, "d_avg")]] <- mean(
          extract_window_stats(site_old, year, SOS_doy, "tmin", direction, w), na.rm = TRUE)
        vals[[paste0("tmax_", direction, w, "d_avg")]] <- mean(
          extract_window_stats(site_old, year, SOS_doy, "tmax", direction, w), na.rm = TRUE)
        vals[[paste0("tavg_", direction, w, "d_avg")]] <- mean(
          extract_window_stats(site_old, year, SOS_doy, "tavg", direction, w), na.rm = TRUE)
      }
    }
    as_tibble(vals)
  }
)

# summary dataframe
sos_with_climate <- bind_cols(sos, climate_summaries)
### gives climate info 15,30, and 60 days before after SOS
### sos_climate gives climate info on SOS date
combined_clean_climate_SOS <- left_join(data, sos_with_climate, by = c("site_old", "year"))


### save as new file
write.csv(combined_clean_climate_SOS, "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate_SOS.csv", row.names = FALSE)

