########## Supplmenetal Figures and Tables Code ############
############ code by R. A. Nelson #######
########## created on 4/30/26 ###########
############ last updated: 4/30/26 ############

# Load data
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")

###### Training vs Testing Split ############
ggplot(combined_data_emg, aes(x = tmean.Sum, fill = Dataset)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "red"))

library(ggplot2)

# Vector of climate variables
climate_vars <- c(
  "MAT", "seasonality", "prcp.Spr", "prcp.Sum", "prcp.Win", "prcp.Fall",
  "tmean.Spr", "tmean.Sum", "tmean.Win", "tmean.Fall",
  "swe_mean.Spr", "swe_mean.Sum", "swe_mean.Win", "swe_mean.Fall",
  "prcp_center30d_mean", "tmin_center30d_mean", "tmax_center30d_mean",
  "tavg_center30d_mean", "tmin_center30d_min", "tmax_center30d_max"
)

# Open PDF device
pdf("climate_histograms.pdf", width = 8, height = 6)

# Loop through variables
for (var in climate_vars) {
  
  p <- ggplot(combined_data_emg, aes_string(x = var, fill = "Dataset")) +
    geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
    theme_minimal() +
    scale_fill_manual(values = c("Training" = "blue", "Testing" = "red")) +
    labs(title = paste("Histogram of", var))
  
  print(p)  # important: prints each plot to a new PDF page
}

# Close PDF
dev.off()

######## Table of Site Years  #########
climate_vars <- c(
  "MAT", "seasonality", "prcp.Spr", "prcp.Sum", "prcp.Win", "prcp.Fall",
  "tmean.Spr", "tmean.Sum", "tmean.Win", "tmean.Fall",
  "swe_mean.Spr", "swe_mean.Sum", "swe_mean.Win", "swe_mean.Fall",
  "prcp_center30d_mean", "tmin_center30d_mean", "tmax_center30d_mean",
  "tavg_center30d_mean", "tmin_center30d_min", "tmax_center30d_max"
)

soil_vars <- c("pH", "EC", "OMpercent", "Protein_g.kg", "X..Sand", "X..Clay", "X..Silt")

meta_vars <- c("site_year", "site", "year", "Lat", "Lon", "Type", "Dataset")

site_year_summary <- combined_data_emg %>%
  dplyr::select(all_of(meta_vars), all_of(climate_vars), all_of(soil_vars)) %>%
  group_by(site_year) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop")

library(flextable)

ft <- flextable(site_year_summary) %>%
  autofit() %>%
  theme_booktabs()

ft <- ft %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all")

library(officer)

doc <- read_docx() %>%
  body_add_par("Site-Year Climate and Soil Summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Table_S1.docx")

### geographic map
library(maps)

# Get US state boundaries
state_map <- map_data("state")

state_map_filtered <- state_map %>%
  filter(long >= -128 & long <= -95 & lat >= 30 & lat <= 52)

# Plot map
map_df <- combined_data_emg %>%
  distinct(site_year, .keep_all = TRUE)

map <- ggplot() +
  geom_polygon(
    data = state_map_filtered,
    aes(x = long, y = lat, group = group),
    fill = "gray90", color = "black"
  ) +
  geom_point(
    data = map_df,
    aes(x = Lon, y = Lat, color = Type, shape = Dataset),
    size = 3, alpha = 0.7
  ) +
  scale_color_manual(values = c("Common_Garden" = "blue", "Satellite" = "red")) +
  scale_shape_manual(values = c("Training" = 16, "Testing" = 17)) +
  coord_cartesian(xlim = c(-128, -95), ylim = c(30, 52)) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Site Type",
    shape = "Dataset"
  ) +
  theme(legend.position = "bottom") 

ggsave("output/map_figure.pdf", plot = map, width = 8, height = 6)
