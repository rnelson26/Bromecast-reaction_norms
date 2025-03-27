
#### regular PCA #######
#### to understand relationships among climate variables ######
##### created 3/27/25 ###########
####### last updated: 3/27/25 ##########

#### Load data and packages #########

library(tidyverse)
library(dplyr)
library(ggplot2)
library(FactoMineR)   
library(factoextra)   

data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE)

###### Select relevant variables #########
data <- data %>%
  mutate(site_year = paste(site, year))

# Select relevant columns (replace with actual column names)
climate_vars <- climate_vars <- c(
  "prcp.Spr", "tmean.Spr", "swe_mean.Spr", "prcp.Sum", "tmean.Sum", 
   "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  "tmean.Fall", "swe_mean.Fall", "MAT", 
  "total_precip", "seasonality"
)



# Subset and scale the data
pca_data <- data %>% filter(Emerged == "Y") %>% filter(Reproduced == "Y") %>% 
  select(site_year, all_of(climate_vars), Fecundity)  %>%
  na.omit()  

# Store site_year separately and remove from scaled PCA variables
site_year_labels <- pca_data$site_year  
pca_scaled <- scale(pca_data %>% select(-site_year))  # Standardize data


pca_result <- PCA(pca_scaled, scale.unit = TRUE, graph = FALSE)

fviz_eig(pca_result)
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("blue", "red"))
fviz_pca_ind(pca_result, geom = "point", col.ind = "cos2")
fviz_pca_biplot(pca_result, repel = TRUE)

fviz_pca_ind(pca_result, 
             geom = "point", 
             col.ind = as.factor(site_year),  # Color by site and year
             palette = "jco", 
             repel = TRUE) +
  theme_minimal() +
  labs(title = "PCA of Sites by Year", color = "Site-Year")


pca_scores <- as.data.frame(pca_result$ind$coord)  # Get PCA coordinates
pca_scores$site_year <- site_year_labels  # Reattach site_year

fviz_pca_ind(pca_result, 
             geom = "point", 
             col.ind = as.factor(site_year_labels),  # Color by site_year
             palette = "Set3",  # Or try another palette like "viridis"
             repel = TRUE, 
             pointshape = 16) +  # Force all points to be circles
  theme_minimal() +
  labs(title = "PCA of Sites by Year", color = "Site-Year") +
  scale_color_manual(values = rainbow(length(unique(site_year_labels))))  # More colors if needed


fviz_pca_biplot(pca_result, 
                geom.ind = "point",  # Show individuals as points
                col.ind = as.factor(site_year_labels),  # Color points by site_year
                palette = rainbow(length(unique(site_year_labels))),  # Use rainbow colors
                pointshape = 16,  # Keep all points as circles
                pointsize = 2,  # Keep point sizes uniform
                alpha.ind = 0.8,  # Adjust transparency of points
                col.var = "black",  # Variables (arrows) in black
                repel = TRUE) + 
  theme_minimal() +
  labs(title = "PCA Biplot: Sites by Year with Variable Contributions", 
       color = "Site-Year")

fviz_cos2(pca_result, choice = "var", axes = 1:2)
fviz_contrib(pca_result, choice = "var", axes = 1:2)

fviz_pca_var(pca_result, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
