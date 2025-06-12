#### Integrated Reaction Norm Model #######
######## code by Becca Nelson and Justin Van Ee ###############################
############# created 3-25-25 ######################
############# Last modified: 6-10-25 ##########################
######## modifies RMD file to pull from one integrated df ########

rm(list = ls())

## to do


###### Load packages #####
library(tidyverse)
library(bayesplot)
library(cmdstanr)
library(reshape2)
library(FactoMineR)   
library(factoextra)
library(verification)
library(VGAM)
library(scoringRules)
library(hypergeo)

#library(ggplot2) #if you don't want to load the whole tidyverse
#library(dplyr)

##### Load Data #########
#data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE) 

data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate_SOS.csv", header = TRUE) 

kinshipIDs <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/93cg_genotypes.csv")

kinship <- read.table("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/BRTE307_IBSmatrix.txt", sep=",")

assigned_genotypes <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/assigned_genotypes.csv")

tips <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/307tips.csv")

cg_WC <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/dailyVWCdata_allgardens_allyears.csv")

cg_temp <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/dailytempdata_allgardens_allyears.csv")

BRTE <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/BRTE_NorthAmerica.csv", header = TRUE)

soil_clean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soil_clean.csv")

###### summarise soil data to site-year ########
## also need to assign cg values

soil_summary <- soil_clean %>% group_by(site_old) %>% summarise(across(c(pH, EC, OMpercent, Protein_g.kg), \(x) mean(x, na.rm = TRUE)))

data <- left_join(data, soil_summary, by = "site_old")


vars_to_fill <- c("pH", "EC", "OMpercent", "Protein_g.kg")

## use Boise Low sat soil values for wildcat cg
reference_values <- data %>%
  dplyr::filter(site_old == "Boise_Low") %>%
  dplyr::select(all_of(vars_to_fill)) %>%
  dplyr::summarise(across(everything(), ~ first(na.omit(.))))

for (var in vars_to_fill) {
  data[[var]][data$site_old == "WI" & is.na(data[[var]])] <- reference_values[[var]]
}

reference_values <- data %>%
  dplyr::filter(site_old == "CG PASTURE") %>%
  dplyr::select(all_of(vars_to_fill)) %>%
  dplyr::summarise(across(everything(), ~ first(na.omit(.))))

for (var in vars_to_fill) {
  data[[var]][data$site_old == "CH" & is.na(data[[var]])] <- reference_values[[var]]
}

library(maps)
library(fields)      
library(RColorBrewer)


colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(100)


par(mar = c(1, 1, 1, 1))
map("state", xlim = c(-128, -95), ylim = c(30, 52))
title("Soil EC")
soil_var <- data$EC
points(data$Lon, data$Lat, col = colors[cut(soil_var, breaks = 100)], pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(soil_var, na.rm = TRUE), 
           col = colors)

par(mar = c(1, 1, 1, 1))
map("state", xlim = c(-128, -95), ylim = c(30, 52))
title("Soil pH")
soil_var <- data$pH
points(data$Lon, data$Lat, col = colors[cut(soil_var, breaks = 100)], pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(soil_var, na.rm = TRUE), 
           col = colors)

par(mar = c(1, 1, 1, 1))
map("state", xlim = c(-128, -95), ylim = c(30, 52))
title("Soil protein")
soil_var <- data$Protein_g.kg
points(data$Lon, data$Lat, col = colors[cut(soil_var, breaks = 100)], pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(soil_var, na.rm = TRUE), 
           col = colors)

par(mar = c(1, 1, 1, 1))
map("state", xlim = c(-128, -95), ylim = c(30, 52))
title("Soil Organic Content")
soil_var <- data$OMpercent
points(data$Lon, data$Lat, col = colors[cut(soil_var, breaks = 100)], pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(soil_var, na.rm = TRUE), 
           col = colors)


###### add cg climate offset #########
## white gravel substract one, black gravel add one


data <- data %>%
  mutate(across(c(tmean.Fall, tmean.Sum, tmean.Spr, tmean.Win, MAT), 
                ~ case_when(
                  albedo == "black" ~ . + 1,  
                  albedo == "white" ~ . - 1,  
                  is.na(albedo) & Type == "Satellite" ~ .,  
                  TRUE ~ .
                )))



####### Prepare data for model ########

### Genotypes info ##########
#K <- diag(1, 93, 93) #indep kinship matrix

BRTE <- left_join(BRTE, tips, by = "PopNum") 

BRTE <- BRTE %>% dplyr::select(PopNum, NewSiteCode, tip.label, IBS.id)

assigned_genotypes <- left_join(assigned_genotypes, BRTE, by = "NewSiteCode") 


#genotypes_common_gardens <- 
 # kinshipIDs %>%
#  mutate(source = as.factor(source)) %>%
 # filter(genotype %in% unique(data$genotype)) %>%
#  arrange(NewSiteCode)

assigned_genotypes$kinshipID

genotypes_all <- kinshipIDs %>%
  mutate(source = as.factor(source)) %>%
  filter(genotype %in% unique(c(data$genotype, assigned_genotypes$genotype))) %>%  # Include assigned genotypes
  arrange(NewSiteCode)


# Filter for common garden genotypes 
#K_common_garden <- as.matrix(K[genotypes_all$kinshipID,genotypes_all$kinshipID])
K_common_garden <- as.matrix(kinship[genotypes_all$kinshipID,genotypes_all$kinshipID])
#use kinship for actual matrix 

#K_common_garden <- as.matrix(kinship[assigned_genotypes$PopNumD,assigned_genotypes$IBS.id]) #doesn't work

#Diana: if you go with PopNum and corresponding IBS.id (row and column number in BRTE307_IBSmatrix.txt) you should get the correct genotypes from for the new kinship matrix.

# Put genotype numbers on rows and columns
colnames(K_common_garden) <- rownames(K_common_garden) <- as.factor(genotypes_all$genotype)


######## Demography info #########
assigned_genotypes$site <- as.factor(assigned_genotypes$site)
assigned_genotypes$genotype <- as.integer(assigned_genotypes$genotype)
assigned_genotypes$NewSiteCode <- as.factor(assigned_genotypes$NewSiteCode)
kinshipIDs$NewSiteCode <- as.factor(kinshipIDs$NewSiteCode)

## make site year column 
data$site <- as.factor(data$site)
data$year <- as.factor(data$year)
data <- data %>%
  mutate(site_year = paste(site, year))

df <- data %>%
  dplyr::filter(Emerged == "Y", Reproduced == "Y") %>%
  mutate(
    site_numeric = as.numeric(as.factor(site)),
    site_year_numeric = as.numeric(as.factor(site_year)),
    year_numeric = as.numeric(as.factor(year)) - 1
  ) %>%
  left_join(assigned_genotypes %>% 
              dplyr::select(site, genotype_assigned = genotype, 
                     NewSiteCode, SeedSource, sample.id), 
            by = "site") %>%
  # Replace NA genotypes from training_data with those from assigned_genotypes
  mutate(
    genotype = ifelse(is.na(genotype), genotype_assigned, genotype)  # Ensure type consistency
  ) %>%
  dplyr::select(-genotype_assigned) %>%  # Remove temporary column
  # Join with kinshipIDs using the newly assigned genotype
  left_join(kinshipIDs, by = c("genotype", "NewSiteCode")) %>%
  dplyr::filter(!is.na(Fecundity)) %>%
  dplyr::filter(!is.na(genotype)) %>%
  dplyr::filter(!is.na(neighbors)) %>%
  dplyr::filter(!is.na(annual)) %>%
  dplyr::filter(!is.na(perennial)) %>%
  dplyr::filter(!is.na(shrub)) %>%
  dplyr::filter(Fecundity > 0) ## compare to flagged column, hopefully any zeros should be flagged 

## scale competition variables
df <- df %>%
  mutate(
    neighbors.s = scale(neighbors)[,1],
    perennial.s = scale(perennial)[,1],
    shrub.s = scale(shrub)[,1],
    annual.s = scale(annual)[,1],
  )


df_rep <- data %>%
  dplyr::filter(Emerged %in% c("Y")) %>%  ## comment out if you don't reproduced to be conditional on Emerged 
  dplyr::filter(Reproduced %in% c("Y", "N")) %>% 
  mutate(
    site_numeric = as.numeric(as.factor(site)),
    site_year_numeric = as.numeric(as.factor(site_year)),
    year_numeric = as.numeric(as.factor(year)) - 1
  ) %>%
  left_join(assigned_genotypes %>% 
              dplyr::select(site, genotype_assigned = genotype, 
                            NewSiteCode, SeedSource, sample.id), 
            by = "site") %>%
  mutate(
    genotype = ifelse(is.na(genotype), genotype_assigned, genotype)  # Ensure type consistency
  ) %>%
  dplyr::select(-genotype_assigned) %>%  # Remove temporary column
  # Join with kinshipIDs using the newly assigned genotype
  left_join(kinshipIDs, by = c("genotype", "NewSiteCode")) %>%
  dplyr::filter(!is.na(genotype)) %>%
  dplyr::filter(!is.na(neighbors)) %>%
  dplyr::filter(!is.na(annual)) %>%
  dplyr::filter(!is.na(perennial)) %>%
  dplyr::filter(!is.na(shrub)) 

df_emg <- data %>%
  dplyr::filter(Emerged %in% c("Y", "N")) %>% 
  mutate(
    site_numeric = as.numeric(as.factor(site)),
    site_year_numeric = as.numeric(as.factor(site_year)),
    year_numeric = as.numeric(as.factor(year)) - 1
  ) %>%
  left_join(assigned_genotypes %>% 
              dplyr::select(site, genotype_assigned = genotype, 
                            NewSiteCode, SeedSource, sample.id), 
            by = "site") %>%
  mutate(
    genotype = ifelse(is.na(genotype), genotype_assigned, genotype)  # Ensure type consistency
  ) %>%
  dplyr::select(-genotype_assigned) %>%  # Remove temporary column
  # Join with kinshipIDs using the newly assigned genotype
  left_join(kinshipIDs, by = c("genotype", "NewSiteCode")) %>%
  dplyr::filter(!is.na(genotype)) %>%
  dplyr::filter(!is.na(neighbors)) %>%
  dplyr::filter(!is.na(annual)) %>%
  dplyr::filter(!is.na(perennial)) %>%
  dplyr::filter(!is.na(shrub)) 

## scale competition variables
df <- df %>%
  mutate(
    neighbors.s = scale(neighbors)[,1],
    perennial.s = scale(perennial)[,1],
    shrub.s = scale(shrub)[,1],
    annual.s = scale(annual)[,1],
  )

df_rep <- df_rep %>%
  mutate(
    neighbors.s = scale(neighbors)[,1],
    perennial.s = scale(perennial)[,1],
    shrub.s = scale(shrub)[,1],
    annual.s = scale(annual)[,1],
  )

df_emg <- df_emg %>%
  mutate(
    neighbors.s = scale(neighbors)[,1],
    perennial.s = scale(perennial)[,1],
    shrub.s = scale(shrub)[,1],
    annual.s = scale(annual)[,1],
  )


df <- df %>%
    filter(!is.na(genotype)) 

df_rep <- df_rep %>%
  filter(!is.na(genotype))

df_emg <- df_emg %>%
  filter(!is.na(genotype))


valid_genotypes <- rownames(K_common_garden)
df <- df %>% filter(genotype %in% valid_genotypes)
df_rep <- df_rep %>% filter(genotype %in% valid_genotypes)
df_emg <- df_emg %>% filter(genotype %in% valid_genotypes)


###### Climate PCA #########
climate_vars <- c(
  "prcp.Spr", "tmean.Spr",  "prcp.Sum", "tmean.Sum", 
  "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  "tmean.Fall", "swe_mean.Fall", "MAT", 
  "total_precip", "seasonality"
)

SOS_vars <- c(
  "prcp.Spr", "tmean.Spr",  "prcp.Sum", "tmean.Sum", 
  "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  "tmean.Fall", "swe_mean.Fall", "MAT", 
  "total_precip", "seasonality", "prcp_center30d_mean",  
"tmin_center30d_mean",   "tmax_center30d_mean",  
"tavg_center30d_mean",   "tmin_center30d_min",   
"tmax_center30d_max"   
)

#fall_vars <- c(
 #  "prcp.Fall", 
#  "tmean.Fall", "swe_mean.Fall"
#)

soil_vars <- c(
  "pH", "EC", "OMpercent", "Protein_g.kg")

#full_vars <- c(
 # "prcp.Spr", "tmean.Spr",  "prcp.Sum", "tmean.Sum", 
  #"prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  #"tmean.Fall", "swe_mean.Fall", "MAT", 
  #"total_precip", "seasonality",  "pH", "EC", "OMpercent", "Protein_g.kg"
#)

pca_data <- df %>% 
  dplyr::select(site_year, all_of(climate_vars))  %>% distinct() %>% 
  na.omit()  

pca_data_SOS <- df %>% 
  dplyr::select(site_year, all_of(SOS_vars))  %>% distinct() %>% 
  na.omit()  

pca_data_rep <- df_rep %>% 
  dplyr::select(site_year, all_of(climate_vars))  %>% distinct() %>% 
  na.omit()  

pca_data_rep_SOS <- df_rep %>% 
  dplyr::select(site_year, all_of(SOS_vars))  %>% distinct() %>% 
  na.omit()  

pca_data_emg <- df_emg %>% 
  dplyr::select(site_year, all_of(climate_vars))  %>% distinct() %>% 
  na.omit()  

pca_data_emg_SOS <- df_emg %>% 
  dplyr::select(site_year, all_of(SOS_vars))  %>% distinct() %>% 
  na.omit()  

#pca_data_emg_fall <- df_emg %>% 
 # dplyr::select(site_year, all_of(fall_vars))  %>% distinct() %>% 
#  na.omit()  


soil_data <- df %>% 
  dplyr::select(site, all_of(soil_vars))  %>% distinct() %>% 
  na.omit() 

soil_data_rep <- df_rep %>% 
  dplyr::select(site, all_of(soil_vars))  %>% distinct() %>% 
  na.omit() 

soil_data_emg <- df_emg %>% 
  dplyr::select(site, all_of(soil_vars))  %>% distinct() %>% 
  na.omit() 


#full_data <- df %>% 
  #dplyr::select(site_year, all_of(full_vars))  %>% distinct() %>% 
 # na.omit() 


site_year_labels <- pca_data$site_year  
site_labels_soil <- soil_data$site
#site_year_labels_full <- full_data$site_year 
site_year_labels_SOS <- pca_data_SOS$site_year 

site_year_labels_rep <- pca_data_rep$site_year  
site_year_labels_rep_SOS <- pca_data_rep_SOS$site_year  
site_year_labels_soil_rep <- soil_data_rep$site_year  
site_labels_soil_rep <- soil_data_rep$site

site_year_labels_emg <- pca_data_emg$site_year  
site_year_labels_emg_SOS <- pca_data_emg_SOS$site_year  
site_year_labels_soil_emg <- soil_data_emg$site_year  
site_labels_soil_emg <- soil_data_emg$site

X <- scale(pca_data %>% dplyr::select(-site_year))
X_SOS <- scale(pca_data_SOS %>% dplyr::select(-site_year))
X_soil <- scale(soil_data %>% dplyr::select(-site))
#X_full <- scale(full_data %>% dplyr::select(-site_year))

X_rep <- scale(pca_data_rep %>% dplyr::select(-site_year))
X_rep_SOS <- scale(pca_data_rep_SOS %>% dplyr::select(-site_year))
X_soil_rep <- scale(soil_data_rep %>% dplyr::select(-site))

X_emg <- scale(pca_data_emg %>% dplyr::select(-site_year))
X_emg_SOS <- scale(pca_data_emg_SOS %>% dplyr::select(-site_year))
#X_emg_fall <- scale(pca_data_emg_fall %>% dplyr::select(-site_year))
X_soil_emg <- scale(soil_data_emg %>% dplyr::select(-site))

pca_out <- prcomp(X)
pca_out_SOS <- prcomp(X_SOS)
pca_out_soil <- prcomp(X_soil)
#pca_out_full <- prcomp(X_full)

pca_out_rep <- prcomp(X_rep)
pca_out_rep_SOS <- prcomp(X_rep_SOS)
pca_out_soil_rep <- prcomp(X_soil_rep)

pca_out_emg <- prcomp(X_emg)
pca_out_emg_SOS <- prcomp(X_emg_SOS)
#pca_out_emg_fall <- prcomp(X_emg_fall)
pca_out_soil_emg <- prcomp(X_soil_emg)

n_X <- nrow(pca_data)
q_X <- 2
Lambda <- as.matrix(pca_out$rotation[, 1:q_X])
Lambda_SOS <- as.matrix(pca_out_SOS$rotation[, 1:q_X])

Lambda_rep <- as.matrix(pca_out_rep$rotation[, 1:q_X])
Lambda_rep_SOS <- as.matrix(pca_out_rep_SOS$rotation[, 1:q_X])

Lambda_emg <- as.matrix(pca_out_emg$rotation[, 1:q_X])
Lambda_emg_SOS <- as.matrix(pca_out_emg_SOS$rotation[, 1:q_X])
#Lambda_emg_fall <- as.matrix(pca_out_emg_fall$rotation[, 1:q_X])

n_X_soil <- nrow(soil_data)
Lambda_soil <- as.matrix(pca_out_soil$rotation[, 1:q_X])
# still have some cg sites that need soil info
Lambda_soil_rep <- as.matrix(pca_out_soil_rep$rotation[, 1:q_X])
Lambda_soil_emg <- as.matrix(pca_out_soil_emg$rotation[, 1:q_X])



fviz_pca_biplot(pca_out,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()

fviz_pca_biplot(pca_out_SOS,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()

fviz_pca_biplot(pca_out_soil,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()


fviz_pca_biplot(pca_out_rep,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()

fviz_pca_biplot(pca_out_soil_rep,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()

#fviz_pca_biplot(pca_out_full,
 #               geom.ind = "point",               
  #              fill.ind = "grey80",              
   #             col.var = "contrib",              
    #            gradient.cols = c("blue", "red"), 
     #           repel = TRUE) +                   
#  theme_minimal()

fviz_pca_biplot(pca_out_emg,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()

#fviz_pca_biplot(pca_out_emg_fall,
 #               geom.ind = "point",               
  #              fill.ind = "grey80",              
   #             col.var = "contrib",              
    #            gradient.cols = c("blue", "red"), 
     #           repel = TRUE) +                   
#  theme_minimal()

fviz_pca_biplot(pca_out_soil_emg,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()

fviz_cos2(pca_out, choice = "var", axes = 1:2)
fviz_cos2(pca_out_SOS, choice = "var", axes = 1:2)
fviz_cos2(pca_out_soil, choice = "var", axes = 1:2)
fviz_cos2(pca_out_full, choice = "var", axes = 1:2)

fviz_cos2(pca_out_rep, choice = "var", axes = 1:2)
fviz_cos2(pca_out_soil_rep, choice = "var", axes = 1:2)

fviz_contrib(pca_out, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_out_SOS, choice = "var", axes = 1, top = 10)

fviz_contrib(pca_out, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_out_SOS, choice = "var", axes = 2, top = 10)

## elbow plots
explained_var <- pca_out$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

explained_var <- pca_out_SOS$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

explained_var <- pca_out_soil$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

explained_var <- pca_out_rep$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

explained_var <- pca_out_soil_rep$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

explained_var <- pca_out_emg$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

explained_var <- pca_out_emg_fall$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

explained_var <- pca_out_soil_emg$sdev^2
prop_var <- explained_var / sum(explained_var)
plot(prop_var, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Elbow Plot")

#### Split the data ########

### split training and test data 

data_sat <- df %>% filter(Type == "Satellite")

set.seed(123)  # For reproducibility

selected_categories <- data_sat %>%
  distinct(site_year) %>%  
  slice_sample(n = 28) %>% 
  pull(site_year)          

training_df <-df %>%
  filter(site_year %in% selected_categories | Type == "Common_Garden")

testing_df <- df %>%
  filter(!(site_year %in% selected_categories) & Type == "Satellite")

training_df_rep <-df_rep %>%
  filter(site_year %in% selected_categories | Type == "Common_Garden")

testing_df_rep <- df_rep %>%
  filter(!(site_year %in% selected_categories) & Type == "Satellite")

training_df_emg <-df_emg %>%
  filter(site_year %in% selected_categories | Type == "Common_Garden")

testing_df_emg <- df_emg %>%
  filter(!(site_year %in% selected_categories) & Type == "Satellite")

### compare data to make sure we have decent coverage of climate 

training_df$Dataset <- "Training"
testing_df$Dataset <- "Testing"

training_df_rep$Dataset <- "Training"
testing_df_rep$Dataset <- "Testing"

training_df_emg$Dataset <- "Training"
testing_df_emg$Dataset <- "Testing"

# Combine the datasets
combined_data <- rbind(training_df, testing_df)
combined_data_rep <- rbind(training_df_rep, testing_df_rep)
combined_data_emg <- rbind(training_df_emg, testing_df_emg)
# Check overlap
ggplot(combined_data, aes(x = tmean.Sum, fill = Dataset)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "red"))

ggplot(combined_data_rep, aes(x = tmean.Sum, fill = Dataset)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "red"))

ggplot(combined_data_emg, aes(x = tmean.Sum, fill = Dataset)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "red"))

### Extract fecundity##### 
y <-
  training_df %>%
  pluck("Fecundity") %>%
  #log() %>%
  c()

r <-
  training_df %>%
  pluck("Reproduced") %>%
  c()

e <-
  training_df %>%
  pluck("Emerged") %>%
  c()


##### Indices ########
training_df$plot_index <- ifelse(training_df$Type == "Common_Garden", training_df$plot[training_df$Type == "Common_Garden"], 0)
plot_levels <- levels(factor(training_df$plot[training_df$Type == "Common_Garden"]))

training_df_rep$plot_index <- ifelse(training_df_rep$Type == "Common_Garden", training_df_rep$plot[training_df_rep$Type == "Common_Garden"], 0)
plot_levels <- levels(factor(training_df_rep$plot[training_df_rep$Type == "Common_Garden"]))

training_df_emg$plot_index <- ifelse(training_df_emg$Type == "Common_Garden", training_df_emg$plot[training_df_emg$Type == "Common_Garden"], 0)
plot_levels <- levels(factor(training_df_emg$plot[training_df_emg$Type == "Common_Garden"]))

training_df$site_year <- factor(training_df$site_year)
testing_df$site_year <- factor(testing_df$site_year)

training_df_rep$site_year <- factor(training_df_rep$site_year)
testing_df_rep$site_year <- factor(testing_df_rep$site_year)

training_df_emg$site_year <- factor(training_df_emg$site_year)
testing_df_emg$site_year <- factor(testing_df_emg$site_year)

# Create index for training site-years 
training_site_years <- sort(unique(training_df$site_year))
site_year_index_train <- data.frame(
  site_year = training_site_years,
  idx = seq_along(training_site_years)  
)

training_site_years_rep <- sort(unique(training_df_rep$site_year))
site_year_index_train_rep <- data.frame(
  site_year = training_site_years_rep,
  idx = seq_along(training_site_years_rep)  
)

training_site_years_emg <- sort(unique(training_df_emg$site_year))
site_year_index_train_emg <- data.frame(
  site_year = training_site_years_emg,
  idx = seq_along(training_site_years_emg)  
)

# Create index for testing site-years 
testing_site_years <- sort(unique(testing_df$site_year))
site_year_index_test <- data.frame(
  site_year = testing_site_years,
  idx = seq_along(testing_site_years) + length(training_site_years)  # Start from 40
)

testing_site_years_rep <- sort(unique(testing_df_rep$site_year))
site_year_index_test_rep <- data.frame(
  site_year = testing_site_years_rep,
  idx = seq_along(testing_site_years_rep) + length(training_site_years_rep)  # Start from 40
)

testing_site_years_emg <- sort(unique(testing_df_emg$site_year))
site_year_index_test_emg <- data.frame(
  site_year = testing_site_years_emg,
  idx = seq_along(testing_site_years_emg) + length(training_site_years_emg)  # Start from 40
)

# Merge site-year indices into the original dataframes
training_df <- left_join(training_df, site_year_index_train, by = "site_year")
testing_df <- left_join(testing_df, site_year_index_test, by = "site_year")

training_df_rep <- left_join(training_df_rep, site_year_index_train_rep, by = "site_year")
testing_df_rep <- left_join(testing_df_rep, site_year_index_test_rep, by = "site_year")

training_df_emg <- left_join(training_df_emg, site_year_index_train_emg, by = "site_year")
testing_df_emg <- left_join(testing_df_emg, site_year_index_test_emg, by = "site_year")

#### site level index for soil PCA
training_df$site <- factor(training_df$site)
testing_df$site <- factor(testing_df$site)

training_df_rep$site <- factor(training_df_rep$site)
testing_df_rep$site <- factor(testing_df_rep$site)

training_df_emg$site <- factor(training_df_emg$site)
testing_df_emg$site <- factor(testing_df_emg$site)

## fecundity 
all_sites <- sort(unique(c(training_df$site, testing_df$site)))

site_index <- data.frame(
  site = all_sites,
  idx_site = seq_along(all_sites)
)

training_df <- training_df %>% left_join(site_index, by = "site")
testing_df  <- testing_df %>% left_join(site_index, by = "site")

### reproduced
all_sites_rep <- sort(unique(c(training_df_rep$site, testing_df_rep$site)))

site_index_rep <- data.frame(
  site = all_sites_rep,
  idx_site = seq_along(all_sites_rep)
)

training_df_rep <- training_df_rep %>% left_join(site_index_rep, by = "site")
testing_df_rep  <- testing_df_rep %>% left_join(site_index_rep, by = "site")

### Emerged
all_sites_emg <- sort(unique(c(training_df_emg$site, testing_df_emg$site)))

site_index_emg <- data.frame(
  site = all_sites_emg,
  idx_site = seq_along(all_sites_emg)
)

training_df_emg <- training_df_emg %>% left_join(site_index_emg, by = "site")
testing_df_emg  <- testing_df_emg %>% left_join(site_index_emg, by = "site")

### genotype indices
 
training_df$NewSiteCode <- as.character(training_df$NewSiteCode)
training_df$NewSiteCode[is.na(training_df$NewSiteCode)] <- "Unknown"
training_df$NewSiteCode <- as.factor(training_df$NewSiteCode) 

training_df_rep$NewSiteCode <- as.character(training_df_rep$NewSiteCode)
training_df_rep$NewSiteCode[is.na(training_df_rep$NewSiteCode)] <- "Unknown"
training_df_rep$NewSiteCode <- as.factor(training_df_rep$NewSiteCode)

training_df_emg$NewSiteCode <- as.character(training_df_emg$NewSiteCode)
training_df_emg$NewSiteCode[is.na(training_df_emg$NewSiteCode)] <- "Unknown"
training_df_emg$NewSiteCode <- as.factor(training_df_emg$NewSiteCode)

valid_genotypes <- rownames(K_common_garden)
genotype_lookup <- setNames(seq_along(valid_genotypes), valid_genotypes)

# Filter df to only rows with genotypes in K
training_df <- training_df %>% filter(genotype %in% valid_genotypes)

training_df_rep <- training_df_rep %>% filter(genotype %in% valid_genotypes)

training_df_emg <- training_df_emg %>% filter(genotype %in% valid_genotypes)

testing_df <- testing_df %>% filter(genotype %in% valid_genotypes)
testing_df_rep <- testing_df_rep %>% filter(genotype %in% valid_genotypes)

testing_df_emg <- testing_df_emg %>% filter(genotype %in% valid_genotypes)

genotype_plant_train <- as.integer(genotype_lookup[as.character(training_df$genotype)])

genotype_plant_test <- as.integer(genotype_lookup[as.character(testing_df$genotype)])

genotype_plant_train_rep <- as.integer(genotype_lookup[as.character(training_df_rep$genotype)])

genotype_plant_test_rep <- as.integer(genotype_lookup[as.character(testing_df_rep$genotype)])

genotype_plant_train_emg <- as.integer(genotype_lookup[as.character(training_df_emg$genotype)])

genotype_plant_test_emg <- as.integer(genotype_lookup[as.character(testing_df_emg$genotype)])


# Check again
range(genotype_plant_train)  # should be 1 to 93
length(genotype_plant_train)  

range(genotype_plant_train_rep)  # should be 1 to 93
length(genotype_plant_train_rep) 

range(genotype_plant_train_emg)  # should be 1 to 93
length(genotype_plant_train_emg) 

range(genotype_plant_test) 
length(genotype_plant_test) 

range(genotype_plant_test_rep) 
length(genotype_plant_test_rep) 

range(genotype_plant_test_emg) 
length(genotype_plant_test_emg) 

#### plant index
idx_plant_train <- as.numeric(training_df$site_year)
idx_plant_test  <- as.numeric(testing_df$site_year)

idx_plant_train_rep <- as.numeric(training_df_rep$site_year)
idx_plant_test_rep  <- as.numeric(testing_df_rep$site_year)

idx_plant_train_emg <- as.numeric(training_df_emg$site_year)
idx_plant_test_emg  <- as.numeric(testing_df_emg$site_year)

## for W soil
idx_plant_train_site <- as.numeric(training_df$site)
idx_plant_test_site  <- as.numeric(testing_df$site)

idx_plant_train_site_rep <- as.numeric(training_df_rep$site)
idx_plant_test_site_rep  <- as.numeric(testing_df_rep$site)

idx_plant_train_site_emg <- as.numeric(training_df_emg$site)
idx_plant_test_site_emg  <- as.numeric(testing_df_emg$site)

####### Fit stan model #########
stan_data <- list(
  # General inputs for the model
  n_X = nrow(X_SOS),  ##X for without SOS
  n_X_soil = n_X_soil,
  p_X = ncol(X_SOS),   
  s_X = ncol(X_soil),
  q_X = ncol(Lambda_SOS),      
  X = X_SOS,  
  X_soil = X_soil, 
  Lambda = Lambda_SOS,  ##Lambda for without SOS variables
  Lambda_soil = Lambda_soil,  
  n_g = length(unique(genotype_plant_train)),  
  K = K_common_garden,    
  n_plot = max(training_df$plot_index),
  n_site_year = length(unique(c(training_df$site_year, testing_df$site_year))),
  
  # Training data specifics
  n_train = nrow(training_df),
  y_train = y,
  idx_plant_train = idx_plant_train,
  idx_plant_train_site = idx_plant_train_site,
  genotype_plant_train = genotype_plant_train,
  neighbors_train = training_df$neighbors.s,
  annual_train = training_df$annual.s,
  perennial_train = training_df$perennial.s,
  shrub_train = training_df$shrub.s,
  plot_index_train = training_df$plot_index,
  n_site_year_train = length(unique(as.integer(as.factor(training_df$site_year)))),
  site_year_id_train = training_df$idx,
  
  # Testing data specifics
  n_test = nrow(testing_df),
  idx_plant_test = idx_plant_test,
  idx_plant_test_site = idx_plant_test_site,
  genotype_plant_test = genotype_plant_test,
  neighbors_test = testing_df$neighbors.s,
  annual_test = testing_df$annual.s,
  perennial_test = testing_df$perennial.s,
  shrub_test = testing_df$shrub.s,
  site_year_id_test = testing_df$idx,
  plot_index_test = rep(0, nrow(testing_df)),
  n_site_year_test = length(unique(as.integer(as.factor(testing_df$site_year))))
)


training_df_rep$r_train <- ifelse(training_df_rep$Reproduced == "Y", 1L, 0L)
testing_df_rep$r_test <- ifelse(testing_df_rep$Reproduced == "Y", 1L, 0L)

stan_data_reproduced <- list(
  # General inputs for the model
  n_X = nrow(X_rep_SOS), ## X_rep for without SOS 
  n_X_soil = nrow(X_soil_rep),
  p_X = ncol(X_rep_SOS),   
  s_X = ncol(X_soil_rep),
  q_X = ncol(Lambda_rep_SOS),      
  X = X_rep_SOS,    
  X_soil = X_soil_rep, 
  Lambda = Lambda_rep_SOS,  
  Lambda_soil = Lambda_soil_rep,  
  n_g = length(unique(genotype_plant_train_rep)),  
  K = K_common_garden,    
  n_plot = max(training_df_rep$plot_index),
  n_site_year = length(unique(c(training_df_rep$site_year, testing_df_rep$site_year))),
  
  # Training data specifics
  n_train = nrow(training_df_rep),
  r_train = training_df_rep$r_train,
  idx_plant_train = idx_plant_train_rep,
  idx_plant_train_site = idx_plant_train_site_rep,
  genotype_plant_train = genotype_plant_train_rep,
  neighbors_train = training_df_rep$neighbors.s,
  annual_train = training_df_rep$annual.s,
  perennial_train = training_df_rep$perennial.s,
  shrub_train = training_df_rep$shrub.s,
  plot_index_train = training_df_rep$plot_index,
  n_site_year_train = length(unique(as.integer(as.factor(training_df_rep$site_year)))),
  site_year_id_train = training_df_rep$idx,
  
  # Testing data specifics
  n_test = nrow(testing_df_rep),
  idx_plant_test = idx_plant_test_rep,
  idx_plant_test_site = idx_plant_test_site_rep,
  genotype_plant_test = genotype_plant_test_rep,
  neighbors_test = testing_df_rep$neighbors.s,
  annual_test = testing_df_rep$annual.s,
  perennial_test = testing_df_rep$perennial.s,
  shrub_test = testing_df_rep$shrub.s,
  site_year_id_test = testing_df_rep$idx,
  plot_index_test = rep(0, nrow(testing_df_rep)),
  n_site_year_test = length(unique(as.integer(as.factor(testing_df_rep$site_year))))
)

### emerged
training_df_emg$e_train <- ifelse(training_df_emg$Emerged == "Y", 1L, 0L)
testing_df_emg$e_test <- ifelse(testing_df_emg$Emerged == "Y", 1L, 0L)

stan_data_emerged_full <- list(
  # General inputs for the model
  n_X = nrow(X_emg_SOS),  
  n_X_soil = nrow(X_soil_emg),
  p_X = ncol(X_emg_SOS),   
  s_X = ncol(X_soil_emg),
  q_X = ncol(Lambda_emg_SOS),      
  X = X_emg_SOS,    
  X_soil = X_soil_emg, 
  Lambda = Lambda_emg_SOS,  
  Lambda_soil = Lambda_soil_emg,  
  n_g = length(unique(genotype_plant_train_emg)),  
  K = K_common_garden,    
  n_plot = max(training_df_emg$plot_index),
  n_site_year = length(unique(c(training_df_emg$site_year, testing_df_emg$site_year))),
  
  # Training data specifics
  n_train = nrow(training_df_emg),
  e_train = training_df_emg$e_train,
  idx_plant_train = idx_plant_train_emg,
  idx_plant_train_site = idx_plant_train_site_emg,
  genotype_plant_train = genotype_plant_train_emg,
  neighbors_train = training_df_emg$neighbors.s,
  annual_train = training_df_emg$annual.s,
  perennial_train = training_df_emg$perennial.s,
  shrub_train = training_df_emg$shrub.s,
  plot_index_train = training_df_emg$plot_index,
  n_site_year_train = length(unique(as.integer(as.factor(training_df_emg$site_year)))),
  site_year_id_train = training_df_emg$idx,
  
  # Testing data specifics
  n_test = nrow(testing_df_emg),
  idx_plant_test = idx_plant_test_emg,
  idx_plant_test_site = idx_plant_test_site_emg,
  genotype_plant_test = genotype_plant_test_emg,
  neighbors_test = testing_df_emg$neighbors.s,
  annual_test = testing_df_emg$annual.s,
  perennial_test = testing_df_emg$perennial.s,
  shrub_test = testing_df_emg$shrub.s,
  site_year_id_test = testing_df_emg$idx,
  plot_index_test = rep(0, nrow(testing_df_emg)),
  n_site_year_test = length(unique(as.integer(as.factor(testing_df_emg$site_year))))
)


#stan_data_emerged_fall <- list(
  # General inputs for the model
 # n_X = nrow(X_emg_fall),  
  #n_X_soil = nrow(X_soil_emg),
#  p_X = ncol(X_emg_fall),   
 # s_X = ncol(X_soil_emg),
  #q_X = ncol(Lambda_emg_fall),      
  #X = X_emg_fall,    
  #X_soil = X_soil_emg, 
  #Lambda = Lambda_emg_fall,  
  #Lambda_soil = Lambda_soil_emg,  
  #n_g = length(unique(genotype_plant_train_emg)),  
  #K = K_common_garden,    
  #n_plot = max(training_df_emg$plot_index),
  #n_site_year = length(unique(c(training_df_emg$site_year, testing_df_emg$site_year))),
  
  # Training data specifics
 # n_train = nrow(training_df_emg),
  #e_train = training_df_emg$e_train,
  #idx_plant_train = idx_plant_train_emg,
  #idx_plant_train_site = idx_plant_train_site_emg,
  #genotype_plant_train = genotype_plant_train_emg,
  #neighbors_train = training_df_emg$neighbors.s,
  #annual_train = training_df_emg$annual.s,
  #perennial_train = training_df_emg$perennial.s,
  #shrub_train = training_df_emg$shrub.s,
  #plot_index_train = training_df_emg$plot_index,
  #n_site_year_train = length(unique(as.integer(as.factor(training_df_emg$site_year)))),
  #site_year_id_train = training_df_emg$idx,
  
  # Testing data specifics
  #n_test = nrow(testing_df_emg),
  #idx_plant_test = idx_plant_test_emg,
  #idx_plant_test_site = idx_plant_test_site_emg,
  #genotype_plant_test = genotype_plant_test_emg,
  #neighbors_test = testing_df_emg$neighbors.s,
  #annual_test = testing_df_emg$annual.s,
  #perennial_test = testing_df_emg$perennial.s,
  #shrub_test = testing_df_emg$shrub.s,
  #site_year_id_test = testing_df_emg$idx,
  #plot_index_test = rep(0, nrow(testing_df_emg)),
  #n_site_year_test = length(unique(as.integer(as.factor(testing_df_emg$site_year))))
#)


# Fit using cmdrstan 
mod <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/ztnb_glm.random.predict.stan")
mod_rep <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_reproduced.stan")
mod_emg <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_survived.stan")

#### Find good starting values ####
### Fecundity 
pathfinder_fit <- mod$pathfinder(
  data = stan_data,          # your named list of data
  init = 0,                  # or a list of reasonable inits
  num_paths = 1              # usually equal to number of chains
)
init_list <- pathfinder_fit$draws(format = "list")

### Reproduced 
pathfinder_fit <- mod_rep$pathfinder(
  data = stan_data_reproduced,          # your named list of data
  init = 0,                  # or a list of reasonable inits
  num_paths = 1              # usually equal to number of chains
)
init_list <- pathfinder_fit$draws(format = "list")

### Emerged == full climate var
pathfinder_fit <- mod_emg$pathfinder(
  data = stan_data_emerged_full,          # your named list of data
  init = 0,                  # or a list of reasonable inits
  num_paths = 1              # usually equal to number of chains
)
init_list <- pathfinder_fit$draws(format = "list")


###### Emerged fall climate var only
pathfinder_fit <- mod_emg$pathfinder(
  data = stan_data_emerged_fall,          # your named list of data
  init = 0,                  # or a list of reasonable inits
  num_paths = 1              # usually equal to number of chains
)
init_list <- pathfinder_fit$draws(format = "list")


# Warmup and iterations 
iter_warmup = 100 
iter_sampling = 1000 ## run for 10,000 and look at overall diff in scores

# Compile and fit the model
### Fecundity 
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  init = init_list
)

        ### Reproduced
fit_rep <- mod_rep$sample(
  data = stan_data_reproduced,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  init = init_list
)

### Emerged Full Climate Vars
fit_emg_full <- mod_emg$sample(
  data = stan_data_emerged_full,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  init = init_list
)

##### Emerged Fall Climate Vars only
fit_emg_fall <- mod_emg$sample(
  data = stan_data_emerged_fall,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  init = init_list
)

## notes: Warning: 2 of 3 chains had an E-BFMI less than 0. on fall only emergence model but not one with full PCA.
#this means that two of the Stan chains are having trouble exploring the posterior efficiently due to an issue with energy-based sampling efficiency. Might be worthwhile to calculate BFMI directly based on past bugs with stan.

# Summary of results
summary <- fit$summary()
range(summary$rhat) ## Gelman Rubin statistic of convergence 

summary_rep <- fit_rep$summary()
range(summary_rep$rhat) #NAs

summary_emg_full <- fit_emg_full$summary()
range(summary_emg_full$rhat) 

# Extract posterior samples
posterior <- fit$draws(variables = c("theta","beta", "sigma", "W", "zeta", "mu_test", "mu_train", "W_soil"))

posterior_rep <- fit_rep$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))

posterior_emg_full <- fit_emg_full$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))

#posterior_emg_fall <- fit_emg_fall$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))

#
# Traceplots for diagnostics
#

# theta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("theta"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("theta"), n_warmup = iter_warmup)
p + facet_text(size = 15)


# beta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# gamma
#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior,  pars = paste0("gamma[", 1:6, "]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

# sigma
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = paste0("sigma[", 1:ncol(X_emg_fall), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# zeta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# W
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# W soil
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# mu
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu_train[191]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("mu_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)



color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

###### posterior means of random effects #######
library(posterior)
#posterior <- fit$draws(variables = c("theta","beta", "sigma", "W", "zeta", "mu_test", "mu_train", "W_soil"))
draws <- fit$draws()
draws_df <- as_draws_df(draws)



### Genotype random intercepts: scaled and centered
beta_0_raw <- draws_df %>%
  dplyr::select(starts_with("beta_0_raw"))

zeta_0 <- draws_df$zeta_0

# Scale
beta_0_scaled <- beta_0_raw * zeta_0

# Center across genotypes *within each draw*
beta_0_centered <- beta_0_scaled - rowMeans(beta_0_scaled)

# Summarize
beta_0_means <- beta_0_centered %>%
  summarise(across(everything(), list(
    mean = ~mean(.),
    q5 = ~quantile(., 0.05),
    q95 = ~quantile(., 0.95)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Genotype", "stat"),
    names_pattern = "(.*)_(mean|q5|q95)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

hist(beta_0_means$mean, main = "Distribution of Genotype Means",
     xlab = "Mean", breaks = 20)
mean(beta_0_means$mean)
range(beta_0_means$mean)
sum(abs(beta_0_means$mean) > 0.05)


### Site-year random effects: scaled and centered
site_year_raw <- draws_df %>%
  dplyr::select(starts_with("site_year_effect_train_raw"))

sigma_sy <- draws_df$sigma_site_year

site_year_scaled <- site_year_raw * sigma_sy
site_year_centered <- site_year_scaled - rowMeans(site_year_scaled)

site_year_means <- site_year_centered %>%
  summarise(across(everything(), list(
    mean = ~mean(.),
    q5 = ~quantile(., 0.05),
    q95 = ~quantile(., 0.95)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("SiteYear", "stat"),
    names_pattern = "(.*)_(mean|q5|q95)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

hist(site_year_means$mean, main = "Distribution of Site-Year Means",
     xlab = "Mean", breaks = 20)
mean(site_year_means$mean)
range(site_year_means$mean)
sum(abs(site_year_means$mean) > 0.05)


### Plot random effects: scaled and centered
eta_plot_raw <- draws_df %>%
  dplyr::select(starts_with("eta_plot_raw"))

sigma_plot <- draws_df$sigma_plot

eta_plot_scaled <- eta_plot_raw * sigma_plot
eta_plot_centered <- eta_plot_scaled - rowMeans(eta_plot_scaled)

eta_plot_means <- eta_plot_centered %>%
  summarise(across(everything(), list(
    mean = ~mean(.),
    q5 = ~quantile(., 0.05),
    q95 = ~quantile(., 0.95)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Plot", "stat"),
    names_pattern = "(.*)_(mean|q5|q95)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

hist(eta_plot_means$mean, main = "Distribution of Plot Means",
     xlab = "Mean", breaks = 20)
range(eta_plot_means$mean)
sum(abs(eta_plot_means$mean) > 0.05)

mean(eta_plot_means$mean)



## visualize random effect means

beta_0_means %>%
  ggplot(aes(x = reorder(Genotype, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_classic() +
  labs(x = "Genotype", y = "Genotype Intercept (mean ± 90% CI)") +
  coord_flip()

site_year_means %>%
  ggplot(aes(x = reorder(SiteYear, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_classic() +
  labs(x = "Site-Year", y = "Site-Year Effect (mean ± 90% CI)") +
  coord_flip()


eta_plot_means %>%
  ggplot(aes(x = reorder(Plot, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_classic() +
  labs(x = "Plot", y = "Plot Effect (mean ± 90% CI)") +
  coord_flip()

##### what we would expect from the prior
rnorm(1000, 0, 1) * sigma_sy  



### explore parameters ##############
library(posterior)
posterior_df <- as_draws_df(posterior)
names(posterior_df)

summary %>%
  filter(variable %in% c("beta_annual", "beta_neighbors", "beta_perennial", "beta_shrub")) %>%
  ggplot(aes(x = variable, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_minimal() +
  labs(y = "Estimate", title = " with 90% Credible Interval")




beta_summary <- posterior_df %>%
  dplyr::select(starts_with("beta")) %>%
  summarise(across(everything(), list(mean = ~mean(.), q5 = ~quantile(., 0.05), q95 = ~quantile(., 0.95))))


beta_long <- beta_summary %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(.*)")

ggplot(beta_long, aes(x = variable, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_minimal() +
  labs(y = "Estimate", title = "Parameter Estimates with 90% Credible Interval")



# Select all W coefficients
 
W_summary <- posterior_df %>%
  dplyr::select(matches("W\\[.*\\]")) %>%  # Match all W[i,j] values
  summarise(across(everything(), list(mean = ~mean(.), q5 = ~quantile(., 0.05), q95 = ~quantile(., 0.95))))

# Reshape data into long format for plotting
W_long <- W_summary %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(.*)")  # Separate variable name from summary stat


ggplot(W_long, aes(x = variable, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_minimal() +
  labs(y = "Estimate", title = "Parameter Estimates with 90% Credible Interval")

#Lambda_df <- data.frame(loading = unlist(Lambda))
#Lambda_df$climate_variable <- rownames(Lambda_df)
#Lambda_rep$climate_variable <- rownames(Lambda_rep)



# Reshape to long format
library(reshape2)
library(reshape2)
Lambda_long <- melt(Lambda, varnames = c("climate_variable", "PC"), value.name = "loading")

# Heatmap
ggplot(Lambda_long, aes(x = PC, y = climate_variable, fill = loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  labs(title = "PCA Loadings: Climate variables on W axes",
       x = "Principal Component",
       y = "Climate Variable")

# Bar plot of loadings
ggplot(Lambda_long, aes(x = reorder(climate_variable, loading), y = loading, fill = loading > 0)) +
  geom_col() +
  facet_wrap(~ PC, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Contribution of Climate Variables to Each PCA Axis",
    x = "Climate Variable",
    y = "Loading"
  ) +
  theme(legend.position = "none")










#PC1 loads negatively on most temperature variables and positively on precipitation/SWE: temperature–moisture tradeoff axis.

#PC2 loads heavily on Fall/Winter precipitation/SWE and prcp.Sum (negatively): a seasonal moisture pattern axis.

### soil
library(reshape2)
Lambda_soil_long <- melt(Lambda_soil, varnames = c("soil_variable", "PC"), value.name = "loading")


## heat map
ggplot(Lambda_soil_long, aes(x = PC, y = soil_variable, fill = loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  labs(title = "PCA Loadings: Soil variables on W axes",
       x = "Principal Component",
       y = "Soil Variable")

# Bar plot of loadings
ggplot(Lambda_soil_long, aes(x = reorder(soil_variable, loading), y = loading, fill = loading > 0)) +
  geom_col() +
  facet_wrap(~ PC, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Contribution of Soil Variables to Each PCA Axis",
    x = "Soil Variable",
    y = "Loading"
  ) +
  theme(legend.position = "none")

#### means
beta_draws <- posterior_df %>% 
  dplyr::select(starts_with("beta["))

beta_names <- colnames(beta_draws)
beta_idx <- stringr::str_match(beta_names, "beta\\[(\\d+),(\\d+)\\]") %>% as.data.frame()
colnames(beta_idx) <- c("full", "row", "col")
beta_idx$row <- as.integer(beta_idx$row)
beta_idx$col <- as.integer(beta_idx$col)

n_g <- max(beta_idx$row)
q_X_beta <- max(beta_idx$col)
stopifnot(q_X == q_X_beta)

beta_mean <- matrix(NA, nrow = n_g, ncol = q_X)
for (i in seq_along(beta_names)) {
  r <- beta_idx$row[i]
  c <- beta_idx$col[i]
  beta_mean[r, c] <- mean(beta_draws[[i]])
}


 
climate_effects <- Lambda %*% t(beta_mean)
soil_effects <- Lambda_soil %*% t(beta_mean)

library(reshape2)


heat_df <- melt(climate_effects, varnames = c("Climate_Variable", "Genotype"), value.name = "Effect")

heat_df_soil <- melt(soil_effects, varnames = c("Soil_Variable", "Genotype"), value.name = "Effect")

ggplot(heat_df, aes(x = Genotype, y = Climate_Variable, fill = Effect)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Effect of Climate Variables on Fecundity by Genotype")

ggplot(heat_df_soil, aes(x = Genotype, y = Soil_Variable, fill = Effect)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Effect of Soil Variables on Fecundity by Genotype")

avg_effects <- rowMeans(climate_effects)
avg_effects_soil <- rowMeans(soil_effects)

ggplot(data.frame(Climate_Variable = names(avg_effects), Effect = avg_effects),
       aes(x = reorder(Climate_Variable, Effect), y = Effect)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Average Effect of Climate Variables on Fecundity",
       x = "Climate Variable", y = "Mean Effect")

ggplot(data.frame(Soil_Variable = names(avg_effects_soil), Effect = avg_effects_soil),
       aes(x = reorder(Soil_Variable, Effect), y = Effect)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Average Effect of Soil Variables on Fecundity",
       x = "Soil", y = "Mean Effect")

### just beta
# Melt the matrix
beta_df <- melt(beta_mean)
colnames(beta_df) <- c("Genotype", "LatentFactor", "Effect")

# Plot
ggplot(beta_df, aes(x = factor(LatentFactor), y = factor(Genotype), fill = Effect)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red") +
  labs(
    x = "Latent Factor",
    y = "Genotype",
    fill = "Beta Effect",
    title = "Heatmap of Beta Coefficients"
  ) +
  theme_minimal(base_size = 14)

##### Evaluate predictions Emergence model: Fall vs Full ######

# ===== FALL MODEL ===== #

# --- Test predictions ---
p_test_fall_post <- fit_emg_fall$draws("p_test", format = "draws_matrix")
p_test_fall_mean <- apply(p_test_fall_post, 2, mean)

# --- Train predictions ---
p_train_fall_post <- fit_emg_fall$draws("p_train", format = "draws_matrix")
p_train_fall_mean <- apply(p_train_fall_post, 2, mean)

# --- Train predictions: fixed effects only ---
mu_train_fixed_fall_post <- fit_emg_fall$draws("mu_train_fixed", format = "draws_matrix")
mu_train_fixed_fall_mean <- apply(mu_train_fixed_fall_post, 2, mean)

rmse <- function(pred, obs) sqrt(mean((pred - obs)^2))
mae <- function(pred, obs) mean(abs(pred - obs))
rsq <- function(pred, obs) cor(pred, obs)^2

# Observed responses
e_train <- training_df_emg$e_train
e_test <- testing_df_emg$e_test

# ==== FALL MODEL ==== #

# Train
rmse(p_train_fall_mean, e_train) #0.3782619
mae(p_train_fall_mean, e_train) #0.2935926
rsq(p_train_fall_mean, e_train) #0.1813819

# Train (fixed only)
rmse(mu_train_fixed_fall_mean, e_train) #0.429194
mae(mu_train_fixed_fall_mean, e_train) #0.4078865
rsq(mu_train_fixed_fall_mean, e_train) # 0.03277769

# Test
rmse(p_test_fall_mean, e_test) #0.5180458
mae(p_test_fall_mean, e_test) #0.5070795
rsq(p_test_fall_mean, e_test) #0.002558524

# ==== FULL MODEL ==== #

# Train
rmse(p_train_full_mean, e_train) #0.3777849
mae(p_train_full_mean, e_train) #0.2908186
rsq(p_train_full_mean, e_train) #0.1828344

# Train (fixed only)
rmse(mu_train_fixed_full_mean, e_train) #0.4239589
mae(mu_train_fixed_full_mean, e_train) #0.3952211
rsq(mu_train_fixed_full_mean, e_train) #0.03537912

# Test
rmse(p_test_full_mean, e_test) #0.5081168
mae(p_test_full_mean, e_test) #0.492082
rsq(p_test_full_mean, e_test) #0.06159678

# Set up layout
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

# ---- Row 1: Train Predictions ----

# Fall - Train
plot(p_train_fall_mean, jitter(e_train), pch = 16, col = "darkorange",
     xlab = "Predicted", ylab = "Observed",
     main = "Train (Fall)")
abline(h = 0:1, col = "gray", lty = 2)

# Full - Train
plot(p_train_full_mean, jitter(e_train), pch = 16, col = "darkred",
     xlab = "Predicted", ylab = "Observed",
     main = "Train (Full)")
abline(h = 0:1, col = "gray", lty = 2)

# ---- Row 2: Train (Fixed Effects Only) ----

# Fall - Fixed
plot(mu_train_fixed_fall_mean, jitter(e_train), pch = 16, col = "orange",
     xlab = "Predicted (Fixed Only)", ylab = "Observed",
     main = "Train Fixed (Fall)")
abline(h = 0:1, col = "gray", lty = 2)

# Full - Fixed
plot(mu_train_fixed_full_mean, jitter(e_train), pch = 16, col = "firebrick",
     xlab = "Predicted (Fixed Only)", ylab = "Observed",
     main = "Train Fixed (Full)")
abline(h = 0:1, col = "gray", lty = 2)

# ---- Row 3: Test Predictions ----

# Fall - Test
plot(p_test_fall_mean, jitter(e_test), pch = 16, col = "dodgerblue",
     xlab = "Predicted", ylab = "Observed",
     main = "Test (Fall)")
abline(h = 0:1, col = "gray", lty = 2)

# Full - Test
plot(p_test_full_mean, jitter(e_test), pch = 16, col = "navy",
     xlab = "Predicted", ylab = "Observed",
     main = "Test (Full)")
abline(h = 0:1, col = "gray", lty = 2)

# Reset layout
par(mfrow = c(1, 1))

### colored by site-year


if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
library(viridis)

# Combine all site-year factors and assign consistent colors
all_site_years <- unique(c(training_df_emg$site_year, testing_df_emg$site_year))
site_year_colors <- setNames(viridis(length(all_site_years)), all_site_years)

# Helper function to get color vector
get_colors <- function(site_year_vec) {
  site_year_colors[as.character(site_year_vec)]
}

# Set up layout
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

# ---- Row 1: Train Predictions ----

# Fall - Train
plot(p_train_fall_mean, jitter(e_train), pch = 16,
     col = get_colors(training_df_emg$site_year),
     xlab = "Predicted", ylab = "Observed",
     main = "Train (Fall)")
abline(h = 0:1, col = "gray", lty = 2)

# Full - Train
plot(p_train_full_mean, jitter(e_train), pch = 16,
     col = get_colors(training_df_emg$site_year),
     xlab = "Predicted", ylab = "Observed",
     main = "Train (Full)")
abline(h = 0:1, col = "gray", lty = 2)

# ---- Row 2: Train (Fixed Effects Only) ----

# Fall - Fixed
plot(mu_train_fixed_fall_mean, jitter(e_train), pch = 16,
     col = get_colors(training_df_emg$site_year),
     xlab = "Predicted (Fixed Only)", ylab = "Observed",
     main = "Train Fixed (Fall)")
abline(h = 0:1, col = "gray", lty = 2)

# Full - Fixed
plot(mu_train_fixed_full_mean, jitter(e_train), pch = 16,
     col = get_colors(training_df_emg$site_year),
     xlab = "Predicted (Fixed Only)", ylab = "Observed",
     main = "Train Fixed (Full)")
abline(h = 0:1, col = "gray", lty = 2)

# ---- Row 3: Test Predictions ----

# Fall - Test
plot(p_test_fall_mean, jitter(e_test), pch = 16,
     col = get_colors(testing_df_emg$site_year),
     xlab = "Predicted", ylab = "Observed",
     main = "Test (Fall)")
abline(h = 0:1, col = "gray", lty = 2)

# Full - Test
plot(p_test_full_mean, jitter(e_test), pch = 16,
     col = get_colors(testing_df_emg$site_year),
     xlab = "Predicted", ylab = "Observed",
     main = "Test (Full)")
abline(h = 0:1, col = "gray", lty = 2)

# Reset layout
par(mfrow = c(1, 1))

###### Evaluate Prediction ######
## test 
mu_test_post  <- fit$draws("mu_test", format = "draws_matrix")  

 mu_test_mean <- apply(log(mu_test_post), 2, mean) 

p_test_post  <- fit_rep$draws("p_test", format = "draws_matrix") 
p_test_mean <- apply(log(p_test_post), 2, mean)


## train
mu_train_post  <- fit$draws("mu_train", format = "draws_matrix")
mu_train_mean <- apply(log(mu_train_post), 2, mean) 
p_train_post  <- fit_rep$draws("p_train", format = "draws_matrix")
p_train_mean <- apply(log(p_train_post), 2, mean)


mu_test_lower <- apply(mu_test_post, 2, quantile, probs = 0.025)
mu_test_upper <- apply(mu_test_post, 2, quantile, probs = 0.975)

mu_train_lower <- apply(mu_train_post, 2, quantile, probs = 0.025)
mu_train_upper <- apply(mu_train_post, 2, quantile, probs = 0.975)

plot(mu_test_mean, log(testing_df$Fecundity), main = "Test: Predicted vs Observed",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")

plot(log(mu_train_mean), log(training_df$Fecundity), main = "Test: Predicted vs Observed",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")

plot(log(mu_train_fixed_mean), log(training_df$Fecundity), main = "Test: Predicted vs Observed",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")


plot(p_test_mean, jitter(testing_df_rep$r_test), 
     xlab = "Predicted probability", ylab = "Observed (0 or 1)", 
     main = "Predicted vs Observed (Binomial)", pch = 16, col = "blue")
abline(h = 0:1, lty = 2, col = "gray")

rmse <- function(pred, obs) sqrt(mean((pred - obs)^2)) #root-mean squared error
mae <- function(pred, obs) mean(abs(pred - obs)) #mean absolute error
rsq <- function(pred, obs) cor(pred, obs)^2 #R2


rmse(mu_test_mean, testing_df$Fecundity) #1970.286
rmse(mu_train_mean, training_df$Fecundity) #422.0078
##upping mcmc sampling slightly reduces error

rmse(p_test_mean, testing_df_rep$r_test)
rmse(p_train_mean, training_df_rep$r_train)
rmse(p_train_fixed_mean, training_df_rep$r_train)

mae(mu_test_mean, testing_df$Fecundity) #484.9957
mae(mu_train_mean, training_df$Fecundity) #122.6011
mae(mu_train_fixed_mean, training_df$Fecundity) #158.4605

mae(p_test_mean, testing_df_rep$r_test)
mae(p_train_mean, training_df_rep$r_train)
mae(p_train_fixed_mean, training_df_rep$r_train)

rsq(mu_test_mean, testing_df$Fecundity) 
rsq(mu_train_mean, training_df$Fecundity) 
rsq(mu_train_fixed_mean, training_df$Fecundity) 

rsq(p_test_mean, testing_df_rep$r_test)
rsq(p_train_mean, training_df_rep$r_train)
rsq(p_train_fixed_mean, training_df_rep$r_train)

testing_df$mu_pred <- mu_test_mean
training_df$mu_pred <- mu_train_mean


testing_df_rep$mu_pred <- p_test_mean
training_df_rep$mu_pred <- p_train_mean
training_df_rep$mu_fixed_pred <- p_train_fixed_mean

ggplot(testing_df, aes(x = log(mu_pred), y = log(Fecundity), color = site_year)) +
  geom_point(alpha = 0.6) +  
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Test: Predicted vs Observed Fecundity",
    x = "Predicted Fecundity",
    y = "Observed Fecundity",
    color = "Site-Year"
  ) +
  theme_minimal()

ggplot(training_df, aes(x = log(mu_pred), y = log(Fecundity), color = Type)) +
  geom_point(alpha = 0.6) +  
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Train: Predicted vs Observed Fecundity",
    x = "Predicted Fecundity",
    y = "Observed Fecundity",
    color = "Site-Year"
  ) +
  theme_minimal()

ggplot(testing_df, aes(x = log(mu_pred), y = log(Fecundity), color = genotype)) +
  geom_point(alpha = 0.6) +  
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Test: Predicted vs Observed Fecundity",
    x = "Predicted Fecundity",
    y = "Observed Fecundity",
    color = "Genotype"
  ) +
  theme_minimal()

### without random effects

ggplot(training_df, aes(x = log(mu_fixed_pred), y = log(Fecundity), color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Train: Predicted vs Observed (Fixed Effects Only)",
    x = "Predicted Fecundity (no random effects)",
    y = "Observed Fecundity",
    color = "Site-Year"
  ) +
  theme_minimal()


ggplot(testing_df_rep, aes(x = mu_pred, y = r_test, color = genotype)) +
  geom_point(alpha = 0.6) +  
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Test: Predicted vs Observed Fecundity",
    x = "Predicted Fecundity",
    y = "Observed Fecundity",
    color = "Genotype"
  ) +
  theme_minimal()

ggplot(testing_df_rep, aes(x = mu_pred, y = r_test, color = site_year)) +
  geom_point(alpha = 0.6) +  
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Test: Predicted vs Observed Fecundity",
    x = "Predicted Fecundity",
    y = "Observed Fecundity",
    color = "Site-year"
  ) +
  theme_minimal()

site_years <- as.factor(testing_df_rep$site_year)
colors <- rainbow(length(levels(site_years)))
plot(p_test_mean, jitter(testing_df_rep$r_test), 
     xlab = "Predicted probability", ylab = "Observed (0 or 1)",
     main = "Test", 
     pch = 16, col = colors[site_years])
abline(h = 0:1, lty = 1, col = "black")
#legend("topright", legend = levels(site_years), 
       #col = colors, pch = 16, cex = 0.6, ncol = 2)

site_years <- as.factor(training_df_rep$site_year)
colors <- rainbow(length(levels(site_years)))
plot(p_train_mean, jitter(training_df_rep$r_train), 
     xlab = "Predicted probability", ylab = "Observed (0 or 1)",
     main = "Train", 
     pch = 16, col = colors[site_years])
abline(h = 0:1, lty = 1, col = "black")

site_years <- as.factor(training_df_rep$site_year)
colors <- rainbow(length(levels(site_years)))
plot(p_train_fixed_mean, jitter(training_df_rep$r_train), 
     xlab = "Predicted probability", ylab = "Observed (0 or 1)",
     main = "Train Fixed only", 
     pch = 16, col = colors[site_years])
abline(h = 0:1, lty = 1, col = "black")


###### Fecundity comparison figure ########
library(ggplot2)
library(patchwork)  

# Panel A-B: Training - mu_pred
p1 <- ggplot(training_df, aes(x = mu_pred, y = log(Fecundity), color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Train: mu_pred", x = "Predicted (log)", y = "Observed (log)") +
  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))


p2 <- ggplot(training_df, aes(x = mu_pred, y = log(Fecundity), color = site_year)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Train: mu_pred", x = "Predicted (log)", y = "Observed (log)") +
  theme_minimal() 

p3 <- ggplot(training_df, aes(x = mu_pred, y = log(Fecundity), color = seasonality)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Train: mu_pred", x = "Predicted (log)", y = "Observed (log)") +
  theme_minimal() 



# Panel D: Testing - mu_pred
p4 <- ggplot(testing_df, aes(x = mu_pred, y = log(Fecundity), color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +

  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))


p5 <- ggplot(testing_df, aes(x = mu_pred, y = log(Fecundity), color = site_year)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Test: mu_fixed_pred", x = "Predicted (log)", y = "Observed (log)") +
  theme_minimal() 

p6 <- ggplot(testing_df, aes(x = mu_pred, y = log(Fecundity), color = seasonality)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() 

 ggplot(testing_df, aes(x = mu_pred, y = log(Fecundity), color = as.factor(genotype))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + facet_wrap(~site_year)
 
 ggplot(training_df, aes(x = mu_pred, y = log(Fecundity), color = as.factor(genotype))) +
   geom_point(alpha = 0.6) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
   theme_minimal() + facet_wrap(~site_year)
 
 ggplot(testing_df, aes(x = mu_pred, y = log(Fecundity), color = seasonality)) +
   geom_point(alpha = 0.6) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
   theme_minimal() + facet_wrap(~site_year)
 
 ggplot(testing_df, aes(x = mu_pred, y = log(Fecundity), color = pH)) +
   geom_point(alpha = 0.6) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
   theme_minimal() + facet_wrap(~site_year)
 
 ggplot(testing_df, aes(x = mu_pred, y = log(Fecundity), color = genotype)) +
   geom_point(alpha = 0.6) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
   theme_minimal() + facet_wrap(~genotype)
 
 ggplot(training_df, aes(x = mu_pred, y = log(Fecundity), color = site_year)) +
   geom_point(alpha = 0.6) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
   theme_minimal() + facet_wrap(~genotype)


# Combine plots into a 2x3 layout
(p1 | p2 | p3) / (p4 | p5 | plot_spacer())

(p1 | p4 )
(p2 | p5 )
(p3 | p6 )

ggsave("Predicted_vs_Observed_Fecundity.png",
       plot = (p1 | p2 | p3) / (p4 | p5 | plot_spacer()),
       dpi = 300,
       width = 12, height = 8, units = "in", bg = "white")

library(pROC)
roc_obj <- roc(testing_df_rep$r_test, p_test_mean)
plot(roc_obj, main = paste("Test ROC Curve, AUC =", round(auc(roc_obj), 3)))

roc_obj <- roc(training_df_rep$r_train, p_train_mean)
plot(roc_obj, main = paste("Train ROC Curve, AUC =", round(auc(roc_obj), 3)))

roc_obj <- roc(training_df_rep$r_train, p_train_fixed_mean)
plot(roc_obj, main = paste("Train Fixed only ROC Curve, AUC =", round(auc(roc_obj), 3)))

###### CRPS with scoring rules package #######
library(posterior)
library(scoringRules)

### Fecundity 
draws_y <- fit$draws(format = "df")
y_train_pred <- draws_y[, grep("^y_train_pred\\[", names(draws_y))]
y_train_fixed_pred <- draws_y[, grep("^y_train_fixed_pred\\[", names(draws_y))]
y_test_pred <- draws_y[, grep("^y_test_pred\\[", names(draws_y))]

# Reproduction
draws_r <- fit_rep$draws(format = "df")
r_train_pred <- draws_r[, grep("^r_train_pred\\[", names(draws_r))]
r_train_fixed_pred <- draws_r[, grep("^r_train_pred_fixed\\[", names(draws_r))]
r_test_pred <- draws_r[, grep("^r_test_pred\\[", names(draws_r))]

### Fall emerged
draws_e_fall <- fit_emg_fall$draws(format = "df")
e_train_pred_fall <- draws_e_fall[, grep("^e_train_pred\\[", names(draws_e_fall))]
e_train_fixed_pred_fall <- draws_e_fall[, grep("^e_train_pred_fixed\\[", names(draws_e_fall))]
e_test_pred_fall <- draws_e_fall[, grep("^e_test_pred\\[", names(draws_e_fall))]

# Full climate emerged
draws_e_full <- fit_emg_full$draws(format = "df")
e_train_pred_full <- draws_e_full[, grep("^e_train_pred\\[", names(draws_e_full))]
e_train_fixed_pred_full <- draws_e_full[, grep("^e_train_pred_fixed\\[", names(draws_e_full))]
e_test_pred_full <- draws_e_full[, grep("^e_test_pred\\[", names(draws_e_full))]

# Rhat -- cutoff > 1.05 indicates convergence issues

# Fecundity Rhat values
rhat_y_train <- rhat(as.matrix(y_train_pred))
rhat_y_train_fixed <- rhat(as.matrix(y_train_fixed_pred))
rhat_y_test <- rhat(as.matrix(y_test_pred))
## rhat above 1.05 with SOS model for train only

# Reproduction Rhat values
rhat_r_train <- rhat(as.matrix(r_train_pred))
rhat_r_train_fixed <- rhat(as.matrix(r_train_fixed_pred))
rhat_r_test <- rhat(as.matrix(r_test_pred))

# Emerged Fall Rhat values
rhat_e_fall_train <- rhat(as.matrix(e_train_pred_fall))
rhat_e_fall_train_fixed <- rhat(as.matrix(e_train_fixed_pred_fall))
rhat_e_fall_test <- rhat(as.matrix(e_test_pred_fall))

# Emerged Full Rhat values
rhat_e_full_train <- rhat(as.matrix(e_train_pred_full))
rhat_e_full_train_fixed <- rhat(as.matrix(e_train_fixed_pred_full))
rhat_e_full_test <- rhat(as.matrix(e_test_pred_full))




# Observed data 
y_train_obs <- training_df$Fecundity
y_test_obs <- testing_df$Fecundity
r_train_obs <- training_df_rep$r_train
r_test_obs <- testing_df_rep$r_test
e_train_obs <- training_df_rep$e_train
e_test_obs <- testing_df_rep$e_test

# ==== CRPS Computation Helper ====
get_crps <- function(obs, pred_df) {
  pred_t <- t(as.matrix(pred_df))
  crps_sample(y = obs, dat = pred_t)
}

# ==== CRPS Calculation ====

# Fecundity
crps_y <- list(
  train = get_crps(y_train_obs, y_train_pred),
  test = get_crps(y_test_obs, y_test_pred)
)

# Reproduction
crps_r <- list(
  train = get_crps(r_train_obs, r_train_pred),
  train_fixed = get_crps(r_train_obs, r_train_fixed_pred),
  test = get_crps(r_test_obs, r_test_pred)
)

# E (Fall)
crps_e_fall <- list(
  train = get_crps(training_df_emg$site_year, e_train_pred_fall),
  train_fixed = get_crps(training_df_emg$site_year, e_train_fixed_pred_fall),
  test = get_crps(testing_df_emg$site_year, e_test_pred_fall)
)

# E (Full)
crps_e_full <- list(
  train = get_crps(training_df_emg$site_year, e_train_pred_full),
  train_fixed = get_crps(training_df_emg$site_year, e_train_fixed_pred_full),
  test = get_crps(testing_df_emg$site_year, e_test_pred_full)
)

# ==== Mean CRPS Summary ====
mean_crps_summary <- list(
  y = sapply(crps_y, mean),
  r = sapply(crps_r, mean),
  e_fall = sapply(crps_e_fall, mean),
  e_full = sapply(crps_e_full, mean)
)

y = sapply(crps_y, mean)

# train train_fixed        test 
#84.95748   152.60931   197.37399 with SOS variables 

## original model without SOS: Root mean squared error of predicted vs observed is 2608.869 for test, 435.6277 for training with random effects, 504.5947 for training without random effects. R2 is 0.00003097785 for test, 0.4933718 for training data with random effects, 0.01291255 for training data without random effects. Mean absolute error is 944.5213 for test, 122.944 for training with random effects, and 161.2163 for training without random effects.
# ==== Optional Histogram ====
hist(crps_y$train, breaks = 30, main = "CRPS - y_train", col = "skyblue")
hist(crps_y$train_fixed, breaks = 30, main = "CRPS - y_train_fixed", col = "orange")
hist(crps_y$test, breaks = 30, main = "CRPS - y_test", col = "purple")

# ==== Single Posterior Predictive Check ====
hist(t(as.matrix(y_train_pred))[1, ],
     breaks = 30,
     main = "Posterior Predictive for 1st Training Point (Fecundity)",
     xlab = "Predicted Value",
     col = "skyblue", border = "white")


#keep at 10,000

####### climate W vs original PCA ######
W_draws <- posterior::as_draws_matrix(fit$draws("W"))
W_draws_rep <- posterior::as_draws_matrix(fit_rep$draws("W"))

W_draws <- W_draws[, grepl("^W\\[", colnames(W_draws))]
W_draws_rep <- W_draws_rep[, grepl("^W\\[", colnames(W_draws))]

param_names <- colnames(W_draws)
param_names_rep <- colnames(W_draws_rep)

param_info <- tibble(param = param_names) %>%
  mutate(i = as.integer(gsub("W\\[(\\d+),.*", "\\1", param)),
         j = as.integer(gsub(".*,(\\d+)\\]", "\\1", param)))

param_info_rep <- tibble(param = param_names_rep) %>%
  mutate(i = as.integer(gsub("W\\[(\\d+),.*", "\\1", param)),
         j = as.integer(gsub(".*,(\\d+)\\]", "\\1", param)))

W_long <- bind_cols(
  param_info[rep(1:nrow(param_info), each = nrow(W_draws)), ],
  value = as.vector(W_draws)
)

W_long_rep <- bind_cols(
  param_info[rep(1:nrow(param_info_rep), each = nrow(W_draws_rep)), ],
  value = as.vector(W_draws_rep)
)

W_static <- X_SOS %*% Lambda_SOS 
W_static_df <- as.data.frame(W_static)
names(W_static_df) <- paste0("PC", 1:ncol(W_static_df))
W_static_df$index <- 1:nrow(W_static_df)

W_static_rep <- X_rep %*% Lambda_rep ## if reproduction  
W_static_df_rep <- as.data.frame(W_static_rep)
names(W_static_df_rep) <- paste0("PC", 1:ncol(W_static_df_rep))
W_static_df_rep$index <- 1:nrow(W_static_df_rep)

W_summary <- W_long %>%
  group_by(i, j) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )

W_summary_rep <- W_long_rep %>%
  group_by(i, j) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )

W_plot_df <- W_summary %>%
  left_join(W_static_df %>% pivot_longer(-index, names_to = "PC", values_to = "static_value") %>%
              mutate(j = as.integer(gsub("PC", "", PC))),
            by = c("i" = "index", "j"))

W_plot_df_rep <- W_summary_rep %>%
  left_join(W_static_df_rep %>% pivot_longer(-index, names_to = "PC", values_to = "static_value") %>%
              mutate(j = as.integer(gsub("PC", "", PC))),
            by = c("i" = "index", "j"))

ggplot(W_plot_df, aes(x = i)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue") +
  geom_point(aes(y = static_value), color = "red", shape = 1, size = 1.5) +
  facet_wrap(~ j, scales = "free_y", labeller = label_both) +
  labs(
    x = "Climate index (i)",
    y = "Latent climate (W) vs PCA projection",
    title = "Posterior W vs. Original PCA Projection"
  ) +
  theme_minimal()

ggplot(W_plot_df_rep, aes(x = i)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue") +
  geom_point(aes(y = static_value), color = "red", shape = 1, size = 1.5) +
  facet_wrap(~ j, scales = "free_y", labeller = label_both) +
  labs(
    x = "Climate index (i)",
    y = "Latent climate (W) vs PCA projection",
    title = "Posterior W vs. Original PCA Projection"
  ) +
  theme_minimal()

W_plot_df <- W_plot_df %>%
  mutate(adjusted = static_value < lower | static_value > upper)

W_plot_df_rep <- W_plot_df_rep %>%
  mutate(adjusted = static_value < lower | static_value > upper)

# which combinations are adjusted and how many
### Fecundity 
W_adjusted <- W_plot_df %>%
  filter(adjusted == TRUE)  %>%  mutate(site_year = site_year_labels[i])
unique(W_adjusted$site_year)

W_plot_df %>% group_by(j) %>% summarise(n_adjusted = sum(adjusted))

#23 adjusted in int 1, 12 in dim 2 (27 site-years in total)

# "CaseAoyamaS1 2023"                 
# "CastValley 2021"                   
 #"dino 2024"                         
# "EnsingS1 SuRDC 2022"               
# "EnsingS2 Summerland-Princeton 2022"
# "EnsingS4 Lundbom 2022"             
 #"GreenCanyon 2023"                  
#"HardwareRanch 2023"                
 #"Peavine 2024"                      
 #"Plymouth 2024"                     
 #"RedBluff 2023"                     
 #"SSHigh 2022"                       
#"SSHigh 2023"                       
 #"SSHQ 2024"                         
#"Symstad2 2022"                     
# "Woodruff 2023"   

### Reproduced
W_adjusted_rep <- W_plot_df_rep %>%
  filter(adjusted == TRUE)  %>%  mutate(site_year = site_year_labels[i])
unique(W_adjusted_rep$site_year)

## 41 site-years adjusted 

W_plot_df_rep %>% group_by(j) %>% summarise(n_adjusted = sum(adjusted))

#### Which ones overlap?

site_years_rep <- unique(W_adjusted_rep$site_year)
site_years <- unique(W_adjusted$site_year)

# overlap
overlap <- intersect(site_years_rep, site_years)
#[1] "CaseAoyamaS1 2023"     "CastValley 2021"      
#[3] "dino 2024"             "EnsingS1 SuRDC 2022"  
#[5] "EnsingS4 Lundbom 2022" "HardwareRanch 2023"   
#[7] "Peavine 2024"          "Plymouth 2024"        
#[9] "SSHigh 2023"           "SSHQ 2024"            
#[11] "Symstad2 2022"   
#  differences
only_in_rep <- setdiff(site_years_rep, site_years) #30
only_in_fec <- setdiff(site_years, site_years_rep) #5




###### soil W vs original PCA #####

W_draws <- posterior::as_draws_matrix(fit$draws("W_soil"))

W_draws <- W_draws[, grepl("^W_soil\\[", colnames(W_draws))]

param_names <- colnames(W_draws)

param_info <- tibble(param = param_names) %>%
  mutate(i = as.integer(gsub("W_soil\\[(\\d+),.*", "\\1", param)),
         j = as.integer(gsub(".*,(\\d+)\\]", "\\1", param)))

W_long <- bind_cols(
  param_info[rep(1:nrow(param_info), each = nrow(W_draws)), ],
  value = as.vector(W_draws)
)

W_static <- X_soil %*% Lambda_soil  
W_static <- X_soil_rep %*% Lambda_soil_rep
W_static_df <- as.data.frame(W_static)
names(W_static_df) <- paste0("PC", 1:ncol(W_static_df))
W_static_df$index <- 1:nrow(W_static_df)

W_summary <- W_long %>%
  group_by(i, j) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )

W_plot_df <- W_summary %>%
  left_join(W_static_df %>% pivot_longer(-index, names_to = "PC", values_to = "static_value") %>%
              mutate(j = as.integer(gsub("PC", "", PC))),
            by = c("i" = "index", "j"))

ggplot(W_plot_df, aes(x = i)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue") +
  geom_point(aes(y = static_value), color = "red", shape = 1, size = 1.5) +
  facet_wrap(~ j, scales = "free_y", labeller = label_both) +
  labs(
    x = "Soil index (i)",
    y = "Latent soil (W_soil) vs PCA projection",
    title = "Posterior W vs. Original PCA Projection"
  ) +
  theme_minimal()


######## W and climate ########
cor_WX <- fit$draws(variables = c("cor_WX"))

cor_WX_draws <- posterior::as_draws_df(cor_WX, variables = "cor_WX")

cor_WX_long <- cor_WX_draws %>%
  pivot_longer(cols = starts_with("cor_WX["), names_to = "element", values_to = "cor") %>%
  mutate(
    element = gsub("cor_WX\\[|\\]", "", element),
    q = as.integer(sub(",.*", "", element)),
    p = as.integer(sub(".*,", "", element))
  )

cor_WX_means <- cor_WX_long %>%
  group_by(q, p) %>%
  summarize(mean_cor = mean(cor), .groups = "drop")

ggplot(cor_WX_means, aes(x = factor(p), y = factor(q), fill = mean_cor)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Climate Variable (X)", y = "Latent Dimension (W)", fill = "Mean Correlation") +
  theme_minimal()


ggplot(cor_WX_long, aes(x = factor(p), y = cor)) +
  geom_violin(fill = "skyblue", alpha = 0.6) +
  facet_wrap(~ q, labeller = label_both) +
  labs(x = "Climate Variable (X)", y = "Correlation with W", title = "Posterior Correlations: W vs. X") +
  theme_minimal()

##### back transforming with lambda #######
climate_effects <- fit$draws(variables = c("climate_effects"))

climate_effect_draws <- posterior::as_draws_df(climate_effects, variables = "climate_effects")

climate_effects_long <- climate_effect_draws %>%
  pivot_longer(cols = starts_with("climate_effects["), names_to = "param", values_to = "value") %>%
  mutate(
    param = gsub("climate_effects\\[|\\]", "", param),
    genotype = as.integer(sub(",.*", "", param)),
    climate_var = as.integer(sub(".*,", "", param))
  )

ggplot(climate_effects_long, aes(x = factor(climate_var), y = value)) +
  geom_violin(fill = "tomato", alpha = 0.6) +
  labs(x = "Climate Variable", y = "Effect Size", title = "Distribution of Climate Effects Across Genotypes") +
  theme_minimal()

climate_effects_mean <- climate_effects_long %>%
  group_by(genotype, climate_var) %>%
  summarize(mean_effect = mean(value), .groups = "drop")

ggplot(climate_effects_mean, aes(x = factor(climate_var), y = factor(genotype), fill = mean_effect)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  labs(x = "Climate Variable", y = "Genotype", fill = "Mean Effect") +
  theme_minimal()


######### Explore intraspecific density values ######

ggplot(df, aes(x = neighbors, y = Fecundity, color = Type)) +
  geom_point() +
  theme_minimal() 

df %>% filter(Type == "Satellite") %>% ggplot(aes(x = neighbors, y = Fecundity)) +
  geom_point() +
  theme_minimal()

df %>% filter(Type == "Common_Garden") %>% ggplot(aes(x = neighbors, y = Fecundity)) +
  geom_point() +
  theme_minimal() 

df %>% filter(Type == "Common_Garden") %>% select(neighbors) %>% distinct()

########## Predict Fitness ########################
## extract posterior predictive distributions 
fecundity_pred_train <- posterior::as_draws_matrix(fit$draws("y_train_pred"))  
fecundity_pred_test <- posterior::as_draws_matrix(fit$draws("y_test_pred"))  
fecundity_pred_train_fixed <- posterior::as_draws_matrix(fit$draws("y_train_fixed_pred")) 

rep_pred_train <- posterior::as_draws_matrix(fit_rep$draws("r_train_pred"))  
rep_pred_test <- posterior::as_draws_matrix(fit_rep$draws("r_test_pred"))  
rep_pred_train_fixed <- posterior::as_draws_matrix(fit_rep$draws("r_train_pred_fixed"))  


emg_pred_train <- posterior::as_draws_matrix(fit_emg_full$draws("e_train_pred"))  
emg_pred_test <- posterior::as_draws_matrix(fit_emg_full$draws("e_test_pred"))  
emg_pred_train_fixed <- posterior::as_draws_matrix(fit_emg_full$draws("e_train_pred_fixed"))  

##Error: vector memory limit of 16.0 Gb reached, see mem.maxVSize() problem 

fitness_pred_test <- y_test_pred_emergence * y_test_pred_reproduction * y_test_pred_fecundity
# dims: n_draws x n_test
