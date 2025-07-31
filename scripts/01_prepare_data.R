################# Bromecast: 01.Prepare Data ##########################
############# created 3-25-25 ######################
############# Last modified: 7-29-25 ##########################
######## Prepares all data for model fitting ################################

source("scripts/00_setup.R")

###### summarise soil data to site-year & assign soil values to common garden ########

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

###### add cg climate offset #########
## white gravel substract one, black gravel add one

data <- data %>%
  mutate(across(c(tmean.Fall, tmean.Sum, tmean.Spr, tmean.Win, MAT, tmin_center30d_mean,   tmax_center30d_mean,
                  tavg_center30d_mean,   tmin_center30d_min,
                  tmax_center30d_max), 
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

####### Random variable K ############

simulate_K_matrices <- function(data, assigned_genotypes, genotypes = 1:93, n_draws = 100, decay_rate = 10) {
  sites <- unique(data$site)
  site_types <- setNames(data$Type[match(sites, data$site)], sites)
  
  # distinguish by type
  common_garden_sites <- names(site_types[site_types == "Common_Garden"])
  satellite_sites <- names(site_types[site_types == "Satellite"])
  
  # Euclidean distances
  assigned_genotypes <- assigned_genotypes %>%
    mutate(site = as.character(site),
           distance = sqrt(Lat_Diff^2 + Lon_Diff^2))
  
  # Grid of all satellite sites × all genotypes
  full_grid <- expand.grid(site = satellite_sites, genotype = genotypes)
  
  # weight information about source gentoypes by distance
  prob_df <- full_grid %>%
    left_join(
      assigned_genotypes %>%
        dplyr::select(site, genotype, distance),
      by = c("site", "genotype")
    ) %>%
    # if missing distance values for a genotype-sat combo, this assigns a large distance for low probability
    mutate(distance = ifelse(is.na(distance), max(distance, na.rm = TRUE) * 2, distance)) %>%
    group_by(site) %>%
    mutate(weight = exp(-distance * decay_rate)) %>%
    mutate(prob = weight / sum(weight)) %>%
    ungroup()
  
  # make site-genotype relationships for K
  generate_K <- function() {
    K <- matrix(0, nrow = length(sites), ncol = length(genotypes),
                dimnames = list(sites, as.character(genotypes)))
    
    # Assign genotypes 1-93 to common garden sites (presence = 1)
    K[common_garden_sites, ] <- 1
    
    # For satellite sites, probablistic sampling
    for (s in satellite_sites) {
      this_df <- prob_df %>% filter(site == s)
      sampled_genotypes <- sample(this_df$genotype, size = length(genotypes), replace = TRUE, prob = this_df$prob)
      tab <- table(sampled_genotypes)
      K[s, names(tab)] <- as.numeric(tab > 0)  # binary presence = 1/absence =0
    }
    ### could switch to a threshold instead
    
    return(K)
  }
  
  # Generates n_draws of randomly sampled K matrices
  K_list <- replicate(n_draws, generate_K(), simplify = FALSE)
  return(K_list)
}

## create list of K matrices 
K_list <- simulate_K_matrices(data, assigned_genotypes, n_draws = 100, decay_rate = 10)

## add kinship information among genotypes to K-List:
site_kinship_list <- lapply(K_list, function(K) {
  K %*% K_common_garden %*% t(K)
})
## note to self to check:
#K_common_garden must be ordered consistently with the genotype columns of K_lists
#K matrices rows are sites, columns are exactly as the genotypes in K_common_garden.
## might be better to use  probabilistic weights instead of binary presence/absence

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

data %>%
  group_by(Type) %>%
  summarise(n_NA = sum(is.na(Reproduced)))

data %>%
  group_by(Type, year) %>%
  summarise(n_NA = sum(is.na(Fecundity)))

### filter censored data, seed drop and smut:  
data <- data %>% filter(Emerged != "missing") %>% filter(Reproduced != "missing") %>%  filter(!notesFlag %in% c("smut", "seeddrop")) %>% filter(!note_standard_harvest %in% c("smut", "seed_drop", "missing")) %>% filter(!note_standard_phen %in% c("resurrection", "smut", "missing", "seed_drop","smut_physical_damage", "smut_herbivory" )) %>% filter(is.na(fecundityflag) | fecundityflag == 0)

## make 2023 estimates count data 
data <- data %>%
  mutate(Fecundity = if_else(year == 2023 & Type == "Common_Garden",
                             pmax(round(Fecundity), 1),
                             Fecundity))

data$Fecundity <- as.integer(data$Fecundity)


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

### reassign NAs to zero in emerged
df_emg$Reproduced <- ifelse(df_emg$Emerged == "N", "N", df_emg$Reproduced)
df_emg$Fecundity <- ifelse(df_emg$Emerged == "N", 0L, df_emg$Fecundity)

df_emg$Fecundity <- ifelse(df_emg$Reproduced == "N", 0L, df_emg$Fecundity)


na_fecundity <- df_emg %>%
  filter(is.na(Fecundity))


table(df_emg$Emerged[is.na(df_emg$Fecundity)])

table(df_emg$Reproduced[is.na(df_emg$Fecundity)])


table(df_emg$Type[is.na(df_emg$Fecundity)])


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


soil_vars <- c(
  "pH", "EC", "OMpercent", "Protein_g.kg")

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

soil_data <- df %>% 
  dplyr::select(site, all_of(soil_vars))  %>% distinct() %>% 
  na.omit() 

soil_data_rep <- df_rep %>% 
  dplyr::select(site, all_of(soil_vars))  %>% distinct() %>% 
  na.omit() 

soil_data_emg <- df_emg %>% 
  dplyr::select(site, all_of(soil_vars))  %>% distinct() %>% 
  na.omit() 


site_year_labels <- pca_data$site_year  
site_labels_soil <- soil_data$site
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


X_rep <- scale(pca_data_rep %>% dplyr::select(-site_year))
X_rep_SOS <- scale(pca_data_rep_SOS %>% dplyr::select(-site_year))
X_soil_rep <- scale(soil_data_rep %>% dplyr::select(-site))

X_emg <- scale(pca_data_emg %>% dplyr::select(-site_year))
X_emg_SOS <- scale(pca_data_emg_SOS %>% dplyr::select(-site_year))
X_soil_emg <- scale(soil_data_emg %>% dplyr::select(-site))

pca_out <- prcomp(X)
pca_out_SOS <- prcomp(X_SOS)
pca_out_soil <- prcomp(X_soil)


pca_out_rep <- prcomp(X_rep)
pca_out_rep_SOS <- prcomp(X_rep_SOS)
pca_out_soil_rep <- prcomp(X_soil_rep)

pca_out_emg <- prcomp(X_emg)
pca_out_emg_SOS <- prcomp(X_emg_SOS)
pca_out_soil_emg <- prcomp(X_soil_emg)

n_X <- nrow(pca_data)
q_X <- 2
Lambda <- as.matrix(pca_out$rotation[, 1:q_X])
Lambda_SOS <- as.matrix(pca_out_SOS$rotation[, 1:q_X])

Lambda_rep <- as.matrix(pca_out_rep$rotation[, 1:q_X])
Lambda_rep_SOS <- as.matrix(pca_out_rep_SOS$rotation[, 1:q_X])

Lambda_emg <- as.matrix(pca_out_emg$rotation[, 1:q_X])
Lambda_emg_SOS <- as.matrix(pca_out_emg_SOS$rotation[, 1:q_X])


n_X_soil <- nrow(soil_data)
Lambda_soil <- as.matrix(pca_out_soil$rotation[, 1:q_X])
Lambda_soil_rep <- as.matrix(pca_out_soil_rep$rotation[, 1:q_X])
Lambda_soil_emg <- as.matrix(pca_out_soil_emg$rotation[, 1:q_X])



fviz_pca_biplot(pca_out,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()


fviz_cos2(pca_out, choice = "var", axes = 1:2)
fviz_contrib(pca_out_SOS, choice = "var", axes = 2, top = 10)

## elbow plot
explained_var <- pca_out$sdev^2
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

######## binary variables ###########
training_df_emg$r_train <- ifelse(training_df_emg$Reproduced == "Y", 1L, 0L)
testing_df_emg$r_test <- ifelse(testing_df_emg$Reproduced == "Y", 1L, 0L)

training_df_emg$e_train <- ifelse(training_df_emg$Emerged == "Y", 1L, 0L)
testing_df_emg$e_test <- ifelse(testing_df_emg$Emerged == "Y", 1L, 0L)

training_df_rep$r_train <- ifelse(training_df_rep$Reproduced == "Y", 1L, 0L)
testing_df_rep$r_test <- ifelse(testing_df_rep$Reproduced == "Y", 1L, 0L)