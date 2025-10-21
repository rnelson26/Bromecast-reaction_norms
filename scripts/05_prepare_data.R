################# Bromecast: 01.Prepare Data ##########################
############# created 3-25-25 ######################
############# Last modified: 10-21-25 ##########################
######## Prepares all data for model fitting ################################
##add source for soils and merge data 

source("scripts/03_landscape_genomics_GLMNET.R")
source("scripts/04_setup.R")


###### summarise soil data to site-year & assign soil values to common garden ########

soil_summary <- soil_clean %>% group_by(site_old) %>% summarise(across(c(pH, EC, OMpercent, Protein_g.kg, X..Sand, X..Clay, X..Silt), \(x) mean(x, na.rm = TRUE)))

data <- left_join(data, soil_summary, by = "site_old")


vars_to_fill <- c("pH", "EC", "OMpercent", "Protein_g.kg", "X..Sand", "X..Clay", "X..Silt")

## use Boise Low sat soil values for wildcat & baltzor cg
reference_values <- data %>%
  dplyr::filter(site_old == "Boise_Low") %>%
  dplyr::select(all_of(vars_to_fill)) %>%
  dplyr::summarise(across(everything(), ~ first(na.omit(.))))

for (var in vars_to_fill) {
  data[[var]][data$site_old == "WI" & is.na(data[[var]])] <- reference_values[[var]]
}

reference_values <- data %>%
  dplyr::filter(site_old == "Boise_Low") %>%
  dplyr::select(all_of(vars_to_fill)) %>%
  dplyr::summarise(across(everything(), ~ first(na.omit(.))))

for (var in vars_to_fill) {
  data[[var]][data$site_old == "BA" & is.na(data[[var]])] <- reference_values[[var]]
}

##use CG pasture to approximate Cheyenne
reference_values <- data %>%
  dplyr::filter(site_old == "CG PASTURE") %>%
  dplyr::select(all_of(vars_to_fill)) %>%
  dplyr::summarise(across(everything(), ~ first(na.omit(.))))

for (var in vars_to_fill) {
  data[[var]][data$site_old == "CH" & is.na(data[[var]])] <- reference_values[[var]]
}




# Check completeness of soil variables 
#vars_to_check <- c("pH", "EC", "OMpercent", "Protein_g.kg", "X..Sand", "X..Clay", "X..Silt")

#missing_by_site <- data %>%
 # group_by(site) %>%
  #summarise(across(all_of(vars_to_check), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
  #ungroup()

#missing_sites <- missing_by_site %>%
#  filter(if_any(starts_with("missing_"), ~ . > 0))

#missing_sites



###### add cg climate offset #########
offsets <- tibble(
  site    = c("BA_black", "BA_white", "CH_black",                   "CH_white", "SS_black", "SS_white", "WI_black",                     
"WI_white"),
  offset  = c(1.035, -1.035, 1.607874, -1.607874, 1.407078, -1.407078, 0.8875247, -0.8875247)   
) #Climate to cause temp diff degrees C from data logged average divided by two except for bA

data <- data %>%
  left_join(offsets, by = c("site")) %>%
  mutate(across(
    c(tmean.Fall, tmean.Sum, tmean.Spr, tmean.Win, MAT, 
      tmin_center30d_mean, tmax_center30d_mean,
      tavg_center30d_mean, tmin_center30d_min, tmax_center30d_max),
    ~ case_when(
      Type == "Common_Garden" & !is.na(offset) ~ . + offset,  
      Type == "Satellite" ~ .,                               
      TRUE ~ .                                                
    )
  )) %>%
  dplyr::select(-offset)  




####### Prepare data for model ########
K_all ## updated kinship matrix
genotype_index_new ## list of synthetic genotype names for satellite sites, should I split by site instead of site year 
genotype_index_all ## all the genotypes in K_all

genotype_index_new$site <- gsub(" [0-9]{4}$", "", genotype_index_new$site)
genotype_index_all$site <- gsub(" [0-9]{4}$", "", genotype_index_all$site)

valid_genotypes <- genotype_index_all$genotype   
genotype_lookup <- setNames(seq_along(valid_genotypes), valid_genotypes)

### add genotype assignments to satellite sites:
data$genotype <- as.character(data$genotype)
genotype_index_new$genotype <- as.character(genotype_index_new$genotype)


# ensure genotype columns are character
site_geno_map <- genotype_index_new %>%
  group_by(site) %>%
  slice(1) %>%  
  ungroup() %>%
  mutate(genotype = as.character(genotype)) %>%
  dplyr::select(site, genotype_map = genotype)  

# fill NAs in data$genotype using site mapping
data <- data %>%
  left_join(site_geno_map, by = "site") %>%
  mutate(genotype = coalesce(genotype, genotype_map)) %>%
  dplyr::select(-genotype_map) %>%
  mutate(genotype = as.factor(genotype))  


 data %>%
  group_by(site) %>%
  summarise(genotypes = paste(unique(genotype), collapse = ", ")) %>%
  arrange(site)


### Genotypes info ##########


### using the original common garden genotypes:
#BRTE <- left_join(BRTE, tips, by = "PopNum") 

#BRTE <- BRTE %>% dplyr::select(PopNum, NewSiteCode, tip.label, IBS.id)

#assigned_genotypes <- left_join(assigned_genotypes, BRTE, by = "NewSiteCode") 


#genotypes_common_gardens <- 
# kinshipIDs %>%
#  mutate(source = as.factor(source)) %>%
# filter(genotype %in% unique(data$genotype)) %>%
#  arrange(NewSiteCode)

#assigned_genotypes$kinshipID

#genotypes_all <- kinshipIDs %>%
 # mutate(source = as.factor(source)) %>%
  #filter(genotype %in% unique(c(data$genotype, assigned_genotypes$genotype))) %>%  # Include assigned genotypes
  #arrange(NewSiteCode)


# Filter for common garden genotypes 
#K_common_garden <- as.matrix(K[genotypes_all$kinshipID,genotypes_all$kinshipID])
#K_common_garden <- as.matrix(kinship[genotypes_all$kinshipID,genotypes_all$kinshipID])
#use kinship for actual matrix 

#K_common_garden <- as.matrix(kinship[assigned_genotypes$PopNumD,assigned_genotypes$IBS.id]) #doesn't work

#Diana: if you go with PopNum and corresponding IBS.id (row and column number in BRTE307_IBSmatrix.txt) you should get the correct genotypes from for the new kinship matrix.

# Put genotype numbers on rows and columns
#colnames(K_common_garden) <- rownames(K_common_garden) <- as.factor(genotypes_all$genotype)


######## Demography info #########
#assigned_genotypes$site <- as.factor(assigned_genotypes$site)
#assigned_genotypes$genotype <- as.integer(assigned_genotypes$genotype)
#assigned_genotypes$NewSiteCode <- as.factor(assigned_genotypes$NewSiteCode)
#kinshipIDs$NewSiteCode <- as.factor(kinshipIDs$NewSiteCode)

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


#valid_genotypes <- rownames(K_common_garden)
#df <- df %>% filter(genotype %in% valid_genotypes)
#df_rep <- df_rep %>% filter(genotype %in% valid_genotypes)
#df_emg <- df_emg %>% filter(genotype %in% valid_genotypes)

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
  "pH", "EC", "OMpercent", "Protein_g.kg", "X..Silt", "X..Sand")
## note shouldn't use all three

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



fviz_pca_biplot(pca_out_soil,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()


fviz_cos2(pca_out_soil, choice = "var", axes = 1:2)
fviz_contrib(pca_out_soil, choice = "var", axes = 2, top = 10)

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

## make relevant variables factors
training_df$site_year <- factor(training_df$site_year)
testing_df$site_year  <- factor(testing_df$site_year, levels = levels(training_df$site_year))

training_df$site <- factor(training_df$site)
testing_df$site  <- factor(testing_df$site, levels = levels(training_df$site))

training_df_rep$site_year <- factor(training_df_rep$site_year)
testing_df_rep$site_year  <- factor(testing_df_rep$site_year, levels = levels(training_df_rep$site_year))
training_df_rep$site <- factor(training_df_rep$site)
testing_df_rep$site  <- factor(testing_df_rep$site, levels = levels(training_df_rep$site))

training_df_emg$site_year <- factor(training_df_emg$site_year)
testing_df_emg$site_year  <- factor(testing_df_emg$site_year, levels = levels(training_df_emg$site_year))
training_df_emg$site <- factor(training_df_emg$site)
testing_df_emg$site  <- factor(testing_df_emg$site, levels = levels(training_df_emg$site))

##### ----------------------------
##### 1. Combine site-years for indexing
##### ----------------------------

# Plant/site-year indices (unique across train + test)
all_site_years <- sort(unique(c(training_df$site_year, testing_df$site_year)))
site_year_index <- setNames(seq_along(all_site_years), all_site_years)

training_df$idx_plant <- site_year_index[as.character(training_df$site_year)]
testing_df$idx_plant  <- site_year_index[as.character(testing_df$site_year)]

training_df_rep$idx_plant <- site_year_index[as.character(training_df_rep$site_year)]
testing_df_rep$idx_plant  <- site_year_index[as.character(testing_df_rep$site_year)]

training_df_emg$idx_plant <- site_year_index[as.character(training_df_emg$site_year)]
testing_df_emg$idx_plant  <- site_year_index[as.character(testing_df_emg$site_year)]

##### ----------------------------
##### 2. Site indices (unique across train + test)
##### ----------------------------

all_sites <- sort(unique(c(training_df$site, testing_df$site)))
site_index <- setNames(seq_along(all_sites), all_sites)

training_df$idx_site <- site_index[as.character(training_df$site)]
testing_df$idx_site  <- site_index[as.character(testing_df$site)]

training_df_rep$idx_site <- site_index[as.character(training_df_rep$site)]
testing_df_rep$idx_site <- site_index[as.character(testing_df_rep$site)]

training_df_emg$idx_site <- site_index[as.character(training_df_emg$site)]
testing_df_emg$idx_site <- site_index[as.character(testing_df_emg$site)]

##### ----------------------------
##### 3. Genotype indices (unique across train + test)
##### ----------------------------

# All genotypes seen in train or test
all_genotypes <- sort(unique(c(training_df$genotype, testing_df$genotype)))
genotype_index <- setNames(seq_along(all_genotypes), all_genotypes)

training_df$idx_genotype <- genotype_index[as.character(training_df$genotype)]
testing_df$idx_genotype  <- genotype_index[as.character(testing_df$genotype)]

training_df_rep$idx_genotype <- genotype_index[as.character(training_df_rep$genotype)]
testing_df_rep$idx_genotype  <- genotype_index[as.character(testing_df_rep$genotype)]

training_df_emg$idx_genotype <- genotype_index[as.character(training_df_emg$genotype)]
testing_df_emg$idx_genotype  <- genotype_index[as.character(testing_df_emg$genotype)]

##### ----------------------------
##### 4. Quick checks
##### ----------------------------

# Site-year indices
range(training_df$idx_plant)
range(testing_df$idx_plant)

# Site indices
range(training_df$idx_site)
range(testing_df$idx_site)

# Genotype indices
range(training_df$idx_genotype)
range(testing_df$idx_genotype)

# NAs?
any(is.na(training_df$idx_plant))
any(is.na(testing_df$idx_plant))
any(is.na(training_df$idx_genotype))
any(is.na(testing_df$idx_genotype))


# ------------------------------
# 1. Site-year indices
# ------------------------------

# Training site-years
training_site_years <- sort(unique(training_df$site_year))
site_year_index_train <- setNames(seq_along(training_site_years), training_site_years)
training_df$idx_plant <- site_year_index_train[as.character(training_df$site_year)]

# Test site-years (new levels get indices after training)
testing_site_years <- sort(unique(testing_df$site_year))
new_test_levels <- setdiff(testing_site_years, training_site_years)
site_year_index_test <- c(site_year_index_train,
                          setNames(seq(length(training_site_years) + 1,
                                       length(training_site_years) + length(new_test_levels)),
                                   new_test_levels))
testing_df$idx_plant <- site_year_index_test[as.character(testing_df$site_year)]

# Repeat for reproductive and emerged datasets
training_site_years_rep <- sort(unique(training_df_rep$site_year))
training_site_years_emg <- sort(unique(training_df_emg$site_year))
testing_site_years_rep <- sort(unique(testing_df_rep$site_year))
testing_site_years_emg <- sort(unique(testing_df_emg$site_year))

training_df_rep$idx_plant <- setNames(seq_along(training_site_years_rep), training_site_years_rep)[as.character(training_df_rep$site_year)]
training_df_emg$idx_plant <- setNames(seq_along(training_site_years_emg), training_site_years_emg)[as.character(training_df_emg$site_year)]

new_test_levels_rep <- setdiff(testing_site_years_rep, training_site_years_rep)
new_test_levels_emg <- setdiff(testing_site_years_emg, training_site_years_emg)

site_year_index_test_rep <- c(setNames(seq_along(training_site_years_rep), training_site_years_rep),
                              setNames(seq(length(training_site_years_rep)+1,
                                           length(training_site_years_rep)+length(new_test_levels_rep)),
                                       new_test_levels_rep))
testing_df_rep$idx_plant <- site_year_index_test_rep[as.character(testing_df_rep$site_year)]

site_year_index_test_emg <- c(setNames(seq_along(training_site_years_emg), training_site_years_emg),
                              setNames(seq(length(training_site_years_emg)+1,
                                           length(training_site_years_emg)+length(new_test_levels_emg)),
                                       new_test_levels_emg))
testing_df_emg$idx_plant <- site_year_index_test_emg[as.character(testing_df_emg$site_year)]


# ------------------------------
# 2. Site indices (for soil/climate PCA)
# ------------------------------
all_sites <- sort(unique(c(training_df$site, testing_df$site)))
site_index <- setNames(seq_along(all_sites), all_sites)
training_df$idx_site <- site_index[as.character(training_df$site)]
testing_df$idx_site <- site_index[as.character(testing_df$site)]

all_sites_rep <- sort(unique(c(training_df_rep$site, testing_df_rep$site)))
site_index_rep <- setNames(seq_along(all_sites_rep), all_sites_rep)
training_df_rep$idx_site <- site_index_rep[as.character(training_df_rep$site)]
testing_df_rep$idx_site <- site_index_rep[as.character(testing_df_rep$site)]

all_sites_emg <- sort(unique(c(training_df_emg$site, testing_df_emg$site)))
site_index_emg <- setNames(seq_along(all_sites_emg), all_sites_emg)
training_df_emg$idx_site <- site_index_emg[as.character(training_df_emg$site)]
testing_df_emg$idx_site <- site_index_emg[as.character(testing_df_emg$site)]

# ------------------------------
# 3. Genotype indices
# ------------------------------
valid_genotypes <- genotype_index_all$genotype
genotype_lookup <- setNames(seq_along(valid_genotypes), valid_genotypes)

training_df$idx_genotype <- genotype_lookup[as.character(training_df$genotype)]
testing_df$idx_genotype <- genotype_lookup[as.character(testing_df$genotype)]

training_df_rep$idx_genotype <- genotype_lookup[as.character(training_df_rep$genotype)]
testing_df_rep$idx_genotype <- genotype_lookup[as.character(testing_df_rep$genotype)]

training_df_emg$idx_genotype <- genotype_lookup[as.character(training_df_emg$genotype)]
testing_df_emg$idx_genotype <- genotype_lookup[as.character(testing_df_emg$genotype)]

### check
# Site-year indices
range(training_df$idx_plant)      # should be 1:N_training
range(testing_df$idx_plant)       # can be > N_training if you allow new levels
# Site indices
range(training_df$idx_site)       # should be 1:N_sites
range(testing_df$idx_site)        # 1:N_sites
# Genotype indices
range(training_df$idx_genotype)   # should be 1:N_genotypes
range(testing_df$idx_genotype)    # same as above

any(is.na(training_df$idx_plant))     # FALSE
any(is.na(testing_df$idx_plant))      # FALSE
any(is.na(training_df$idx_genotype))  # FALSE
any(is.na(testing_df$idx_genotype))   # FALSE



### other
# 1. Combine all genotypes from training and test
all_genotypes <- unique(c(training_df$genotype, testing_df$genotype))
genotype_lookup <- setNames(seq_along(all_genotypes), all_genotypes)

# 2. Map integer genotype indices
training_df$genotype_idx <- as.integer(genotype_lookup[as.character(training_df$genotype)])
testing_df$genotype_idx <- as.integer(genotype_lookup[as.character(testing_df$genotype)])

training_df_rep$genotype_idx <- as.integer(genotype_lookup[as.character(training_df_rep$genotype)])
testing_df_rep$genotype_idx <- as.integer(genotype_lookup[as.character(testing_df_rep$genotype)])

training_df_emg$genotype_idx <- as.integer(genotype_lookup[as.character(training_df_emg$genotype)])
testing_df_emg$genotype_idx <- as.integer(genotype_lookup[as.character(testing_df_emg$genotype)])

# 3. Site-year indices
train_site_years <- sort(unique(training_df$site_year))
test_site_years <- sort(unique(testing_df$site_year))

site_year_lookup <- setNames(seq_along(unique(c(train_site_years, test_site_years))),
                             unique(c(train_site_years, test_site_years)))

training_df$site_year_idx <- as.integer(site_year_lookup[as.character(training_df$site_year)])
testing_df$site_year_idx <- as.integer(site_year_lookup[as.character(testing_df$site_year)])

training_df_rep$site_year_idx <- as.integer(site_year_lookup[as.character(training_df_rep$site_year)])
testing_df_rep$site_year_idx <- as.integer(site_year_lookup[as.character(testing_df_rep$site_year)])

training_df_emg$site_year_idx <- as.integer(site_year_lookup[as.character(training_df_emg$site_year)])
testing_df_emg$site_year_idx <- as.integer(site_year_lookup[as.character(testing_df_emg$site_year)])

# 4. Site indices (similar logic)
train_sites <- sort(unique(training_df$site))
site_lookup <- setNames(seq_along(unique(c(train_sites, testing_df$site))),
                        unique(c(train_sites, testing_df$site)))

training_df$site_idx <- as.integer(site_lookup[as.character(training_df$site)])
testing_df$site_idx <- as.integer(site_lookup[as.character(testing_df$site)])

training_df_rep$site_idx <- as.integer(site_lookup[as.character(training_df_rep$site)])
testing_df_rep$site_idx <- as.integer(site_lookup[as.character(testing_df_rep$site)])

training_df_emg$site_idx <- as.integer(site_lookup[as.character(training_df_emg$site)])
testing_df_emg$site_idx <- as.integer(site_lookup[as.character(testing_df_emg$site)])


####### site-year indices
#training site-year indices
site_year_index_train <- setNames(seq_along(levels(training_df$site_year)), levels(training_df$site_year))
training_df$idx_plant_train <- site_year_index_train[as.character(training_df$site_year)]


test_new_levels <- setdiff(levels(testing_df$site_year), levels(training_df$site_year))


site_year_index_test <- c(site_year_index_train,
                          setNames(seq(length(site_year_index_train)+1,
                                       length(site_year_index_train)+length(test_new_levels)),
                                   test_new_levels))
# testing site-yer indices
testing_df$idx_plant_test <- site_year_index_test[as.character(testing_df$site_year)]

site_year_index_train_rep <- setNames(seq_along(levels(training_df_rep$site_year)), levels(training_df_rep$site_year))
training_df_rep$idx_plant_train <- site_year_index_train_rep[as.character(training_df_rep$site_year)]

test_new_levels_rep <- setdiff(levels(testing_df_rep$site_year), levels(training_df_rep$site_year))
site_year_index_test_rep <- c(site_year_index_train_rep,
                              setNames(seq(length(site_year_index_train_rep)+1,
                                           length(site_year_index_train_rep)+length(test_new_levels_rep)),
                                       test_new_levels_rep))
testing_df_rep$idx_plant_test <- site_year_index_test_rep[as.character(testing_df_rep$site_year)]

site_year_index_train_emg <- setNames(seq_along(levels(training_df_emg$site_year)), levels(training_df_emg$site_year))
training_df_emg$idx_plant_train <- site_year_index_train_emg[as.character(training_df_emg$site_year)]

test_new_levels_emg <- setdiff(levels(testing_df_emg$site_year), levels(training_df_emg$site_year))
site_year_index_test_emg <- c(site_year_index_train_emg,
                              setNames(seq(length(site_year_index_train_emg)+1,
                                           length(site_year_index_train_emg)+length(test_new_levels_emg)),
                                       test_new_levels_emg))
testing_df_emg$idx_plant_test <- site_year_index_test_emg[as.character(testing_df_emg$site_year)]



####### site indices 
site_index_train       <- setNames(seq_along(levels(training_df$site)), levels(training_df$site))
site_index_train_rep   <- setNames(seq_along(levels(training_df_rep$site)), levels(training_df_rep$site))
site_index_train_emg   <- setNames(seq_along(levels(training_df_emg$site)), levels(training_df_emg$site))

training_df$idx_plant_train_site     <- site_index_train[as.character(training_df$site)]
testing_df$idx_plant_test_site       <- site_index_train[as.character(testing_df$site)]
training_df_rep$idx_plant_train_site <- site_index_train_rep[as.character(training_df_rep$site)]
testing_df_rep$idx_plant_test_site   <- site_index_train_rep[as.character(testing_df_rep$site)]
training_df_emg$idx_plant_train_site <- site_index_train_emg[as.character(training_df_emg$site)]
testing_df_emg$idx_plant_test_site   <- site_index_train_emg[as.character(testing_df_emg$site)]

####### genotype indices
datasets <- list(training_df, testing_df,
                 training_df_rep, testing_df_rep,
                 training_df_emg, testing_df_emg)

datasets <- lapply(datasets, function(df) df %>% filter(genotype %in% valid_genotypes))

training_df       <- datasets[[1]]
testing_df        <- datasets[[2]]
training_df_rep   <- datasets[[3]]
testing_df_rep    <- datasets[[4]]
training_df_emg   <- datasets[[5]]
testing_df_emg    <- datasets[[6]]

# Map genotypes to integer indices
genotype_plant_train      <- as.integer(genotype_lookup[as.character(training_df$genotype)])
genotype_plant_test       <- as.integer(genotype_lookup[as.character(testing_df$genotype)])
genotype_plant_train_rep  <- as.integer(genotype_lookup[as.character(training_df_rep$genotype)])
genotype_plant_test_rep   <- as.integer(genotype_lookup[as.character(testing_df_rep$genotype)])
genotype_plant_train_emg  <- as.integer(genotype_lookup[as.character(training_df_emg$genotype)])
genotype_plant_test_emg   <- as.integer(genotype_lookup[as.character(testing_df_emg$genotype)])


# Check again
range(genotype_plant_train)  
length(genotype_plant_train)  

range(genotype_plant_train_rep)  
length(genotype_plant_train_rep) 

range(genotype_plant_train_emg)  
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


### older code

#training_df$site_year <- factor(training_df$site_year)
#testing_df$site_year <- factor(testing_df$site_year)

#training_df_rep$site_year <- factor(training_df_rep$site_year)
#testing_df_rep$site_year <- factor(testing_df_rep$site_year)

#training_df_emg$site_year <- factor(training_df_emg$site_year)
#testing_df_emg$site_year <- factor(testing_df_emg$site_year)

# Create index for training site-years 
#training_site_years <- sort(unique(training_df$site_year))
#site_year_index_train <- data.frame(
 # site_year = training_site_years,
#  idx = seq_along(training_site_years)  
#)

#training_site_years_rep <- sort(unique(training_df_rep$site_year))
#site_year_index_train_rep <- data.frame(
#  site_year = training_site_years_rep,
#  idx = seq_along(training_site_years_rep)  
#)

#training_site_years_emg <- sort(unique(training_df_emg$site_year))
#site_year_index_train_emg <- data.frame(
#  site_year = training_site_years_emg,
#  idx = seq_along(training_site_years_emg)  
#)

# Create index for testing site-years 
#testing_site_years <- sort(unique(testing_df$site_year))
#site_year_index_test <- data.frame(
 # site_year = testing_site_years,
  #idx = seq_along(testing_site_years) + length(training_site_years)  # Start from 40
#)

#testing_site_years_rep <- sort(unique(testing_df_rep$site_year))
#site_year_index_test_rep <- data.frame(
#  site_year = testing_site_years_rep,
#  idx = seq_along(testing_site_years_rep) + length(training_site_years_rep)  # Start from 40
#)

#testing_site_years_emg <- sort(unique(testing_df_emg$site_year))
#site_year_index_test_emg <- data.frame(
 # site_year = testing_site_years_emg,
  #idx = seq_along(testing_site_years_emg) + length(training_site_years_emg)  # Start from 40
#)

# Merge site-year indices into the original dataframes
#training_df <- left_join(training_df, site_year_index_train, by = "site_year")
#testing_df <- left_join(testing_df, site_year_index_test, by = "site_year")

#training_df_rep <- left_join(training_df_rep, site_year_index_train_rep, by = "site_year")
#testing_df_rep <- left_join(testing_df_rep, site_year_index_test_rep, by = "site_year")

#training_df_emg <- left_join(training_df_emg, site_year_index_train_emg, by = "site_year")
#testing_df_emg <- left_join(testing_df_emg, site_year_index_test_emg, by = "site_year")

#### site level index for soil PCA
#training_df$site <- factor(training_df$site)
#testing_df$site <- factor(testing_df$site)

#training_df_rep$site <- factor(training_df_rep$site)
#testing_df_rep$site <- factor(testing_df_rep$site)

#training_df_emg$site <- factor(training_df_emg$site)
#testing_df_emg$site <- factor(testing_df_emg$site)

## fecundity 
#all_sites <- sort(unique(c(training_df$site, testing_df$site)))

#site_index <- data.frame(
 # site = all_sites,
  #idx_site = seq_along(all_sites)
#)

#training_df <- training_df %>% left_join(site_index, by = "site")
#testing_df  <- testing_df %>% left_join(site_index, by = "site")

### reproduced
#all_sites_rep <- sort(unique(c(training_df_rep$site, testing_df_rep$site)))

#site_index_rep <- data.frame(
 # site = all_sites_rep,
  #idx_site = seq_along(all_sites_rep)
#)

#training_df_rep <- training_df_rep %>% left_join(site_index_rep, by = "site")
#testing_df_rep  <- testing_df_rep %>% left_join(site_index_rep, by = "site")

### Emerged
#all_sites_emg <- sort(unique(c(training_df_emg$site, testing_df_emg$site)))

#site_index_emg <- data.frame(
 # site = all_sites_emg,
  #idx_site = seq_along(all_sites_emg)
#)

#training_df_emg <- training_df_emg %>% left_join(site_index_emg, by = "site")
#testing_df_emg  <- testing_df_emg %>% left_join(site_index_emg, by = "site")

### genotype indices
## filter to only include valid genotypes
#datasets <- list(training_df, testing_df,
 #                training_df_rep, testing_df_rep,
  #               training_df_emg, testing_df_emg)

#datasets <- lapply(datasets, function(df) {
 # df %>% filter(genotype %in% valid_genotypes)
#})

#training_df <- datasets[[1]]
#testing_df  <- datasets[[2]]
#training_df_rep <- datasets[[3]]
#testing_df_rep <- datasets[[4]]
#training_df_emg <- datasets[[5]]
#testing_df_emg <- datasets[[6]]


#genotype_plant_train      <- as.integer(genotype_lookup[as.character(training_df$genotype)])
#genotype_plant_test       <- as.integer(genotype_lookup[as.character(testing_df$genotype)])
#genotype_plant_train_rep  <- as.integer(genotype_lookup[as.character(training_df_rep$genotype)])
#genotype_plant_test_rep   <- as.integer(genotype_lookup[as.character(testing_df_rep$genotype)])
#genotype_plant_train_emg  <- as.integer(genotype_lookup[as.character(training_df_emg$genotype)])
#genotype_plant_test_emg   <- as.integer(genotype_lookup[as.character(testing_df_emg$genotype)])


#training_df$NewSiteCode <- as.character(training_df$NewSiteCode)
#training_df$NewSiteCode[is.na(training_df$NewSiteCode)] <- "Unknown"
#training_df$NewSiteCode <- as.factor(training_df$NewSiteCode) 

#training_df_rep$NewSiteCode <- as.character(training_df_rep$NewSiteCode)
#training_df_rep$NewSiteCode[is.na(training_df_rep$NewSiteCode)] <- "Unknown"
#training_df_rep$NewSiteCode <- as.factor(training_df_rep$NewSiteCode)

#training_df_emg$NewSiteCode <- as.character(training_df_emg$NewSiteCode)
#training_df_emg$NewSiteCode[is.na(training_df_emg$NewSiteCode)] <- "Unknown"
#training_df_emg$NewSiteCode <- as.factor(training_df_emg$NewSiteCode)

#valid_genotypes <- rownames(K_common_garden)
#genotype_lookup <- setNames(seq_along(valid_genotypes), valid_genotypes)

# Filter df to only rows with genotypes in K
#training_df <- training_df %>% filter(genotype %in% valid_genotypes)

#training_df_rep <- training_df_rep %>% filter(genotype %in% valid_genotypes)

#training_df_emg <- training_df_emg %>% filter(genotype %in% valid_genotypes)

#testing_df <- testing_df %>% filter(genotype %in% valid_genotypes)
#testing_df_rep <- testing_df_rep %>% filter(genotype %in% valid_genotypes)

#testing_df_emg <- testing_df_emg %>% filter(genotype %in% valid_genotypes)

#genotype_plant_train <- as.integer(genotype_lookup[as.character(training_df$genotype)])

#genotype_plant_test <- as.integer(genotype_lookup[as.character(testing_df$genotype)])

#genotype_plant_train_rep <- as.integer(genotype_lookup[as.character(training_df_rep$genotype)])

#genotype_plant_test_rep <- as.integer(genotype_lookup[as.character(testing_df_rep$genotype)])

#genotype_plant_train_emg <- as.integer(genotype_lookup[as.character(training_df_emg$genotype)])

#genotype_plant_test_emg <- as.integer(genotype_lookup[as.character(testing_df_emg$genotype)])

