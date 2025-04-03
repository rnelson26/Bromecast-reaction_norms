#### Integrated Reaction Norm Model #######
######## code by Becca Nelson and Justin Van Ee ###############################
############# created 3-25-25 ######################
############# Last modified: 4-2-25 ##########################
######## modifies RMD file to pull from one integrated df ########

rm(list = ls())

## to do

## add cg climate data 

###### Load packages #####
library(tidyverse)
library(bayesplot)
library(cmdstanr)
#library(ggplot2) #if you don't want to load the whole tidyverse
#library(dplyr)

##### Load Data #########
data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE) 

kinshipIDs <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/93cg_genotypes.csv")

kinship <- read.table("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/BRTE307_IBSmatrix.txt", sep=",")

assigned_genotypes <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/assigned_genotypes.csv")

#### Split the data ########

### split training and test data 

data$site <- as.factor(data$site)
data$year <- as.factor(data$year)

data <- data %>%
  mutate(site_year = paste(site, year))

data_sat <- data %>% filter(Type == "Satellite")

set.seed(123)  # For reproducibility

selected_categories <- data_sat %>%
  distinct(site_year) %>%  
  slice_sample(n = 36) %>% 
  pull(site_year)          

training_data <-data %>%
  filter(site_year %in% selected_categories | Type == "Common_Garden")

testing_data <- data %>%
  filter(!(site_year %in% selected_categories) & Type == "Satellite")

### compare data to make sure we have decent coverage of climate 

training_data$Dataset <- "Training"
testing_data$Dataset <- "Testing"

# Combine the datasets
combined_data <- rbind(training_data, testing_data)

# Check overlap
ggplot(combined_data, aes(x = tmean.Spr, fill = Dataset)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "red"))

####### Prepare data for model ########

### Genotypes info ##########
genotypes_common_gardens <- 
  kinshipIDs %>%
  mutate(source = as.factor(source)) %>%
  filter(genotype %in% unique(data$genotype)) %>%
  arrange(NewSiteCode)

# Filter for common garden genotypes 
K_common_garden <- as.matrix(kinship[genotypes_common_gardens$kinshipID,genotypes_common_gardens$kinshipID])

# Put genotype numbers on rows and columns
colnames(K_common_garden) <- rownames(K_common_garden) <- as.factor(genotypes_common_gardens$NewSiteCode)

######## Demography info #########
assigned_genotypes$site <- as.factor(assigned_genotypes$site)
assigned_genotypes$genotype <- as.integer(assigned_genotypes$genotype)
assigned_genotypes$NewSiteCode <- as.factor(assigned_genotypes$NewSiteCode)
kinshipIDs$NewSiteCode <- as.factor(kinshipIDs$NewSiteCode)

df <- training_data %>%
  filter(Emerged == "Y", Reproduced == "Y") %>%
  mutate(
    site_numeric = as.numeric(as.factor(site)),
    site_year_numeric = as.numeric(as.factor(site_year)),
    year_numeric = as.numeric(as.factor(year)) - 1
  ) %>%
  left_join(assigned_genotypes %>% 
              select(site, genotype_assigned = genotype, 
                     NewSiteCode, SeedSource, sample.id), 
            by = "site") %>%
  # Replace NA genotypes from training_data with those from assigned_genotypes
  mutate(
    genotype = ifelse(is.na(genotype), genotype_assigned, genotype)  # Ensure type consistency
  ) %>%
  select(-genotype_assigned) %>%  # Remove temporary column
  # Join with kinshipIDs using the newly assigned genotype
  left_join(kinshipIDs, by = c("genotype", "NewSiteCode")) %>%
  filter(!is.na(Fecundity)) %>%
  filter(!is.na(genotype)) %>%
  filter(Fecundity > 0) 

df <- df %>%
  filter(!site_year %in% c("GoeblS1", "Pearlwise")) #filter problem children sites that are missing climate info 


#some that have zero seeds otherwise included bc coded as reproduced 



### Extract fecundity 
y <-
  df %>%
  pluck("Fecundity") 

###### Climate PCA #########
climate_vars <- c(
  "prcp.Spr", "tmean.Spr",  "prcp.Sum", "tmean.Sum", 
  "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  "tmean.Fall", "swe_mean.Fall", "MAT", 
  "total_precip", "seasonality"
)

pca_data <- df %>% 
  select(site_year, all_of(climate_vars))  %>% distinct() %>% 
  na.omit()  
## need to determine why certain sites are missing



site_year_labels <- pca_data$site_year  
X <- scale(pca_data %>% select(-site_year))

pca_out <- prcomp(X)

n_X <- nrow(pca_data)
q_X <- 2
Lambda <- as.matrix(pca_out$rotation[, 1:q_X])


##### Indices ########

unique_sites <- unique(df$site) 
idx_sites <- match(unique_sites, df$site) 


### Create linkage matrix 
df$site_year <- as.factor(df$site_year)
df$site_year <- droplevels(df$site_year)
Z <- model.matrix(~ site_year - 1, data = df)

### Create linkage matrix (for four sites)
unique_sites <- unique(df$site)  
site_match <- match(unique_sites, df$site)  
idx_plant <- match(df$site, unique_sites)  


print(range(idx_plant))   # Should be between 1 and length(idx_sites)
print(length(idx_sites))  # Should match the max value in idx_plant


## site year idx
site_year_idx <- unique(as.integer(factor(df$site_year)))
## do we want this as an integer or categorical variable? 

#n = nrow(V), #number of observations
#  p_V = ncol(V), #Number of treatments + 1 for intercept
V <- Z

####### Fit stan model #########
stan_data <- list(
  # Dimension of objects
  n_g = length(unique(df$genotype)), #number of genotypes, 95
  n = nrow(V), #number of observations
  p_V = ncol(V), #Number of treatments + 1 for intercept
  n_X = nrow(pca_data), #number of obs of climate variables 
  q_X = ncol(Lambda), #Number of latent factors
  p_X = nrow(Lambda), #number of climate variables 
  # Response (fecundity)
  y = y,
  # Matrices 
  Lambda = Lambda,
  V = V,
  X = X,
  # Kinship
  K = K_common_garden,
  # For linking plants with genotypes and treatments 
  idx_sites = idx_sites,
  idx_plant = idx_plant,
  #genotype_plant = genotype_plant #,
  # site_year_idx = site_year_idx #,  
  #n_site_year = length(unique(site_year_idx))  
)

# Warmup and iterations 
iter_warmup = 100
iter_sampling = 1000

# Fit using cmdrstan 
mod <- cmdstan_model("ztp_glm.stan")
#mod <- cmdstan_model("ztp_glm.stan_with_intercept") #for site_year random intercept 

# Compile and fit the model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling
)

# Summary of results
fit$summary()

# Extract posterior samples
posterior <- fit$draws(variables = c("beta", "gamma", "sigma", "W", "zeta"))

#
# Traceplots for diagnostics
#

# beta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("beta[1,1]","beta[2,1]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# gamma
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior[51:200,,],  pars = paste0("gamma[", 1:ncol(V), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# sigma
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# zeta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior[51:200,,],  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# W
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W[1,1]","W[1,2]", "W[101,1]", "W[101,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)
