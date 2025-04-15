#### Integrated Reaction Norm Model #######
######## code by Becca Nelson and Justin Van Ee ###############################
############# created 3-25-25 ######################
############# Last modified: 4-15-25 ##########################
######## modifies RMD file to pull from one integrated df ########

rm(list = ls())

## to do


###### Load packages #####
library(tidyverse)
library(bayesplot)
library(cmdstanr)
library(reshape2)

#library(ggplot2) #if you don't want to load the whole tidyverse
#library(dplyr)

##### Load Data #########
data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE) 

kinshipIDs <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/93cg_genotypes.csv")

kinship <- read.table("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/BRTE307_IBSmatrix.txt", sep=",")

assigned_genotypes <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/assigned_genotypes.csv")

tips <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/307tips.csv")

cg_WC <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/dailyVWCdata_allgardens_allyears.csv")

cg_temp <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/common_gardens/dailytempdata_allgardens_allyears.csv")

BRTE <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/BRTE_NorthAmerica.csv", header = TRUE)

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
  slice_sample(n = 12) %>% ##36 for whole split
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
ggplot(combined_data, aes(x = tmean.Sum, fill = Dataset)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "red"))

####### Prepare data for model ########

### Genotypes info ##########
K <- diag(1, 93, 93) #indep kinship matrix

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

df <- training_data %>%
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

df <- df %>%
    filter(!is.na(genotype)) 

valid_genotypes <- rownames(K_common_garden)
df <- df %>% filter(genotype %in% valid_genotypes)
#some that have zero seeds otherwise included bc coded as reproduced 



### Extract fecundity 
y <-
  df %>%
  pluck("Fecundity") %>%
  #log() %>%
  c()

###### Climate PCA #########
climate_vars <- c(
  "prcp.Spr", "tmean.Spr",  "prcp.Sum", "tmean.Sum", 
  "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  "tmean.Fall", "swe_mean.Fall", "MAT", 
  "total_precip", "seasonality"
)

pca_data <- df %>% 
  dplyr::select(site_year, all_of(climate_vars))  %>% distinct() %>% 
  na.omit()  




site_year_labels <- pca_data$site_year  
X <- scale(pca_data %>% dplyr::select(-site_year))

pca_out <- prcomp(X)

n_X <- nrow(pca_data)
q_X <- 2
Lambda <- as.matrix(pca_out$rotation[, 1:q_X])


##### Indices ########
df$plot_index <- ifelse(df$Type == "Common_Garden", df$plot[df$Type == "Common_Garden"], 0)
plot_levels <- levels(factor(df$plot[df$Type == "Common_Garden"]))

site_year_levels <- pca_data$site_year
df$site_year <- factor(df$site_year, levels = site_year_levels)
idx_plant <- as.integer(df$site_year)  # This goes from 1 to n_X
stopifnot(max(idx_plant) <= n_X)




#df$idx_site <- as.integer(as.factor(df$site))


#range(stan_data$idx_plant)  # Should be between 1 and stan_data$n_X
#stopifnot(max(stan_data$idx_plant) <= stan_data$n_X)


### Create linkage matrix 
df$site_year <- as.factor(df$site_year)
df$site_year <- droplevels(df$site_year)
Z <- model.matrix(~ site_year - 1, data = df) 



### Create linkage matrix (for four sites)
#unique_sites <- unique(df$site)  
#site_match <- match(unique_sites, df$site)  
#idx_plant <- match(df$site, unique_sites)  


print(range(idx_plant))   # Should be between 1 and length(idx_sites)
#print(length(idx_sites))  # Should match the max value in idx_plant


## site year idx
site_year_idx <- unique(as.integer(factor(df$site_year)))
## do we want this as an integer or categorical variable? 

#n = nrow(V), #number of observations
#  p_V = ncol(V), #Number of treatments + 1 for intercept
V <- Z

 
df$NewSiteCode <- as.character(df$NewSiteCode)
df$NewSiteCode[is.na(df$NewSiteCode)] <- "Unknown"
df$NewSiteCode <- as.factor(df$NewSiteCode) 

# Genotype IDs that are in the kinship matrix
valid_genotypes <- rownames(K_common_garden)

# Make a lookup table
genotype_lookup <- setNames(seq_along(valid_genotypes), valid_genotypes)

# Filter df to only rows with genotypes in K
df <- df %>% filter(genotype %in% valid_genotypes)

# Recode genotype_plant as indices into K
genotype_plant <- as.integer(genotype_lookup[as.character(df$genotype)])

# Check again
range(genotype_plant)  # should be 1 to 93
length(unique(genotype_plant))  # should be ≤ 93




#genotype_plant <-df$genotype

#genotype_plant <- as.numeric(as.factor(df$NewSiteCode))

####### Fit stan model #########
stan_data <- list(
  # Dimension of objects
  n_g =  nrow(K_common_garden), #number of genotypes
  n_s = length(unique(df$site)), ## number of sites 
  n = nrow(df), #number of observations
  plot_index = df$plot_index,
  n_plot = max(df$plot_index),  # number of unique common garden plots
  p_V = ncol(V), #Number of treatments + 1 for intercept, now site year
  n_X = nrow(pca_data), #number of obs of climate variables 
  q_X = ncol(Lambda), #Number of latent factors
  p_X = nrow(Lambda), #number of climate variables 
  neighbors = df$neighbors.s, ## cheatgrass density 
  annual = df$annual.s, ## interspecific competition variables
  perennial = df$perennial.s,
  shrub = df$shrub.s,
  # Response (fecundity)
  y = y,
  # Matrices 
  Lambda = Lambda,
  #V = V,
  X = X,
  # Kinship
  K = K_common_garden,
  # For linking plants with genotypes and treatments 
 # idx_sites = idx_sites,
  idx_plant = idx_plant,
  genotype_plant = genotype_plant,
 # Site-year random effect 
 site_year_id = as.integer(as.factor(df$site_year)),                  # Integer index: 1..n_site_year for each row in df
 n_site_year = length(unique(as.integer(as.factor(df$site_year))))
)                      
  # site_year_idx = site_year_idx #,  
  #n_site_year = length(unique(site_year_idx))  

# Fit using cmdrstan 
mod <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/ztnb_glm.random.stan")
#mod <- cmdstan_model("ztp_glm_with_intercept.stan") #for site_year random intercept 


#### Find good starting values ####
pathfinder_fit <- mod$pathfinder(
  data = stan_data,          # your named list of data
  init = 0,                  # or a list of reasonable inits
  num_paths = 1              # usually equal to number of chains
)
init_list <- pathfinder_fit$draws(format = "list")


# Warmup and iterations 
iter_warmup = 100 
iter_sampling = 1000

# Compile and fit the model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  init = init_list
)

# Summary of results
summary <- fit$summary()
range(summary$rhat)

# Extract posterior samples
posterior <- fit$draws(variables = c("theta","beta", "sigma", "W", "zeta", "mu"))

#
# Traceplots for diagnostics
#

# theta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("theta"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# beta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# gamma
#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior,  pars = paste0("gamma[", 1:6, "]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

# sigma
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# zeta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# W
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# mu
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

### explore parameters
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
  select(starts_with("beta")) %>%
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
  select(matches("W\\[.*\\]")) %>%  # Match all W[i,j] values
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




Lambda$climate_variable <- rownames(Lambda)

# Reshape to long format
library(reshape2)
Lambda_long <- melt(Lambda, id.vars = "climate_variable",
                    variable.name = "PC", value.name = "loading")

# Heatmap
ggplot(Lambda_long, aes(x = Var2, y = Var1, fill = loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  labs(title = "PCA Loadings: Climate variables on W axes",
       x = "Principal Component",
       y = "Climate Variable")

#PC1 loads negatively on most temperature variables and positively on precipitation/SWE: temperature–moisture tradeoff axis.

#PC2 loads heavily on Fall/Winter precipitation/SWE and prcp.Sum (negatively): a seasonal moisture pattern axis.

ggplot(Lambda_long, aes(x = reorder(Var1, loading), y = loading, fill = loading > 0)) +
  geom_col() +
  facet_wrap(~ Var2, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Contribution of Climate Variables to Each PCA Axis",
    x = "Climate Variable",
    y = "Loading"
  ) +
  theme(legend.position = "none")

# Get the matrix values
lambda_values <- unlist(Lambda[1:(length(Lambda)-1)])  # remove the 'climate_variable' entry

p_X <- length(Lambda$climate_variable)  # number of climate variables
q_X <- length(lambda_values) / p_X     # number of PCs

Lambda_mat <- matrix(lambda_values, nrow = p_X, ncol = q_X, byrow = FALSE)
rownames(Lambda_mat) <- Lambda$climate_variable

#### means
beta_draws <- posterior_df %>% 
  select(starts_with("beta["))

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


climate_effects <- Lambda_mat %*% t(beta_mean)


library(reshape2)


heat_df <- melt(climate_effects, varnames = c("Climate_Variable", "Genotype"), value.name = "Effect")

ggplot(heat_df, aes(x = Genotype, y = Climate_Variable, fill = Effect)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Effect of Climate Variables on Fecundity by Genotype")

avg_effects <- rowMeans(climate_effects)

ggplot(data.frame(Climate_Variable = names(avg_effects), Effect = avg_effects),
       aes(x = reorder(Climate_Variable, Effect), y = Effect)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Average Effect of Climate Variables on Fecundity",
       x = "Climate Variable", y = "Mean Effect")

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
