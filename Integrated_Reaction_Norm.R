#### Integrated Reaction Norm Model #######
######## code by Becca Nelson and Justin Van Ee ###############################
############# created 3-25-25 ######################
############# Last modified: 4-18-25 ##########################
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

soil_clean <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/data/sat_sites/soil_clean.csv")

###### summarise soil data to site-year ########
## also need to assign cg values

soil_summary <- soil_clean %>% group_by(site_old) %>% summarise(across(c(pH, EC, OMpercent, Protein_g.kg), \(x) mean(x, na.rm = TRUE)))

data <- left_join(data, soil_summary, by = "site_old")


vars_to_fill <- c("pH", "EC", "OMpercent", "Protein_g.kg")

## use Boise Low sat soil values for wildcat cg
reference_values <- data %>%
  filter(site_old == "Boise_Low") %>%
  select(all_of(vars_to_fill)) %>%
  summarise(across(everything(), ~ first(na.omit(.))))

for (var in vars_to_fill) {
  data[[var]][data$site_old == "WI" & is.na(data[[var]])] <- reference_values[[var]]
}


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

df <- df %>%
    filter(!is.na(genotype)) 


valid_genotypes <- rownames(K_common_garden)
df <- df %>% filter(genotype %in% valid_genotypes)




###### Climate PCA #########
climate_vars <- c(
  "prcp.Spr", "tmean.Spr",  "prcp.Sum", "tmean.Sum", 
  "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  "tmean.Fall", "swe_mean.Fall", "MAT", 
  "total_precip", "seasonality"
)

soil_vars <- c(
  "pH", "EC", "OMpercent", "Protein_g.kg")

full_vars <- c(
  "prcp.Spr", "tmean.Spr",  "prcp.Sum", "tmean.Sum", 
  "prcp.Win", "tmean.Win", "swe_mean.Win", "prcp.Fall", 
  "tmean.Fall", "swe_mean.Fall", "MAT", 
  "total_precip", "seasonality",  "pH", "EC", "OMpercent", "Protein_g.kg"
)

pca_data <- df %>% 
  dplyr::select(site_year, all_of(climate_vars))  %>% distinct() %>% 
  na.omit()  

soil_data <- df %>% 
  dplyr::select(site_year, all_of(soil_vars))  %>% distinct() %>% 
  na.omit() 

full_data <- df %>% 
  dplyr::select(site_year, all_of(full_vars))  %>% distinct() %>% 
  na.omit() 


site_year_labels <- pca_data$site_year  
site_year_labels_soil <- soil_data$site_year  
site_year_labels_full <- full_data$site_year 

X <- scale(pca_data %>% dplyr::select(-site_year))
X_soil <- scale(soil_data %>% dplyr::select(-site_year))
X_full <- scale(full_data %>% dplyr::select(-site_year))

pca_out <- prcomp(X)
pca_out_soil <- prcomp(X_soil)
pca_out_full <- prcomp(X_full)

n_X <- nrow(pca_data)
q_X <- 2
Lambda <- as.matrix(pca_out$rotation[, 1:q_X])

fviz_pca_biplot(pca_out,
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

fviz_pca_biplot(pca_out_full,
                geom.ind = "point",               
                fill.ind = "grey80",              
                col.var = "contrib",              
                gradient.cols = c("blue", "red"), 
                repel = TRUE) +                   
  theme_minimal()

fviz_cos2(pca_out, choice = "var", axes = 1:2)
fviz_cos2(pca_out_soil, choice = "var", axes = 1:2)
fviz_cos2(pca_out_full, choice = "var", axes = 1:2)

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

### compare data to make sure we have decent coverage of climate 

training_df$Dataset <- "Training"
testing_df$Dataset <- "Testing"

# Combine the datasets
combined_data <- rbind(training_df, testing_df)

# Check overlap
ggplot(combined_data, aes(x = tmean.Sum, fill = Dataset)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "red"))

### Extract fecundity##### 
y <-
  training_df %>%
  pluck("Fecundity") %>%
  #log() %>%
  c()


##### Indices ########
training_df$plot_index <- ifelse(training_df$Type == "Common_Garden", training_df$plot[training_df$Type == "Common_Garden"], 0)
plot_levels <- levels(factor(training_df$plot[training_df$Type == "Common_Garden"]))

training_df$site_year <- factor(training_df$site_year)
testing_df$site_year <- factor(testing_df$site_year)

# Create index for training site-years 
training_site_years <- sort(unique(training_df$site_year))
site_year_index_train <- data.frame(
  site_year = training_site_years,
  idx = seq_along(training_site_years)  
)

# Create index for testing site-years 
testing_site_years <- sort(unique(testing_df$site_year))
site_year_index_test <- data.frame(
  site_year = testing_site_years,
  idx = seq_along(testing_site_years) + length(training_site_years)  # Start from 40
)


# Merge site-year indices into the original dataframes
training_df <- left_join(training_df, site_year_index_train, by = "site_year")
testing_df <- left_join(testing_df, site_year_index_test, by = "site_year")


### genotype indices
 
training_df$NewSiteCode <- as.character(training_df$NewSiteCode)
training_df$NewSiteCode[is.na(training_df$NewSiteCode)] <- "Unknown"
training_df$NewSiteCode <- as.factor(training_df$NewSiteCode) 

valid_genotypes <- rownames(K_common_garden)
genotype_lookup <- setNames(seq_along(valid_genotypes), valid_genotypes)

# Filter df to only rows with genotypes in K
training_df <- training_df %>% filter(genotype %in% valid_genotypes)

testing_df <- testing_df %>% filter(genotype %in% valid_genotypes)

genotype_plant_train <- as.integer(genotype_lookup[as.character(training_df$genotype)])

genotype_plant_test <- as.integer(genotype_lookup[as.character(testing_df$genotype)])


# Check again
range(genotype_plant_train)  # should be 1 to 93
length(genotype_plant_train)  

range(genotype_plant_test) 
length(genotype_plant_test) 

#### plant index
idx_plant_train <- as.numeric(training_df$site_year)
idx_plant_test  <- as.numeric(testing_df$site_year)

####### Fit stan model #########
stan_data <- list(
  # General inputs for the model
  n_X = nrow(X),           
  p_X = ncol(X),           
  q_X = ncol(Lambda),      
  X = X,                   
  Lambda = Lambda,         
  n_g = length(unique(genotype_plant_train)),  
  K = K_common_garden,    
  n_plot = max(training_df$plot_index),
  n_site_year = length(unique(c(training_df$site_year, testing_df$site_year))),
  
  # Training data specifics
  n_train = nrow(training_df),
  y_train = y,
  idx_plant_train = idx_plant_train,
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
  genotype_plant_test = genotype_plant_test,
  neighbors_test = testing_df$neighbors.s,
  annual_test = testing_df$annual.s,
  perennial_test = testing_df$perennial.s,
  shrub_test = testing_df$shrub.s,
  site_year_id_test = testing_df$idx,
  plot_index_test = rep(0, nrow(testing_df)),
  n_site_year_test = length(unique(as.integer(as.factor(testing_df$site_year))))
)



## checks
#stopifnot(length(stan_data$y_train) == stan_data$n_train)
#stopifnot(length(stan_data$genotype_plant_train) == stan_data$n_train)
#stopifnot(length(stan_data$idx_plant_train) == stan_data$n_train)
#stopifnot(nrow(stan_data$X_test) == stan_data$n_test)


# Sanity check
#range(stan_data$site_year_id_train)  # Should be 1 to n_site_year_train
#length(unique(stan_data$site_year_id_train))  # Should be n_site_year_train


# Fit using cmdrstan 
mod <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/ztnb_glm.random.predict.stan")


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
posterior <- fit$draws(variables = c("theta","beta", "sigma", "W", "zeta", "mu_test", "mu_train"))

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
p <- mcmc_trace(posterior,  pars = c("mu_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
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

###### Evaluate Prediction ######
mu_test_post  <- fit$draws("mu_test", format = "draws_matrix")  
mu_test_mean <- apply(mu_test_post, 2, mean)

mu_train_post  <- fit$draws("mu_train", format = "draws_matrix")  
mu_train_mean <- apply(mu_train_post, 2, mean)

mu_train_fixed_post  <- fit$draws("mu_train_fixed", format = "draws_matrix")  
mu_train_fixed_mean <- apply(mu_train_fixed_post, 2, mean)

mu_test_lower <- apply(mu_test_post, 2, quantile, probs = 0.025)
mu_test_upper <- apply(mu_test_post, 2, quantile, probs = 0.975)

mu_train_lower <- apply(mu_train_post, 2, quantile, probs = 0.025)
mu_train_upper <- apply(mu_train_post, 2, quantile, probs = 0.975)

plot(log(mu_test_mean), log(testing_df$Fecundity), main = "Test: Predicted vs Observed",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")

plot(log(mu_train_mean), log(training_df$Fecundity), main = "Test: Predicted vs Observed",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")

plot(log(mu_train_fixed_mean), log(training_df$Fecundity), main = "Test: Predicted vs Observed",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")

rmse <- function(pred, obs) sqrt(mean((pred - obs)^2)) #root-mean squared error
mae <- function(pred, obs) mean(abs(pred - obs)) #mean absolute error
rsq <- function(pred, obs) cor(pred, obs)^2 #R2


rmse(mu_test_mean, testing_df$Fecundity)
rmse(mu_train_mean, training_df$Fecundity)
rmse(mu_train_fixed_mean, training_df$Fecundity)

mae(mu_test_mean, testing_df$Fecundity)
mae(mu_train_mean, training_df$Fecundity)
mae(mu_train_fixed_mean, training_df$Fecundity)

rsq(mu_test_mean, testing_df$Fecundity)
rsq(mu_train_mean, training_df$Fecundity)
rsq(mu_train_fixed_mean, training_df$Fecundity)

testing_df$mu_pred <- mu_test_mean
training_df$mu_pred <- mu_train_mean
training_df$mu_fixed_pred <- mu_train_fixed_mean

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
