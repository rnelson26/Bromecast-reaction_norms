######### Landscape Genomics ##############
########### Bromecast Reaction Norms ############
######## Create K and assign genotypes ##########
######## code by Justin Van Ee and Becca Nelson ###############
############ created 8-19-25 #############
############ last modified 10-10-25 ##########################


## to do:
### add genotypes code that correspond to 127
## update with glmnet
## export outputs of K and genotype index for satellite sites so that they could be added to reaction norm model 
 
## notes from Diana Gamba on full set of WNA genotypes: BRTE127_LDfilteredSNPs.bed is the SNP/genotype matrix for 158,420 snps/rows and 127 genotypes/columns. BRTE127_IBSmatrix.txt is the kinship matrix and BRTE_127wna_ordered.csv has the order of genotypes in those matrices (column ibs_id) and the chelsa climate variables. The first three columns of the bed file are the site id, major allele, minor allele; genotypes start on the 4th
 

rm(list = ls())
### Load Packages ################

library(rwc) 
library(ape)
library(MASS) 
library(tidyverse)
library(poppr)
library(pegas)
library(sf)
library(RcppCNPy)
library(ggridges)
library(nlme)
library(statgenGWAS)

######## Part 1: PC Exp Decay Method with known source sites ###########
######## Load data ###################
## seed source daymet
clim <- 
  read.csv("data/seed_climate_info.csv",header=T) 


### Get genotype key matrix for connecting with genotype matrix 
#genotype_codes <- 
 # read.csv("https://raw.githubusercontent.com/pbadler/bromecast-data/main/traits/data/rawdata/gamba_growthchamber/BRTEcg_genotypesCode.csv",header=T) %>%
  ## Sort Data
#  arrange(genotype) %>%
  ## Remove non-sequenced genotype 
 # filter(!is.na(SNPmatrix_column))
### only for common garden and not finding a similar file for all 127


### Get number of genotypes
n_g <- nrow(genotype_codes)

### Connect to genotype/SNP matrix
#SNPs <- as.data.frame(read.table("data/BRTEcg_SNPs.bed", header = FALSE, sep=",",stringsAsFactors=FALSE)) ##original 93 genotypes

SNPs <- as.data.frame(read.table("data/BRTE127_LDfilteredSNPs.bed", header = FALSE, sep=",",stringsAsFactors=FALSE)) ## all the western North American genotypes 


SNPs <- SNPs[,c(1:3,genotype_codes$SNPmatrix_column)] ## Columns 1:3 provide line name, reference and alternate allele.
# genotype_codes$SNPmatrix_column in index sorts to match bioclim

## Calculate principal components of genotype matrix 
PC_out <- prcomp(t(SNPs[,-c(1:3)]))

# Calculate proportion of variance explained
var_explained <- PC_out$sdev^2 / sum(PC_out$sdev^2)

# Create elbow plot (11 principal components looks good)
plot(var_explained, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained",
     main = "Elbow Plot of Principal Components")

# Select PCs
#n_pc <- 11 # This is elbow but doesn't explain genetic distance well enough for our purpose 
n_pc <- 60 # This gets 99% of variation explained 
PCs <- PC_out$x[,1:n_pc]

# Augment dataset (PCs automatically labeled to PC1-PC11) 
data <-
  cbind(
    PCs,
    clim
  ) 
## ask Justin how these map on

# Standardize predictors (mean 0, sd 1)
#data <- data %>%
 # mutate(across(lon:prc.cld.q, scale))


## data has the 60 PCS along with spatial coordintes, genotype and bioclimate variables for the seed source locations. 

##### Leave one out cross validation #########
library(purrr)
library(dplyr)

# predictors
#predictor_vars_LM <- c(
 # "lon", "lat", "ann.mean.tmp", "mean.diurn.rng", "isotherm",
 # "tmp.seas", "max.tmp.wrm.m", "min.tmp.cld.m", "tmp.ann.rng",
 # "mean.tmp.wet.q", "mean.tmp.dry.q", "mean.tmp.wrm.q", "mean.tmp.cld.q",
 # "ann.prc", "prc.wet.m", "prc.dry.m", "prc.seas", "prc.wet.q",
  #"prc.dry.q", "prc.wrm.q", "prc.cld.q"
#)

predictor_vars_LM <- c("Latitude",  "Longitude",  "bioclim_1",  "bioclim_2",  "bioclim_3",  "bioclim_4",  "bioclim_5", "bioclim_6", "bioclim_7", "bioclim_8",  "bioclim_9",  "bioclim_10", "bioclim_11", "bioclim_12", "bioclim_13", "bioclim_14", "bioclim_15", "bioclim_16", "bioclim_17", "bioclim_18", "bioclim_19")
## should be same 19 bioclim variables with daymet

# Run LOOCV
loocv_results <- map_dfr(1:nrow(data), function(i) {
  
  # Training data (all except ith row)
  train_data <- data[-i, ]
  test_data  <- data[i, ]
  
  # Fit models for each PC
  mods <- map(1:n_pc, function(l) {
    response <- paste0("PC", l)
    form <- reformulate(predictor_vars_LM, response)
    lm(form, data = train_data)
  })
  
  # Predict PCs for held-out site
  predPCs <- map_dbl(mods, ~ predict(.x, newdata = test_data))
  truePCs <- as.numeric(test_data[paste0("PC", 1:n_pc)])
  
  tibble(
    genotype = test_data$genotype,
    site_row = i,
    PC = paste0("PC", 1:n_pc),
    observed = truePCs,
    predicted = predPCs
  )
})

# Calculate RMSE for each PC
loocv_summary <- loocv_results %>%
  group_by(PC) %>%
  summarise(
    rmse = sqrt(mean((observed - predicted)^2)),
    cor  = cor(observed, predicted),
    .groups = "drop"
  )

loocv_summary

### ask Justin loocv if something further should be done...

###
### Fit models (linear regression)
###
## here, predictors are environmental covariates, response variables are the PCs of the SNPs for seed source locations.

# Identify predictor columns
predictor_vars <- names(data)[which(names(data) == "lon"):which(names(data) == "prc.cld.q")]

# Can fit model independently because PCs are orthogonal 
mods_LM <- map(1:n_pc, function(l) {
  response <- paste0("PC", l)
  formula_str <- paste(response, "~", paste(predictor_vars, collapse = " + "))
  lm(as.formula(formula_str), data = data)
})

# Get R squared (note RMSE goes down while R^2 goes down because larger PCs have more variance and more things we can easily observe)
df_LM <- map_dfr(mods_LM, ~{
  summ <- summary(.x)
  rmse <- sqrt(mean(residuals(.x)^2))
  
  tibble(
    r.squared = summ$r.squared,
    adj.r.squared = summ$adj.r.squared, # Adjusted r-squared is garbage because we feeding the model a bunch of correlated predictors
    rmse = rmse
  )
}, .id = "PC")

## df_LM provides rmse associated with each PC.

###
### Fit models (generalized least squares with exponential covariance function (no nugget))
###
## this now takes the same PCS and fits a model that directly considers spatial structure 

# Identify predictor columns (remove lat and lon)
predictor_vars <- names(data)[which(names(data) == "elevation"):which(names(data) == "prc.cld.q")]

# Can fit model independently because PCs are orthogonal 
mods_GLS <- map(1:n_pc, function(l) {
  response <- paste0("PC", l)
  formula_str <- paste(response, "~", paste(predictor_vars, collapse = " + "))
  gls(
    as.formula(formula_str),
    data = data,
    correlation = corExp(form = ~ lon + lat, nugget = FALSE),
    method = "REML"
  )
}) ### put lat lon into glm net 

# Get RMSE 
df_GLS <- map_dfr(mods_GLS, ~{
  rmse <- sqrt(mean(residuals(.x)^2))
  
  tibble(
    rmse = rmse
  )
}, .id = "PC")
## df_GLS provides rmse for PCs

### Compare spatial and no-spatial prediction (extend for out-of-sample RMSE using leave one out cross-validation)
summary(df_GLS$rmse)
summary(df_LM$rmse)

###
### Calculate kinship matrix from principal component genetic distance matrix 
###

# Get distance matrix (this is only for observed genotypes, we'll need to predict for satellite sites)
D <- PCs %>%
  dist(method = "euclidean", diag=TRUE, upper=TRUE) %>%
  as.matrix()
## distance matrix in PC space, currently has 92 in it, genetic distance 

### Calculate IBS matrix, note that increasing MAF will cause off diagonals to decrease
## genetic distance, kinship matrix 
K <- 
  SNPs[,-c(1:3)] %>%
  t() %>%
  kinship(method="IBS", MAF=0.10) %>%
  cov2cor()
### check with Justin that column headers in V are genotype numbers 
### supposed to correspond to what Diana has in google drive 

## IBS is identity by state
## MAF is minor allele frequency threshold for less common alleles
### K is a kinship covariance matrix 

###
### Find relationship between derived kinship and environmental distance among genotypes (in PC space)  using quadratic regression
###

# Use simple linear regression to find the optimal range parameter 
distance <- c(D)
hist(distance) # Can see the near clonal pairs in this plot on the fair left --- this doesn't make sense because it's environment PC among genotypes
log_kinship <- c(log(K))
hist(log_kinship) # Now far right

plot(x=distance, y=log_kinship) # as you get further away in genetic distance, then kinship less related

# Fit model (no intercept)
opt_range <- lm(log_kinship ~ distance + I(distance^2) - 1)
summary(opt_range) # excellent fit (i.e., genetic distance + distance^2 in PC space is a good predictor of kinship)

# Predict new kinship matrix (not guaranteed to be positive definite unless both beta_1 and beta_2 < 0)
K_new_raw <- matrix((exp(predict(opt_range))), n_g, n_g) ## full one, 92 known genotypes and interelated for all common garden and satellite site, use as new kinship matrix in stan code with different genotype, and replace assigning by distance to synethetic genotype mapping, link them to data file in list  

# Enforce it to be PD 
K_new <- K_new_raw %>%
  nearPD() %>%
  pluck("mat") %>%
  as.matrix()

# Check mean absolute difference (very small, rounding error)
# We will want to recheck this when we add predictions of genetic distance for the other genotypes
summary(c(abs(K_new-K_new_raw)))

# Extract off diagonals 
K_off <- K[upper.tri(K)]
Knew_off <- K_new[upper.tri(K_new)]

### chose name or index for synthetic genotypes based on new site code,first 92 or so known genotypes in kinship matrix
# Build data frame
df <- data.frame(
  value = c(K_off, Knew_off),
  Method = rep(c("IBS", "PC - Exponential Decay"),
               times = c(length(K_off), length(Knew_off)))
)

# Plot 
ggplot(df, aes(x = value, fill = Method)) +
  geom_density(alpha = 0.5, position = "identity") +
  theme_bw() +
  labs(
    x = "Kinship",
    y = "Density",
    title = "Distribution of Kinship Coefficients"
  ) +
  theme(legend.position = "bottom")

## they are similar! So using the PC exp decay method, we can predict kinship for genotypes without knowing SNP data by using PC space to estimate genetic distances. We will do this next. 


########## Part 2: Assign genotype for satellite sites ######################
### check that column headers on K align with numbers of synthetic genotypes 

## name variables
predictor_vars_LM <- c(
  "lon", "lat", "ann.mean.tmp", "mean.diurn.rng", "isotherm",
  "tmp.seas", "max.tmp.wrm.m", "min.tmp.cld.m", "tmp.ann.rng",
  "mean.tmp.wet.q", "mean.tmp.dry.q", "mean.tmp.wrm.q", "mean.tmp.cld.q",
  "ann.prc", "prc.wet.m", "prc.dry.m", "prc.seas", "prc.wet.q",
  "prc.dry.q", "prc.wrm.q", "prc.cld.q"
)

# convert to numeric 
train_predictors_raw <- bioclim %>% dplyr::select(all_of(predictor_vars_LM))
train_means <- sapply(train_predictors_raw, mean, na.rm = TRUE)
train_sds   <- sapply(train_predictors_raw, sd, na.rm = TRUE)

# Scaling function
scale_with_training <- function(df, means, sds) {
  out <- df
  for (nm in names(means)) {
    out[[nm]] <- (df[[nm]] - means[[nm]]) / sds[[nm]]
  }
  out
}

# Scale new sites based on training info
new_sites_scaled <- new_sites %>%
  mutate(across(all_of(predictor_vars_LM), as.numeric)) %>%
  scale_with_training(train_means, train_sds)

# convert to numeric
#data <- data %>%
 # dplyr::mutate(across(all_of(predictor_vars_LM), ~ as.numeric(.x)))

data <- data %>%
  mutate(across(lon:prc.cld.q, ~ as.numeric(.x)))

# Fit models for sat sites
mods_LM <- lapply(1:n_pc, function(l) {
  response <- paste0("PC", l)
  formula_str <- paste(response, "~", paste(predictor_vars_LM, collapse = " + "))
  lm(as.formula(formula_str), data = data)
})


# Subset scaled predictors for sat sites
new_for_pred_LM <- new_sites_scaled %>%
  dplyr::select(all_of(predictor_vars_LM)) %>%
  mutate(across(everything(), as.numeric))  # ensure numeric

# predict PCs for sat sites 
predPC_list <- lapply(mods_LM, function(mod) predict(mod, newdata = new_for_pred_LM))

# Bind predictions into a matrix
PCs_new <- do.call(cbind, predPC_list)
colnames(PCs_new) <- paste0("PC", 1:n_pc)
rownames(PCs) <- genotype_codes$genotype

##### outputs: K and synthetic genotypes for sat sites #########
# --- After you've created PCs_new ---

# 1. Assign synthetic genotype IDs (starting at 200)
n_new <- nrow(PCs_new)
new_ids <- paste0(200:(199 + n_new))
rownames(PCs_new) <- new_ids
### make sure genotype ID order matches position in matrix

genotype_index_new <- tibble(
  site     = new_sites$site_code,
  genotype = paste0(new_ids),
)

# 2. Combine observed + new PCs
PCs_all <- rbind(PCs, PCs_new)

# 3. Calculate pairwise Euclidean distances in PC space
D_all <- dist(PCs_all, method = "euclidean") %>%
  as.matrix()

# 4. Predict kinship using the quadratic regression you fit earlier (opt_range)
K_new_raw <- matrix(exp(predict(opt_range, newdata = data.frame(distance = c(D_all), 
                                                                `I(distance^2)` = c(D_all^2)))),
                    nrow = nrow(D_all), ncol = ncol(D_all))

# 5. Enforce positive definiteness
K_all <- Matrix::nearPD(K_new_raw)$mat %>% as.matrix()

# 6. Build index of genotypes
genotype_index_obs <- tibble(
  site     = bioclim$site_code,         
  genotype = genotype_codes$genotype
)


###### Part 3: Does an RDA approach yield better predictions for genetic disimilarity at satallite sites? ################

## prepare data
library(vegan)  # for RDA

# Genotype matrix (rows = genotypes, columns = SNPs)
# Make sure it's numeric (0,1,2 for SNP counts)
geno_mat <- t(SNPs[,-c(1:3)])  # transpose so rows = genotypes
rownames(geno_mat) <- genotype_codes$genotype

# Environmental predictors for seed source sites
env_mat <- bioclim %>%
  dplyr::select(all_of(predictor_vars_LM)) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()
rownames(env_mat) <- bioclim$genotype


### fit RDA
rda_mod <- rda(geno_mat ~ ., data = as.data.frame(env_mat))
summary(rda_mod)
RsquareAdj(rda_mod)  # adjusted R²
constraining_scores <- scores(rda_mod, display = "sites", choices = 1:rda_mod$CCA$rank)  # scores of genotypes on constrained axes

### predict synthetic satellite genotypes
# Prepare new_sites environment matrix (scaled with training)
sat_env <- new_sites_scaled %>%
  dplyr::select(all_of(predictor_vars_LM)) %>%
  as.data.frame()

# Predict scores on constrained RDA axes
pred_rda_scores <- predict(rda_mod, newdata = sat_env, type = "lc")[, 1:rda_mod$CCA$rank]
pred_rda_matrix <- as.matrix(pred_rda_scores)
rownames(pred_rda_matrix) <- paste0(200:(199 + nrow(new_sites)))


### compare RDA vs PCA approaches
rda_all <- rbind(constraining_scores, pred_rda_matrix)

# Euclidean distances in RDA space
D_rda <- dist(rda_all, method = "euclidean") %>% as.matrix()

## fit kinship model for RDA
K_rda_raw <- matrix(exp(predict(opt_range, 
                                newdata = data.frame(distance = c(D_rda),
                                                     `I(distance^2)` = c(D_rda^2)))),
                    nrow = nrow(D_rda), ncol = ncol(D_rda))

K_rda <- Matrix::nearPD(K_rda_raw)$mat %>% as.matrix()

## compare predictive peformance for satellite sites 
# For satellite sites only
sat_ids <- paste0(200:(199 + nrow(new_sites)))

# Compare distance distributions
dist_PCA <- D_all[sat_ids, sat_ids]
dist_RDA <- D_rda[sat_ids, sat_ids]

# Simple correlation
cor(c(dist_PCA), c(dist_RDA))
## 94.7% correlation suggests both approaches are similar
### 97.2% correlation when additional genotypes are added

plot(c(dist_PCA), c(dist_RDA),
     xlab = "PCA distances",
     ylab = "RDA distances",
     main = "Comparison of genetic dissimilarity predictions")


#PCA method: Unsupervised — reduces the original genotype (SNP) variation into axes that explain variance without considering environment. Later, you fit linear models of the PCs as a function of environmental predictors to predict PC scores for satellite sites. So for the satellite sites, their predicted PC scores are informed by the environment, but the axes themselves (directions in genotype space) come from the genetic data.

#RDA method: Constrained — ordination axes are influenced by the environmental predictors (bioclimate), so genetic variation is aligned with environmental gradients.

#In short: a correlation of 0.947 indicates that your RDA-based synthetic genotypes largely agree with PCA-based predictions, but RDA may provide more biologically interpretable predictions if environment is expected to shape genetic differences.



# Save outputs
write.csv(genotype_index_new, "synthetic_satallite_genotypes.csv", row.names = FALSE)

## clean workspace
#rm(list = setdiff(ls(), c("K_all", "genotype_index_new")))

seed_sites <- bioclim %>% dplyr::select(site_code, lon, lat) %>% distinct()
write.csv(seed_sites, "seed_sites.csv", row.names = FALSE)






####### old version###############
rm(list = ls())

### Load Packages ################
library(rwc) 
library(ape)
library(MASS) 
library(tidyverse)
library(poppr)
library(pegas)
library(sf)
library(RcppCNPy)
library(ggridges)
library(nlme)
library(statgenGWAS)
library(glmnet)
library(purrr)
library(dplyr)

######## Part 1: PC Exp Decay Method with known source sites ###########
######## Load data ###################
## seed source daymet (all WNA seed source sites except for 4 that were too far north in BC for daymet)
clim <- read.csv("data/seed_climate_info.csv", header = TRUE)
sat_clim <- read.csv("data/sat_climate_info.csv", header = TRUE)

### Get genotype key matrix for connecting with genotype matrix 
genotype_codes <- read.csv("https://raw.githubusercontent.com/pbadler/bromecast-data/main/traits/data/rawdata/gamba_growthchamber/BRTEcg_genotypesCode.csv", header = TRUE) %>%
  arrange(genotype) %>%
  filter(!is.na(SNPmatrix_column))  # only for common garden

### assign genotypes to full list of genotypes for WNA, using existing genotype codes where appropriate:

clim_with_geno <- clim %>%
  left_join(genotype_codes %>% dplyr::select(NewSiteCode, genotype), by = "NewSiteCode")

missing_idx <- which(is.na(clim_with_geno$genotype))

if (length(missing_idx) > 0) {
  max_existing <- max(genotype_codes$genotype, na.rm = TRUE)
  new_genos <- seq(from = max_existing + 1, length.out = length(missing_idx))
  
  clim_with_geno$genotype[missing_idx] <- new_genos
}

clim_with_geno$genotype <- as.integer(clim_with_geno$genotype)


### Get number of genotypes
n_g <- nrow(clim_with_geno)

### Connect to genotype/SNP matrix
SNPs <- as.data.frame(read.table("data/BRTE127_LDfilteredSNPs.bed", header = FALSE, sep = ",", stringsAsFactors = FALSE))  ## all western North American genotypes

## make a new column for order of genotypes in the snp matrix (.bed file)
clim_with_geno <- clim_with_geno %>%
  mutate(SNPmatrix_column = ibs_id + 3)


SNPs <- SNPs[, c(1:3, clim_with_geno$SNPmatrix_column)] ## columns 1:3 = line name, reference, alt allele

## Calculate principal components of genotype matrix 
PC_out <- prcomp(t(SNPs[,-c(1:3)]))

# Calculate proportion of variance explained
var_explained <- PC_out$sdev^2 / sum(PC_out$sdev^2)

# Create elbow plot (visual check)
plot(var_explained, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained",
     main = "Elbow Plot of Principal Components")

# Select number of PCs
n_pc <- 60  # 99% of variance explained
PCs <- PC_out$x[, 1:n_pc]

# Augment dataset (PCs + environmental predictors)
data <- cbind(PCs, clim_with_geno)

# Standardize predictors if needed
data <- data %>%
  mutate(across(c(Latitude, Longitude, starts_with("bioclim_")), scale))


# has response variables (PCs of genetic distance) and predictors (daymet bioclimatic variables, latitude, longitude)

##### Compare Lasso vs Ridge Regression #######

#X <- as.matrix(data[, predictor_vars_LM])
#Y <- as.matrix(data[, paste0("PC", 1:n_pc)])

#fit_cv_predict <- function(X, y, alpha_val) {
# cv_fit <- cv.glmnet(X, y, alpha = alpha_val)
#preds <- predict(cv_fit, newx = X, s = "lambda.min")
#list(preds = preds, lambda = cv_fit$lambda.min)
#}

# Compare LASSO vs. Ridge Regression
#comparison_results <- map_dfr(1:n_pc, function(l) {
# y <- Y[, l]

# LASSO
#lasso <- fit_cv_predict(X, y, alpha_val = 1)
# Ridge
#ridge <- fit_cv_predict(X, y, alpha_val = 0)

#tibble(
# PC = paste0("PC", l),
#rmse_lasso = sqrt(mean((y - lasso$preds)^2)),
#cor_lasso  = cor(y, lasso$preds),
#rmse_ridge = sqrt(mean((y - ridge$preds)^2)),
#cor_ridge  = cor(y, ridge$preds)
# )
#})

#comparison_results
#apply(Y, 2, sd)  




#plot_data <- comparison_results %>%
# pivot_longer(cols = c(rmse_lasso, rmse_ridge, cor_lasso, cor_ridge),
#             names_to = c(".value", "method"),
#            names_pattern = "(.*)_(.*)")

#RMSE (lower better)
#ggplot(plot_data, aes(x = PC, y = rmse, fill = method)) +
# geom_col(position = "dodge") +
#theme_minimal() +
#labs(title = "RMSE comparison: LASSO vs Ridge", y = "RMSE")

# Correlation (higher better)
#ggplot(plot_data, aes(x = PC, y = cor, fill = method)) +
# geom_col(position = "dodge") +
#theme_minimal() +
#labs(title = "Correlation comparison: LASSO vs Ridge", y = "Correlation")

## slight differences ridge seems slightly better, especially for first few PCs

### GLMNET #########
library(glmnet)
library(purrr)

predictor_vars_LM <- c("Latitude",  "Longitude", paste0("bioclim_", 1:19))  # predictors for glmnet

# Predictor and response matrices
X <- as.matrix(data[, predictor_vars_LM])        # predictors: coords + bioclim
Y <- as.matrix(data[, paste0("PC", 1:n_pc)])     # response PCs


###
### Fit final glmnet models for each PC (all data)
###
glmnet_models <- map(1:n_pc, function(l) {
  y <- Y[, l]
  cv.glmnet(X, y, alpha = 0)  # Ridge with CV to select lambda, set to 1 for Lasso
  ## add a predict function here new data for satellite sites 
})
# mostly just need this part 
#Trains the final Ridge models for each PC using all the data. These models will later be used to predict PCs for new sites (satellite sites).

###
### Calculate kinship matrix from principal component genetic distance matrix
###
D <- dist(PCs, method = "euclidean", diag = TRUE, upper = TRUE) %>% as.matrix() ## rbind predict from synthetic genotypes, append that to PCs 
K <- SNPs[,-c(1:3)] %>% t() %>% kinship(method="IBS", MAF=0.10) %>% cov2cor()

#D = Euclidean distance between genotypes in PC space (genetic distance).
#K = Kinship matrix based on identity-by-state (IBS).
#MAF=0.10 removes very rare alleles that can otherwise skew kinship estimates.

### should have PCs for satellites as well in what ends up in K_new_raw

# Quadratic regression for PC-based kinship prediction
distance <- c(D)
log_kinship <- c(log(K))
opt_range <- lm(log_kinship ~ distance + I(distance^2) - 1)
K_new_raw <- matrix(exp(predict(opt_range)), nrow(Y), nrow(Y))
# original notes from meeting with Justin: full one, 92 known genotypes and interelated for all common garden and satellite site, use as new kinship matrix in stan code with different genotype, and replace assigning by distance to synethetic genotype mapping, link them to data file in list  


#Fits a quadratic relationship between genetic distance in PC space and log kinship.
#Predicts a new kinship matrix (K_new_raw) for all genotypes including ones without SNP data (satellite sites)

# Enforce positive definiteness
K_new <- K_new_raw %>% nearPD() %>% pluck("mat") %>% as.matrix()
summary(c(abs(K_new - K_new_raw)))  # check difference

# Extract off-diagonals for plotting
K_off <- K[upper.tri(K)]
Knew_off <- K_new[upper.tri(K_new)]

df <- data.frame(
  value = c(K_off, Knew_off),
  Method = rep(c("IBS", "PC - Exponential Decay"),
               times = c(length(K_off), length(Knew_off)))
)

# Plot kinship distributions
ggplot(df, aes(x = value, fill = Method)) +
  geom_density(alpha = 0.5, position = "identity") +
  theme_bw() +
  labs(
    x = "Kinship",
    y = "Density",
    title = "Distribution of Kinship Coefficients"
  ) +
  theme(legend.position = "bottom")

## PC exp decay method allows predicting kinship for genotypes without SNP data

########## Part 2: Assign synthetic genotypes for satellite sites ######################
### most of this part is not necessary except the parts that need to be moved up described above 
## Figure: where known genotypes are in PC space (standard ordination figure) vs in different color synthetic satellite site genotypes paired with geographic map or environmental map with precip and temperature space 


# Combine predictions into a matrix
PCs_new <- do.call(cbind, predPC_list)
colnames(PCs_new) <- paste0("PC", 1:n_pc)

# Step 2: Assign unique synthetic genotype IDs for satellite sites
n_new <- nrow(PCs_new)
new_ids <- paste0("G", 200:(199 + n_new))
rownames(PCs_new) <- new_ids

genotype_index_new <- tibble(
  site     = new_sites$site_code,
  genotype = new_ids
)

# Step 3: Combine observed PCs and new PCs
PCs_all <- rbind(PCs, PCs_new) ## move up to first part

# Step 4: Compute pairwise Euclidean distances in PC space
D_all <- as.matrix(dist(PCs_all, method = "euclidean"))

# Step 5: Predict kinship using quadratic regression
# Do NOT manually supply I(distance^2); R handles it from formula
K_new_raw <- matrix(
  exp(predict(opt_range, newdata = data.frame(distance = c(D_all)))),
  nrow = nrow(D_all),
  ncol = ncol(D_all)
)

# Step 6: Enforce positive definiteness
K_all <- Matrix::nearPD(K_new_raw)$mat %>% as.matrix()

# Step 7: Combine genotype index for observed + satellite sites
genotype_index_all <- bind_rows(
  tibble(site = bioclim$site_code, genotype = genotype_codes$genotype),
  genotype_index_new
)

# Step 8: Final checks (optional)
stopifnot(all(rownames(PCs_all) == genotype_index_all$genotype))
stopifnot(all(rownames(K_all) == genotype_index_all$genotype))

#### Step 9: Save for analysis
# Save as .RData (can save multiple objects)
save(K_all, genotype_index_all, file = "data/K_all_genotypes.RData")

# load("data/K_all_genotypes.RData")
write.csv(K_all, "data/K_all_genotypes.csv", row.names = TRUE)
