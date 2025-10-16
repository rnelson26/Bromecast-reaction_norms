
######### Landscape Genomics with GLMNET ##############
########### Bromecast Reaction Norms ############
######## Create K and assign genotypes ##########
######## code by Justin Van Ee and Becca Nelson ###############
############ created 8-19-25 #############
############ last modified 10-15-25 ##########################

## throughout the code "SNPS" refers to each individual genotype

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

X <- as.matrix(data[, predictor_vars_LM])
Y <- as.matrix(data[, paste0("PC", 1:n_pc)])

fit_cv_predict <- function(X, y, alpha_val) {
  cv_fit <- cv.glmnet(X, y, alpha = alpha_val)
  preds <- predict(cv_fit, newx = X, s = "lambda.min")
  list(preds = preds, lambda = cv_fit$lambda.min)
}

# Compare LASSO vs. Ridge Regression
comparison_results <- map_dfr(1:n_pc, function(l) {
  y <- Y[, l]
  
  # LASSO
  lasso <- fit_cv_predict(X, y, alpha_val = 1)
  # Ridge
  ridge <- fit_cv_predict(X, y, alpha_val = 0)
  
  tibble(
    PC = paste0("PC", l),
    rmse_lasso = sqrt(mean((y - lasso$preds)^2)),
    cor_lasso  = cor(y, lasso$preds),
    rmse_ridge = sqrt(mean((y - ridge$preds)^2)),
    cor_ridge  = cor(y, ridge$preds)
  )
})

comparison_results
#apply(Y, 2, sd)  




plot_data <- comparison_results %>%
  pivot_longer(cols = c(rmse_lasso, rmse_ridge, cor_lasso, cor_ridge),
               names_to = c(".value", "method"),
               names_pattern = "(.*)_(.*)")

#RMSE (lower better)
ggplot(plot_data, aes(x = PC, y = rmse, fill = method)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "RMSE comparison: LASSO vs Ridge", y = "RMSE")

# Correlation (higher better)
ggplot(plot_data, aes(x = PC, y = cor, fill = method)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Correlation comparison: LASSO vs Ridge", y = "Correlation")

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