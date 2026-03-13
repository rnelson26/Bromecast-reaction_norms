################# Bromecast: 13.extract_point_estimates.R ##########################
############# created 3-9-26 ######################
############# Last modified: 3-11-26 ##########################
######## Extract Point Estimates for Peter ################################
###########################

##### Load Packages ######
library(cmdstanr)
library(posterior)
library(scoringRules)
library(tibble)
library(dplyr)
library(readr)
library(tidyverse)
library(purrr)

### Load model fits #######
#base_dir <- "/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026"

#all_files <- list.files(base_dir, pattern = "^fit_.*\\.rds$", full.names = TRUE)

#file_info <- tibble(file_path = all_files) %>%
 # mutate(
  #  file_name   = basename(file_path),
   # stage       = sub("^fit_(.*?)_.*\\.rds$", "\\1", file_name),
    #variant     = sub("^fit_.*?_(.*?)\\.rds$", "\\1", file_name),
    #crps_train  = NA_real_,
    #crps_train_fixed = NA_real_,
    #crps_test   = NA_real_,
    #null_crps_train = NA_real_,
    #null_crps_test  = NA_real_,
    #skill_train = NA_real_,
    #skill_train_fixed = NA_real_,
    #skill_test  = NA_real_
  #)

## load full models
fit_emg_full <- readRDS("/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026/fit_emerged_full_all.rds")
fit_rep_full <- readRDS("/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026/fit_reproduced_full_allparams.rds")
fit_fec_full <- readRDS("/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026/fit_fecundity_full_all.rds")

#fit_rep <- readRDS("output/fit_reproduced_full.rds") #from my computer run
#fit_emg <- readRDS("output/fit_emerged_full.rds") #from my computer run

#fit_rep_full <- fit_rep$draws(format = "df") #if from my computer


#cn <- colnames(fit_rep_full)
#base_names <- sub("\\[.*\\]", "", cn)
#unique(base_names)

#fit_emg_full <- fit_emg$draws(format = "df") 

##### get point estimates ##########

## negative binomial dispersion
theta_median_fec <- median(fit_fec_full$theta)
#0.971208

## alpha (global intercept)
alpha_median_emg <- median(fit_emg_full$alpha)
#1.058775
alpha_median_rep <- median(fit_rep_full$alpha)
#0.3060346
alpha_median_fec <- median(fit_fec_full$alpha)
#3.464301

## density dependence 
beta_neighbors_median_emg <- median(fit_emg_full$beta_neighbors)
#-0.03486385
beta_neighbors_median_rep <- median(fit_rep_full$beta_neighbors)
#0.341517
beta_neighbors_median_fec <- median(fit_fec_full$beta_neighbors)
#-0.8355671

## site-year variance (stan estimtes SD)
site_year_sd_emg  <- median(fit_emg_full$sigma_site_year)
#0.0187921
site_year_var_emg <- median(fit_emg_full$sigma_site_year^2)
#0.0003531432

site_year_sd_rep  <- median(fit_rep_full$sigma_site_year)
# 0.01925452
site_year_var_rep <- median(fit_rep_full$sigma_site_year^2)
#0.0003707365

site_year_sd_fec  <- median(fit_fec_full$sigma_site_year)
#0.0180197
site_year_var_fec <- median(fit_fec_full$sigma_site_year^2)
#0.0003247097

## genotype-specific intercepts 
grep("beta_0_centered", colnames(fit_emg_full), value = TRUE)

geno_intercepts_emg <- apply(
  fit_emg_full[, grep("beta_0_centered", colnames(fit_emg_full))],
  2,
  median
)
## acount for global interecept 
geno_total_emg <- apply(
  fit_emg_full[, grep("beta_0_centered", colnames(fit_emg_full))],
  2,
  function(x) median(fit_emg_full$alpha + x))
  
  grep("beta_0_centered", colnames(fit_emg_full), value = TRUE)
  
  geno_intercepts_emg <- apply(
    fit_emg_full[, grep("beta_0_centered", colnames(fit_emg_full))],
    2,
    median
  )
  write.csv(geno_total_emg, "output/genotype_intercepts_emerged.csv", row.names = TRUE)
  

  ## genotype-specific intercepts 
  grep("beta_0_centered", colnames(fit_rep_full), value = TRUE)
  
  geno_intercepts_rep <- apply(
    fit_emg_full[, grep("beta_0_centered", colnames(fit_rep_full))],
    2,
    median
  )
  ## acount for global interecept 
  geno_total_rep <- apply(
    fit_rep_full[, grep("beta_0_centered", colnames(fit_emg_full))],
    2,
    function(x) median(fit_rep_full$alpha + x))
  
  grep("beta_0_centered", colnames(fit_rep_full), value = TRUE)
  
  geno_intercepts_rep <- apply(
    fit_rep_full[, grep("beta_0_centered", colnames(fit_emg_full))],
    2,
    median
  )
  
  write.csv(geno_total_rep, "output/genotype_intercepts_reproduced.csv", row.names = TRUE)

grep("beta_0_centered", colnames(fit_fec_full), value = TRUE)

geno_intercepts_fec <- apply(
  fit_fec_full[, grep("beta_0_centered", colnames(fit_fec_full))],
  2,
  median
)
## acount for global interecept 
geno_total_fec <- apply(
  fit_fec_full[, grep("beta_0_centered", colnames(fit_fec_full))],
  2,
  function(x) median(fit_fec_full$alpha + x)
)

write.csv(geno_total_fec, "output/genotype_intercepts_fecundity.csv", row.names = TRUE)
## genotype x climate slopes
grep("beta\\[[0-9]+,1\\]", colnames(fit_emg_full), value = TRUE)
geno_climate_emg <- apply(
  fit_emg_full[, grep("beta\\[[0-9]+,1\\]", colnames(fit_emg_full))],
  2,
  median
)
write.csv(geno_climate_emg, "output/genotype_by_climate_emerged.csv", row.names = TRUE)

grep("beta\\[[0-9]+,1\\]", colnames(fit_rep_full), value = TRUE)
geno_climate_rep <- apply(
  fit_rep_full[, grep("beta\\[[0-9]+,1\\]", colnames(fit_rep_full))],
  2,
  median
)
write.csv(geno_climate_rep, "output/genotype_by_climate_reproduced.csv", row.names = TRUE)

grep("beta\\[[0-9]+,1\\]", colnames(fit_fec_full), value = TRUE)
geno_climate_fec <- apply(
  fit_fec_full[, grep("beta\\[[0-9]+,1\\]", colnames(fit_fec_full))],
  2,
  median
)
write.csv(geno_climate_fec, "output/genotype_by_climate_fecundity.csv", row.names = TRUE)

######## Get site-year randome effects for each site year ########
site_year_cols_emg <- grep("site_year_effect_train_scaled_centered",
                       colnames(fit_emg_full),
                       value = TRUE)

site_year_cols_rep <- grep("site_year_effect_train_scaled_centered",
                           colnames(fit_rep_full),
                           value = TRUE)

site_year_cols_fec <- grep("site_year_effect_train_scaled_centered",
                           colnames(fit_fec_full),
                           value = TRUE)


## calculate point estimates:
site_year_effects_emg <- apply(
  fit_emg_full[, site_year_cols_emg],
  2,
  median
)

site_year_effects_rep <- apply(
  fit_rep_full[, site_year_cols_rep],
  2,
  median
)

site_year_effects_fec <- apply(
  fit_fec_full[, site_year_cols_fec],
  2,
  median
)


site_year_emg <- data.frame(
  site_year = site_year_index_train_emg,
  site_year_effect = site_year_effects_emg
)

site_year_rep <- data.frame(
  site_year = site_year_index_train_rep,
  site_year_effect = site_year_effects_rep
)

site_year_fec <- data.frame(
  site_year = site_year_index_train,
  site_year_effect = site_year_effects_fec
)

write.csv(site_year_emg, "output/training_site_year_emerged.csv", row.names = TRUE)
write.csv(site_year_rep, "output/training_site_year_reprouced.csv", row.names = TRUE)
write.csv(site_year_fec, "output/training_site_year_fecundity.csv", row.names = TRUE)

####### PPCA #######
#Here, we can take the slope of PC1 versus scaled temperature from the probabilistic PCA in our models and multiply it by the change in temperature in standard units. This gives the axis shift along PC1 that corresponds to a given change in temperature (C). We can thus translate climate PCA effects in our model back into meaningful temperature units (C).

## emerged 
W_cols_emg <- grep("^W\\[", colnames(fit_emg_full), value = TRUE)
W_post_emg <- apply(fit_emg_full[, W_cols_emg], 2, median)
W_mat_emg <- matrix(W_post_emg,
                    nrow = nrow(X_emg_SOS),
                    ncol = q_X,
                    byrow = FALSE)
PC1 <- W_mat_emg[,1]
temp_scaled_emg <- X_emg_SOS[,"MAT"]
## estimate relationship between MAT and PC1 
fit_temp_pc1_emg <- lm(PC1 ~ temp_scaled_emg)
summary(fit_temp_pc1_emg)
slope <- coef(fit_temp_pc1_emg)[2] #slope of relationship
sd_temp <- attr(X_emg_SOS, "scaled:scale")["MAT"]
delta_temp_scaled <- 2 / sd_temp
delta_PC1_emg <- slope * delta_temp_scaled

#A 2 °C increase in MAT corresponds to about a -1.718036  change in PC1 units in the climate space used by the emerged model.


## reproduced 
W_cols_rep <- grep("^W\\[", colnames(fit_rep_full), value = TRUE)
W_post_rep <- apply(fit_rep_full[, W_cols_rep], 2, median)
W_mat_rep <- matrix(W_post_rep,
                    nrow = nrow(X_rep_SOS),
                    ncol = q_X,
                    byrow = FALSE)
PC1 <- W_mat_rep[,1]
temp_scaled_rep <- X_rep_SOS[,"MAT"]
## estimate relationship between MAT and PC1 
fit_temp_pc1_rep <- lm(PC1 ~ temp_scaled_rep)
summary(fit_temp_pc1_rep)
slope <- coef(fit_temp_pc1_rep)[2] #slope of relationship
sd_temp <- attr(X_rep_SOS, "scaled:scale")["MAT"]
delta_temp_scaled <- 2 / sd_temp
delta_PC1_emg <- slope * delta_temp_scaled

#A 2 °C increase in MAT corresponds to about a -1.644498   change in PC1 units in the climate space used by the reproduced model.

## fecundity 
W_cols_fec <- grep("^W\\[", colnames(fit_fec_full), value = TRUE)
W_post_fec <- apply(fit_fec_full[, W_cols_fec], 2, median)
W_mat_fec <- matrix(W_post_fec,
                    nrow = nrow(X_SOS),
                    ncol = q_X,
                    byrow = FALSE)
PC1 <- W_mat_fec[,1]
temp_scaled_fec <- X_SOS[,"MAT"]
## estimate relationship between MAT and PC1 
fit_temp_pc1_fec <- lm(PC1 ~ temp_scaled_fec)
summary(fit_temp_pc1_fec)
slope <- coef(fit_temp_pc1_fec)[2] #slope of relationship
sd_temp <- attr(X_SOS, "scaled:scale")["MAT"]
delta_temp_scaled <- 2 / sd_temp
delta_PC1_fec <- slope * delta_temp_scaled

#A 2 °C increase in MAT corresponds to about a -1.487172  change in PC1 units in the climate space used by the fecundity model.
