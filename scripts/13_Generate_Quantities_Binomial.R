### Generate Quantities Emergence/Reproduction #################
############# created 1-8-26 ######################
############# Last modified: 1-8-26 ##########################
######## Extracts generated quantities in R instead of Stan ################################
########### code by R. Nelson ###############


library(dplyr)
library(cmdstanr)

####### Helper Functions ################

# Bernoulli posterior predictive function
predict_bernoulli <- function(
    draws,
    data,
    W, W_soil,
    beta,
    beta_0,
    mu_beta_soil,
    sigma_site_year,
    sigma_plot,
    site_year_effect = NULL,
    eta_plot = NULL,
    site_year_mode = c("conditional", "noise", "none"),
    plot_mode = c("conditional", "noise", "none")
) {
  site_year_mode <- match.arg(site_year_mode)
  plot_mode <- match.arg(plot_mode)
  
  n_draws <- length(draws$alpha)
  n_obs   <- data$n_obs
  
  p_mat <- matrix(NA_real_, n_draws, n_obs)
  y_mat <- matrix(NA_integer_, n_draws, n_obs)
  
  for (d in seq_len(n_draws)) {
    for (i in seq_len(n_obs)) {
      
      g        <- data$genotype[i]
      idx      <- data$idx_plant[i]
      idx_site <- data$idx_plant_site[i]
      
      logit_p <- draws$alpha[d] +
        sum(W[idx, ] * beta[d, g, ]) +
        sum(W_soil[idx_site, ] * mu_beta_soil[d, ]) +
        beta_0[d, g] +
        draws$beta_neighbors[d]  * data$neighbors[i] +
        draws$beta_annual[d]     * data$annual[i] +
        draws$beta_perennial[d]  * data$perennial[i] +
        draws$beta_shrub[d]      * data$shrub[i]
      
      # Site-year random effect
      if (site_year_mode == "conditional" && !is.null(site_year_effect)) {
        logit_p <- logit_p + site_year_effect[d, data$site_year_id[i]]
      } else if (site_year_mode == "noise") {
        logit_p <- logit_p + rnorm(1, 0, sigma_site_year[d])
      }
      
      # Plot random effect
      if (data$plot_index[i] != 0) {
        if (plot_mode == "conditional" && !is.null(eta_plot)) {
          logit_p <- logit_p + eta_plot[d, data$plot_index[i]]
        } else if (plot_mode == "noise") {
          logit_p <- logit_p + rnorm(1, 0, sigma_plot[d])
        }
      }
      
      # Probability and Bernoulli draw
      p <- plogis(logit_p)
      p_mat[d, i] <- p
      y_mat[d, i] <- rbinom(1, 1, p)
    }
  }
  
  list(p = p_mat, y = y_mat)
}


####### Prepare Draws: stan fit object ####################

# Load your Stan fit object
fit_emg <- readRDS("output/fit_emergence_full.rds")

emg_draws <- list(
  alpha           = as.numeric(fit_emg$draws("alpha")),
  beta_neighbors  = as.numeric(fit_emg$draws("beta_neighbors")),
  beta_annual     = as.numeric(fit_emg$draws("beta_annual")),
  beta_perennial  = as.numeric(fit_emg$draws("beta_perennial")),
  beta_shrub      = as.numeric(fit_emg$draws("beta_shrub")),
  beta            = fit_emg$draws("beta", format = "array"),
  beta_0          = fit_emg$draws("beta_0", format = "matrix"),
  mu_beta_soil    = fit_emg$draws("mu_beta_soil", format = "matrix"),
  sigma_site_year = as.numeric(fit_emg$draws("sigma_site_year")),
  sigma_plot      = as.numeric(fit_emg$draws("sigma_plot")),
  site_year_effect = fit_emg$draws("site_year_effect_train_scaled_centered", format = "matrix"),
  eta_plot        = fit_emg$draws("eta_plot_centered", format = "matrix")
)

# Flatten if needed (same as fecundity workflow)
flatten_array <- function(x) {
  dims <- dim(x)
  if (length(dims) == 4) dim(x) <- c(dims[1]*dims[2], dims[3], dims[4])
  else if (length(dims) == 3) dim(x) <- c(dims[1]*dims[2], dims[3])
  else x
}
emg_draws$beta <- flatten_array(emg_draws$beta)
emg_draws$beta_0 <- flatten_array(emg_draws$beta_0)
emg_draws$mu_beta_soil <- flatten_array(emg_draws$mu_beta_soil)
emg_draws$site_year_effect <- flatten_array(emg_draws$site_year_effect)
emg_draws$eta_plot <- flatten_array(emg_draws$eta_plot)

# Dimensions
n_draws <- dim(emg_draws$beta)[1]
n_genotypes <- dim(emg_draws$beta)[2]
n_covariates <- dim(emg_draws$beta)[3]
emg_draws$beta <- array(emg_draws$beta, dim = c(n_draws, n_genotypes, n_covariates))

####### Prepare draws .csv files ###########
# Base R reading — avoids any package masking issues
csv_dir <- "/Users/Becca/Desktop/Adler Lab/from megan/emerged_full"
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

# Read CSVs using base read.csv
csv_list <- lapply(csv_files, function(f) read.csv(f, stringsAsFactors = FALSE))

# Name the list elements by file name
names(csv_list) <- tools::file_path_sans_ext(basename(csv_files))

# Quick check
lapply(csv_list, dim)




# --- Example: converting them to arrays like the RDS draws ---
# This depends on the structure of your CSVs
# Suppose each CSV is one variable's posterior draws (rows = draws, columns = observations)

convert_csv_to_draw_matrix <- function(df) {
  as.matrix(df)  # rows = draws, columns = observations
}

posterior_draws <- map(csv_list, convert_csv_to_draw_matrix)

# Now posterior_draws is a named list like: list(alpha = ..., beta = ..., etc.)
# You may need to rename the list elements to match what your prediction function expects:
# Example:
# names(posterior_draws) <- c("alpha", "beta_0", "beta", "mu_beta_soil", "sigma_site_year", "sigma_plot", "site_year_effect", "eta_plot")

# Make sure each array matches the expected dimensions
map(posterior_draws, dim)

# Then you can feed this into your Bernoulli prediction function
# Example for train data:
train_data <- make_predict_data(stan_data_emg_full, "train", full = TRUE)

pred_train_from_csv <- predict_bernoulli(
  draws = posterior_draws,
  data = train_data,
  W = stan_data_emg_full$W_plant,
  W_soil = stan_data_emg_full$W_soil,
  beta = posterior_draws$beta,
  beta_0 = posterior_draws$beta_0,
  mu_beta_soil = posterior_draws$mu_beta_soil,
  sigma_site_year = posterior_draws$sigma_site_year,
  sigma_plot = posterior_draws$sigma_plot,
  site_year_effect = posterior_draws$site_year_effect,
  eta_plot = posterior_draws$eta_plot,
  site_year_mode = "conditional",
  plot_mode = "conditional"
)

# Save predictions
saveRDS(pred_train_from_csv, "output/pred_train_from_csv.rds")
cat("✅ Predictions from CSV inputs saved.\n")



####### Covariate Matrices ################
W      <- stan_data_emg_full$W_plant
W_soil <- stan_data_emg_full$W_soil


####### Prepare Data for Prediction #######
make_predict_data <- function(stan_data, train_or_test = "train", full = FALSE) {
  suffix <- if(full) "_full" else ""
  data_list <- list(
    n_obs        = stan_data[[paste0("n_", train_or_test, suffix)]],
    genotype     = stan_data[[paste0("genotype_plant_", train_or_test, suffix)]],
    idx_plant    = stan_data[[paste0("idx_plant_", train_or_test, suffix)]],
    idx_plant_site = stan_data[[paste0("idx_plant_", train_or_test, "_site", suffix)]],
    site_year_id = stan_data[[paste0("site_year_id_", train_or_test, suffix)]],
    plot_index   = stan_data[[paste0("plot_index_", train_or_test, suffix)]]
  )
  optional_vars <- c("neighbors", "annual", "perennial", "shrub")
  for(var in optional_vars){
    var_name <- paste0(var, "_", train_or_test, suffix)
    if(!is.null(stan_data[[var_name]])) data_list[[var]] <- stan_data[[var_name]]
  }
  data_list
}

train_data      <- make_predict_data(stan_data_emg_full, "train")
train_full_data <- make_predict_data(stan_data_emg_full, "train", full = TRUE)
test_data       <- make_predict_data(stan_data_emg_full, "test")
test_full_data  <- make_predict_data(stan_data_emg_full, "test", full = TRUE)


####### Generate Posterior Predictive ########

# 1) Train (conditional site-year, conditional plot)
pred_train <- predict_bernoulli(emg_draws, train_data, W, W_soil,
                                beta=emg_draws$beta, beta_0=emg_draws$beta_0,
                                mu_beta_soil=emg_draws$mu_beta_soil,
                                sigma_site_year=emg_draws$sigma_site_year,
                                sigma_plot=emg_draws$sigma_plot,
                                site_year_effect=emg_draws$site_year_effect,
                                eta_plot=emg_draws$eta_plot,
                                site_year_mode="conditional",
                                plot_mode="conditional")

# 2) Test (noise site-year, conditional plot)
pred_test <- predict_bernoulli(emg_draws, test_data, W, W_soil,
                               beta=emg_draws$beta, beta_0=emg_draws$beta_0,
                               mu_beta_soil=emg_draws$mu_beta_soil,
                               sigma_site_year=emg_draws$sigma_site_year,
                               sigma_plot=emg_draws$sigma_plot,
                               eta_plot=emg_draws$eta_plot,
                               site_year_mode="noise",
                               plot_mode="conditional")

# 3) Train fixed (noise site-year, noise plot)
pred_train_fixed <- predict_bernoulli(emg_draws, train_data, W, W_soil,
                                      beta=emg_draws$beta, beta_0=emg_draws$beta_0,
                                      mu_beta_soil=emg_draws$mu_beta_soil,
                                      sigma_site_year=emg_draws$sigma_site_year,
                                      sigma_plot=emg_draws$sigma_plot,
                                      site_year_mode="noise",
                                      plot_mode="noise")

# 4) Full train (conditional)
pred_train_full <- predict_bernoulli(emg_draws, train_full_data, W, W_soil,
                                     beta=emg_draws$beta, beta_0=emg_draws$beta_0,
                                     mu_beta_soil=emg_draws$mu_beta_soil,
                                     sigma_site_year=emg_draws$sigma_site_year,
                                     sigma_plot=emg_draws$sigma_plot,
                                     site_year_effect=emg_draws$site_year_effect,
                                     eta_plot=emg_draws$eta_plot,
                                     site_year_mode="conditional",
                                     plot_mode="conditional")

# 5) Full train fixed (noise)
pred_train_full_fixed <- predict_bernoulli(emg_draws, train_full_data, W, W_soil,
                                           beta=emg_draws$beta, beta_0=emg_draws$beta_0,
                                           mu_beta_soil=emg_draws$mu_beta_soil,
                                           sigma_site_year=emg_draws$sigma_site_year,
                                           sigma_plot=emg_draws$sigma_plot,
                                           site_year_mode="noise",
                                           plot_mode="noise")

# 6) Full test (noise site-year, conditional plot)
pred_test_full <- predict_bernoulli(emg_draws, test_full_data, W, W_soil,
                                    beta=emg_draws$beta, beta_0=emg_draws$beta_0,
                                    mu_beta_soil=emg_draws$mu_beta_soil,
                                    sigma_site_year=emg_draws$sigma_site_year,
                                    sigma_plot=emg_draws$sigma_plot,
                                    eta_plot=emg_draws$eta_plot,
                                    site_year_mode="noise",
                                    plot_mode="conditional")


####### Save Outputs ########################
saveRDS(
  list(
    train = pred_train,
    test = pred_test,
    train_fixed = pred_train_fixed,
    train_full = pred_train_full,
    train_full_fixed = pred_train_full_fixed,
    test_full = pred_test_full
  ),
  "output/predictions_all_emergence.rds"
)

cat("✅ All emergence/reproduction posterior predictive quantities generated and saved.\n")


####### CSV-to-Prediction Workflow ####################

library(dplyr)

# --- 1) Paths to CSVs -------------------
csv_dir <- "/Users/Becca/Desktop/Adler Lab/from megan/emerged_full"
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

# Name by chain number (optional, ensures order)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))
csv_files <- csv_files[order(names(csv_files))]

# --- 2) Read CSVs -------------------
# Base R read.csv avoids package masking issues
csv_list <- lapply(csv_files, function(f) read.csv(f, stringsAsFactors = FALSE))

# Quick check
lapply(csv_list, dim)

# --- 3) Combine chains -------------------
# Each CSV = one chain. Combine vertically (rbind)
combined_draws <- do.call(rbind, lapply(csv_list, as.matrix))

# Quick check
dim(combined_draws)
head(combined_draws, 5)

# --- 4) Assign to posterior_draws -------------------
# Example if this CSV corresponds to 'alpha'
posterior_draws <- list(alpha = combined_draws)
posterior_draws$beta_0 <- do.call(rbind, lapply(beta0_csv_list, as.matrix))
posterior_draws$beta <- do.call(rbind, lapply(beta_csv_list, as.matrix))
posterior_draws$mu_beta_soil <- do.call(rbind, lapply(mu_csv_list, as.matrix))
posterior_draws$sigma_site_year <- do.call(rbind, lapply(sigma_sy_csv_list, as.matrix))
posterior_draws$sigma_plot <- do.call(rbind, lapply(sigma_plot_csv_list, as.matrix))
posterior_draws$site_year_effect <- do.call(rbind, lapply(sy_effect_csv_list, as.matrix))
posterior_draws$eta_plot <- do.call(rbind, lapply(eta_plot_csv_list, as.matrix))

## hard to locate objects 

# --- 5) Flatten multi-dimensional arrays if needed -------------------
flatten_array <- function(x) {
  dims <- dim(x)
  if (length(dims) == 4) dim(x) <- c(dims[1]*dims[2], dims[3], dims[4])
  else if (length(dims) == 3) dim(x) <- c(dims[1]*dims[2], dims[3])
  else x
}

# Example flattening for multi-dimensional parameters
# posterior_draws$beta <- flatten_array(posterior_draws$beta)
# posterior_draws$beta_0 <- flatten_array(posterior_draws$beta_0)
# posterior_draws$mu_beta_soil <- flatten_array(posterior_draws$mu_beta_soil)
# posterior_draws$site_year_effect <- flatten_array(posterior_draws$site_year_effect)
# posterior_draws$eta_plot <- flatten_array(posterior_draws$eta_plot)

# --- 6) Prepare data for prediction -------------------
make_predict_data <- function(stan_data, train_or_test = "train", full = FALSE) {
  suffix <- if(full) "_full" else ""
  data_list <- list(
    n_obs        = stan_data[[paste0("n_", train_or_test, suffix)]],
    genotype     = stan_data[[paste0("genotype_plant_", train_or_test, suffix)]],
    idx_plant    = stan_data[[paste0("idx_plant_", train_or_test, suffix)]],
    idx_plant_site = stan_data[[paste0("idx_plant_", train_or_test, "_site", suffix)]],
    site_year_id = stan_data[[paste0("site_year_id_", train_or_test, suffix)]],
    plot_index   = stan_data[[paste0("plot_index_", train_or_test, suffix)]]
  )
  optional_vars <- c("neighbors", "annual", "perennial", "shrub")
  for(var in optional_vars){
    var_name <- paste0(var, "_", train_or_test, suffix)
    if(!is.null(stan_data[[var_name]])) data_list[[var]] <- stan_data[[var_name]]
  }
  data_list
}

train_data <- make_predict_data(stan_data_emg_full, "train", full = TRUE)
W <- stan_data_emg_full$W_plant
W_soil <- stan_data_emg_full$W_soil

# --- 7) Run predictions -------------------
pred_train_from_csv <- predict_bernoulli(
  draws = posterior_draws,
  data = train_data,
  W = W,
  W_soil = W_soil,
  beta = posterior_draws$beta,
  beta_0 = posterior_draws$beta_0,
  mu_beta_soil = posterior_draws$mu_beta_soil,
  sigma_site_year = posterior_draws$sigma_site_year,
  sigma_plot = posterior_draws$sigma_plot,
  site_year_effect = posterior_draws$site_year_effect,
  eta_plot = posterior_draws$eta_plot,
  site_year_mode = "conditional",
  plot_mode = "conditional"
)

# --- 8) Save predictions -------------------
saveRDS(pred_train_from_csv, "output/pred_train_from_csv.rds")
cat("✅ Predictions from CSV inputs saved.\n")
