############### Bromecast: 12. Generate Quantities ##########################
############# created 1-6-26 ######################
############# Last modified: 1-7-26 ##########################
######## Extracts generated quantities in R instead of Stan ################################
########### code by R. Nelson ###############

########### Fecundity #############

# Stan GQ block         | R call                                                  |
  # `mu_train`            | `site_year_mode="conditional", plot_mode="conditional"` |
  # `mu_test`             | `site_year_mode="noise", plot_mode="noise"`             |
  # `mu_train_fixed`      | `site_year_mode="noise", plot_mode="noise"`             |
 
 # `mu_train_full`       | `site_year_mode="conditional", plot_mode="conditional"` |
  # `mu_train_full_fixed` | `site_year_mode="noise", plot_mode="noise"`             |
  ## mu_test_full         |site_year =  "noise", plot_mode = "conditional          |

###### load packages and data ############

library(cmdstanr)
library(dplyr)


#source("scripts/04_setup.R")
#source("scripts/05_prepare_data.R")
#source("scripts/06_prepare_standata_emg.R")
#source("scripts/07_prepare_standata_rep.R")
#source("scripts/08_prepare_standata_fec.R")

###### define functions ################

# Negative binomial helper
rztnegbin <- function(n, mu, theta) {
  y <- rnbinom(n, size = theta, mu = mu)
  while (any(y == 0)) {
    idx <- which(y == 0)
    y[idx] <- rnbinom(length(idx), size = theta, mu = mu)
  }
  y
}

# Fecundity prediction function
predict_ztnb <- function(
    draws,
    data,
    W, W_soil,
    beta,
    beta_0,
    mu_beta_soil,
    theta,
    sigma_site_year,
    sigma_plot,
    site_year_effect = NULL,
    eta_plot = NULL,
    site_year_mode = c("conditional", "noise", "none"),
    plot_mode = c("conditional", "noise", "none"),
    mu_cap = 10,
    return_mu = TRUE
) {
  
  site_year_mode <- match.arg(site_year_mode)
  plot_mode <- match.arg(plot_mode)
  
  n_draws <- length(theta)
  n_obs   <- data$n_obs  
  
  mu_mat <- matrix(NA_real_, n_draws, n_obs)
  y_mat  <- matrix(NA_integer_, n_draws, n_obs)
  
  
  for (d in seq_len(n_draws)) {
    for (i in seq_len(n_obs)) {
      
      g        <- data$genotype[i]
      idx      <- data$idx_plant[i]
      idx_site <- data$idx_plant_site[i]
      
      lp <-
        draws$alpha[d] +
        sum(W[idx, ] * beta[d, g, ]) +
        sum(W_soil[idx_site, ] * mu_beta_soil[d, ]) +
        beta_0[d, g] +
        draws$beta_neighbors[d]  * data$neighbors[i] +
        draws$beta_annual[d]     * data$annual[i] +
        draws$beta_perennial[d]  * data$perennial[i] +
        draws$beta_shrub[d]      * data$shrub[i]
      
      if (site_year_mode == "conditional" && !is.null(site_year_effect)) {
        lp <- lp + site_year_effect[d, data$site_year_id[i]]
      }
      
      if (site_year_mode == "noise") {
        lp <- lp + rnorm(1, 0, sigma_site_year[d])
      }
      
      if (data$plot_index[i] != 0) {
        if (plot_mode == "conditional" && !is.null(eta_plot)) {
          lp <- lp + eta_plot[d, data$plot_index[i]]
        }
        if (plot_mode == "noise") {
          lp <- lp + rnorm(1, 0, sigma_plot[d])
        }
      }
      
      mu <- exp(pmin(lp, mu_cap))
      
      mu_mat[d, i] <- mu
      y_mat[d, i]  <- rztnegbin(1, mu, theta[d])
    }
  }
  
  if (return_mu) {
    list(mu = mu_mat, y = y_mat)
  } else {
    y_mat
  }
}

######## load fitted model objects ######################


fit_fec_full <- readRDS("output/fit_fecundity_full.rds")




######## Extract posterior draws for generating quanitites ########

## Scalar fixed effects
# ---- Extract ALL needed draws in one go ----

fec_draws <- list(
  
  ## fixed effects
  alpha           = as.numeric(fit_fec_full$draws("alpha")),
  beta_neighbors  = as.numeric(fit_fec_full$draws("beta_neighbors")),
  beta_annual     = as.numeric(fit_fec_full$draws("beta_annual")),
  beta_perennial  = as.numeric(fit_fec_full$draws("beta_perennial")),
  beta_shrub      = as.numeric(fit_fec_full$draws("beta_shrub")),
  
  ## genotype effects 
  beta            = fit_fec_full$draws("beta", format = "array"),
  beta_0          = fit_fec_full$draws("beta_0", format = "matrix"),
  
  ## soil effects
  mu_beta_soil    = fit_fec_full$draws("mu_beta_soil", format = "matrix"),
  
  ## negative binomial dispersion
  theta           = as.numeric(fit_fec_full$draws("theta")),
  
  ## random-effect standard deviations 
  sigma_site_year = as.numeric(fit_fec_full$draws("sigma_site_year")),
  sigma_plot      = as.numeric(fit_fec_full$draws("sigma_plot")),
  
  ## random effects 
  site_year_effect = fit_fec_full$draws(
    "site_year_effect_train_scaled_centered",
    format = "matrix"
  ),
  
  eta_plot = fit_fec_full$draws(
    "eta_plot_centered",
    format = "matrix"
  )
)

# --- Flatten posterior draws to remove the chains dimension ---
# --- Flatten chains to match predict_ztnb expected dimensions ---
flatten_array <- function(x) {
  dims <- dim(x)
  
  if (length(dims) == 4) {
    # iter x chain x dim2 x dim3 -> (iter*chain) x dim2 x dim3
    dim(x) <- c(dims[1]*dims[2], dims[3], dims[4])
    return(x)
    
  } else if (length(dims) == 3) {
    # iter x chain x dim2 -> (iter*chain) x dim2
    dim(x) <- c(dims[1]*dims[2], dims[3])
    return(x)
    
  } else if (length(dims) == 2) {
    # iter x chain? just flatten first dimension if needed
    return(x)
    
  } else {
    stop("Unexpected array shape in flatten_array")
  }
}

# Apply flattening
fec_draws$beta             <- flatten_array(fec_draws$beta)        # draws x genotype x covariate
fec_draws$beta_0           <- flatten_array(fec_draws$beta_0)      # draws x genotype
fec_draws$mu_beta_soil     <- flatten_array(fec_draws$mu_beta_soil)# draws x covariate
fec_draws$site_year_effect <- flatten_array(fec_draws$site_year_effect) # draws x site_year
fec_draws$eta_plot         <- flatten_array(fec_draws$eta_plot)    # draws x plot

n_draws <- dim(fec_draws$beta)[1]
n_genotypes <- 121
n_covariates <- 2

fec_draws$beta <- array(fec_draws$beta, dim = c(n_draws, n_genotypes, n_covariates))
dim(fec_draws$beta)
# [1] 4000 121 2


# Scalars
fec_draws$alpha           <- as.numeric(fec_draws$alpha)
fec_draws$beta_neighbors  <- as.numeric(fec_draws$beta_neighbors)
fec_draws$beta_annual     <- as.numeric(fec_draws$beta_annual)
fec_draws$beta_perennial  <- as.numeric(fec_draws$beta_perennial)
fec_draws$beta_shrub      <- as.numeric(fec_draws$beta_shrub)
fec_draws$theta           <- as.numeric(fec_draws$theta)
fec_draws$sigma_site_year <- as.numeric(fec_draws$sigma_site_year)
fec_draws$sigma_plot      <- as.numeric(fec_draws$sigma_plot)


saveRDS(fec_draws, "output/fecundity_draws_extracted.rds")

fec_draws <- readRDS("output/fecundity_draws_extracted.rds")

###### Load covariate matrices #################

W      <- stan_data_fec_full$W_plant   # plant-level covariates
W_soil <- stan_data_fec_full$W_soil    # soil-level covariates

########## Assign Data for Predict Functions  ################

make_predict_data <- function(stan_data, train_or_test = "train", full = FALSE) {
  
  suffix <- if (full) "_full" else ""
  
  # required columns
  data_list <- list(
    n_obs        = stan_data[[paste0("n_", train_or_test, suffix)]],
    genotype     = stan_data[[paste0("genotype_plant_", train_or_test, suffix)]],
    idx_plant    = stan_data[[paste0("idx_plant_", train_or_test, suffix)]],
    idx_plant_site = stan_data[[paste0("idx_plant_", train_or_test, "_site", suffix)]],
    site_year_id = stan_data[[paste0("site_year_id_", train_or_test, suffix)]],
    plot_index   = stan_data[[paste0("plot_index_", train_or_test, suffix)]]
  )
  
  # optional competition covariates (only include if they exist)
  optional_vars <- c("neighbors", "annual", "perennial", "shrub")
  for (var in optional_vars) {
    var_name <- paste0(var, "_", train_or_test, suffix)
    if (!is.null(stan_data[[var_name]])) {
      data_list[[var]] <- stan_data[[var_name]]
    }
  }
  
  data_list
}

# Fecundity full
train_data       <- make_predict_data(stan_data_fec_full, "train", full = FALSE)
train_full_data  <- make_predict_data(stan_data_fec_full, "train", full = TRUE)
test_data        <- make_predict_data(stan_data_fec_full, "test", full = FALSE)
test_full_data   <- make_predict_data(stan_data_fec_full, "test", full = TRUE)

####### Generate Quantities for Posterior Predictive ###########
## 1) Training (conditional site-year, conditional plot)
mu_train <- predict_ztnb(
  draws = fec_draws,
  data = train_data,
  W = W, W_soil = W_soil,
  beta = fec_draws$beta, 
  beta_0 = fec_draws$beta_0,
  mu_beta_soil = fec_draws$mu_beta_soil,
  theta = fec_draws$theta,
  sigma_site_year = fec_draws$sigma_site_year,
  sigma_plot = fec_draws$sigma_plot,
  site_year_effect = fec_draws$site_year_effect,
  eta_plot = fec_draws$eta_plot,
  site_year_mode = "conditional",
  plot_mode = "conditional"
)

## 2) Test (noise site-year, conditional plot)
mu_test <- predict_ztnb(
  draws = fec_draws,
  data = test_data,
  W = W, W_soil = W_soil,
  beta = fec_draws$beta, 
  beta_0 = fec_draws$beta_0,
  mu_beta_soil = fec_draws$mu_beta_soil,
  theta = fec_draws$theta,
  sigma_site_year = fec_draws$sigma_site_year,
  sigma_plot = fec_draws$sigma_plot,
  eta_plot = fec_draws$eta_plot,
  site_year_mode = "noise",
  plot_mode = "conditional"
)

## 3) Training fixed (noise site-year, noise plot)
mu_train_fixed <- predict_ztnb(
  draws = fec_draws,
  data = train_data,
  W = W, W_soil = W_soil,
  beta = fec_draws$beta, 
  beta_0 = fec_draws$beta_0,
  mu_beta_soil = fec_draws$mu_beta_soil,
  theta = fec_draws$theta,
  sigma_site_year = fec_draws$sigma_site_year,
  sigma_plot = fec_draws$sigma_plot,
  site_year_mode = "noise",
  plot_mode = "noise"
)

## 4) Full training (conditional site-year, conditional plot)
mu_train_full <- predict_ztnb(
  draws = fec_draws,
  data = train_full_data,
  W = W, W_soil = W_soil,
  beta = fec_draws$beta, 
  beta_0 = fec_draws$beta_0,
  mu_beta_soil = fec_draws$mu_beta_soil,
  theta = fec_draws$theta,
  sigma_site_year = fec_draws$sigma_site_year,
  sigma_plot = fec_draws$sigma_plot,
  site_year_effect = fec_draws$site_year_effect,
  eta_plot = fec_draws$eta_plot,
  site_year_mode = "conditional",
  plot_mode = "conditional"
)

## 5) Full training fixed (noise site-year, noise plot)
mu_train_full_fixed <- predict_ztnb(
  draws = fec_draws,
  data = train_full_data,
  W = W, W_soil = W_soil,
  beta = fec_draws$beta, 
  beta_0 = fec_draws$beta_0,
  mu_beta_soil = fec_draws$mu_beta_soil,
  theta = fec_draws$theta,
  sigma_site_year = fec_draws$sigma_site_year,
  sigma_plot = fec_draws$sigma_plot,
  site_year_mode = "noise",
  plot_mode = "noise"
)

## 6) Full test (noise site-year, conditional plot)
mu_test_full <- predict_ztnb(
  draws = fec_draws,
  data = test_full_data,
  W = W, W_soil = W_soil,
  beta = fec_draws$beta, 
  beta_0 = fec_draws$beta_0,
  mu_beta_soil = fec_draws$mu_beta_soil,
  theta = fec_draws$theta,
  sigma_site_year = fec_draws$sigma_site_year,
  sigma_plot = fec_draws$sigma_plot,
  eta_plot = fec_draws$eta_plot,
  site_year_mode = "noise",
  plot_mode = "conditional"
)

####### save predictions #################
saveRDS(
  list(
    mu_train = mu_train,
    mu_test = mu_test,
    mu_train_fixed = mu_train_fixed #,
   # mu_train_full = mu_train_full,
  #  mu_train_full_fixed = mu_train_full_fixed,
  #  mu_test_full = mu_test_full
  ),
  "output/predictions_all_fecundity.rds"
)

cat("✅ All fecundity generated quantities have been generated and saved.\n")


#list(
 # mu = mu_mat,  # matrix: n_draws x n_obs
#  y  = y_mat    # matrix: n_draws x n_obs
#)
#mu — the expected mean of the zero-truncated negative binomial (ZTNB) for each observation.

#This is before the random sampling.
#Dimensions: n_draws × n_obs 
#Can be interpreted as the posterior predictive mean (conditional on parameters and random effects specified in that call).

#y — draws from the ZTNB distribution using rztnegbin().
#Each element is an integer ≥ 1 (because it’s zero-truncated).
#Also dimensions: n_draws × n_obs.
#This is the posterior predictive distribution, the stochastic outcomes you would expect in reality that are used for CRPS. 