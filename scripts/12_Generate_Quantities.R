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
  n_obs   <- nrow(data)
  
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
  
  ## random effects centered 
  eta_site_year   = fit_fec_full$draws("eta_site_year", format = "matrix"),
  eta_plot        = fit_fec_full$draws("eta_plot", format = "matrix")
)

saveRDS(fec_draws, "output/fecundity_draws_extracted.rds")

fec_draws <- readRDS("output/fecundity_draws_extracted.rds")

####### Generate Quantities for Posterior Predictive ###########


## 1) Training (conditional site-year, conditional plot)
mu_train <- predict_ztnb(
  draws = draws,
  data = train_data,
  W = W, W_soil = W_soil,
  beta = beta, beta_0 = beta_0,
  mu_beta_soil = mu_beta_soil,
  theta = theta,
  sigma_site_year = sigma_site_year,
  sigma_plot = sigma_plot,
  site_year_effect = site_year_effect_train_scaled_centered,
  eta_plot = eta_plot_centered,
  site_year_mode = "conditional",
  plot_mode = "conditional"
)

## 2) Test (noise site-year, conditional plot)
mu_test <- predict_ztnb(
  draws = draws,
  data = test_data,
  W = W, W_soil = W_soil,
  beta = beta, beta_0 = beta_0,
  mu_beta_soil = mu_beta_soil,
  theta = theta,
  sigma_site_year = sigma_site_year,
  sigma_plot = sigma_plot,
  eta_plot = eta_plot_centered,
  site_year_mode = "noise",       # site_year noise
  plot_mode = "conditional"       # plot effect conditional
)

## 3) Training fixed (noise site-year, noise plot)
mu_train_fixed <- predict_ztnb(
  draws = draws,
  data = train_data,
  W = W, W_soil = W_soil,
  beta = beta, beta_0 = beta_0,
  mu_beta_soil = mu_beta_soil,
  theta = theta,
  sigma_site_year = sigma_site_year,
  sigma_plot = sigma_plot,
  site_year_mode = "noise",
  plot_mode = "noise"
)

## 4) Full training (conditional site-year, conditional plot)
mu_train_full <- predict_ztnb(
  draws = draws,
  data = train_full_data,
  W = W, W_soil = W_soil,
  beta = beta, beta_0 = beta_0,
  mu_beta_soil = mu_beta_soil,
  theta = theta,
  sigma_site_year = sigma_site_year,
  sigma_plot = sigma_plot,
  site_year_effect = site_year_effect_train_scaled_centered,
  eta_plot = eta_plot_centered,
  site_year_mode = "conditional",
  plot_mode = "conditional"
)

## 5) Full training fixed (noise site-year, noise plot)
mu_train_full_fixed <- predict_ztnb(
  draws = draws,
  data = train_full_data,
  W = W, W_soil = W_soil,
  beta = beta, beta_0 = beta_0,
  mu_beta_soil = mu_beta_soil,
  theta = theta,
  sigma_site_year = sigma_site_year,
  sigma_plot = sigma_plot,
  site_year_mode = "noise",
  plot_mode = "noise"
)

## 6) Full test (noise site-year, conditional plot)
mu_test_full <- predict_ztnb(
  draws = draws,
  data = test_full_data,
  W = W, W_soil = W_soil,
  beta = beta, beta_0 = beta_0,
  mu_beta_soil = mu_beta_soil,
  theta = theta,
  sigma_site_year = sigma_site_year,
  sigma_plot = sigma_plot,
  eta_plot = eta_plot_centered,
  site_year_mode = "noise",
  plot_mode = "conditional"
)


####### save predictions #################
saveRDS(
  list(
    mu_train = mu_train,
    mu_test = mu_test,
    mu_train_fixed = mu_train_fixed,
    mu_train_full = mu_train_full,
    mu_train_full_fixed = mu_train_full_fixed,
    mu_test_full = mu_test_full
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