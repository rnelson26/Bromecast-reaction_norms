library(tidyverse)
library(bayesplot)
library(cmdstanr)
library(posterior)

### Set directory
path <- "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms"

### Load data
stan_data <- readRDS(file.path(path, "stan_data_fec_full.rds"))

### Compile the model
mod       <- cmdstan_model(file.path(path, "ztnb_glm_fecundity.stan"))

### Get starting values
pf <- mod$pathfinder(
  data = stan_data,
  num_paths = 1
)

### Fit model, using pathfinder for good starting values
fit       <- mod$sample(
  data = stan_data,
  init = pf,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = 200,
  iter_sampling = 1000,
)

### List model parameters
params <- c("theta", "mu_beta", "mu_beta_soil",
            "alpha", "sigma_plot", "sigma_site_year",
            "beta_neighbors", "beta_annual", "beta_perennial", "beta_shrub")

### Summarize parameters
params_summary <- fit$summary(params)

### Some traceplots
draws <- fit$draws(params)
mcmc_trace(draws)

mu_test_sum <- fit$summary("mu_test")
mu_train_sum <- fit$summary("mu_train")
y_train_pred_sum <- fit$summary("y_train_pred")
y_test_pred_sum <- fit$summary("y_test_pred")
y_train_pred <-
  fit$draws("y_train_pred") %>%
  as_draws_matrix()
y_test_pred <-
  fit$draws("y_test_pred") %>%
  as_draws_matrix()
mu_test <-
  fit$draws("mu_test") %>%
  as_draws_matrix()
mu_train <-
  fit$draws("mu_train") %>%
  as_draws_matrix()

mean(y_test_pred)
mean(mu_test)
mean(y_train_pred)
mean(mu_train)
