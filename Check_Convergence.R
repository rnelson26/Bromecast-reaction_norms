#### Integrated Reaction Norm Model ########
######## Check model convergence and priors ##########
######## code by Becca Nelson and Justin Van Ee ###############################
############# created 3-25-25 ######################
############# Last modified: 10-15-25 ##########################

### load data 
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")


mod_fec <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/fecundity_model_checking.stan")

mod_rep <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/reproduced_model_checking.stan")
mod_emg <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/emerged_model_checking.stan")

###### Fit Models  ######
# emergence 
pathfinder_emg <- mod_emg$pathfinder(data = stan_data_emg_full, init = 0, num_paths = 1)
init_emg <- pathfinder_emg$draws(format = "list")

fit_emg <- mod_emg$sample(
  data = stan_data_emg_full,
  init = init_emg,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000,
  seed = 123
)

# reproduction 
pathfinder_rep <- mod_rep$pathfinder(data = stan_data_rep_full, init = 0, num_paths = 1)
init_rep <- pathfinder_rep$draws(format = "list")

fit_rep <- mod_rep$sample(
  data = stan_data_rep_full,
  init = init_rep,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000,
  seed = 123
)

# fecundity 
#pathfinder_fec <- mod_fec$pathfinder(data = stan_data_fec_full, init = 0, num_paths = 1)
#init_fec <- pathfinder_fec$draws(format = "list")

## with warmup based on empirical values:
init_fec <- function() {
  data <- stan_data_fec_full
  
  y_mean <- mean(data$y_train)
  y_var <- var(data$y_train)
  disp_guess <- max(1, y_mean^2 / (y_var - y_mean))  # rough NB dispersion estimate
  
## warm up 
  lm_fit <- lm(log(data$y_train + 1) ~ data$neighbors_train + data$annual_train + 
                 data$perennial_train + data$shrub_train)
  coefs <- coef(lm_fit)
  
  alpha_init <- unname(coefs[1])
  beta_neighbors_init <- if ("neighbors_train" %in% names(coefs)) coefs["neighbors_train"] else 0
  beta_annual_init <- if ("annual_train" %in% names(coefs)) coefs["annual_train"] else 0
  beta_perennial_init <- if ("perennial_train" %in% names(coefs)) coefs["perennial_train"] else 0
  beta_shrub_init <- if ("shrub_train" %in% names(coefs)) coefs["shrub_train"] else 0
  
  list(
    beta_raw = matrix(0, nrow = data$n_g, ncol = data$q_X),
    mu_beta = rep(0, data$q_X),
    beta_soil_raw = matrix(0, nrow = data$n_g, ncol = data$q_X_soil),
    mu_beta_soil = rep(0, data$q_X_soil),
    u_sigma = rep(atan(0.5), data$p_X),
    u_sigma_soil = rep(atan(0.5), data$s_X),
    u_zeta = rep(atan(0.5), data$q_X),
    u_zeta_soil = rep(atan(0.5), data$q_X_soil),
    W = matrix(rnorm(data$n_X * data$q_X, 0, 0.1), data$n_X, data$q_X),
    W_soil = matrix(rnorm(data$n_X_soil * data$q_X_soil, 0, 0.1), data$n_X_soil, data$q_X_soil),
    beta_neighbors = beta_neighbors_init,
    beta_annual = beta_annual_init,
    beta_perennial = beta_perennial_init,
    beta_shrub = beta_shrub_init,
    theta = disp_guess,
    site_year_effect_train_raw = rep(0, data$n_site_year_train),
    sigma_site_year = 1,
    eta_plot_raw = rep(0, data$n_plot),
    sigma_plot = 1,
    alpha = alpha_init,
    beta_0_raw = rep(0, data$n_g),
    u_zeta_0 = atan(0.5)
  )
}


#init_fec <- pathfinder_fec$draws(format = "list")

pathfinder_fit <- mod_fec$pathfinder(
  data = stan_data_fec_full,
  num_paths = 1,
  init = init_fec
)


fit_fec <- mod_fec$sample(
  data = stan_data_fec_full,
  init = init_fec,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000,
  seed = 123
)

####### Traceplots ##################
# Extract posterior samples
posterior <- fit_fec$draws(variables = c("theta","beta", "sigma", "W", "zeta",  "W_soil"))

posterior_rep <- fit_rep$draws(variables = c("beta", "sigma", "W", "zeta", "W_soil"))

posterior_emg_full <- fit_emg$draws(variables = c("beta", "sigma", "W", "zeta",  "W_soil"))

iter_warmup <- 500  
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


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


# sigma
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)



# zeta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


# W
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# W soil
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


# mu
#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior,  pars = c("mu_train[191]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_rep,  pars = c("mu_train[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_rep,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)



#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior,  pars = c("p_train[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior,  pars = c("p_test[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_rep,  pars = c("p_train[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_rep,  pars = c("p_test[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_emg_full,  pars = c("p_train[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_emg_full,  pars = c("p_test[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_emg_fall,  pars = c("p_train[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior_emg_fall,  pars = c("p_test[199]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

##### rhat############
library(posterior)
#R-hat < 1.01: excellent convergence
#1.01–1.05 → okay but worth checking
#1.05 → potential non-convergence

# Fecundity model
rhat_fec <- fit_fec$summary() |> dplyr::select(variable, rhat)
print(head(rhat_fec, 10))  #first 10
mean(rhat_fec$rhat, na.rm = TRUE)  # mean Rhat = 1.00271
hist(rhat_fec$rhat)

# Reproduction model
rhat_rep <- fit_rep$summary() |> dplyr::select(variable, rhat)
print(head(rhat_rep, 10))
mean(rhat_rep$rhat, na.rm = TRUE) #mean Rhat = 1.001342
hist(rhat_rep$rhat) 

# Emergence model
rhat_emg_full <- fit_emg$summary() |> dplyr::select(variable, rhat)
print(head(rhat_emg_full, 10))
mean(rhat_emg_full$rhat, na.rm = TRUE) #1.002497
hist(rhat_emg_full$rhat) 

###### Look at fecundity parameter values for warm start: 
library(posterior)

draws_prev <- as_draws_df(fit_fec$draws())
means_prev <- summarise_draws(fit_fec$draws(), mean)
names_prev <- means_prev$variable
values_prev <- means_prev$mean
names(values_prev) <- names_prev
#write.csv(values_prev, "data/prev_fec_means_for_warm_start.csv", row.names = TRUE)

