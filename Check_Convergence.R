#### Integrated Reaction Norm Model ########
######## Check model convergence and priors ##########
######## code by Becca Nelson and Justin Van Ee ###############################
############# created 3-25-25 ######################
############# Last modified: 10-10-25 ##########################

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
pathfinder_fec <- mod_fec$pathfinder(data = stan_data_fec_full, init = 0, num_paths = 1)
init_fec <- pathfinder_fec$draws(format = "list")

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
posterior <- fit_fec$draws(variables = c("theta","beta", "sigma", "W", "zeta", "mu_test", "mu_train", "W_soil"))

posterior_rep <- fit_rep$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))

posterior_emg_full <- fit_emg_full$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))


#
# Traceplots for diagnostics
#

# theta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("theta"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("theta"), n_warmup = iter_warmup)
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

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
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

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = paste0("sigma[", 1:ncol(X_emg_fall), "]"), n_warmup = iter_warmup)
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

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
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

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# mu
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu_train[191]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("mu_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)



color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

##### rhat############
library(posterior)

# Fecundity model
rhat_fec <- fit_fec$summary() |> dplyr::select(variable, rhat)
print(head(rhat_fec, 10))  # peek at first 10
mean(rhat_fec$rhat, na.rm = TRUE)  # mean Rhat

# Reproduction model
rhat_rep <- fit_rep$summary() |> dplyr::select(variable, rhat)
mean(rhat_rep$rhat, na.rm = TRUE)

# Emergence model
rhat_emg_full <- fit_emg_full$summary() |> dplyr::select(variable, rhat)
mean(rhat_emg_full$rhat, na.rm = TRUE)

