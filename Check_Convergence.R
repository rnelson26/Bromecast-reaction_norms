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

init_fun <- function() list(
  beta = matrix(0, nrow = data$P, ncol = data$T),
  L_Omega = diag(1, data$T),
  tau_trait = rep(1, data$T),
  sigma_g_env = rep(1, data$E),
  u = array(0, dim = c(data$E, data$G, data$T)),
  u_mat = array(0, dim = c(data$G * data$T, data$E)),
  sigma = matrix(1, nrow = data$T, ncol = data$E),
  sigma_sigma = rep(0.1, data$T),
  mu_sigma = rep(0, data$T)
)
pathfinder_fit <- mod$pathfinder(
  data = data,
  num_paths = 1,
  init = init_fun
)
#just got to make sure names and dimension match will the model statement in stan.

# 1. Extract parameter names and dimensions from the compiled Stan model
stan_pars_dims <- lapply(mod_rep$metadata()$stan_variable_dims, function(d) {
  if (length(d) == 0) return(NULL) else d
})

# 2. Create a function to generate mean-based initial values
make_mean_init_auto <- function(prev_values, stan_dims) {
  # Split previous draws by base parameter name
  params_split <- split(prev_values, gsub("\\[.*\\]", "", names(prev_values)))
  
  init_list <- lapply(names(params_split), function(param_name) {
    param_vals <- as.numeric(params_split[[param_name]])
    
    dims <- stan_dims[[param_name]]
    
    if (is.null(dims)) {
      # scalar or vector
      rep(mean(param_vals, na.rm = TRUE), length(param_vals))
    } else {
      # array or matrix
      array(mean(param_vals, na.rm = TRUE), dim = dims)
    }
  })
  names(init_list) <- names(params_split)
  init_list
}

# 3. Apply function to create a single-chain init
init_chain <- list(make_mean_init_auto(values_prev, stan_pars_dims))

# 4. Run Pathfinder with these mean-based initial values
pathfinder_rep <- mod_rep$pathfinder(
  data = stan_data_rep_full,
  init = init_chain,
  num_paths = 1
)



params_split <- split(values_prev, gsub("\\[.*\\]", "", names(values_prev)))
init_fec_list <- lapply(params_split, function(x) as.numeric(x))
make_mean_init <- function(init_list) {
  lapply(init_list, function(param) {
    if (is.numeric(param)) {
      # replace all entries with the mean of that parameter
      rep(mean(param, na.rm = TRUE), length(param))
    } else {
      param  # leave non-numeric parameters as is
    }
  })
}
# wrap as a single chain
init_fec_chain_mean <- list(make_mean_init(init_fec_list))
pathfinder_fec <- mod_fec$pathfinder(
  data = stan_data_fec_full,
  init = init_fec_chain_mean,
  num_paths = 1
)







### prev

# 1. Split previous draws by base parameter name
params_split <- split(values_prev, gsub("\\[.*\\]", "", names(values_prev)))

# 2. Convert each group to numeric (vector or scalar)
init_fec_list <- lapply(params_split, function(x) as.numeric(x))

# 3. Wrap as a list for each chain
# For example, 1 chain here
init_fec_chain <- list(init_fec_list)

# Function to create mean-based initial values
mean_init <- function(init_list) {
  lapply(init_list, function(chain) {
    lapply(chain, function(param) {
      if (is.numeric(param)) {
        # Replace all entries with the mean of the original vector
        rep(mean(param, na.rm = TRUE), length(param))
      } else {
        param  # leave non-numeric parameters as is
      }
    })
  })
}

# Apply to your existing init_fec_chain
init_fec_chain_mean <- mean_init(init_fec_chain)


# 4. Now pass to pathfinder
pathfinder_fec <- mod_fec$pathfinder(
  data = stan_data_fec_full,
  init = init_fec_chain,
  num_paths = 1
)

# 3. Wrap as a list for chains
init_fec_chain <- list(init_fec_list)

# 4. Pass to pathfinder
pathfinder_fec <- mod_fec$pathfinder(
  data = stan_data_fec_full,
  init = init_fec_chain,
  num_paths = 1
)

init_fec_chain <- make_init_from_values(values_prev)

pathfinder_fec <- mod_fec$pathfinder(
  data = stan_data_fec_full,
  init = init_fec_chain,
  num_paths = 1
)



# Now you can pass this to pathfinder
pathfinder_fec <- mod_fec$pathfinder(
  data = stan_data_fec_full,
  init = init_fec,
  num_paths = 1
)

# Extract draws for sampling
init_fec_draws <- pathfinder_fec$draws(format = "list")

init_fun <- function() as.list(values_prev)
pathfinder_fec <- mod_fec$pathfinder(
  data = stan_data_fec_full,
  init = init_fun,       
  num_paths = 1
)
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
## chains look funky when widening priors

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
## chains look funky

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
#chains are funky

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
#chains funky

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
#chains look funky

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
mean(rhat_fec$rhat, na.rm = TRUE)  # mean Rhat = 1.743881
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

