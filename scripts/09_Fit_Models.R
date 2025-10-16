################# Bromecast: 09.Fit Models ##########################
############# created 3-25-25 ######################
############# Last modified: 10-16-25 ##########################
######## Fits and Runs all models ################################

# Load data
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")

### save prepped data workspace for HPC use 
#save.image(file = "prepped_workspace.RData")
## too large to easily load to github

###### Run model functions ################

# Warmup to help fecundity models converge 
init_fec <- function(stan_data) {
  y_mean <- mean(stan_data$y_train)
  y_var <- var(stan_data$y_train)
  disp_guess <- max(1, y_mean^2 / (y_var - y_mean))  
  
  lm_fit <- lm(log(stan_data$y_train + 1) ~ 
                 stan_data$neighbors_train +
                 stan_data$annual_train + 
                 stan_data$perennial_train + 
                 stan_data$shrub_train)
  coefs <- coef(lm_fit)
  
  alpha_init <- unname(coefs[1])
  beta_neighbors_init <- if ("stan_data$neighbors_train" %in% names(coefs)) coefs["stan_data$neighbors_train"] else 0
  beta_annual_init <- if ("stan_data$annual_train" %in% names(coefs)) coefs["stan_data$annual_train"] else 0
  beta_perennial_init <- if ("stan_data$perennial_train" %in% names(coefs)) coefs["stan_data$perennial_train"] else 0
  beta_shrub_init <- if ("stan_data$shrub_train" %in% names(coefs)) coefs["stan_data$shrub_train"] else 0
  
  list(
    beta_raw = matrix(0, nrow = stan_data$n_g, ncol = stan_data$q_X),
    mu_beta = rep(0, stan_data$q_X),
    beta_soil_raw = matrix(0, nrow = stan_data$n_g, ncol = stan_data$q_X_soil),
    mu_beta_soil = rep(0, stan_data$q_X_soil),
    u_sigma = rep(atan(0.5), stan_data$p_X),
    u_sigma_soil = rep(atan(0.5), stan_data$s_X),
    u_zeta = rep(atan(0.5), stan_data$q_X),
    u_zeta_soil = rep(atan(0.5), stan_data$q_X_soil),
    W = matrix(rnorm(stan_data$n_X * stan_data$q_X, 0, 0.1), stan_data$n_X, stan_data$q_X),
    W_soil = matrix(rnorm(stan_data$n_X_soil * stan_data$q_X_soil, 0, 0.1), stan_data$n_X_soil, stan_data$q_X_soil),
    beta_neighbors = beta_neighbors_init,
    beta_annual = beta_annual_init,
    beta_perennial = beta_perennial_init,
    beta_shrub = beta_shrub_init,
    theta = disp_guess,
    site_year_effect_train_raw = rep(0, stan_data$n_site_year_train),
    sigma_site_year = 1,
    eta_plot_raw = rep(0, stan_data$n_plot),
    sigma_plot = 1,
    alpha = alpha_init,
    beta_0_raw = rep(0, stan_data$n_g),
    u_zeta_0 = atan(0.5)
  )
}

# Run a single model 
run_model <- function(config) {
  cat("Running model:", config$name, "\n")
  
  # Compile model
  mod <- cmdstan_model(file.path("models", config$file))
  
  # for fecundity models apply warmup
  if (grepl("fecundity", config$name)) {
    pf_fit <- mod$pathfinder(
      data = config$data,
      init = function() init_fec(config$data),
      num_paths = 1
    )
    init_list <- pf_fit$draws(format = "list")
  } else {
    init_list <- 0  # regular init for non-fecundity models 
  }
  
  # Fit model
  fit <- mod$sample(
    data = config$data,
    init = init_list,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 1000,
    seed = 123
  )
  
  # Save model fit object
  saveRDS(fit, file.path("output", paste0("fit_", config$name, ".rds")))
  
  # Extract generated quantities 
  gq_names <- fit$metadata()$names_gq
  pred_vars <- grep("_pred$", gq_names, value = TRUE)
  gqs <- lapply(pred_vars, function(v) fit$draws(variables = v, format = "matrix"))
  names(gqs) <- pred_vars
  saveRDS(gqs, file.path("output", paste0("gqs_", config$name, ".rds")))
  
  cat("Saved fit and predictions for:", config$name, "\n\n")
}

######## 1. Run models in parallel on rstudio server ############

# Run in parallel on R studio server 
library(future.apply)
plan(multisession, workers = length(model_configs))  
future_lapply(model_configs, run_model)

####### 2. manually run individual models on personal computer ###########
## also a backup option if parallel server is not working

## manually call each model 
# Emergence
run_model(list(name = "emerged_full",       file = "binomial_glm_emerged_full.stan", data = stan_data_emg_full))
run_model(list(name = "emerged_climate",    file = "binomial_glm_emerged_climateonly.stan", data = stan_data_climate_only_emg))
run_model(list(name = "emerged_nogene",     file = "binomial_glm_emerged_nogene.stan", data = stan_data_nogen_emg))
run_model(list(name = "emerged_nocomp",     file = "binomial_glm_emerged_nocomp.stan", data = stan_data_nocomp_emg))
run_model(list(name = "emerged_nointer",    file = "binomial_glm_emerged_nointer.stan", data = stan_data_intra_emg))
run_model(list(name = "emerged_null",       file = "binomial_glm_emerged_null.stan", data = stan_data_emg_full))

# Reproduction
run_model(list(name = "reproduced_full",    file = "binomial_glm_reproduced_full.stan", data = stan_data_rep_full))
run_model(list(name = "reproduced_climate", file = "binomial_glm_reproduced_climateonly.stan", data = stan_data_climate_only_rep))
run_model(list(name = "reproduced_nogene",  file = "binomial_glm_reproduced_nogene.stan", data = stan_data_nogen_rep))
run_model(list(name = "reproduced_nocomp",  file = "binomial_glm_reproduced_nocomp.stan", data = stan_data_nocomp_rep))
run_model(list(name = "reproduced_nointer", file = "binomial_glm_reproduced_nointer.stan", data = stan_data_intra_rep))
run_model(list(name = "reproduced_null",    file = "binomial_glm_reproduced_null.stan", data = stan_data_rep_full))

# Fecundity (warmup applied)
run_model(list(name = "fecundity_full",     file = "ztnb_glm_fecundity_full.stan", data = stan_data_fec_full))
run_model(list(name = "fecundity_climate",  file = "ztnb_glm_fecundity_climateonly.stan", data = stan_data_climate_only_fec))
run_model(list(name = "fecundity_nogene",   file = "ztnb_glm_fecundity_nogene.stan", data = stan_data_nogen_fec))
run_model(list(name = "fecundity_nocomp",   file = "ztnb_glm_fecundity_nocomp.stan", data = stan_data_nocomp_fec))
run_model(list(name = "fecundity_nointer",  file = "ztnb_glm_fecundity_nointer.stan", data = stan_data_intra_fec))
run_model(list(name = "fecundity_null",     file = "ztnb_glm_fecundity_null.stan", data = stan_data_fec_full))

