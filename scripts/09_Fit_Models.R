

############### Bromecast: 09.Fit Models ##########################
############# created 3-25-25 ######################
############# Last modified: 1-7-26 ##########################
######## Fits and Runs all models ################################

############ normal cool start for all models #################

# Load data
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")

### save prepped data workspace for HPC use 
# save.image(file = "prepped_workspace.RData")
## too large to easily load to github

###### Run model functions ################

# Run a single model (no warm start for fecundity)
run_model <- function(config) {
  cat("Running model:", config$name, "\n")
  
  # Compile model
  mod <- cmdstan_model(file.path("models", config$file))
  
  # Standard initialization for all models (no warm startup)
  init_list <- 0
  
  # Define a permanent folder for model CSVs
  out_dir <- file.path("output", config$name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Fit model with permanent output folder
  fit <- mod$sample(
    data = config$data,
    init = init_list,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 1000,
    seed = 123,
    output_dir = out_dir,
    output_basename = paste0("fit_", config$name)
  )
  
  
  # Fit model
  #fit <- mod$sample(
   # data = config$data,
    #init = init_list,
    #chains = 4,
    #parallel_chains = 4,
    #iter_warmup = 500,
    #iter_sampling = 1000,
    #seed = 123
  #)
  
  # Save model fit object
  saveRDS(fit, file.path("output", paste0("fit_", config$name, ".rds")))
  
  
  # Extract generated quantities of the posterior predictive 
  gq_names <- fit$metadata()$names_gq
  pred_vars <- grep("_pred$", gq_names, value = TRUE)
  gqs <- lapply(pred_vars, function(v) fit$draws(variables = v, format = "matrix"))
  names(gqs) <- pred_vars
  saveRDS(gqs, file.path("output", paste0("gqs_", config$name, ".rds")))
  
  cat("Saved fit and predictions for:", config$name, "\n\n")
}

# if we want posterior mean objects for other reasons like graphing we could further add (mu_*)
## fecundity posterior:
#mu_vars <- grep("^mu_", gq_names, value = TRUE)
#mu_list <- lapply(mu_vars, function(v) fit$draws(variables = v, format = "matrix"))
#names(mu_list) <- mu_vars
#saveRDS(mu_list, file.path("output", paste0("mu_", config$name, ".rds")))
## emerged and reproduced prosterior
#p_vars <- grep("^p_", gq_names, value = TRUE)
#p_list <- lapply(p_vars, function(v) fit$draws(variables = v, format = "matrix"))
#names(p_list) <- p_vars
#saveRDS(p_list, file.path("output", paste0("mu_", config$name, ".rds")))



######## 1. Run models in parallel on rstudio server ############
# Define all models
model_configs <- list(
  # Emergence models
  list(name = "emerged_full",    file = "binomial_glm_emerged_full.stan",       data = stan_data_emg_full),
  list(name = "emerged_climate", file = "binomial_glm_emerged_climateonly.stan", data = stan_data_climate_only_emg),
  list(name = "emerged_nogene",  file = "binomial_glm_emerged_nogene.stan",     data = stan_data_nogen_emg),
  list(name = "emerged_nocomp",  file = "binomial_glm_emerged_nocomp.stan",     data = stan_data_nocomp_emg),
  list(name = "emerged_nointer", file = "binomial_glm_emerged_nointer.stan",    data = stan_data_intra_emg),
  list(name = "emerged_null",    file = "binomial_glm_emerged_null.stan",       data = stan_data_emg_full),
  
  # Reproduction models
  list(name = "reproduced_full",    file = "binomial_glm_reproduced_full.stan",       data = stan_data_rep_full),
  list(name = "reproduced_climate", file = "binomial_glm_reproduced_climateonly.stan", data = stan_data_climate_only_rep),
  list(name = "reproduced_nogene",  file = "binomial_glm_reproduced_nogene.stan",     data = stan_data_nogen_rep),
  list(name = "reproduced_nocomp",  file = "binomial_glm_reproduced_nocomp.stan",     data = stan_data_nocomp_rep),
  list(name = "reproduced_nointer", file = "binomial_glm_reproduced_nointer.stan",    data = stan_data_intra_rep),
  list(name = "reproduced_null",    file = "binomial_glm_reproduced_null.stan",       data = stan_data_rep_full),
  
  # Fecundity models (no warm start)
  list(name = "fecundity_full",    file = "ztnb_glm_fecundity_full.stan",       data = stan_data_fec_full),
  list(name = "fecundity_climate", file = "ztnb_glm_fecundity_climateonly.stan", data = stan_data_climate_only_fec),
  list(name = "fecundity_nogene",  file = "ztnb_glm_fecundity_nogene.stan",     data = stan_data_nogen_fec),
  list(name = "fecundity_nocomp",  file = "ztnb_glm_fecundity_nocomp.stan",     data = stan_data_nocomp_fec),
  list(name = "fecundity_nointer", file = "ztnb_glm_fecundity_nointer.stan",    data = stan_data_intra_fec),
  list(name = "fecundity_null",    file = "ztnb_glm_fecundity_null.stan",       data = stan_data_fec_full)
)

# Run in parallel on R studio server 
library(future.apply)
plan(multisession, workers = length(model_configs))  
future_lapply(model_configs, run_model)

####### 2. manually run individual models on personal computer ###########

## manually call each model if needed 
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

# Fecundity (no warm start)
run_model(list(name = "fecundity_full",     file = "ztnb_glm_fecundity_full.stan", data = stan_data_fec_full))
run_model(list(name = "fecundity_climate",  file = "ztnb_glm_fecundity_climateonly.stan", data = stan_data_climate_only_fec))
run_model(list(name = "fecundity_nogene",   file = "ztnb_glm_fecundity_nogene.stan", data = stan_data_nogen_fec))
run_model(list(name = "fecundity_nocomp",   file = "ztnb_glm_fecundity_nocomp.stan", data = stan_data_nocomp_fec))
run_model(list(name = "fecundity_nointer",  file = "ztnb_glm_fecundity_nointer.stan", data = stan_data_intra_fec))
run_model(list(name = "fecundity_null",     file = "ztnb_glm_fecundity_null.stan", data = stan_data_fec_full))
