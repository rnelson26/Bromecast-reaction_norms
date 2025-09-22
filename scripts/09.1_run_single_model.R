################# Bromecast: 09.1_run_single_model.R ##########################
############# created 9-22-25 ######################
############# Last modified: 9-22-25 ##########################
######## Fits and Runs all models as signle models ################################
###### Wrapper Script for HPC #########

#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
model_id <- as.integer(args[1])

if (is.na(model_id)) stop("You must supply a model ID, e.g. Rscript run_single_model.R 1")

# ================================
# Load data and prepare stan_data
# ================================
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")

# ================================
# Define models list
# ================================
model_configs <- list(
  ## Reproduced
  list(name = "reproduced_full",      file = "binomial_glm_reproduced.stan",              data = stan_data_rep),
  list(name = "reproduced_climate",   file = "binomial_glm_reproduced_climateonly.stan",  data = stan_data_climate_only_rep),
  list(name = "reproduced_nogene",    file = "binomial_glm_reproduced_nogene.stan",       data = stan_data_noge_rep),
  list(name = "reproduced_nocomp",    file = "binomial_glm_reproduced_nocomp.stan",       data = stan_data_nocomp_rep),
  list(name = "reproduced_nointer",   file = "binomial_glm_reproduced_nointer.stan",      data = stan_data_intra_rep),
  
  ## Emerged
  list(name = "emerged_full",         file = "binomial_glm_survived.stan",                data = stan_data_emerged_full),
  list(name = "emerged_climate",      file = "binomial_glm_emerged_climateonly.stan",     data = stan_data_climate_only_emg),
  list(name = "emerged_nogene",       file = "binomial_glm_emerged_nogene.stan",          data = stan_data_nogen_emg),
  list(name = "emerged_nocomp",       file = "binomial_glm_emerged_nocomp.stan",          data = stan_data_nocomp_emg),
  list(name = "emerged_nointer",      file = "binomial_glm_emerged_nointer.stan",         data = stan_data_intra_emg),
  
  ## Fecundity
  list(name = "fecundity_full",       file = "bztnb_glm.random.predict.stan",             data = stan_data_emerged_full),
  list(name = "fecundity_climate",    file = "ztnb_glm.random.predict_climateonly.stan",  data = stan_data_climate_only),
  list(name = "fecundity_nogene",     file = "ztnb_glm.random.predict_nogene.stan",       data = stan_data_nogen),
  list(name = "fecundity_nocomp",     file = "ztnb_glm.random.predict_nocomp.stan",       data = stan_data_nocomp),
  list(name = "fecundity_nointer",    file = "ztnb_glm.random.predict_nointer.stan",      data = stan_data_intra),
  
  ## Nulls (treat same as other configs so array covers them too)
  list(name = "null_emerged",    file = "null_model_emerged.stan",    data = list(
    n_train = nrow(training_df_emg),
    n_test  = nrow(testing_df_emg),
    e_train = training_df_emg$e_train)),
  list(name = "null_reproduced", file = "null_model_reproduced.stan", data = list(
    n_train = nrow(training_df_rep),
    n_test  = nrow(testing_df_rep),
    r_train = training_df_rep$r_train)),
  list(name = "null_fecundity",  file = "null_model_fecundity.stan", data = list(
    n_train = nrow(training_df),
    n_test  = nrow(testing_df),
    y_train = training_df$Fecundity))
)

# ================================
# Select config for this job
# ================================
if (model_id < 1 || model_id > length(model_configs)) {
  stop(paste("Model ID must be between 1 and", length(model_configs)))
}

config <- model_configs[[model_id]]
cat("Running model:", config$name, "\n")

library(cmdstanr)

mod <- cmdstan_model(file.path("models", config$file))

# Pathfinder init (only for non-null models)
if (!grepl("^null_", config$name)) {
  pathfinder_fit <- mod$pathfinder(data = config$data, init = 0, num_paths = 1)
  init_list <- pathfinder_fit$draws(format = "list")
} else {
  init_list <- 0
}

fit <- mod$sample(
  data = config$data,
  init = init_list,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = 100,
  iter_sampling = 1000,
  seed = 123
)

out_file <- file.path("results/draws", paste0("fit_", config$name, ".rds"))
fit$save_object(out_file)

cat("Finished model:", config$name, " -> saved to ", out_file, "\n")
