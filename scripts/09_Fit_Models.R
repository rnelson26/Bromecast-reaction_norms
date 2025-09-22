################# Bromecast: 09.Fit Models ##########################
############# created 3-25-25 ######################
############# Last modified: 7-29-25 ##########################
######## Fits and Runs all models ################################

# Load data
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")

# Define models
model_configs <- list(
  ## Reproduced
  list(name = "reproduced_full", file = "binomial_glm_reproduced.stan", data = stan_data_rep),
  list(name = "reproduced_climate", file = "binomial_glm_reproduced_climateonly.stan", data = stan_data_climate_only_rep),
  list(name = "reproduced_nogene", file = "binomial_glm_reproduced_nogene.stan", data = stan_data_noge_rep),
  list(name = "reproduced_nocomp", file = "binomial_glm_reproduced_nocomp.stan", data = stan_data_nocomp_rep),
  list(name = "reproduced_nointer", file = "binomial_glm_reproduced_nointer.stan", data = stan_data_intra_rep),
  
  ## Emerged
  list(name = "emerged_full", file = "binomial_glm_survived.stan", data = stan_data_emerged_full),
  list(name = "emerged_climate", file = "binomial_glm_emerged_climateonly.stan", data = stan_data_climate_only_emg),
  list(name = "emerged_nogene", file = "binomial_glm_emerged_nogene.stan", data = stan_data_nogen_emg),
  list(name = "emerged_nocomp", file = "binomial_glm_emerged_nocomp.stan", data = stan_data_nocomp_emg),
  list(name = "emerged_nointer", file = "binomial_glm_emerged_nointer.stan", data = stan_data_intra_emg),
  
  ## Fecundity
  list(name = "fecundity_full", file = "bztnb_glm.random.predict.stan", data = stan_data_emerged_full),
  list(name = "fecundity_climate", file = "ztnb_glm.random.predict_climateonly.stan", data = stan_data_climate_only),
  list(name = "fecundity_nogene", file = "ztnb_glm.random.predict_nogene.stan", data = stan_data_nogen),
  list(name = "fecundity_nocomp", file = "ztnb_glm.random.predict_nocomp.stan", data = stan_data_nocomp),
  list(name = "fecundity_nointer", file = "ztnb_glm.random.predict_nointer.stan", data = stan_data_intra)
)

# Iterate and Fit models 
for (config in model_configs) {
  cat("Running:", config$name, "\n")
  
  mod <- cmdstan_model(file.path("models", config$file))
  
  pathfinder_fit <- mod$pathfinder(
    data = config$data,
    init = 0,
    num_paths = 1
  )
  init_list <- pathfinder_fit$draws(format = "list")
  
  fit <- mod$sample(
    data = config$data,
    init = init_list,
    chains = 3,
    parallel_chains = 3,
    iter_warmup = 100,
    iter_sampling = 1000,
    seed = 123
  )
  
  fit$save_object(file = file.path("output", paste0("fit_", config$name, ".rds")))
}

# ================
# Null models
# ================

# Precompiled null models
mod_null_emg <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/null_model_emerged.stan")
mod_null_rep <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/null_model_reproduced.stan")
mod_null_fec <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/null_model_fecundity.stan")

cat("Running null: emerged\n")
fit_null_emg <- mod_null_emg$sample(
  data = list(
    n_train = nrow(training_df_emg),
    n_test = nrow(testing_df_emg),
    e_train = training_df_emg$e_train
  ),
  seed = 123,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = 100,
  iter_sampling = 1000
)
fit_null_emg$save_object("output/fit_null_emerged.rds")

cat("Running null: reproduced\n")
fit_null_rep <- mod_null_rep$sample(
  data = list(
    n_train = nrow(training_df_rep),
    n_test = nrow(testing_df_rep),
    r_train = training_df_rep$r_train
  ),
  seed = 123,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = 100,
  iter_sampling = 1000
)
fit_null_rep$save_object("output/fit_null_reproduced.rds")

cat("Running null: fecundity\n")
fit_null_fec <- mod_null_fec$sample(
  data = list(
    n_train = nrow(training_df),
    n_test = nrow(testing_df),
    y_train = training_df$Fecundity
  ),
  seed = 123,
  chains = 3,
  parallel_chains = 3,
  iter_warmup = 100,
  iter_sampling = 1000
)
fit_null_fec$save_object("output/fit_null_fecundity.rds")
