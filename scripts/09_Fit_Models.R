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
  # Emergence
  list(name = "emerged_full",       file = "binomial_glm_emerged_full.stan", data = stan_data_emg_full),
  list(name = "emerged_climate",    file = "binomial_glm_emerged_climateonly.stan", data = stan_data_climate_only_emg),
  list(name = "emerged_nogene",     file = "binomial_glm_emerged_nogene.stan", data = stan_data_nogen_emg),
  list(name = "emerged_nocomp",     file = "binomial_glm_emerged_nocomp.stan", data = stan_data_nocomp_emg),
  list(name = "emerged_nointer",    file = "binomial_glm_emerged_nointer.stan", data = stan_data_intra_emg),
  list(name = "emerged_null",       file = "binomial_glm_emerged_null.stan", data = stan_data_emg_full),
  
  # Reproduction
  list(name = "reproduced_full",    file = "binomial_glm_reproduced_full.stan", data = stan_data_rep_full),
  list(name = "reproduced_climate", file = "binomial_glm_reproduced_climateonly.stan", data = stan_data_climate_only_rep),
  list(name = "reproduced_nogene",  file = "binomial_glm_reproduced_nogene.stan", data = stan_data_nogen_rep),
  list(name = "reproduced_nocomp",  file = "binomial_glm_reproduced_nocomp.stan", data = stan_data_nocomp_rep),
  list(name = "reproduced_nointer", file = "binomial_glm_reproduced_nointer.stan", data = stan_data_intra_rep),
  list(name = "reproduced_null",    file = "binomial_glm_reproduced_null.stan", data = stan_data_rep_full),
  
  # Fecundity
  list(name = "fecundity_full",     file = "ztnb_glm_fecundity_full.stan", data = stan_data_fec_full),
  list(name = "fecundity_climate",  file = "ztnb_glm_fecundity_climateonly.stan", data = stan_data_climate_only_fec),
  list(name = "fecundity_nogene",   file = "ztnb_glm_fecundity_nogene.stan", data = stan_data_nogen_fec),
  list(name = "fecundity_nocomp",   file = "ztnb_glm_fecundity_nocomp.stan", data = stan_data_nocomp_fec),
  list(name = "fecundity_nointer",  file = "ztnb_glm_fecundity_nointer.stan", data = stan_data_intra_fec),
  list(name = "fecundity_null",     file = "ztnb_glm_fecundity_null.stan", data = stan_data_fec_full)
)

## check that the models compile
compile_results <- list()

for (config in model_configs) {
  cat("Checking compilation for:", config$name, "\n")
  
  # Try compiling the model
  result <- tryCatch({
    mod <- cmdstan_model(file.path("models", config$file))
    # Pathfinder is optional; can also just stop here if compilation succeeds
    TRUE  # compilation succeeded
  }, error = function(e) {
    cat("ERROR compiling", config$name, ":", e$message, "\n")
    FALSE  # compilation failed
  })
  
  compile_results[[config$name]] <- result
}

# Summary of which models compile
compile_results




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
