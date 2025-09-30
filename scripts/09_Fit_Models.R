################# Bromecast: 09.Fit Models ##########################
############# created 3-25-25 ######################
############# Last modified: 9-30-25 ##########################
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
#compile_results <- list()

#for (config in model_configs) {
 # cat("Checking compilation for:", config$name, "\n")
  
  # Try compiling the model
  #result <- tryCatch({
   # mod <- cmdstan_model(file.path("models", config$file))
    # Pathfinder is optional; can also just stop here if compilation succeeds
    #TRUE  # compilation succeeded
#  }, error = function(e) {
 #   cat("ERROR compiling", config$name, ":", e$message, "\n")
  #  FALSE  # compilation failed
  #})
  
  #compile_results[[config$name]] <- result
#}

# Summary of which models compile
#compile_results



# Iterate and Fit models 
for (config in model_configs) {
  cat("Running model:", config$name, "\n")
  
  # Compile model
  mod <- cmdstan_model(file.path("models", config$file))
  
  # Pathfinder initialization
  pathfinder_fit <- mod$pathfinder(
    data = config$data,
    init = 0,
    num_paths = 1
  )
  init_list <- pathfinder_fit$draws(format = "list")
  
  # Fit the model
  fit <- mod$sample(
    data = config$data,
    init = init_list,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 1000,
    seed = 123
  )
  
  # Assign fit as a unique object in workspace
  assign(paste0("fit_", config$name), fit)
  

  # Dynamically extract prediction-type variables
  gq_names <- fit$metadata()$names_gq          # all generated quantities
  pred_vars <- grep("_pred$", gq_names, value = TRUE)  # only prediction matrices
  
  # Extract draws for each prediction variable
  gqs <- lapply(pred_vars, function(v) fit$draws(variables = v, format = "matrix"))
  names(gqs) <- pred_vars
  
  # Save predictions to RDS (memory safe)
  saveRDS(gqs, file.path("output", paste0("gqs_", config$name, ".rds")))
  
  cat("Saved prediction matrices for:", config$name, "\n\n")
}

