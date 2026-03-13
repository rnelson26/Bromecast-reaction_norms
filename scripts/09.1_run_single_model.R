################# Bromecast: 09.1_run_single_model.R ##########################
############# created 9-22-25 ######################
############# Last modified: 9-30-25 ##########################
######## Fits and Runs all models as single models ################################
###### Wrapper Script for HPC #########
#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
model_id <- as.integer(args[1])

if (is.na(model_id)) stop("You must supply a model ID, e.g. Rscript scripts/09.1_run_single_model.R 1")

# ================================
# Load data
# ================================
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")

# ================================
# Model configurations
# ================================
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

# Pathfinder init
pathfinder_fit <- mod$pathfinder(data = config$data, init = 0, num_paths = 1)
init_list <- pathfinder_fit$draws(format = "list")

fit <- mod$sample(
  data = config$data,
  init = init_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000,
  seed = 123
)

# ================================
# Save outputs
# ================================
out_dir <- "results/draws"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- file.path(out_dir, paste0("fit_", config$name, ".rds"))
fit$save_object(out_file)

cat("Finished model:", config$name, " -> saved to ", out_file, "\n")

# Save prediction matrices too (optional)
gq_names <- fit$metadata()$names_gq
pred_vars <- grep("_pred$", gq_names, value = TRUE)
if (length(pred_vars) > 0) {
  gqs <- lapply(pred_vars, function(v) fit$draws(variables = v, format = "matrix"))
  names(gqs) <- pred_vars
  if (!dir.exists("results/gqs")) dir.create("results/gqs", recursive = TRUE)
  gq_file <- file.path("results/gqs", paste0("gqs_", config$name, ".rds"))
  saveRDS(gqs, gq_file)
  cat("Saved prediction matrices for:", config$name, " -> ", gq_file, "\n")
}
