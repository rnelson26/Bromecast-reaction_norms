# ---- SETUP ----
rm(list = ls())
library(cmdstanr)
library(tidyverse)
library(furrr)  # optional for parallelism
library(here)

# Define paths
model_dir <- here("stan_models")
data_dir <- here("data")
results_dir <- here("results")

# Create output folders if missing
dir.create(results_dir, showWarnings = FALSE)
dir.create(file.path(results_dir, "draws"), showWarnings = FALSE)
dir.create(file.path(results_dir, "plots"), showWarnings = FALSE)
dir.create(file.path(results_dir, "crps"), showWarnings = FALSE)

# ---- MODEL VARIANTS ----
model_variants <- list(
  full = c("climate", "soil", "genotype", "intra", "inter"),
  null = character(0),
  clim_soil = c("climate", "soil"),
  clim_geno = c("climate", "soil", "genotype"),
  clim_intra = c("climate", "intra"),
  clim_inter = c("climate", "inter"),
  clim_geno_inter = c("climate", "soil", "genotype", "inter")
)

# ---- FUNCTION: Build Stan Data ----
build_stan_data <- function(df_train, df_test, predictors, stage) {
  # Subset or transform your data based on selected predictors
  # Return list formatted for Stan
  list(
    n_train = nrow(df_train),
    n_test = nrow(df_test),
    e_train = ifelse(df_train$Emerged == "Y", 1L, 0L),
    neighbors_train = if ("intra" %in% predictors) df_train$neighbors.s else rep(0, nrow(df_train)),
    # Add remaining fields similarly...
    X = if ("climate" %in% predictors) X_emg_SOS else matrix(0, nrow(df_train), 1),
    X_soil = if ("soil" %in% predictors) X_soil_emg else matrix(0, nrow(df_train), 1),
    # ...
    K = K_common_garden,
    # ...
    plot_index_train = df_train$plot_index
  )
}

# ---- FUNCTION: Fit Stan Model ----
fit_model <- function(model_file, stan_data, model_name, variant) {
  mod <- cmdstan_model(model_file)
  message("Fitting: ", model_name, " (", variant, ")")
  
  path_out <- file.path(results_dir, "draws", paste0(model_name, "_", variant, ".rds"))
  
  pathfinder_fit <- mod$pathfinder(data = stan_data, init = 0, num_paths = 1)
  init_list <- pathfinder_fit$draws(format = "list")
  
  fit <- mod$sample(
    data = stan_data,
    seed = 123,
    chains = 3,
    parallel_chains = 3,
    iter_warmup = 100,
    iter_sampling = 1000,
    init = init_list
  )
  
  saveRDS(fit$draws(), path_out)
  invisible(NULL)
}

# ---- LOOP THROUGH VARIANTS ----
for (variant in names(model_variants)) {
  predictors <- model_variants[[variant]]
  
  # Build each stan_data list per stage
  stan_data_emg <- build_stan_data(training_df_emg, testing_df_emg, predictors, stage = "emergence")
  stan_data_rep <- build_stan_data(training_df_rep, testing_df_rep, predictors, stage = "reproduction")
  stan_data_fec <- build_stan_data(training_df_fec, testing_df_fec, predictors, stage = "fecundity")
  
  # Fit each model variant
  fit_model(file.path(model_dir, "binomial_glm_survived.stan"), stan_data_emg, "emergence", variant)
  fit_model(file.path(model_dir, "binomial_glm_reproduced.stan"), stan_data_rep, "reproduction", variant)
  fit_model(file.path(model_dir, "ztnb_glm.random.predict.stan"), stan_data_fec, "fecundity", variant)
}

# ---- TODO: Add script for CRPS + posterior multiplication + plotting ----
