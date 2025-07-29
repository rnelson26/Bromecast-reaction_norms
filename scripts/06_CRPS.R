################# Bromecast: 06.CRPS Table ##########################
############# created 3-25-25 ######################
############# Last modified: 7-29-25 ##########################
######## CRPS & Skill Scores for all models ################################

source("scripts/00_setup.R")
source("scripts/01_prepare_data.R")

# ========== SETUP ==========
model_names <- c("emerged", "repro", "fecundity")
fit_paths <- paste0("output/fit_", model_names, "_full.rds")
null_paths <- paste0("output/fit_null_", model_names, ".rds")

# hard-coded null model CRPS values (as backup)
null_manual <- c(
  emerged = 0.332,
  repro   = 0.489,
  fecundity = 0.365
)

# Observed test data (need to check)
obs_test <- list(
  emerged   = testing_df_emg$e_test,
  repro     = testing_df_emg$r_test,
  fecundity = testing_df_emg$y_test
)

# Output containers
crps_full <- numeric(length(model_names))
crps_null <- numeric(length(model_names))
skill_score <- numeric(length(model_names))

# ========== LOOP THROUGH MODELS ==========
for (i in seq_along(model_names)) {
  mod <- model_names[i]
  message("Processing model: ", mod)
  
  # ---- Load full model and extract predictions
  fit <- readRDS(fit_paths[i])
  varname <- paste0(substr(mod, 1, 1), "_test_pred")  # e_test_pred, r_test_pred, y_test_pred
  
  pred_draws <- fit$draws(variables = varname, format = "matrix")
  obs <- obs_test[[mod]]
  
  # Compute CRPS for full model
  if (mod == "fecundity") {
    crps_full[i] <- mean(crps_nb(y = obs, size = 10, mean = rowMeans(pred_draws)))  # Replace size if needed
  } else {
    crps_full[i] <- mean(crps_binom(y = obs, size = 1, prob = rowMeans(pred_draws)))
  }
  
  # ---- Null model: try loading .rds, fallback to manual
  if (file.exists(null_paths[i])) {
    fit_null <- readRDS(null_paths[i])
    pred_draws_null <- fit_null$draws(variables = varname, format = "matrix")
    if (mod == "fecundity") {
      crps_null[i] <- mean(crps_nb(y = obs, size = 10, mean = rowMeans(pred_draws_null)))
    } else {
      crps_null[i] <- mean(crps_binom(y = obs, size = 1, prob = rowMeans(pred_draws_null)))
    }
  } else {
    message("Using manual null CRPS for ", mod)
    crps_null[i] <- null_manual[[mod]]
  }
  
  # ---- Skill Score
  skill_score[i] <- 1 - (crps_full[i] / crps_null[i])
}

# ========== OUTPUT TABLE ==========
crps_df <- tibble(
  Model = model_names,
  CRPS_Full = round(crps_full, 3),
  CRPS_Null = round(crps_null, 3),
  Skill_Score = round(skill_score, 3)
)

print(crps_df)
write_csv(crps_df, "output/crps_skill_scores.csv")
