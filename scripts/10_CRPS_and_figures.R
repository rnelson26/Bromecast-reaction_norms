################# Bromecast: 10.CRPS_and_figures.R ##########################
############# created 3-25-25 ######################
############# Last modified: 9-22-25 ##########################
######## CRPS & Skill Scores for all model variants ################################


#source("scripts/09_Fit_Models.R")

# ========== SETUP ==========
stages <- c("emerged", "repro", "fecundity")
variants <- paste0("var", 1:5)   # adjust to var1:7 if 7 variants
model_names <- as.vector(outer(stages, variants, paste, sep = "_"))

fit_paths <- paste0("output/fit_", model_names, "_full.rds")
null_paths <- paste0("output/fit_null_", model_names, ".rds")

# hard-coded null model CRPS values (stage-level, reused for all variants)
null_manual_stage <- c(
  emerged   = 0.332,
  repro     = 0.489,
  fecundity = 0.365
)

# Observed test data (same across variants, depends only on stage)
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
  
  # figure out the stage (strip off variant suffix)
  stage <- strsplit(mod, "_")[[1]][1]
  
  # ---- Load full model and extract predictions
  fit <- readRDS(fit_paths[i])
  varname <- paste0(substr(stage, 1, 1), "_test_pred")  # e_test_pred, r_test_pred, y_test_pred
  
  pred_draws <- fit$draws(variables = varname, format = "matrix")
  obs <- obs_test[[stage]]
  
  # Compute CRPS for full model
  if (stage == "fecundity") {
    crps_full[i] <- mean(crps_nb(y = obs, size = 10, mean = rowMeans(pred_draws)))
  } else {
    crps_full[i] <- mean(crps_binom(y = obs, size = 1, prob = rowMeans(pred_draws)))
  }
  
  # ---- Null model: try loading .rds, fallback to stage-level manual value
  if (file.exists(null_paths[i])) {
    fit_null <- readRDS(null_paths[i])
    pred_draws_null <- fit_null$draws(variables = varname, format = "matrix")
    if (stage == "fecundity") {
      crps_null[i] <- mean(crps_nb(y = obs, size = 10, mean = rowMeans(pred_draws_null)))
    } else {
      crps_null[i] <- mean(crps_binom(y = obs, size = 1, prob = rowMeans(pred_draws_null)))
    }
  } else {
    message("Using manual null CRPS for ", mod)
    crps_null[i] <- null_manual_stage[[stage]]
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

# ===================== CALCULATE FITNESS =====================
library(dplyr)

# Observed fitness
training_df_emg <- training_df_emg %>%
  mutate(Obs_Fitness = e_train * r_train * Fecundity,
         Obs_Fitness_log = log(Obs_Fitness))

testing_df_emg <- testing_df_emg %>%
  mutate(Obs_Fitness = e_test * r_test * Fecundity,
         Obs_Fitness_log = log(Obs_Fitness))

# ---------------- Posterior draws ----------------
# Extract draws
p_emg_train <- fit_emg_full$draws("p_train", format = "draws_matrix")
p_emg_test  <- fit_emg_full$draws("p_test", format = "draws_matrix")

p_rep_train <- fit_rep$draws("p_train_full", format = "draws_matrix")
p_rep_test  <- fit_rep$draws("p_test_full", format = "draws_matrix")

p_fec_train <- fit$draws("mu_train_full", format = "draws_matrix")
p_fec_test  <- fit$draws("mu_test_full", format = "draws_matrix")

# Posterior predictive draws
e_emg_train <- fit_emg_full$draws("e_train_pred", format = "draws_matrix")
e_emg_test  <- fit_emg_full$draws("e_test_pred", format = "draws_matrix")

r_rep_train <- fit_rep$draws("r_train_full", format = "draws_matrix")
r_rep_test  <- fit_rep$draws("r_test_full", format = "draws_matrix")

y_fec_train <- fit$draws("y_train_pred_full", format = "draws_matrix")
y_fec_test  <- fit$draws("y_test_pred_full", format = "draws_matrix")

# Fixed-effects-only draws for training
p_emg_train_fixed <- fit_emg_full$draws("p_train_fixed", format = "draws_matrix")
p_rep_train_fixed <- fit_rep$draws("p_train_full_fixed", format = "draws_matrix")
p_fec_train_fixed <- fit$draws("mu_train_full_fixed", format = "draws_matrix")

e_emg_train_fixed <- fit_emg_full$draws("e_train_pred_fixed", format = "draws_matrix")
r_rep_train_fixed <- fit_rep$draws("r_train_full_fixed", format = "draws_matrix")
y_fec_train_fixed <- fit$draws("y_train_pred_full_fixed", format = "draws_matrix")

# ---------------- Compute fitness ----------------
# Posterior mean fitness
fitness_draws_train <- p_emg_train * p_rep_train * p_fec_train
fitness_draws_test  <- p_emg_test  * p_rep_test  * p_fec_test

fitness_draws_train_fixed <- p_emg_train_fixed * p_rep_train_fixed * p_fec_train_fixed

log_fitness_draws_train <- log(p_emg_train) + log(p_rep_train) + log(p_fec_train)
log_fitness_draws_test  <- log(p_emg_test)  + log(p_rep_test)  + log(p_fec_test)
log_fitness_draws_train_fixed <- log(p_emg_train_fixed) + log(p_rep_train_fixed) + log(p_fec_train_fixed)

# Posterior predictive fitness
fitness_draws_train_pred <- e_emg_train * r_rep_train * y_fec_train
fitness_draws_test_pred  <- e_emg_test  * r_rep_test  * y_fec_test

fitness_draws_train_pred_fixed <- e_emg_train_fixed * r_rep_train_fixed * y_fec_train_fixed

log_fitness_draws_train_pred <- log(e_emg_train) + log(r_rep_train) + log(y_fec_train)
log_fitness_draws_test_pred  <- log(e_emg_test)  + log(r_rep_test)  + log(y_fec_test)
log_fitness_draws_train_pred_fixed <- log(e_emg_train_fixed) + log(r_rep_train_fixed) + log(y_fec_train_fixed)

# ---------------- Posterior means ----------------
training_df_emg <- training_df_emg %>%
  mutate(
    Predicted_Fitness = apply(fitness_draws_train, 2, mean),
    Predicted_Fitness_log = apply(log_fitness_draws_train, 2, mean),
    Predicted_Fitness_fixed = apply(fitness_draws_train_fixed, 2, mean)
  )

testing_df_emg <- testing_df_emg %>%
  mutate(
    Predicted_Fitness = apply(fitness_draws_test, 2, mean),
    Predicted_Fitness_log = apply(log_fitness_draws_test, 2, mean)
  )

training_df_emg <- training_df_emg %>%
  mutate(
    Predicted_Fitness_PostPred = apply(fitness_draws_train_pred, 2, mean),
    Predicted_Fitness_log_PostPred = apply(log_fitness_draws_train_pred, 2, mean),
    Predicted_Fitness_PostPred_fixed = apply(fitness_draws_train_pred_fixed, 2, mean),
    Predicted_Fitness_log_PostPred_fixed = apply(log_fitness_draws_train_pred_fixed, 2, mean)
  )

testing_df_emg <- testing_df_emg %>%
  mutate(
    Predicted_Fitness_PostPred = apply(fitness_draws_test_pred, 2, mean),
    Predicted_Fitness_log_PostPred = apply(log_fitness_draws_test_pred, 2, mean)
  )

# ===================== FIGURE GENERATION =====================
library(ggplot2)
library(ggpointdensity)
library(viridis)
library(dplyr)
library(tidyr)
library(patchwork)

# Create figures directory if it doesn't exist
if (!dir.exists("figures")) dir.create("figures")

# --------- Figure 2: Heatmap of Predicted vs Observed Fitness (Full Model) ----------
# Training
pdf("figures/Figure2_Training_Fitness_Heatmap.pdf", width = 7, height = 6)
ggplot(training_df_emg, aes(x = Predicted_Fitness_log_PostPred, y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  labs(title = "Training: Predicted vs Observed Fitness (Full Model)",
       x = "Predicted Fitness (log)", y = "Observed Fitness (log)") +
  theme_minimal()
dev.off()

# Testing
pdf("figures/Figure2_Test_Fitness_Heatmap.pdf", width = 7, height = 6)
ggplot(testing_df_emg, aes(x = Predicted_Fitness_log_PostPred, y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  labs(title = "Testing: Predicted vs Observed Fitness (Full Model)",
       x = "Predicted Fitness (log)", y = "Observed Fitness (log)") +
  theme_minimal()
dev.off()

# --------- Figure 3: Predicted vs Observed by Life Stage & Submodel ----------
# Combine draws for each life stage (posterior predictive)
stages <- c("Emerged" = "e_emg_train", 
            "Reproduced" = "r_rep_train",
            "Fecundity" = "y_fec_train")

for(stage in names(stages)) {
  stage_train <- get(stages[stage])
  stage_test  <- get(sub("train", "test", stages[stage]))
  
  # Compute mean predictions
  mean_train <- apply(stage_train, 2, mean)
  mean_test  <- apply(stage_test, 2, mean)
  
  df_train <- tibble(Stage = stage, Type = "Training",
                     Predicted = mean_train,
                     Observed = if(stage=="Emerged") training_df_emg$e_train else if(stage=="Reproduced") training_df_emg$r_train else training_df_emg$Fecundity)
  
  df_test <- tibble(Stage = stage, Type = "Testing",
                    Predicted = mean_test,
                    Observed = if(stage=="Emerged") testing_df_emg$e_test else if(stage=="Reproduced") testing_df_emg$r_test else testing_df_emg$Fecundity)
  
  assign(paste0(stage, "_df"), bind_rows(df_train, df_test))
}

# Combine all stages
all_stages_df <- bind_rows(Emerged_df, Reproduced_df, Fecundity_df)

# Plot
pdf("figures/Figure3_Predicted_vs_Observed_byStage.pdf", width = 10, height = 8)
ggplot(all_stages_df, aes(x = Predicted, y = Observed)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C") +
  facet_grid(Stage ~ Type, scales = "free") +
  labs(title = "Predicted vs Observed by Life Stage & Data Type") +
  theme_minimal()
dev.off()

# --------- Figure S1: Site-Year Pair-Level Variations ----------
# Aggregate mean predicted fitness per site-year
training_site_year <- training_df_emg %>%
  group_by(site_year) %>%
  summarise(MeanPred = mean(Predicted_Fitness_PostPred), MeanObs = mean(Obs_Fitness), .groups = "drop")

testing_site_year <- testing_df_emg %>%
  group_by(site_year) %>%
  summarise(MeanPred = mean(Predicted_Fitness_PostPred), MeanObs = mean(Obs_Fitness), .groups = "drop")

site_year_df <- bind_rows(training_site_year %>% mutate(Type="Training"),
                          testing_site_year %>% mutate(Type="Testing"))

pdf("figures/FigureS1_SiteYear_Fitness.pdf", width = 8, height = 6)
ggplot(site_year_df, aes(x = MeanPred, y = MeanObs)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  facet_wrap(~Type) +
  labs(title="Site-Year Mean Predicted vs Observed Fitness",
       x="Mean Predicted Fitness", y="Mean Observed Fitness") +
  scale_color_viridis_c(option="C") +
  theme_minimal()
dev.off()

# --------- Figures S2 & S4: Climate PCA & Soil PCA Visualization ----------
# Assuming W_summary / W_plot_df from your earlier PCA workflow
pdf("figures/FigureS2_ClimatePCA.pdf", width = 10, height = 6)
ggplot(W_plot_df, aes(x=i, y=mean)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="lightblue", alpha=0.3) +
  geom_line(color="blue") +
  geom_point(aes(y=static_value), color="red") +
  facet_wrap(~j, scales="free_y", labeller=label_both) +
  labs(x="Climate Index", y="Latent Climate (W) vs PCA Projection",
       title="Posterior W vs Original Climate PCA") +
  theme_minimal()
dev.off()

# Replace W_plot_df with soil PCA equivalent for S4
# Assume W_plot_df_soil created earlier in workflow
pdf("figures/FigureS4_SoilPCA.pdf", width = 10, height = 6)
ggplot(W_plot_df_soil, aes(x=i, y=mean)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="lightgreen", alpha=0.3) +
  geom_line(color="darkgreen") +
  geom_point(aes(y=static_value), color="red") +
  facet_wrap(~j, scales="free_y", labeller=label_both) +
  labs(x="Soil Index", y="Latent Soil (W) vs PCA Projection",
       title="Posterior W vs Original Soil PCA") +
  theme_minimal()
dev.off()

