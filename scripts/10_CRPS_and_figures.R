################# Bromecast: 10.CRPS_and_figures.R ##########################
############# created 3-25-25 ######################
############# Last modified: 10-13-25 ##########################
######## CRPS & Skill Scores for all model variants ################################

# ========== SETUP ==========
library(cmdstanr)
library(posterior)
library(scoringRules)
library(tibble)
library(dplyr)
library(readr)


# ---------- Observed test data ----------
obs_test <- list(
  emerged    = testing_df_emg$e_test,
  reproduced = testing_df_emg$r_test,
  fecund     = testing_df_emg$Fecundity
)

# ---------- Manual fallback for missing null ----------
null_manual_stage <- c(
  emerged    = 0.332,
  reproduced = 0.489,
  fecund     = 0.365
)

# ---------- Get actual existing files ----------
all_files <- list.files("output", pattern = "^gqs_.*\\.rds$", full.names = TRUE)

# Extract stage and variant from filename
file_info <- tibble(
  file_path = all_files
) %>%
  mutate(
    file_name = basename(file_path),
    stage   = sub("^gqs_(.*?)_.*\\.rds$", "\\1", file_name),
    variant = sub("^gqs_.*?_(.*?)\\.rds$", "\\1", file_name)
  )

# ---------- Initialize result columns ----------
file_info <- file_info %>%
  mutate(
    crps_full  = NA_real_,
    crps_null  = NA_real_,
    skill_score = NA_real_
  )

# ---------- Loop through existing files ----------
for (i in seq_len(nrow(file_info))) {
  stage   <- file_info$stage[i]
  variant <- file_info$variant[i]
  file_path <- file_info$file_path[i]
  
  gqs <- readRDS(file_path)
  pred_names <- names(gqs)[grepl("test_pred", names(gqs))]
  
  if (length(pred_names) == 0) {
    message("No test predictions found in ", file_path)
    next
  }
  
  pred_draws <- as.matrix(gqs[[pred_names[1]]])
  obs <- obs_test[[stage]]
  
  # ---- CRPS for full model ----
  if (stage == "reproduced") {
    file_info$crps_full[i] <- mean(crps_binom(y = obs, size = 1, prob = rowMeans(pred_draws)))
  } else {
    file_info$crps_full[i] <- mean(crps_nb(y = obs, size = 10, mean = rowMeans(pred_draws)))
  }
  
  # ---- CRPS for null model ----
  if (variant == "null") {
    file_info$crps_null[i] <- file_info$crps_full[i]
  } else {
    null_file <- paste0("output/gqs_", stage, "_null.rds")
    if (file.exists(null_file)) {
      gqs_null <- readRDS(null_file)
      null_pred_names <- names(gqs_null)[grepl("test_pred", names(gqs_null))]
      pred_draws_null <- as.matrix(gqs_null[[null_pred_names[1]]])
      
      if (stage == "reproduced") {
        file_info$crps_null[i] <- mean(crps_binom(y = obs, size = 1, prob = rowMeans(pred_draws_null)))
      } else {
        file_info$crps_null[i] <- mean(crps_nb(y = obs, size = 10, mean = rowMeans(pred_draws_null)))
      }
    } else {
      file_info$crps_null[i] <- null_manual_stage[[stage]]
    }
  }
  
  # ---- Skill score ----
  file_info$skill_score[i] <- 1 - (file_info$crps_full[i] / file_info$crps_null[i])
}

# ---------- Final output ----------
crps_df <- file_info %>%
  select(Stage = stage, Variant = variant, CRPS_Full = crps_full, CRPS_Null = crps_null, Skill_Score = skill_score) %>%
  mutate(across(c(CRPS_Full, CRPS_Null, Skill_Score), ~round(., 3)))

print(crps_df)
write_csv(crps_df, "output/crps_skill_scores.csv")


# ========== OPTIONAL: COMBINE ACROSS LIFE STAGES ==========
crps_summary <- crps_df %>%
  tidyr::separate(Model, into = c("Stage", "Variant"), sep = "_") %>%
  group_by(Variant) %>%
  summarise(
    mean_skill = mean(Skill_Score, na.rm = TRUE),
    stages_included = paste(Stage[!is.na(Skill_Score)], collapse = ", ")
  )

print(crps_summary)
write_csv(crps_summary, "output/crps_skill_summary.csv")

# ===================== CALCULATE FITNESS (unchanged) =====================
# [Your fitness computation code starts here, same as in your current workflow]

#source("scripts/09_Fit_Models.R")

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

