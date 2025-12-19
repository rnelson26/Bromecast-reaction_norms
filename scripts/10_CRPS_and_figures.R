################# Bromecast: 10.CRPS_and_figures.R ##########################
############# created 3-25-25 ######################
############# Last modified: 12-19-25 ##########################
######## CRPS & Skill Scores for all model variants ################################



# ####### load packages and data
source("scripts/05_prepare_data.R")

library(cmdstanr)
library(posterior)
library(scoringRules)
library(tibble)
library(dplyr)
library(readr)
library(tidyverse)

##### CRPS updated version #######


# --- Function to extract draws as numeric matrix ---
extract_draws <- function(draws, var_base) {
  cols <- grep(paste0("^", var_base, "\\["), names(draws))
  if (length(cols) == 0) stop(paste("Variable", var_base, "not found in draws df"))
  mat <- as.matrix(draws[, cols])
  mat <- apply(mat, 2, as.numeric)
  return(mat)
}

# --- Function to calculate CRPS ---
get_crps <- function(obs, pred_df) {
  pred_mat <- as.matrix(pred_df)
  if (nrow(pred_mat) != length(obs)) pred_mat <- t(pred_mat)
  crps_sample(y = as.numeric(obs), dat = pred_mat)
}

# --- Observed data ---
obs_list <- list(
  e_train  = training_df_emg$e_train,
  e_train_fixed = training_df_emg$e_train,
  e_test   = testing_df_emg$e_test,
  
  r_train = training_df_rep$r_train,
  r_train_fixed = training_df_rep$r_train,
  r_test  = testing_df_rep$r_test,
  
  y_train = training_df$Fecundity,
  y_train_fixed = training_df$Fecundity,
  y_test  = testing_df$Fecundity
)

# --- Set up draw files ---
base_dir <- "/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Dec_2025"

all_files <- list.files(base_dir, pattern = "^fit_.*\\.rds$", full.names = TRUE)

file_info <- tibble(file_path = all_files) %>%
  mutate(
    file_name   = basename(file_path),
    stage       = sub("^fit_(.*?)_.*\\.rds$", "\\1", file_name),
    variant     = sub("^fit_.*?_(.*?)\\.rds$", "\\1", file_name),
    crps_train  = NA_real_,
    crps_train_fixed = NA_real_,
    crps_test   = NA_real_,
    null_crps_train = NA_real_,   # <-- ADD
    null_crps_test  = NA_real_,   # <-- ADD
    skill_train = NA_real_,
    skill_train_fixed = NA_real_,
    skill_test  = NA_real_
  )



null_crps <- list()

for (stg in unique(file_info$stage)) {
  
  null_file <- file.path(base_dir, paste0("fit_", stg, "_null_draws.rds"))
  if (!file.exists(null_file)) next
  
  null_draws <- readRDS(null_file)
  
  var_map <- switch(stg,
                    emerged    = c(train = "e_train_pred", test = "e_test_pred"),
                    reproduced = c(train = "r_train_pred", test = "r_test_pred"),
                    fecundity  = c(train = "y_train_pred", test = "y_test_pred")
  )
  
  obs_map <- switch(stg,
                    emerged    = c(train = "e_train", test = "e_test"),
                    reproduced = c(train = "r_train", test = "r_test"),
                    fecundity  = c(train = "y_train", test = "y_test")
  )
  
  null_crps[[stg]] <- list()
  
  for (nm in names(var_map)) {
    pred <- extract_draws(null_draws, var_map[nm])
    obs  <- obs_list[[obs_map[nm]]]
    if (nrow(pred) != length(obs)) pred <- t(pred)
    null_crps[[stg]][[nm]] <- mean(get_crps(obs, pred))
  }
}

# ================================================================
# STEP 2: MAIN LOOP
# ================================================================
for (i in seq_len(nrow(file_info))) {
  
  draws   <- readRDS(file_info$file_path[i])
  stage   <- file_info$stage[i]
  variant <- file_info$variant[i]
  
  var_map <- switch(stage,
                    emerged    = c(train = "e_train_pred",
                                   train_fixed = "e_train_pred_fixed",
                                   test = "e_test_pred"),
                    reproduced = c(train = "r_train_pred",
                                   train_fixed = "r_train_pred_fixed",
                                   test = "r_test_pred"),
                    fecundity  = c(train = "y_train_pred",
                                   train_fixed = "y_train_pred_fixed",
                                   test = "y_test_pred")
  )
  
  obs_map <- switch(stage,
                    emerged    = c(train = "e_train",
                                   train_fixed = "e_train_fixed",
                                   test = "e_test"),
                    reproduced = c(train = "r_train",
                                   train_fixed = "r_train_fixed",
                                   test = "r_test"),
                    fecundity  = c(train = "y_train",
                                   train_fixed = "y_train_fixed",
                                   test = "y_test")
  )
  
  # --- CRPS ---
  for (nm in names(var_map)) {
    
    pred <- tryCatch(
      extract_draws(draws, var_map[nm]),
      error = function(e) NULL
    )
    if (is.null(pred)) next
    
    obs <- obs_list[[obs_map[nm]]]
    if (nrow(pred) != length(obs)) pred <- t(pred)
    
    file_info[[paste0("crps_", nm)]][i] <-
      mean(get_crps(obs, pred))
  }
  
  # --- Attach null CRPS ---
  file_info$null_crps_train[i] <- null_crps[[stage]]$train
  file_info$null_crps_test[i]  <- null_crps[[stage]]$test
  
  # --- Skill scores ---
  if (variant == "null") {
    file_info$skill_train[i]        <- 0
    file_info$skill_train_fixed[i]  <- 0
    file_info$skill_test[i]         <- 0
  } else {
    if (!is.na(file_info$null_crps_train[i])) {
      file_info$skill_train[i] <-
        1 - file_info$crps_train[i] / file_info$null_crps_train[i]
      
      file_info$skill_train_fixed[i] <-
        1 - file_info$crps_train_fixed[i] / file_info$null_crps_train[i]
    }
    
    if (!is.na(file_info$null_crps_test[i])) {
      file_info$skill_test[i] <-
        1 - file_info$crps_test[i] / file_info$null_crps_test[i]
    }
  }
  
  message(
    "Processed: ", file_info$file_name[i],
    " | Skill train = ", round(file_info$skill_train[i], 3),
    " | Skill test = ", round(file_info$skill_test[i], 3)
  )
}



# --- Final output ---
file_info
output <- as.data.frame(file_info)

########### results table #######
library(flextable)
library(officer)
library(dplyr)


table_df <- output %>%
  select(
    stage,
    file_name,
    crps_train,
    crps_train_fixed,
    crps_test,
    null_crps_train,
    null_crps_test,
    skill_train,
    skill_train_fixed,
    skill_test
  ) %>%
  arrange(stage, desc(skill_test)) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

ft <- flextable(table_df) %>%
  set_header_labels(
    stage              = "Stage",
    file_name            = "Model variant",
    crps_train         = "CRPS (train)",
    crps_train_fixed   = "CRPS (train, fixed)",
    crps_test          = "CRPS (test)",
    null_crps_train    = "Null CRPS (train)",
    null_crps_test     = "Null CRPS (test)",
    skill_train        = "Skill (train)",
    skill_train_fixed  = "Skill (train, fixed)",
    skill_test         = "Skill (test)"
  ) %>%
  theme_booktabs() %>%
  autofit()


#ft <- ft %>%
 # bold(j = "skill_test", bold = TRUE) %>%
  #color(
   # i = ~ grepl("null", variant),
    #color = "gray40"
 # )

ft <- ft %>%
  merge_v(j = "stage") %>%
  valign(j = "stage", valign = "top")

doc <- read_docx() %>%
  body_add_par("Model performance summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "CRPS_skill_summary.docx")



######## confusion matrix for Reproduced & Emerged models #################

library(caret)
library(purrr)


# --- Function to compute posterior mean confusion matrix ---
get_posterior_cm <- function(draws, var_name, obs) {
  
  pred_mat <- extract_draws(draws, var_name)
  
  # Ensure orientation: rows = draws, cols = observations
  if (ncol(pred_mat) != length(obs)) pred_mat <- t(pred_mat)
  
  # Confusion matrix per draw
  cm_df <- map_dfr(seq_len(nrow(pred_mat)), function(d) {
    pred_class <- as.integer(pred_mat[d, ])
    
    tab <- table(
      Prediction = factor(pred_class, levels = c(0, 1)),
      Reference  = factor(obs,        levels = c(0, 1))
    )
    
    tibble(
      TP = tab["1", "1"],
      FP = tab["1", "0"],
      FN = tab["0", "1"],
      TN = tab["0", "0"]
    )
  })
  
  # Posterior mean confusion matrix
  cm_df %>%
    summarise(across(TP:TN, mean))
}

# loop over models 

confusion_results <- list()

for (i in seq_len(nrow(file_info))) {
  
  stage   <- file_info$stage[i]
  variant <- file_info$variant[i]
  
  # Only binary stages
  if (!stage %in% c("emerged", "reproduced")) next
  
  draws <- readRDS(file_info$file_path[i])
  
  var_map <- switch(stage,
                    emerged = c(
                      train        = "e_train_pred",
                      train_fixed  = "e_train_pred_fixed",
                      test         = "e_test_pred"
                    ),
                    reproduced = c(
                      train        = "r_train_pred",
                      train_fixed  = "r_train_pred_fixed",
                      test         = "r_test_pred"
                    )
  )
  
  obs_map <- switch(stage,
                    emerged = c(
                      train        = "e_train",
                      train_fixed  = "e_train_fixed",
                      test         = "e_test"
                    ),
                    reproduced = c(
                      train        = "r_train",
                      train_fixed  = "r_train_fixed",
                      test         = "r_test"
                    )
  )
  
  for (nm in names(var_map)) {
    
    # Skip missing prediction variables
    pred <- tryCatch(
      extract_draws(draws, var_map[nm]),
      error = function(e) NULL
    )
    if (is.null(pred)) next
    
    obs <- obs_list[[obs_map[nm]]]
    
    cm <- get_posterior_cm(draws, var_map[nm], obs)
    
    confusion_results[[length(confusion_results) + 1]] <-
      cm %>%
      mutate(
        stage    = stage,
        variant  = variant,
        dataset  = nm,
        file     = file_info$file_name[i]
      )
  }
}


# FINAL CONFUSION MATRIX TABLE


confusion_df <- bind_rows(confusion_results) %>%
  relocate(stage, variant, dataset, file)

confusion_df
#Posterior mean counts of observations classified as TP (true positive), FP (false postive), FN (false negative), and TN (and true negative).

## plot them
cm_long <- confusion_df %>%
  pivot_longer(
    cols = TP:TN,
    names_to = "cell",
    values_to = "count"
  ) %>%
  mutate(
    Reference = if_else(cell %in% c("TP", "FN"), "1", "0"),
    Prediction = if_else(cell %in% c("TP", "FP"), "1", "0"),
    Reference = factor(Reference, levels = c("1", "0")),
    Prediction = factor(Prediction, levels = c("1", "0"))
  )

cm_long <- cm_long %>%
  mutate(
    data_split = case_when(
      dataset %in% c("train") ~ "train",
      dataset %in% c("train_fixed") ~ "train_fixed",
      dataset == "test"                      ~ "test"
    )
  )


plot_confusion_heatmap_split <- function(df, variant_name, split_name) {
  
  ggplot(df, aes(x = Prediction, y = Reference, fill = count)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.1f", count)), size = 4) +
    scale_fill_viridis_c(option = "C", direction = -1) +
    facet_wrap(~ stage, nrow = 1) +
    coord_equal() +
    labs(
      title = paste(
        "Posterior Mean Confusion Matrix:",
        variant_name, "-", split_name
      ),
      x = "Predicted",
      y = "Observed",
      fill = "Mean count"
    ) +
    theme_classic(base_size = 12) +
    theme(
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

output_dir <- "confusion_matrix_heatmaps"
dir.create(output_dir, showWarnings = FALSE)

for (f in unique(cm_long$file)) {
  for (s in c("train", "test", "train_fixed")) {
    
    df_plot <- cm_long %>%
      filter(file == f, data_split == s)
    
    # Skip if no data
    if (nrow(df_plot) == 0) next
    
    p <- plot_confusion_heatmap_split(
      df = df_plot,
      variant_name = f,
      split_name = s
    )
    
    safe_f <- gsub("[^A-Za-z0-9_]", "_", f)
    
    ggsave(
      filename = file.path(
        output_dir,
        paste0("confusion_heatmap_", safe_f, "_", s, ".pdf")
      ),
      plot = p,
      width = 7,
      height = 4
    )
  }
}





################# Old CODE/ IN PROGRESSS ####################


##### version 2

# Load posterior predictive draws
gqs <- readRDS("output/gqs_emerged_full.rds")  # example for one model

# Extract predictions
r_train_pred <- gqs$e_train_pred      # array: n_draws x n_train
r_test_pred  <- gqs$r_test_pred
r_train_pred_fixed <- gqs$r_train_pred_fixed
r_test_pred_fixed  <- gqs$r_test_pred_fixed

# Load observed outcomes
r_train <- stan_data_emg_full$r_train  # vector of 0/1
r_test  <- stan_data_emg_full$r_test   # vector of 0/1 (if defined)


# Majority vote across posterior draws
r_train_pred_summary <- apply(r_train_pred, 2, function(x) as.integer(mean(x) > 0.5))
r_test_pred_summary <- apply(r_test_pred, 2, function(x) as.integer(mean(x) > 0.5))
r_train_fixed_summary <- apply(r_train_pred_fixed, 2, function(x) as.integer(mean(x) > 0.5))
confusionMatrix(factor(r_train_fixed_summary), factor(r_train))

# Base R confusion matrix
conf_train <- table(Predicted = r_train_pred_summary, Observed = r_train)
conf_test  <- table(Predicted = r_test_pred_summary, Observed = r_test)

print(conf_train)
print(conf_test)

library(caret)

confusionMatrix(
  factor(r_train_pred_summary),
  factor(r_train),
  positive = "1"
)

confusionMatrix(
  factor(r_test_pred_summary),
  factor(r_test),
  positive = "1"
)

## graph matrix
library(tidyverse)

# Example for test set
# r_test_pred is n_draws x n_test (matrix)
n_draws <- nrow(r_test_pred)
n_test <- ncol(r_test_pred)

# Compute predicted probability per observation
pred_prob <- colMeans(r_test_pred) # mean over draws

df_plot <- tibble(
  observed = r_test,      # 0 or 1
  predicted_prob = pred_prob
)

ggplot(df_plot, aes(x = predicted_prob, y = as.factor(observed))) +
  geom_jitter(width = 0.02, height = 0.1, alpha = 0.5, color = "blue") +
  geom_point(aes(y = 0.5), color = "red") +  # optional: threshold line
  labs(x = "Predicted probability", y = "Observed outcome") +
  theme_minimal()

library(caret)
library(purrr)

# Threshold predicted probability for each draw
threshold <- 0.5
conf_mats <- map(1:n_draws, function(d) {
  pred_class <- ifelse(r_test_pred[d, ] > threshold, 1, 0)
  confusionMatrix(factor(pred_class), factor(r_test))
})

# Extract metrics (accuracy, sensitivity, specificity)
metrics <- map_dfr(conf_mats, ~ as.data.frame(t(.x$byClass)))
metrics$draw <- 1:n_draws

# Plot distribution of accuracy
ggplot(metrics, aes(x = draw, y = Accuracy)) +
  geom_line(alpha = 0.3) +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 2) +
  labs(y = "Accuracy", x = "Posterior draw") +
  theme_minimal()


df_density <- tibble(
  observed = rep(r_test, each = n_draws),
  predicted = as.vector(t(r_test_pred))
)

ggplot(df_density, aes(x = predicted, fill = as.factor(observed))) +
  geom_density(alpha = 0.4) +
  labs(x = "Posterior predictive", fill = "Observed") +
  theme_minimal()

cm <- table(Predicted = pred_label, Observed = r_test)
library(ggplot2)
library(reshape2)

cm_melt <- melt(cm)
ggplot(cm_melt, aes(x = Observed, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()



########## older code #######################

##### null model #######
draws <- readRDS("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/output/from megan/fit_emerged_draws_sub/fit_emerged_null_draws_sub.rds")

# 2. Check the structure
str(draws)        # gives an overview of the object type and contents
class(draws)      # tells you if it's a list, CmdStanMCMC, data.frame, etc.
names(draws)      # if it's a list, shows what elements you can access

focal_vars <- c("e_test_pred", "e_train_pred", "e_train_pred_fixed")

# Use grepl to see if the variable names exist
sapply(focal_vars, function(fv) any(grepl(fv, names(draws))))

####### CRPS older version ######
library(cmdstanr)
library(posterior)
library(scoringRules)
library(tibble)
library(dplyr)
library(readr)
library(tidyverse)


# --- Function to extract draws as numeric matrix ---
extract_draws <- function(draws, var_base) {
  # select columns that start with var_base + "["
  cols <- grep(paste0("^", var_base, "\\["), names(draws))
  if (length(cols) == 0) stop(paste("Variable", var_base, "not found in draws df"))
  
  mat <- as.matrix(draws[, cols])
  mat <- apply(mat, 2, as.numeric)  # ensure numeric
  return(mat)  # rows = obs, columns = draws
}

# --- Function to calculate CRPS using posterior draws ---
get_crps <- function(obs, pred_df) {
  # crps_sample expects rows = observations, columns = draws
  pred_mat <- as.matrix(pred_df)
  if (nrow(pred_mat) != length(obs)) {
    pred_mat <- t(pred_mat)
  }
  crps_sample(y = as.numeric(obs), dat = pred_mat)
}

# --- Observed test data ---
obs_test <- list(
  emerged    = testing_df_emg$e_test,
  reproduced = testing_df_emg$r_test,
  fecundity  = testing_df_emg$Fecundity
)

# --- Set up draw files ---
base_dir <- "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/output/from megan/fit_emerged_draws_sub"
all_files <- list.files(base_dir, pattern = "^fit_emerged_.*\\.rds$", full.names = TRUE)

# --- Main loop to compute CRPS and skill scores ---
for (i in seq_len(nrow(file_info))) {
  
  draws   <- readRDS(file_info$file_path[i])
  stage   <- file_info$stage[i]
  variant <- file_info$variant[i]
  
  # Determine posterior predictive variable
  pred_var <- switch(stage,
                     emerged    = "e_test_pred",
                     reproduced = "r_test_pred",
                     fecundity  = "y_test_pred")
  
  # Safe extraction: skip file if variable not present
  pred_draws <- tryCatch(
    extract_draws(draws, pred_var),
    error = function(e) {
      warning(paste("Skipping file:", file_info$file_name[i], " —", e$message))
      return(NULL)
    }
  )
  if (is.null(pred_draws)) next
  
  # Observed values
  if (!stage %in% names(obs_test)) {
    warning(paste("No observed data for stage:", stage))
    next
  }
  obs <- as.numeric(obs_test[[stage]])
  
  # Ensure matrix rows = observations, columns = draws
  if (nrow(pred_draws) != length(obs)) pred_draws <- t(pred_draws)
  
  # --- CRPS for full model ---
  file_info$crps_full[i] <- mean(get_crps(obs, pred_draws))
  
  # --- CRPS for null model ---
  if (variant == "null") {
    file_info$crps_null[i] <- file_info$crps_full[i]
  } else {
    null_file <- file.path(base_dir, paste0("fit_", stage, "_null.rds"))
    if (file.exists(null_file)) {
      null_draws <- readRDS(null_file)
      pred_null  <- tryCatch(extract_draws(null_draws, pred_var),
                             error = function(e) {
                               warning(paste("Skipping null for file:", file_info$file_name[i]))
                               return(NULL)
                             })
      if (!is.null(pred_null)) {
        if (nrow(pred_null) != length(obs)) pred_null <- t(pred_null)
        file_info$crps_null[i] <- mean(get_crps(obs, pred_null))
      } else {
        file_info$crps_null[i] <- NA_real_
      }
    } else {
      file_info$crps_null[i] <- NA_real_
    }
  }
  
  # --- Skill score ---
  if (!is.na(file_info$crps_null[i]) && file_info$crps_null[i] != 0) {
    file_info$skill_score[i] <- 1 - (file_info$crps_full[i] / file_info$crps_null[i])
  } else {
    file_info$skill_score[i] <- NA_real_
  }
  
  message("Processed file: ", file_info$file_name[i], 
          " | CRPS full = ", round(file_info$crps_full[i], 4))
}

# --- Inspect results ---
file_info




####### CRPS ################
library(tidyverse)
library(scoringRules)  # for crps_sample

# --- Function to extract draws as numeric matrix ---
extract_draws <- function(draws, var_name) {
  if (is.data.frame(draws)) {
    cols <- grep(paste0("^", var_name, "(\\[|$)"), names(draws))
    if (length(cols) == 0) stop(paste("Variable", var_name, "not found in draws df"))
    mat <- as.matrix(draws[, cols])
    mat <- apply(mat, 2, as.numeric)
    return(mat)
  } else if (is.list(draws)) {
    # list with named elements
    cols <- grep(paste0("^", var_name, "(\\[|$)"), names(draws))
    if (length(cols) == 0) stop(paste("Variable", var_name, "not found in draws list"))
    mat <- as.matrix(draws[cols])
    mat <- apply(mat, 2, as.numeric)
    return(mat)
  } else if (is.matrix(draws)) {
    return(apply(draws, 2, as.numeric))
  } else {
    stop("Unknown draws object type")
  }
}

# --- Function to calculate CRPS using posterior draws ---
get_crps <- function(obs, pred_df) {
  pred_mat <- t(as.matrix(pred_df))   # transpose: rows = obs, columns = draws
  obs <- as.numeric(obs)              # ensure numeric
  crps_sample(y = obs, dat = pred_mat)
}

# --- Observed test data ---
obs_test <- list(
  emerged    = testing_df_emg$e_test,
  reproduced = testing_df_emg$r_test,
  fecundity  = testing_df_emg$Fecundity
)

# --- Set up draw files ---
base_dir <- "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/output/from megan/fit_emerged_draws_sub"
all_files <- list.files(base_dir, pattern = "^fit_emerged_.*\\.rds$", full.names = TRUE)

file_info <- tibble(file_path = all_files) %>%
  mutate(
    file_name   = basename(file_path),
    stage       = sub("^fit_(.*?)_.*\\.rds$", "\\1", file_name),
    variant     = sub("^fit_.*?_(.*?)\\.rds$", "\\1", file_name),
    crps_full   = NA_real_,
    crps_null   = NA_real_,
    skill_score = NA_real_
  )

# --- Main loop to compute CRPS and skill scores ---
for (i in seq_len(nrow(file_info))) {
  
  draws   <- readRDS(file_info$file_path[i])
  stage   <- file_info$stage[i]
  variant <- file_info$variant[i]
  
  # Determine posterior predictive variable
  pred_var <- switch(stage,
                     emerged    = "e_test_pred",
                     reproduced = "r_test_pred",
                     fecundity  = "y_test_pred")
  
  # Extract posterior predictive draws
  pred_draws <- extract_draws(draws, pred_var)
  
  # Observed values
  if (!stage %in% names(obs_test)) {
    warning(paste("No observed data for stage:", stage))
    next
  }
  obs <- as.numeric(obs_test[[stage]])
  
  # Check lengths match
  if (length(obs) != nrow(pred_draws)) {
    warning(paste("Length mismatch for stage:", stage, 
                  "obs =", length(obs), "rows in draws =", nrow(pred_draws)))
    next
  }
  
  # --- CRPS for full model ---
  file_info$crps_full[i] <- mean(get_crps(obs, pred_draws))
  
  # --- CRPS for null model ---
  if (variant == "null") {
    file_info$crps_null[i] <- file_info$crps_full[i]
  } else {
    null_file <- file.path(base_dir, paste0("fit_", stage, "_null.rds"))
    if (file.exists(null_file)) {
      null_draws <- readRDS(null_file)
      pred_null  <- extract_draws(null_draws, pred_var)
      if (nrow(pred_null) == length(obs)) {
        file_info$crps_null[i] <- mean(get_crps(obs, pred_null))
      } else {
        file_info$crps_null[i] <- NA_real_
        warning("Null draws and obs length mismatch, skipping CRPS for null")
      }
    } else {
      file_info$crps_null[i] <- NA_real_
    }
  }
  
  # --- Skill score ---
  if (!is.na(file_info$crps_null[i]) && file_info$crps_null[i] != 0) {
    file_info$skill_score[i] <- 1 - (file_info$crps_full[i] / file_info$crps_null[i])
  } else {
    file_info$skill_score[i] <- NA_real_
  }
  
  message("Processed file: ", file_info$file_name[i], 
          " | CRPS full = ", round(file_info$crps_full[i], 4))
}

# --- Inspect results ---
file_info


## old 
# 1. Load the RDS file
draws <- readRDS( "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/output/from megan/fit_emerged_draws_sub/fit_emerged_full_draws_sub.rds")

# 2. Inspect what is inside
str(draws)
names(draws)  # if it's a list

# 3. Extract the draws you want
# If draws is a list with matrices/vectors
draws_e_full <- draws  # sometimes just the whole object
# Train posterior draws
e_train_pred_full <- draws %>% dplyr::select(starts_with("e_train_pred[")) %>% as.matrix()

# Train fixed effect draws (if present)
e_train_fixed_full <- draws %>% dplyr::select(starts_with("e_train_pred_fixed[")) %>% as.matrix()

# Test posterior draws
e_test_pred_full <- draws %>% dplyr::select(starts_with("e_test_pred[")) %>% as.matrix()


# 4. Check dimensions
dim(e_train_pred_full)
dim(e_train_fixed_pred_full)
dim(e_test_pred_full)

# 5. Calculate rowMeans if needed for CRPS
pred_mean <- rowMeans(e_test_pred_full)

# 6. Observed test data
obs <- testing_df$e_test  # or testing_df_emg$e_test

# 7. Compute CRPS for this single file
crps_value <- mean(crps_binom(y = obs, size = 1, prob = pred_mean))
crps_value



#draws_e_full <- fit_emg_full$draws(format = "df")
#e_train_pred_full <- draws_e_full[, grep("^e_train_pred\\[", names(draws_e_full))]
#e_train_fixed_pred_full <- draws_e_full[, grep("^e_train_pred_fixed\\[", names(draws_e_full))]
#e_test_pred_full <- draws_e_full[, grep("^e_test_pred\\[", names(draws_e_full))]
  

# Calculate CRPS and save results ########################
crps_df <- file_info %>%
  select(Stage = stage, Variant = variant, CRPS_Full = crps_full, CRPS_Null = crps_null, Skill_Score = skill_score) %>%
  mutate(across(c(CRPS_Full, CRPS_Null, Skill_Score), ~round(., 3)))

print(crps_df)
write_csv(crps_df, "output/crps_skill_scores.csv")

crps_summary <- crps_df %>%
  tidyr::separate(Model, into = c("Stage", "Variant"), sep = "_") %>%
  group_by(Variant) %>%
  summarise(
    mean_skill = mean(Skill_Score, na.rm = TRUE),
    stages_included = paste(Stage[!is.na(Skill_Score)], collapse = ", ")
  )

print(crps_summary)
write_csv(crps_summary, "output/crps_skill_summary.csv")

########### Calculate Fitness ################################

#training_df_emg$r_train <- ifelse(training_df_emg$Reproduced == "Y", 1L, 0L)
#testing_df_emg$r_test <- ifelse(testing_df_emg$Reproduced == "Y", 1L, 0L)

## calculate observed fitness 
testing_df_emg <- testing_df_emg %>% dplyr::mutate(Obs_Fitness = e_test * r_test * Fecundity)
training_df_emg <- training_df_emg %>% mutate(Obs_Fitness = e_train * r_train * Fecundity)

testing_df_emg$Obs_Fitness_log <- log(testing_df_emg$Obs_Fitness)
training_df_emg$Obs_Fitness_log <- log(training_df_emg$Obs_Fitness)


##### with draws
### posterior 
p_emg_test <- fit_emg_full$draws("p_test", format = "draws_matrix")  
p_emg_train <- fit_emg_full$draws("p_train", format = "draws_matrix") 

p_rep_test <- fit_rep$draws("p_test_full", format = "draws_matrix")  
p_rep_train <- fit_rep$draws("p_train_full", format = "draws_matrix") 

p_fec_test <- fit$draws("mu_test_full", format = "draws_matrix")  
p_fec_train <- fit$draws("mu_train_full", format = "draws_matrix")

## don't do both distributions at same time because of vector memory limit 
### posterior predictive 
e_emg_test <- fit_emg_full$draws("e_test_pred", format = "draws_matrix")  
e_emg_train <- fit_emg_full$draws("e_train_pred", format = "draws_matrix") 

r_rep_test <- fit_rep$draws("r_test_full", format = "draws_matrix")  
r_rep_train <- fit_rep$draws("r_train_full", format = "draws_matrix") 

y_fec_test <- fit$draws("y_test_pred_full", format = "draws_matrix")  
y_fec_train <- fit$draws("y_train_pred_full", format = "draws_matrix")

###### fixed effects only for training data
### posterior 
p_emg_train_fixed <- fit_emg_full$draws("p_train_fixed", format = "draws_matrix") 

p_rep_train_fixed <- fit_rep$draws("p_train_full_fixed", format = "draws_matrix") 

p_fec_train_fixed <- fit$draws("mu_train_full_fixed", format = "draws_matrix")

## don't do both distributions at same time because of vector memory limit 
### posterior predictive 
e_emg_train_fixed <- fit_emg_full$draws("e_train_pred_fixed", format = "draws_matrix") 

r_rep_train_fixed <- fit_rep$draws("r_train_full_fixed", format = "draws_matrix") 

y_fec_train_fixed <- fit$draws("y_train_pred_full_fixed", format = "draws_matrix")


## posterior 
fitness_draws_test <- p_emg_test * p_rep_test * p_fec_test
fitness_draws_train <- p_emg_train * p_rep_train * p_fec_train
fitness_draws_train_fixed <- p_emg_train_fixed * p_rep_train_fixed * p_fec_train_fixed

log_fitness_draws_test =log(p_emg_test) + log(p_rep_test) + log(p_fec_test)
log_fitness_draws_train =log(p_emg_train) + log(p_rep_train) + log(p_fec_train)
log_fitness_draws_train_fixed =log(p_emg_train_fixed) + log(p_rep_train_fixed) + log(p_fec_train_fixed)
#transform each realization to take e off, either fitness or log draws lines good --> plot this




mean_fitness_test <- apply(fitness_draws_test, 2, mean)
mean_fitness_train <- apply(fitness_draws_train, 2, mean)
mean_fitness_test_log <- apply(log_fitness_draws_test, 2, mean)
mean_fitness_train_log <- apply(log_fitness_draws_train, 2, mean)
mean_fitness_train_fixed <- apply(fitness_draws_train_fixed, 2, mean)
mean_fitness_train_log_fixed <- apply(log_fitness_draws_train_fixed, 2, mean)

testing_df_emg$Predicted_Fitness <- mean_fitness_test
training_df_emg$Predicted_Fitness <- mean_fitness_train
training_df_emg$Predicted_Fitness_fixed <- mean_fitness_train_fixed

testing_df_emg$Predicted_Fitness_log <- mean_fitness_test_log
training_df_emg$Predicted_Fitness_log <- mean_fitness_train_log
training_df_emg$Predicted_Fitness_log_fixed <- mean_fitness_train_log_fixed

#testing_df_emg$Predicted_Fitness_log <- log(mean_fitness_test + 1)
#testing_df_emg$Predicted_Fitness_log <- log(mean_fitness_test + 1)

## posteior predictive 
fitness_draws_test_pred <- e_emg_test * r_rep_test * y_fec_test
fitness_draws_train_pred <- e_emg_train * r_rep_train * y_fec_train
fitness_draws_train_pred_fixed <- e_emg_train_fixed * r_rep_train_fixed * y_fec_train_fixed

log_fitness_draws_test_pred  = log(e_emg_test) + log(r_rep_test) + log(y_fec_test)
log_fitness_draws_train_pred = log(e_emg_train) + log(r_rep_train) + log(y_fec_train)
log_fitness_draws_train_pred_fixed = log(e_emg_train_fixed) + log(r_rep_train_fixed) + log(y_fec_train_fixed)

#transform each realization to take e off, either fitness or log draws lines good --> plot this

mean_fitness_test_pred <- apply(fitness_draws_test_pred, 2, mean)
mean_fitness_train_pred <- apply(fitness_draws_train_pred, 2, mean)
mean_fitness_test_log_pred <- apply(log_fitness_draws_test_pred, 2, mean)
mean_fitness_train_log_pred <- apply(log_fitness_draws_train_pred, 2, mean)
mean_fitness_train_pred_fixed <- apply(fitness_draws_train_pred_fixed, 2, mean)
mean_fitness_train_log_pred_fixed <- apply(log_fitness_draws_train_pred_fixed, 2, mean)

testing_df_emg$Predicted_Fitness_PostPred <- mean_fitness_test_pred
training_df_emg$Predicted_Fitness_PostPred <- mean_fitness_train_pred
training_df_emg$Predicted_Fitness_PostPred_fixed <- mean_fitness_train_pred_fixed

testing_df_emg$Predicted_Fitness_log_PostPred <- mean_fitness_test_log_pred
training_df_emg$Predicted_Fitness_log_PostPred <- mean_fitness_train_log_pred
training_df_emg$Predicted_Fitness_log_PostPred_fixed <- mean_fitness_train_log_pred_fixed


### from Mevin
## apply transformation first before summary
#fitness_draws <- draws_mu * draws_p_rep * draws_p_emg  # dim: [n_draws x n_ind]
#log_fitness_draws =log(draw_mu) + log(draws_p_rep) + log(draws_p_emg) #transform each realization to take e off, either fitness or log draws lines good --> plot this
#apply(log_fitness_draws, 2, mean) ## posterior mean of log fitness by ind plant
## could get individual plant variance/CI here but won't work for site-year subsets

## mean by individuals in site-year
#mean(of individual mean from apply) --> okay for visualization
## would still be inference on a log scale 
## could messier with non-mean things like variance which is special derived quantity, would have to first make additional derived quantities from which to subset 

#testing_df_emg$PosteriorFitness_drawAvg <- fitness_mean
#testing_df_emg$PosteriorFitness_log <- log(fitness_mean)  # avoids -Inf for 0s

## discrete distribution

### save .csv with model predictions
write.csv(training_df_emg, "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/training_fitness.csv", row.names = FALSE)
write.csv(testing_df_emg, "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/testing_fitness.csv", row.names = FALSE)

###### Fitness Graphs ######
training_fitness <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/training_fitness.csv")
testing_fitness <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/testing_fitness.csv")

### posterior
ggplot(training_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log, color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))

ggplot(training_fitness, aes(x = Predicted_Fitness_log_fixed, y = Obs_Fitness_log, color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))

ggplot(testing_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log, color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))

### posterior Pred
ggplot(training_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = log(Obs_Fitness + 1), color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))

ggplot(training_fitness, aes(x = log(Predicted_Fitness_PostPred_fixed + 1), y = log(Obs_Fitness + 1), color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))

ggplot(testing_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = log(Obs_Fitness + 1), color = Type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + scale_color_manual(values = c("Satellite" = "blue", "Common_Garden"  = "lightblue"))

### heat map


## Posterior - training
ggplot(training_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log)) +
  stat_density_2d_filled(contour = TRUE, bins = 20, alpha = 0.8) +
  facet_wrap(~Type) +  # split by Type
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Training - Posterior", fill = "Point Density")

## Posterior - testing
ggplot(testing_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log)) +
  stat_density_2d_filled(contour = TRUE, bins = 20, alpha = 0.8) +
  facet_wrap(~Type) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Testing - Posterior", fill = "Point Density")

## Posterior Predictive - training
ggplot(training_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = log(Obs_Fitness + 1))) +
  stat_density_2d_filled(contour = TRUE, bins = 20, alpha = 0.8) +
  facet_wrap(~Type) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Training - Posterior Predictive", fill = "Point Density")

## Posterior Predictive - testing
ggplot(testing_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = log(Obs_Fitness + 1))) +
  stat_density_2d_filled(contour = TRUE, bins = 20, alpha = 0.8) +
  facet_wrap(~Type) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Testing - Posterior Predictive", fill = "Point Density")

##### heatmap graphs with points themselves colored ##############
library(ggplot2)
library(ggpointdensity)

## Posterior - training
ggplot(training_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training - Posterior")

ggplot(training_fitness, aes(x = Predicted_Fitness_log_fixed, y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Fixed Only - Posterior")

## Posterior - testing
ggplot(testing_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Testing - Posterior")

## Posterior Predictive - training
ggplot(training_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = log(Obs_Fitness)))  +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training - Posterior Predictive")

ggplot(training_fitness, aes(x = log(Predicted_Fitness_PostPred_fixed + 1), y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Fixed only - Posterior Predictive")

## Posterior Predictive - testing
ggplot(testing_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = log(Obs_Fitness + 1))) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Testing - Posterior Predictive")


### seasonality

ggplot(training_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log, color = seasonality)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() 

ggplot(testing_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log, color = seasonality)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() 

ggplot(training_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = Obs_Fitness_log, color = seasonality)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() 

ggplot(testing_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = Obs_Fitness_log, color = seasonality)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() 


ggplot(training_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log, color = as.factor(genotype))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + facet_wrap(~site_year) +   theme(legend.position = "none")

ggplot(testing_fitness, aes(x = Predicted_Fitness_log, y = Obs_Fitness_log, color = as.factor(genotype))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + facet_wrap(~site_year) + theme(legend.position = "none")

ggplot(training_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = Obs_Fitness_log, color = as.factor(genotype))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + facet_wrap(~site_year) +   theme(legend.position = "none")

ggplot(testing_fitness, aes(x = log(Predicted_Fitness_PostPred + 1), y = Obs_Fitness_log, color = as.factor(genotype))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() + facet_wrap(~site_year) + theme(legend.position = "none")

##############  CRPS Code -- old version ##############################

library(posterior)
library(scoringRules)

# ==== Extract Draws (Main Models) ====

# Fecundity
y_train_pred <- fit$draws("y_train_pred_full", format = "draws_matrix")
y_test_pred  <- fit$draws("y_test_pred_full", format = "draws_matrix")
y_train_pred_fixed <- fit$draws("y_train_pred_full_fixed", format = "draws_matrix")

# Reproduction
r_train_pred <- fit_rep$draws("r_train_full", format = "draws_matrix")
r_test_pred  <- fit_rep$draws("r_test_full", format = "draws_matrix")
r_train_pred_fixed <- fit_rep$draws("r_train_full_fixed", format = "draws_matrix")

r_train_pred_clim <- fit_rep_clim$draws("r_train_full", format = "draws_matrix")
r_test_pred_clim  <- fit_rep_clim$draws("r_test_full", format = "draws_matrix")
#r_train_pred_fixed_clim <- fit_rep_clim$draws("r_train_full_fixed", format = "draws_matrix")
## still ned to add 

r_train_pred_nogene <- fit_rep_nogene$draws("r_train_full", format = "draws_matrix")
r_test_pred_nogene  <- fit_rep_nogene$draws("r_test_full", format = "draws_matrix")
#r_train_pred_nogene <- fit_rep_nogene$draws("r_train_full_fixed", format = "draws_matrix")

r_train_pred_nocomp <- fit_rep_nocomp$draws("r_train_full", format = "draws_matrix")
r_test_pred_nocomp <- fit_rep_nocomp$draws("r_test_full", format = "draws_matrix")
#r_train_pred_fixed_nocomp <- fit_rep_nocomp$draws("r_train_full_fixed", format = "draws_matrix")

r_train_pred_nointer <- fit_rep_nointer$draws("r_train_full", format = "draws_matrix")
r_test_pred_nointer  <- fit_rep_nointer$draws("r_test_full", format = "draws_matrix")
#r_train_pred_fixed_nointer <- fit_rep_nointer$draws("r_train_full_fixed", format = "draws_matrix")

# Emergence
e_train_pred <- fit_emg_full$draws("e_train_pred", format = "draws_matrix")
e_test_pred  <- fit_emg_full$draws("e_test_pred", format = "draws_matrix")
e_train_pred_fixed <- fit_emg_full$draws("e_train_pred_fixed", format = "draws_matrix")

e_train_pred_clim <- fit_emg_clim$draws("e_train_pred", format = "draws_matrix")
e_test_pred_clim  <- fit_emg_clim$draws("e_test_pred", format = "draws_matrix")
#r_train_pred_fixed_clim <- fit_rep_clim$draws("r_train_full_fixed", format = "draws_matrix")
## still ned to add 

e_train_pred_nogene <- fit_emg_nogene$draws("e_train_pred", format = "draws_matrix")
e_test_pred_nogene  <- fit_emg_nogene$draws("e_test_pred", format = "draws_matrix")
#r_train_pred_nogene <- fit_rep_nogene$draws("r_train_full_fixed", format = "draws_matrix")

e_train_pred_nocomp <- fit_emg_nocomp$draws("e_train_pred", format = "draws_matrix")
e_test_pred_nocomp <- fit_emg_nocomp$draws("e_test_pred", format = "draws_matrix")
#r_train_pred_fixed_nocomp <- fit_rep_nocomp$draws("r_train_full_fixed", format = "draws_matrix")

e_train_pred_nointer <- fit_emg_nointer$draws("e_train_pred", format = "draws_matrix")
e_test_pred_nointer  <- fit_emg_nointer$draws("e_test_pred", format = "draws_matrix")
#r_train_pred_fixed_nointer <- fit_rep_nointer$draws("r_train_full_fixed", format = "draws_matrix")


# ==== Observed Data ====
y_train_obs <- training_df_emg$Fecundity
y_test_obs  <- testing_df_emg$Fecundity

r_train_obs <- training_df_emg$r_train
r_test_obs  <- testing_df_emg$r_test

e_train_obs <- training_df_emg$e_train
e_test_obs  <- testing_df_emg$e_test

# ==== Null Model Draws (draws_matrix format assumed) ====
# You can skip these lines if you already have the precomputed values below
#y_train_pred_null <- fit_null_fec$draws("y_train_pred", format = "draws_matrix")
#r_train_pred_null <- fit_null_rep$draws("r_train_pred", format = "draws_matrix")
#e_train_pred_null <- fit_null_emg$draws("e_train_pred", format = "draws_matrix")

# ==== CRPS Calculation Function ====
get_crps <- function(obs, pred_draws) {
  pred_draws <- as.matrix(pred_draws)
  crps_sample(y = obs, dat = t(pred_draws))  # transpose!
}

# ==== Compute CRPS Values ====

# Fecundity
crps_y <- list(
  train = get_crps(y_train_obs, y_train_pred),
  test  = get_crps(y_test_obs,  y_test_pred),
  train_fixed = get_crps(y_train_obs, y_train_pred_fixed)
)

# Reproduction
crps_r <- list(
  train = get_crps(r_train_obs, r_train_pred),
  test  = get_crps(r_test_obs,  r_test_pred),
  train_fixed = get_crps(r_train_obs, r_train_pred_fixed)
)

crps_r_submodels <- list(
  #train = get_crps(r_train_obs, r_train_pred),
  #test  = get_crps(r_test_obs,  r_test_pred),
  train_clim = get_crps(r_train_obs, r_train_pred_clim),
  test_clim  = get_crps(r_test_obs,  r_test_pred_clim),
  train_nogene = get_crps(r_train_obs, r_train_pred_nogene),
  test_nogene  = get_crps(r_test_obs,  r_test_pred_nogene),
  train_nocomp = get_crps(r_train_obs, r_train_pred_nocomp),
  test_nocomp  = get_crps(r_test_obs,  r_test_pred_nocomp),
  train_nointer = get_crps(r_train_obs, r_train_pred_nointer),
  test_nointer  = get_crps(r_test_obs,  r_test_pred_nointer)
)

# Emergence
crps_e <- list(
  train = get_crps(e_train_obs, e_train_pred),
  test  = get_crps(e_test_obs,  e_test_pred),
  train_fixed = get_crps(e_train_obs, e_train_pred_fixed)
)


crps_e_submodels <- list(
  #train = get_crps(e_train_obs, e_train_pred),
  #test  = get_crps(e_test_obs,  e_test_pred),
  train_clim = get_crps(e_train_obs, e_train_pred_clim),
  test_clim  = get_crps(e_test_obs,  e_test_pred_clim),
  train_nogene = get_crps(e_train_obs, e_train_pred_nogene),
  test_nogene  = get_crps(e_test_obs,  e_test_pred_nogene),
  train_nocomp = get_crps(e_train_obs, e_train_pred_nocomp),
  test_nocomp  = get_crps(e_test_obs,  e_test_pred_nocomp),
  train_nointer = get_crps(e_train_obs, e_train_pred_nointer),
  test_nointer  = get_crps(e_test_obs,  e_test_pred_nointer)
)

# ==== Null Model CRPS ====

# Option A: Compute directly from null model draws
crps_y_null <- get_crps(y_train_obs, y_train_pred_null)
crps_r_null <- get_crps(r_train_obs, r_train_pred_null)
crps_e_null <- get_crps(e_train_obs, e_train_pred_null)

# Option B: Use precomputed values (replace if needed)
crps_y_null <- 202.9695  # mean: 202.9695
crps_r_null <- 0.2500765 # mean: 0.2500765
crps_e_null <- 0.1821024  # mean: 0.1821024

# ==== Skill Score Calculation ====
skill_score <- function(main, null) {
  1 - (mean(main) / mean(null))
}


skill_scores <- list(
  y_train       = skill_score(crps_y$train, crps_y_null),
  y_test        = skill_score(crps_y$test,  crps_y_null),
  y_train_fixed = skill_score(crps_y$train_fixed, crps_y_null),
  
  r_train       = skill_score(crps_r$train, crps_r_null),
  r_test        = skill_score(crps_r$test,  crps_r_null),
  r_train_fixed = skill_score(crps_r$train_fixed, crps_r_null),
  
  e_train       = skill_score(crps_e$train, crps_e_null),
  e_test        = skill_score(crps_e$test,  crps_e_null),
  e_train_fixed = skill_score(crps_e$train_fixed, crps_e_null)
)

skill_scores_sub <- list(
  r_train_clim       = skill_score(crps_r_submodels$train_clim, crps_r_null),
  r_test_clim       = skill_score(crps_r_submodels$test_clim, crps_r_null),
  r_train_nogene      = skill_score(crps_r_submodels$train_nogene, crps_r_null),
  r_test_nogene      = skill_score(crps_r_submodels$test_nogene, crps_r_null),
  r_train_nointer      = skill_score(crps_r_submodels$train_nointer, crps_r_null),
  r_test_nointer     = skill_score(crps_r_submodels$test_nointer, crps_r_null),
  r_train_nocomp      = skill_score(crps_r_submodels$train_nocomp, crps_r_null),
  r_test_nocomp      = skill_score(crps_r_submodels$test_nocomp, crps_r_null)
)

skill_scores_sub_e <- list(
  e_train_clim       = skill_score(crps_e_submodels$train_clim, crps_e_null),
  e_test_clim       = skill_score(crps_e_submodels$test_clim, crps_e_null),
  e_train_nogene      = skill_score(crps_e_submodels$train_nogene, crps_e_null),
  e_test_nogene      = skill_score(crps_e_submodels$test_nogene, crps_e_null),
  e_train_nointer      = skill_score(crps_e_submodels$train_nointer, crps_e_null),
  e_test_nointer     = skill_score(crps_e_submodels$test_nointer, crps_e_null),
  e_train_nocomp      = skill_score(crps_e_submodels$train_nocomp, crps_e_null),
  e_test_nocomp      = skill_score(crps_e_submodels$test_nocomp, crps_e_null)
)




# ==== CRPS Summary Table ====
crps_table <- data.frame(
  Component = c(
    "Fecundity (train)", "Fecundity (test)", "Fecundity (train, fixed)",
    "Reproduction (train)", "Reproduction (test)", "Reproduction (train, fixed)",
    "Emergence (train)", "Emergence (test)", "Emergence (train, fixed)"
  ),
  CRPS = round(c(
    mean(crps_y$train), mean(crps_y$test), mean(crps_y$train_fixed),
    mean(crps_r$train), mean(crps_r$test), mean(crps_r$train_fixed),
    mean(crps_e$train), mean(crps_e$test), mean(crps_e$train_fixed)
  ), 3),
  Skill_Score = round(c(
    skill_scores$y_train,
    skill_scores$y_test,
    skill_scores$y_train_fixed,
    skill_scores$r_train,
    skill_scores$r_test,
    skill_scores$r_train_fixed,
    skill_scores$e_train,
    skill_scores$e_test,
    skill_scores$e_train_fixed
  ), 3)
)

print(crps_table)

# ==== Export Table to Word (optional) ====
library(flextable)
flextable::flextable(crps_table) %>%
  flextable::save_as_docx(path = "CRPS_Table.docx")

#### ============== submodel table ======== #####
model_labels <- c("Climate only", "No genotype", "No interspecific", "No competition")
datasets <- c("Train", "Test")

# Construct clean vectors for CRPS and skill scores
crps_values <- c(
  mean(crps_r_submodels$train_clim),
  mean(crps_r_submodels$test_clim),
  mean(crps_r_submodels$train_nogene),
  mean(crps_r_submodels$test_nogene),
  mean(crps_r_submodels$train_nointer),
  mean(crps_r_submodels$test_nointer),
  mean(crps_r_submodels$train_nocomp),
  mean(crps_r_submodels$test_nocomp)
)

crps_values <- c(
  mean(crps_e_submodels$train_clim),
  mean(crps_e_submodels$test_clim),
  mean(crps_e_submodels$train_nogene),
  mean(crps_e_submodels$test_nogene),
  mean(crps_e_submodels$train_nointer),
  mean(crps_e_submodels$test_nointer),
  mean(crps_e_submodels$train_nocomp),
  mean(crps_e_submodels$test_nocomp)
)

skill_scores_values <- c(
  skill_scores_sub$r_train_clim,
  skill_scores_sub$r_test_clim,
  skill_scores_sub$r_train_nogene,
  skill_scores_sub$r_test_nogene,
  skill_scores_sub$r_train_nointer,
  skill_scores_sub$r_test_nointer,
  skill_scores_sub$r_train_nocomp,
  skill_scores_sub$r_test_nocomp
)

skill_scores_values <- c(
  skill_scores_sub_e$e_train_clim,
  skill_scores_sub_e$e_test_clim,
  skill_scores_sub_e$e_train_nogene,
  skill_scores_sub_e$e_test_nogene,
  skill_scores_sub_e$e_train_nointer,
  skill_scores_sub_e$e_test_nointer,
  skill_scores_sub_e$e_train_nocomp,
  skill_scores_sub_e$e_test_nocomp
)


# Make data frame
submodel_table <- data.frame(
  Dataset = rep(datasets, times = 4),
  Model_Type = rep(model_labels, each = 2),
  CRPS = round(crps_values, 3),
  Skill_Score = round(skill_scores_values, 3)
)

library(flextable)
ft <- flextable(submodel_table) %>%
  set_header_labels(
    Data = "Dataset",
    Model_Type = "Model Type",
    CRPS = "CRPS",
    Skill_Score = "Skill Score"
  ) %>%
  autofit()

library(officer)

doc <- read_docx() %>%
  body_add_par("CRPS and Skill Score Summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "CRPS_Sub_Skill_Summary.docx")
print(doc, target = "CRPS_Sub_Skill_Summary_emerge.docx")


####### Fitness by site year #####
agg_train <- training_fitness %>%
  group_by(site_year, Type, seasonality, pH, MAT, prcp.Fall, tmean.Sum, total_precip, tavg_center30d_mean) %>%
  summarise(
    mean_obs_fitness = mean(Obs_Fitness_log, na.rm = TRUE), 
    mean_pred_fitness = mean(Predicted_Fitness_log, na.rm = TRUE),
    .groups = "drop"
  )

agg_test <- testing_fitness %>%
  group_by(site_year, Type, seasonality, pH, MAT, prcp.Fall, tmean.Sum, total_precip, tavg_center30d_mean) %>%
  summarise(
    mean_obs_fitness = mean(Obs_Fitness_log, na.rm = TRUE), 
    mean_pred_fitness = mean(Predicted_Fitness_log, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(agg_train, aes(x = mean_pred_fitness, y = mean_obs_fitness, color = seasonality)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + theme_minimal()  +
  xlim(0, 6) +
  ylim(0, 6)

ggplot(agg_test, aes(x = mean_pred_fitness, y = mean_obs_fitness, color = seasonality)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + theme_minimal()
#prcp
ggplot(agg_train, aes(x = mean_pred_fitness, y = mean_obs_fitness, color = tmean.Sum)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + theme_minimal()

ggplot(agg_test, aes(x = mean_pred_fitness, y = mean_obs_fitness, color = tmean.Sum)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + theme_minimal()

###### CRPS ################################

###### CRPS with scoring rules package #######
library(posterior)
library(scoringRules)

### null models 
draws_y <- fit_null_fec$draws(format = "df")
y_train_pred_null <- draws_y[, grep("^y_train_pred\\[", names(draws_y))]

draws_r <- fit_null_rep$draws(format = "df")
r_train_pred_null <- draws_r[, grep("^r_train_pred\\[", names(draws_r))]

draws_e_null <- fit_null_emg$draws(format = "df")
e_train_pred_null <- draws_e_null[, grep("^e_train_pred\\[", names(draws_e_null))]

### Fecundity 
draws_y <- fit$draws(format = "df")
y_train_pred <- draws_y[, grep("^y_train_pred\\[", names(draws_y))]
y_train_fixed_pred <- draws_y[, grep("^y_train_fixed_pred\\[", names(draws_y))]
y_test_pred <- draws_y[, grep("^y_test_pred\\[", names(draws_y))]

# Reproduction
draws_r <- fit_rep$draws(format = "df")
r_train_pred <- draws_r[, grep("^r_train_pred\\[", names(draws_r))]
r_train_fixed_pred <- draws_r[, grep("^r_train_pred_fixed\\[", names(draws_r))]
r_test_pred <- draws_r[, grep("^r_test_pred\\[", names(draws_r))]

# Full climate emerged
draws_e_full <- fit_emg_full$draws(format = "df")
e_train_pred_full <- draws_e_full[, grep("^e_train_pred\\[", names(draws_e_full))]
e_train_fixed_pred_full <- draws_e_full[, grep("^e_train_pred_fixed\\[", names(draws_e_full))]
e_test_pred_full <- draws_e_full[, grep("^e_test_pred\\[", names(draws_e_full))]

# Rhat -- cutoff > 1.05 indicates convergence issues

# Fecundity Rhat values
rhat_y_train <- rhat(as.matrix(y_train_pred))
rhat_y_train_fixed <- rhat(as.matrix(y_train_fixed_pred))
rhat_y_test <- rhat(as.matrix(y_test_pred))
## rhat above 1.05 with SOS model for train only

# Reproduction Rhat values
rhat_r_train <- rhat(as.matrix(r_train_pred))
rhat_r_train_fixed <- rhat(as.matrix(r_train_fixed_pred))
rhat_r_test <- rhat(as.matrix(r_test_pred))

# Emerged Full Rhat values
rhat_e_full_train <- rhat(as.matrix(e_train_pred_full))
rhat_e_full_train_fixed <- rhat(as.matrix(e_train_fixed_pred_full))
rhat_e_full_test <- rhat(as.matrix(e_test_pred_full))


# Observed data 
y_train_obs <- training_df$Fecundity
y_test_obs <- testing_df$Fecundity
r_train_obs <- training_df_rep$r_train
r_test_obs <- testing_df_rep$r_test
e_train_obs <- training_df_emg$e_train
e_test_obs <- testing_df_emg$e_test

# ====== Null model CRPS ======
y_train_pred_null_mat <- as.matrix(y_train_pred_null)
r_train_pred_null_mat <- as.matrix(r_train_pred_null)
e_train_pred_null_mat <- as.matrix(e_train_pred_null)


crps_sample(y = y_train_obs, dat = y_train_pred_null)

# Fecundity null CRPS
crps_y_null <- crps_sample(y = y_train_obs, dat = t(y_train_pred_null_mat))

# Reproduction null CRPS
crps_r_null <- crps_sample(y = r_train_obs, dat = t(r_train_pred_null_mat))

# Emergence null CRPS
crps_e_null <- crps_sample(y = e_train_obs, dat = t(e_train_pred_null_mat))

hist(crps_y_null)
mean(crps_y_null) #202.9695

hist(crps_r_null)
mean(crps_r_null) # 0.2500765

hist(crps_e_null)
mean(crps_e_null) #0.1821024

skill_score_y <- 1 - (mean(crps_y) / mean(crps_y_null))

# ==== CRPS Computation Helper ====
get_crps <- function(obs, pred_df) {
  pred_t <- t(as.matrix(pred_df))
  crps_sample(y = obs, dat = pred_t)
}

# ==== CRPS Calculation ====

# Fecundity
crps_y <- list(
  train = get_crps(y_train_obs, y_train_pred),
  test = get_crps(y_test_obs, y_test_pred)
)

# Reproduction
crps_r <- list(
  train = get_crps(r_train_obs, r_train_pred),
  train_fixed = get_crps(r_train_obs, r_train_fixed_pred),
  test = get_crps(r_test_obs, r_test_pred)
)

# E (Full)
crps_e_full <- list(
  train = get_crps(training_df_emg$site_year, e_train_pred_full),
  train_fixed = get_crps(training_df_emg$site_year, e_train_fixed_pred_full),
  test = get_crps(testing_df_emg$site_year, e_test_pred_full)
)

# ==== Mean CRPS Summary ====
mean_crps_summary <- list(
  y = sapply(crps_y, mean),
  r = sapply(crps_r, mean),
  e_full = sapply(crps_e_full, mean)
)

y_crps = sapply(crps_y, mean)
r_crps = sapply(crps_r, mean)
e_full_crps = sapply(crps_e_full, mean)


# ==== Optional Histogram ====
hist(crps_y$train, breaks = 30, main = "CRPS - y_train", col = "skyblue")
hist(crps_y$train_fixed, breaks = 30, main = "CRPS - y_train_fixed", col = "orange")
hist(crps_y$test, breaks = 30, main = "CRPS - y_test", col = "purple")

# Single Posterior Predictive Check 
hist(t(as.matrix(y_train_pred))[1, ],
     breaks = 30,
     main = "Posterior Predictive for 1st Training Point (Fecundity)",
     xlab = "Predicted Value",
     col = "skyblue", border = "white")


