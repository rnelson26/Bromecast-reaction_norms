################# Bromecast: 10.CRPS_and_figures.R ##########################
############# created 3-25-25 ######################
############# Last modified: 3-17-26 ##########################
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
library(purrr)

######## look for missing info
## two files are not having the fixed only values pulled from in our CRPS function
#rep_clim <- "/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Dec_2025/fit_reproduced_climate_draws.rds"

#rep_gene <- "/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Dec_2025/fit_reproduced_nogene_draws.rds"

#obj_clim <- readRDS(rep_clim)
#obj_gene <- readRDS(rep_gene)
## both have columns called #r_train_pred_fixed_only

#rep_full <- "/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Dec_2025/fit_reproduced_full_draws.rds"
#obj_full <- readRDS(rep_full)
## r_trained_fixed

##### CRPS updated version #######

# extract draws function
extract_draws <- function(draws, var_bases) {
  
  for (vb in var_bases) {                  # vb is length-1 character
    pattern <- paste0("^", vb, "\\[")
    cols <- grep(pattern, names(draws))
    
    if (length(cols) > 0) {
      mat <- as.matrix(draws[, cols])
      mat <- apply(mat, 2, as.numeric)
      return(mat)
    }
  }
  
  stop(
    paste(
      "None of these variables found:",
      paste(var_bases, collapse = ", ")
    )
  )
}

# get crps function
get_crps <- function(obs, pred_df) {
  pred_mat <- as.matrix(pred_df)
  if (nrow(pred_mat) != length(obs)) pred_mat <- t(pred_mat)
  crps_sample(y = as.numeric(obs), dat = pred_mat)
}

# list data
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

# set up draws files
base_dir <- "/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026"

all_files <- list.files(base_dir, pattern = "^fit_.*\\.rds$", full.names = TRUE)

file_info <- tibble(file_path = all_files) %>%
  mutate(
    file_name   = basename(file_path),
    stage       = sub("^fit_(.*?)_.*\\.rds$", "\\1", file_name),
    variant     = sub("^fit_.*?_(.*?)\\.rds$", "\\1", file_name),
    crps_train  = NA_real_,
    crps_train_fixed = NA_real_,
    crps_test   = NA_real_,
    null_crps_train = NA_real_,
    null_crps_test  = NA_real_,
    skill_train = NA_real_,
    skill_train_fixed = NA_real_,
    skill_test  = NA_real_
  )

# get null model values

null_crps <- list()

for (stg in unique(file_info$stage)) {
  

  null_candidates <- list.files(
    base_dir,
    pattern = paste0("^fit_", stg, "_null.*\\.rds$"),
    full.names = TRUE
  )

  if (length(null_candidates) == 0) {
    message("⚠ No null file found for stage: ", stg)
    next
  }
  
  null_file <- null_candidates[1]
  
  
  null_draws <- readRDS(null_file)
  
  var_map <- switch(stg,
                    emerged    = list(train = c("e_train_pred"),
                                      test  = c("e_test_pred")),
                    reproduced = list(train = c("r_train_pred"),
                                      test  = c("r_test_pred")),
                    fecundity  = list(train = c("y_train_pred"),
                                      test  = c("y_test_pred"))
  )
  
  obs_map <- switch(stg,
                    emerged    = c(train = "e_train", test = "e_test"),
                    reproduced = c(train = "r_train", test = "r_test"),
                    fecundity  = c(train = "y_train", test = "y_test")
  )
  
  null_crps[[stg]] <- list()
  
  for (nm in names(var_map)) {
    pred <- extract_draws(null_draws, var_map[[nm]])   
    obs  <- obs_list[[obs_map[nm]]]
    if (nrow(pred) != length(obs)) pred <- t(pred)
    null_crps[[stg]][[nm]] <- mean(get_crps(obs, pred))
  }
}

# main loop
for (i in seq_len(nrow(file_info))) {
  
  draws   <- readRDS(file_info$file_path[i])
  stage   <- file_info$stage[i]
  variant <- file_info$variant[i]
  
  var_map <- switch(stage,
                    
                    emerged = list(
                      train       = c("e_train_pred"),
                      train_fixed = c("e_train_pred_fixed"),
                      test        = c("e_test_pred")
                    ),
                    
                    reproduced = list(
                      train       = c("r_train_pred"),
                      train_fixed = c("r_train_pred_fixed",
                                      "r_train_pred_fixed_only"),  #for multiple naming schemes in files
                      test        = c("r_test_pred")
                    ),
                    
                    fecundity = list(
                      train       = c("y_train_pred"),
                      train_fixed = c("y_train_pred_fixed"),
                      test        = c("y_test_pred")
                    )
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
  
  # -------------------------
  # CRPS

  for (nm in names(var_map)) {
    
    pred <- tryCatch(
      extract_draws(draws, var_map[[nm]]),   
      error = function(e) NULL
    )
    if (is.null(pred)) next
    
    obs <- obs_list[[obs_map[nm]]]
    if (nrow(pred) != length(obs)) pred <- t(pred)
    
    file_info[[paste0("crps_", nm)]][i] <-
      mean(get_crps(obs, pred))
  }
 # message(
  #  "Processed (CRPS only): ", file_info$file_name[i],
   # " | CRPS train = ", round(file_info$crps_train[i], 3),
  #  " | CRPS test = ", round(file_info$crps_test[i], 3)
  #)
  
  # -------------------------
  # Attach null CRPS
  # -------------------------
  #file_info$null_crps_train[i] <- null_crps[[stage]]$train
  #file_info$null_crps_test[i]  <- null_crps[[stage]]$test
  
  if (!is.null(null_crps[[stage]])) {
    
    file_info$null_crps_train[i] <-
      if (!is.null(null_crps[[stage]]$train))
        null_crps[[stage]]$train
    else NA_real_
    
    file_info$null_crps_test[i]  <-
      if (!is.null(null_crps[[stage]]$test))
        null_crps[[stage]]$test
    else NA_real_
    
  } else {
    
    message("⚠ No null CRPS found for stage: ", stage,
            " | File: ", file_info$file_name[i])
    
    file_info$null_crps_train[i] <- NA_real_
    file_info$null_crps_test[i]  <- NA_real_
  }
  
  
  # -------------------------
  # Skill scores 
  # -------------------------
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

# ================================================================
# Final output
# ================================================================
output <- as.data.frame(file_info)
output


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


library(stringr)

stage_order <- c("emerged", "reproduced", "fecundity")

submodel_order <- c(
  "full",
  "nointer",
  "nocomp",
  "nogene",
  "climate",
  "null"
)

table_df_ordered <- table_df %>%
  mutate(
    stage = str_extract(file_name, "(?<=fit_)[^_]+"),
    submodel = str_extract(file_name, "(?<=_)[^_]+(?=_draws)")
  )

## put models in a logic order for interpretation 
table_df_ordered <- table_df_ordered %>%
  mutate(
    stage = factor(stage, levels = stage_order),
    submodel = factor(submodel, levels = submodel_order)
  ) %>%
  arrange(stage, submodel)


ft <- flextable(table_df_ordered) %>%
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

print(doc, target = "CRPS_skill_summary_Feb_2026.docx")


########### Visualize CRPS #############
table_df_ordered <- table_df_ordered %>%
  mutate(
    file_clean = str_remove(file_name, "^fit_") %>%
      str_remove("\\.rds$"),
    stage = str_split(file_clean, "_", simplify = TRUE)[,1],
    submodel = str_split(file_clean, "_", simplify = TRUE)[,2]
  )



crps_long <- table_df_ordered %>%
 select(stage, submodel,
       crps_train,
       crps_train_fixed,
      crps_test) %>%
  pivot_longer(
    cols = starts_with("crps_"),
    names_to = "dataset",
    values_to = "crps"
  ) %>%
  mutate(
    dataset = recode(
      dataset,
      crps_train = "Training",
      crps_train_fixed = "Training (fixed only)",
      crps_test = "Testing"
    )
  )

crps_long <- crps_long %>%
  mutate(
    stage = factor(stage,
                   levels = c("emerged", "reproduced", "fecundity"))
  )

#crps_fig <- ggplot(crps_long,
 #      aes(x = submodel,
  #         y = crps,
   #        color = dataset,
    #       group = dataset)) +
  #geom_point(size = 2.5) +
  #geom_line(linewidth = 0.8) +
  #facet_wrap(~ stage, nrow = 1, scales = "free_y") +
  #theme_classic() +
  #labs(
   # x = "Submodel",
  #  y = "CRPS",
   # color = "Dataset"
  #) +
  #theme(
   # axis.text.x = element_text(angle = 45, hjust = 1),
  #  strip.background = element_blank(),
   # strip.text = element_text(face = "bold")
  #)

crps_long <- crps_long %>%
  mutate(
    dataset = factor(dataset,
                     levels = c("Training",
                                "Training (fixed only)",
                                "Testing"))
  )



my_cols <- c(
  "Training" = "lightslateblue",              
  "Training (fixed only)" = "deeppink1", 
  "Testing" = "darkturquoise"               
)



crps_fig <- ggplot(crps_long,
                   aes(x = submodel,
                       y = crps,
                       color = dataset,
                       shape = dataset)) +
  geom_point(size = 5,
             position = position_dodge(width = 0.4)) +
  facet_wrap(~ stage, nrow = 1, scales = "free_y") +
  scale_color_manual(values = my_cols,
                     name = "Dataset") +
  scale_shape_manual(values = c(
    "Training" = 16,
    "Training (fixed only)" = 17,
    "Testing" = 15
  ),
  name = "Dataset") +
  theme_classic() +
  labs(
    x = "Submodel",
    y = "CRPS"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

crps_fig
## save graph
ggsave("crps_fig.pdf", plot = crps_fig, width = 6, height = 5, units = "in")



crps_skill_long <- table_df_ordered %>%
  select(stage, submodel,
         starts_with("crps_"),
         starts_with("skill_")) %>%
  pivot_longer(
    cols = -c(stage, submodel),
    names_to = c(".value", "dataset"),
    names_pattern = "(crps|skill)_(.*)"
  ) %>%
  mutate(
    dataset = recode(
      dataset,
      train = "Training",
      train_fixed = "Training (fixed only)",
      test = "Testing"
    )
  )

crps_skill_long <- crps_skill_long %>%
  mutate(
    stage = factor(stage,
                   levels = c("emerged", "reproduced", "fecundity"))
  ) 


crps_skill_long <- crps_skill_long %>% filter(submodel != "null") %>% 
  mutate(
    dataset = factor(dataset,
                     levels = c("Training",
                                "Training (fixed only)",
                                "Testing"))
  ) %>%  mutate(
    submodel = factor(submodel,
                     levels = c("full",
                                "nointer",
                                "nocomp", "nogene", "climate"))
  )




my_cols <- c(
  "Training" = "lightslateblue",              
  "Training (fixed only)" = "deeppink1", 
  "Testing" = "darkturquoise"               
)



skills_fig <- ggplot(crps_skill_long,
                   aes(x = submodel,
                       y = skill,
                       color = dataset,
                       shape = dataset)) +
  geom_point(size = 5,
             position = position_dodge(width = 0.4)) +
  facet_wrap(~ stage, nrow = 1, scales = "free_y") +
  scale_color_manual(values = my_cols,
                     name = "Dataset") +
  scale_shape_manual(values = c(
    "Training" = 16,
    "Training (fixed only)" = 17,
    "Testing" = 15
  ),
  name = "Dataset") +
  theme_classic() +
  labs(
    x = "Submodel",
    y = "Skill Score"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )


skills_fig
## save graph
ggsave("figures/skill_fig_updated.pdf", plot = skills_fig, width = 6, height = 5, units = "in")
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
  
  if (!stage %in% c("emerged", "reproduced")) next
  
  draws <- readRDS(file_info$file_path[i])
  
  var_map <- switch(stage,
                    emerged = list(
                      train        = c("e_train_pred"),
                      train_fixed  = c("e_train_pred_fixed",
                                       "e_train_pred_fixed_only"),
                      test         = c("e_test_pred")
                    ),
                    reproduced = list(
                      train        = c("r_train_pred"),
                      train_fixed  = c("r_train_pred_fixed",
                                       "r_train_pred_fixed_only"),
                      test         = c("r_test_pred")
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
    
    pred <- tryCatch(
      extract_draws(draws, var_map[[nm]]),
      error = function(e) NULL
    )
    if (is.null(pred)) next
    
    obs <- obs_list[[obs_map[nm]]]
    
    cm <- get_posterior_cm(draws, var_map[[nm]], obs)
    
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

######## Extract Predicted vs Observed Cheatgrass Fitness #############
ls()
rm(list = setdiff(ls(), c("training_df_emg", "testing_df_emg")))

## for full model
base_dir <- "/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026"

all_files <- list.files(base_dir, pattern = "^fit_.*\\.rds$", full.names = TRUE)

file_info <- tibble(file_path = all_files) %>%
  mutate(
    file_name   = basename(file_path),
    stage       = sub("^fit_(.*?)_.*\\.rds$", "\\1", file_name),
    variant     = sub("^fit_.*?_(.*?)\\.rds$", "\\1", file_name),
    crps_train  = NA_real_,
    crps_train_fixed = NA_real_,
    crps_test   = NA_real_,
    null_crps_train = NA_real_,
    null_crps_test  = NA_real_,
    skill_train = NA_real_,
    skill_train_fixed = NA_real_,
    skill_test  = NA_real_
  )

#### Observed fitness:
training_df_emg <- training_df_emg %>%
  mutate(
    Obs_Fitness = e_train * r_train * Fecundity,
    Obs_Fitness_log = log(Obs_Fitness)
  )

testing_df_emg <- testing_df_emg %>%
  mutate(
    Obs_Fitness = e_test * r_test * Fecundity,
    Obs_Fitness_log = log(Obs_Fitness)
  )

## load full models
fit_emg_full <- readRDS(file_info$file_path[file_info$file_name == "fit_emerged_full_draws.rds"])
fit_rep_full <- readRDS(file_info$file_path[file_info$file_name == "fit_reproduced_full_draws.rds"])
fit_fec_full <- readRDS(file_info$file_path[file_info$file_name == "fit_fecundity_full.rds"])

#fit_rep <- readRDS("output/fit_reproduced_full.rds") #from my computer run

## Posterior probabilities 



## Posterior predictive draws
## run one set at time to avoid memory overload 

#fit_rep_full <- fit_rep$draws(format = "df") #if from my computer


cn <- colnames(fit_rep_full)
base_names <- sub("\\[.*\\]", "", cn)
unique(base_names)
#[1] "r_train_pred"       "r_test_pred"        "r_train_pred_fixed"


cn <- colnames(fit_emg_full)
base_names <- sub("\\[.*\\]", "", cn)
unique(base_names)
#"e_train_pred"       "e_test_pred"        "e_train_pred_fixed"

cn <- colnames(fit_fec_full)
base_names <- sub("\\[.*\\]", "", cn)
unique(base_names)
#[1] "y_train_pred"            "y_test_pred"             "y_train_pred_fixed"     
#[4] "y_test_pred_fixed"       "y_train_pred_full"       "y_test_pred_full"       
#[7] "y_train_pred_full_fixed"


# Emerged Posterior Predictive
e_emg_train <- fit_emg_full %>%
  select(starts_with("e_train_pred")) %>%
  as.matrix()
e_emg_test <- fit_emg_full %>%
  select(starts_with("e_test_pred")) %>%
  as.matrix()
e_emg_train_fixed <- fit_emg_full %>%
  select(starts_with("e_train_pred_fixed")) %>%
  as.matrix()

## Emerged Fecundity Posterior
fec_train <- fit_fec_full %>%
  select(starts_with("mu_train")) %>%
  as.matrix()
fec_test <- fit_emg_full %>%
  select(starts_with("mu_test")) %>%
  as.matrix()
fec_train_fixed <- fit_emg_full %>%
  select(starts_with("mu_train_fixed")) %>%
  as.matrix()

#Reproduced Posterior Predictive 
r_rep_train <- fit_rep_full %>%
  select(starts_with("r_train_full")) %>%
  as.matrix()
r_rep_test <- fit_rep_full %>%
  select(starts_with("r_test_full")) %>%
  as.matrix()
r_rep_train_fixed <- fit_rep_full %>%
  select(starts_with("r_train_full_fixed")) %>%
  as.matrix()
rm(fit_rep_full)

#Fecundity Posterior Predictive 
y_fec_train <- fit_fec_full %>%
  select(starts_with("y_train_pred_full")) %>%
  as.matrix()
y_fec_test <- fit_fec_full %>%
  select(starts_with("y_test_pred_full")) %>%
  as.matrix()
y_fec_train_fixed <- fit_fec_full %>%
  select(starts_with("y_train_pred_full_fixed")) %>%
  as.matrix()


##### Compute fitness #####

# Posterior mean fitness
#training_df_emg$Predicted_Fitness <- colMeans(p_emg_train * p_rep_train * p_fec_train)
#training_df_emg$Predicted_Fitness_log <- colMeans(log(p_emg_train) + log(p_rep_train) + log(p_fec_train))
#training_df_emg$Predicted_Fitness_fixed <- colMeans(p_emg_train_fixed * #p_rep_train_fixed * p_fec_train_fixed)
#training_df_emg$Predicted_Fitness_log_fixed <- colMeans(log(p_emg_train_fixed) + log(p_rep_train_fixed) + log(p_fec_train_fixed))
#testing_df_emg$Predicted_Fitness <- colMeans(p_emg_test * p_rep_test * p_fec_test)
#testing_df_emg$Predicted_Fitness_log <- colMeans(log(p_emg_test) + log(p_rep_test) + log(p_fec_test))

# Posterior predictive
dim(e_emg_train)
dim(r_rep_train)
dim(y_fec_train)


training_df_emg$Predicted_Fitness_PostPred <- colMeans(e_emg_train * r_rep_train * y_fec_train)
training_df_emg$Predicted_Fitness_log_PostPred <- colMeans(log(e_emg_train) + log(r_rep_train) + log(y_fec_train))
rm(e_emg_train, r_rep_train, y_fec_train)

training_df_emg$Predicted_Fitness_PostPred_fixed <- colMeans(e_emg_train_fixed * r_rep_train_fixed * y_fec_train_fixed)
training_df_emg$Predicted_Fitness_log_PostPred_fixed <- colMeans(log(e_emg_train_fixed) + log(r_rep_train_fixed) + log(y_fec_train_fixed))
rm(e_emg_train_fixed, r_rep_train_fixed, y_fec_train_fixed)

testing_df_emg$Predicted_Fitness_PostPred <- colMeans(e_emg_test * r_rep_test * y_fec_test)
testing_df_emg$Predicted_Fitness_log_PostPred <- colMeans(log(e_emg_test) + log(r_rep_test) + log(y_fec_test))
rm(e_emg_test, r_rep_test, y_fec_test)


#write.csv(training_df_emg, "data/training_df_emg_Fitness.csv", row.names = TRUE)
#write.csv(testing_df_emg, "data/testing_df_emg_Fitness.csv", row.names = TRUE)

### Posterior Predictive - Training Fixed
ggplot(training_df_emg, aes(x = Predicted_Fitness_PostPred_fixed, y = Obs_Fitness)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Fixed Effects - Posterior Predictive",
       x = "Predicted Fitness",
       y = "Observed Fitness")
## on log scale
ggplot(training_df_emg, aes(x = log(Predicted_Fitness_PostPred_fixed), y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Fixed Effects - Posterior Predictive",
       x = "Log Predicted Fitness",
       y = "Log Observed Fitness")

### Posterior Predictive - Testing
ggplot(testing_df_emg, aes(x = Predicted_Fitness_PostPred, y = Obs_Fitness)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Testing - Posterior Predictive",
       x = "Predicted  Fitness",
       y = "Observed  Fitness")

ggplot(testing_df_emg, aes(x = log(Predicted_Fitness_PostPred), y = Obs_Fitness_log)) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Testing - Posterior Predictive",
       x = "Predicted Log Fitness",
       y = "Observed Log Fitness")

######### Fecundity Figures ###############


fit_emg_full <- readRDS("/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026/fit_emerged_full_all.rds")
fit_rep_full <- readRDS("/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026/fit_reproduced_full_allparams.rds")
fit_fec_full <- readRDS("/Users/Becca/Desktop/Adler Lab/from megan/Outputs_Feb_2026/fit_fecundity_full_all.rds")

cn <- colnames(fit_fec_full)
base_names <- sub("\\[.*\\]", "", cn)
unique(base_names)

train_cols <- grep("^mu_train_full\\[", colnames(fit_fec_full))
pred_train <- colMeans(log(fit_fec_full[, train_cols]))
training_df_emg$Posterior_Fecundity_Train <- pred_train #then change 

train_cols_fixed <- grep("^mu_train_full_fixed\\[", colnames(fit_fec_full))
pred_train_fixed <- colMeans(fit_fec_full[, train_cols_fixed])
training_df_emg$Posterior_Fecundity_Train_Fixed <- pred_train_fixed


test_cols <- grep("^mu_test_full\\[", colnames(fit_fec_full))
pred_test <- colMeans(fit_fec_full[, test_cols])
testing_df_emg$Posterior_Fecundity_Test <- pred_test


####### Posterior for Observed Fecundity > 0 ###########
pa <- training_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Train),y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Posterior",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity")

pa_type <- training_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Train),y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) + facet_wrap(~Type) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Posterior",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity") 

pa_site <- training_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Train),y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) + facet_wrap(~site) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Posterior",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity") 

pb <- training_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Train_Fixed),y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Posterior Fixed Only",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity")

pb_type <- training_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Train_Fixed),y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Posterior Fixed Only",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity") + facet_wrap(~Type)

pb_site <- training_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Train_Fixed),y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Training Posterior Fixed Only",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity") + facet_wrap(~site)


pc <- testing_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Test), y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Testing Posterior",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity")

pc_site <- testing_df_emg %>% filter(Fecundity > 0) %>% ggplot(aes(x = log(Posterior_Fecundity_Test), y = log(Fecundity))) +
  geom_pointdensity(adjust = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_color_viridis_c(option = "C", name = "Point Density") +
  theme_minimal() +
  labs(title = "Testing Posterior",
       x = "Predicted  Fecundity",
       y = "Observed  Fecundity") + facet_wrap(~site)

##### Posterior for Observed Fecundity Equal to Zero ###########
pd <- training_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Train)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "D  Training Posterior",
       x = "Predicted Fecundity",
       y = "Count")

pd_type <- training_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Train)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "D  Training Posterior",
       x = "Predicted Fecundity",
       y = "Count") + facet_wrap(~Type)

pd_site <- training_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Train)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "D  Training Posterior",
       x = "Predicted Fecundity",
       y = "Count") + facet_wrap(~site)

pe <- training_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Train_Fixed)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "E  Training Posterior (Fixed Only)",
       x = "Predicted Fecundity",
       y = "Count")

pe_type <- training_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Train_Fixed)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "E  Training Posterior (Fixed Only)",
       x = "Predicted Fecundity",
       y = "Count") + facet_wrap(~Type)

pe_site <- training_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Train_Fixed)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "E  Training Posterior (Fixed Only)",
       x = "Predicted Fecundity",
       y = "Count") + facet_wrap(~site)

pf <- testing_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Test)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "F  Testing Posterior",
       x = "Predicted Fecundity",
       y = "Count")

pf_site <- testing_df_emg %>%
  filter(Fecundity == 0) %>%
  ggplot(aes(x = Posterior_Fecundity_Test)) +
  geom_histogram(bins = 40) +
  theme_minimal() +
  labs(title = "F  Testing Posterior",
       x = "Predicted Fecundity",
       y = "Count") + facet_wrap(~site)


library(patchwork)

fig_fecundity <- (pa | pb | pc) /
  (pd | pe | pf) +
  plot_annotation(tag_levels = "A")

ggsave("figures/fecundity_posterior_checks.pdf",
       fig_fecundity,
       width = 12,
       height = 8,
       device = "pdf")


fig_fecundity_type <- (pa_type | pb_type | pc) /
  (pd_type | pe_type | pf) +
  plot_annotation(tag_levels = "A")

ggsave("figures/fecundity_posterior_checks_bytype.pdf",
       fig_fecundity_type,
       width = 12,
       height = 8,
       device = "pdf")

## by site
plots_site <- list(pa_site, pb_site, pc_site,
                   pd_site, pe_site, pf_site)

pdf("figures/fig_fecundity_site.pdf", width = 8, height = 6)

for (p in plots_site) {
  print(p)
}

dev.off()
