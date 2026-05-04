rm(list = ls())

library(tidyverse)
library(flextable)
library(officer)


gc()

path <- "/Users/Becca/Desktop"
fit <- readRDS(file.path(path, "fit_emerged_full.rds"))


######## Fixed Effects Table #############

fixed_effects <- fit$summary(
  variables = c(
    "alpha",
    "mu_beta",
    "mu_beta_soil",
    "beta_neighbors",
    "beta_annual",
    "beta_perennial",
    "beta_shrub"
  )
)

fixed_effects_table <- fixed_effects[, c(
  "variable", "mean", "sd", "q5", "q95", "rhat"
)]

fixed_effects_table

fixed_effects_ft <- fixed_effects_table %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3),
    q5   = round(q5, 3),
    q95  = round(q95, 3),
    rhat = round(rhat, 3)
  )

ft <- flextable(fixed_effects_ft)

ft <- ft %>%
  autofit() %>%
  theme_vanilla() %>%
  
  doc <- read_docx() %>%
  body_add_par("Fixed Effects Summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = file.path(path, "fixed_effects_table.docx"))
  bold(part = "header")
  
################# Fixed Effects Graph #######################
  library(ggplot2)
  
  plot_data <- fixed_effects_table %>%
    mutate(
      overlaps_zero = ifelse(q5 <= 0 & q95 >= 0, "Yes", "No")
    )
  
  ggplot(plot_data, aes(x = mean, y = reorder(variable, mean))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbarh(aes(xmin = q5, xmax = q95, color = overlaps_zero), height = 0.2) +
    geom_point(aes(color = overlaps_zero), size = 3) +
    scale_color_manual(values = c("Yes" = "gray60", "No" = "black")) +
    labs(
      x = "Estimate (mean with 90% CI)",
      y = "Parameter",
      color = "CI overlaps 0"
    ) +
    theme_minimal()
 
   ggsave(
    filename = file.path(path, "fixed_effects_intervals.pdf"),
    width = 6,
    height = 4
  )