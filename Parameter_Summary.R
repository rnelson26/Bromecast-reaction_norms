
rm(list = setdiff(ls(), keep))
gc()
path <- "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/output"
fit <- readRDS(file.path(path, "fit_emerged_full.rds"))

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
