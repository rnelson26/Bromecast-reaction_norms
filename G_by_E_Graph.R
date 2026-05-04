climate_effects_mean <- climate_effects_long %>%
  group_by(genotype, climate_var) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  ) %>%
  mutate(
    overlaps_zero = lower <= 0 & upper >= 0
  )

ggplot(climate_effects_mean,
       aes(x = factor(climate_var),
           y = factor(genotype),
           fill = mean)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  labs(
    x = "Climate variable",
    y = "Genotype",
    fill = "Effect on emergence",
    title = "Genotype × Climate Interactions"
  ) +
  theme_minimal()

ggplot(climate_effects_mean,
       aes(x = factor(climate_var),
           y = factor(genotype),
           fill = mean,
           alpha = !overlaps_zero)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  scale_alpha_manual(values = c(0.3, 1)) +
  labs(
    x = "Climate variable",
    y = "Genotype",
    fill = "Effect",
    alpha = "Credible interval excludes 0",
    title = "Significant Genotype × Climate Interactions"
  ) +
  theme_minimal()

## reaction norms graph 
selected_vars <- c(1, 2, 3)  # choose important variables

ggplot(
  climate_effects_long %>%
    filter(climate_var %in% selected_vars),
  aes(x = factor(climate_var), y = value, group = genotype)
) +
  stat_summary(fun = mean, geom = "line", alpha = 0.3) +
  stat_summary(fun = mean, geom = "point") +
  labs(
    x = "Climate variable",
    y = "Effect on emergence",
    title = "Genotype-specific climate responses (reaction norms)"
  ) +
  theme_minimal()

climate_variability <- climate_effects_mean %>%
  group_by(climate_var) %>%
  summarise(
    sd_effect = sd(mean),
    .groups = "drop"
  )

ggplot(climate_variability,
       aes(x = factor(climate_var), y = sd_effect)) +
  geom_col() +
  labs(
    x = "Climate variable",
    y = "SD across genotypes",
    title = "Strength of genotype × climate interaction"
  ) +
  theme_minimal()