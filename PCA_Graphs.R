library(dplyr)

####### climate W vs original PCA ######
W_draws <- posterior::as_draws_matrix(fit$draws("W"))
W_draws_rep <- posterior::as_draws_matrix(fit_rep$draws("W"))

W_draws <- W_draws[, grepl("^W\\[", colnames(W_draws))]
W_draws_rep <- W_draws_rep[, grepl("^W\\[", colnames(W_draws))]

param_names <- colnames(W_draws)
param_names_rep <- colnames(W_draws_rep)

param_info <- tibble(param = param_names) %>%
  mutate(i = as.integer(gsub("W\\[(\\d+),.*", "\\1", param)),
         j = as.integer(gsub(".*,(\\d+)\\]", "\\1", param)))

param_info_rep <- tibble(param = param_names_rep) %>%
  mutate(i = as.integer(gsub("W\\[(\\d+),.*", "\\1", param)),
         j = as.integer(gsub(".*,(\\d+)\\]", "\\1", param)))

W_long <- bind_cols(
  param_info[rep(1:nrow(param_info), each = nrow(W_draws)), ],
  value = as.vector(W_draws)
)

W_long_rep <- bind_cols(
  param_info[rep(1:nrow(param_info_rep), each = nrow(W_draws_rep)), ],
  value = as.vector(W_draws_rep)
)

W_static <- X_SOS %*% Lambda_SOS 
W_static_df <- as.data.frame(W_static)
names(W_static_df) <- paste0("PC", 1:ncol(W_static_df))
W_static_df$index <- 1:nrow(W_static_df)

W_static_rep <- X_rep %*% Lambda_rep ## if reproduction  
W_static_df_rep <- as.data.frame(W_static_rep)
names(W_static_df_rep) <- paste0("PC", 1:ncol(W_static_df_rep))
W_static_df_rep$index <- 1:nrow(W_static_df_rep)

W_summary <- W_long %>%
  group_by(i, j) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )

W_summary_rep <- W_long_rep %>%
  group_by(i, j) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )

W_plot_df <- W_summary %>%
  left_join(W_static_df %>% pivot_longer(-index, names_to = "PC", values_to = "static_value") %>%
              mutate(j = as.integer(gsub("PC", "", PC))),
            by = c("i" = "index", "j"))

W_plot_df_rep <- W_summary_rep %>%
  left_join(W_static_df_rep %>% pivot_longer(-index, names_to = "PC", values_to = "static_value") %>%
              mutate(j = as.integer(gsub("PC", "", PC))),
            by = c("i" = "index", "j"))

ggplot(W_plot_df, aes(x = i)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue") +
  geom_point(aes(y = static_value), color = "red", shape = 1, size = 1.5) +
  facet_wrap(~ j, scales = "free_y", labeller = label_both) +
  labs(
    x = "Climate index (i)",
    y = "Latent climate (W) vs PCA projection",
    title = "Posterior W vs. Original PCA Projection"
  ) +
  theme_minimal()

ggplot(W_plot_df_rep, aes(x = i)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue") +
  geom_point(aes(y = static_value), color = "red", shape = 1, size = 1.5) +
  facet_wrap(~ j, scales = "free_y", labeller = label_both) +
  labs(
    x = "Climate index (i)",
    y = "Latent climate (W) vs PCA projection",
    title = "Posterior W vs. Original PCA Projection"
  ) +
  theme_minimal()

W_plot_df <- W_plot_df %>%
  mutate(adjusted = static_value < lower | static_value > upper)

W_plot_df_rep <- W_plot_df_rep %>%
  mutate(adjusted = static_value < lower | static_value > upper)

# which combinations are adjusted and how many
### Fecundity 
W_adjusted <- W_plot_df %>%
  filter(adjusted == TRUE)  %>%  mutate(site_year = site_year_labels[i])
unique(W_adjusted$site_year)

W_plot_df %>% group_by(j) %>% summarise(n_adjusted = sum(adjusted))

#23 adjusted in int 1, 12 in dim 2 (27 site-years in total)

# "CaseAoyamaS1 2023"                 
# "CastValley 2021"                   
#"dino 2024"                         
# "EnsingS1 SuRDC 2022"               
# "EnsingS2 Summerland-Princeton 2022"
# "EnsingS4 Lundbom 2022"             
#"GreenCanyon 2023"                  
#"HardwareRanch 2023"                
#"Peavine 2024"                      
#"Plymouth 2024"                     
#"RedBluff 2023"                     
#"SSHigh 2022"                       
#"SSHigh 2023"                       
#"SSHQ 2024"                         
#"Symstad2 2022"                     
# "Woodruff 2023"   

### Reproduced
W_adjusted_rep <- W_plot_df_rep %>%
  filter(adjusted == TRUE)  %>%  mutate(site_year = site_year_labels[i])
unique(W_adjusted_rep$site_year)

## 41 site-years adjusted 

W_plot_df_rep %>% group_by(j) %>% summarise(n_adjusted = sum(adjusted))

#### Which ones overlap?

site_years_rep <- unique(W_adjusted_rep$site_year)
site_years <- unique(W_adjusted$site_year)

# overlap
overlap <- intersect(site_years_rep, site_years)
#[1] "CaseAoyamaS1 2023"     "CastValley 2021"      
#[3] "dino 2024"             "EnsingS1 SuRDC 2022"  
#[5] "EnsingS4 Lundbom 2022" "HardwareRanch 2023"   
#[7] "Peavine 2024"          "Plymouth 2024"        
#[9] "SSHigh 2023"           "SSHQ 2024"            
#[11] "Symstad2 2022"   
#  differences
only_in_rep <- setdiff(site_years_rep, site_years) #30
only_in_fec <- setdiff(site_years, site_years_rep) #5




###### soil W vs original PCA #####

W_draws <- posterior::as_draws_matrix(fit$draws("W_soil"))

W_draws <- W_draws[, grepl("^W_soil\\[", colnames(W_draws))]

param_names <- colnames(W_draws)

param_info <- tibble(param = param_names) %>%
  mutate(i = as.integer(gsub("W_soil\\[(\\d+),.*", "\\1", param)),
         j = as.integer(gsub(".*,(\\d+)\\]", "\\1", param)))

W_long <- bind_cols(
  param_info[rep(1:nrow(param_info), each = nrow(W_draws)), ],
  value = as.vector(W_draws)
)

W_static <- X_soil %*% Lambda_soil  
W_static <- X_soil_rep %*% Lambda_soil_rep
W_static_df <- as.data.frame(W_static)
names(W_static_df) <- paste0("PC", 1:ncol(W_static_df))
W_static_df$index <- 1:nrow(W_static_df)

W_summary <- W_long %>%
  group_by(i, j) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )

W_plot_df <- W_summary %>%
  left_join(W_static_df %>% pivot_longer(-index, names_to = "PC", values_to = "static_value") %>%
              mutate(j = as.integer(gsub("PC", "", PC))),
            by = c("i" = "index", "j"))

ggplot(W_plot_df, aes(x = i)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue") +
  geom_point(aes(y = static_value), color = "red", shape = 1, size = 1.5) +
  facet_wrap(~ j, scales = "free_y", labeller = label_both) +
  labs(
    x = "Soil index (i)",
    y = "Latent soil (W_soil) vs PCA projection",
    title = "Posterior W vs. Original PCA Projection"
  ) +
  theme_minimal()


######## W and climate ########
cor_WX <- fit$draws(variables = c("cor_WX"))

cor_WX_draws <- posterior::as_draws_df(cor_WX, variables = "cor_WX")

cor_WX_long <- cor_WX_draws %>%
  pivot_longer(cols = starts_with("cor_WX["), names_to = "element", values_to = "cor") %>%
  mutate(
    element = gsub("cor_WX\\[|\\]", "", element),
    q = as.integer(sub(",.*", "", element)),
    p = as.integer(sub(".*,", "", element))
  )

cor_WX_means <- cor_WX_long %>%
  group_by(q, p) %>%
  summarize(mean_cor = mean(cor), .groups = "drop")

ggplot(cor_WX_means, aes(x = factor(p), y = factor(q), fill = mean_cor)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Climate Variable (X)", y = "Latent Dimension (W)", fill = "Mean Correlation") +
  theme_minimal()


ggplot(cor_WX_long, aes(x = factor(p), y = cor)) +
  geom_violin(fill = "skyblue", alpha = 0.6) +
  facet_wrap(~ q, labeller = label_both) +
  labs(x = "Climate Variable (X)", y = "Correlation with W", title = "Posterior Correlations: W vs. X") +
  theme_minimal()

##### back transforming with lambda #######
climate_effects <- fit$draws(variables = c("climate_effects"))

climate_effect_draws <- posterior::as_draws_df(climate_effects, variables = "climate_effects")

climate_effects_long <- climate_effect_draws %>%
  pivot_longer(cols = starts_with("climate_effects["), names_to = "param", values_to = "value") %>%
  mutate(
    param = gsub("climate_effects\\[|\\]", "", param),
    genotype = as.integer(sub(",.*", "", param)),
    climate_var = as.integer(sub(".*,", "", param))
  )

ggplot(climate_effects_long, aes(x = factor(climate_var), y = value)) +
  geom_violin(fill = "tomato", alpha = 0.6) +
  labs(x = "Climate Variable", y = "Effect Size", title = "Distribution of Climate Effects Across Genotypes") +
  theme_minimal()

climate_effects_mean <- climate_effects_long %>%
  group_by(genotype, climate_var) %>%
  summarize(mean_effect = mean(value), .groups = "drop")

ggplot(climate_effects_mean, aes(x = factor(climate_var), y = factor(genotype), fill = mean_effect)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  labs(x = "Climate Variable", y = "Genotype", fill = "Mean Effect") +
  theme_minimal()


######### new code ##########

climate_effects_summary <- climate_effects_long %>%
  group_by(climate_var) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  ) %>%
  mutate(
    overlaps_zero = lower <= 0 & upper >= 0
  )

## which abiotic variables matter
library(ggplot2)

ggplot(climate_effects_summary,
       aes(x = mean, y = reorder(climate_var, mean))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lower, xmax = upper,
                     color = overlaps_zero), height = 0.2) +
  geom_point(aes(color = overlaps_zero), size = 3) +
  scale_color_manual(values = c("TRUE" = "gray60", "FALSE" = "black")) +
  labs(
    x = "Effect on emergence (logit scale)",
    y = "Climate variable (original X)",
    color = "CI overlaps 0",
    title = "Climate Effects on Emergence (Back-transformed from W)"
  ) +
  theme_minimal()

## how emergence responds to PCA axes in W space
beta_draws <- posterior::as_draws_df(fit$draws("mu_beta"))

beta_long <- beta_draws %>%
  pivot_longer(cols = starts_with("mu_beta"),
               names_to = "axis",
               values_to = "value") %>%
  mutate(axis = as.integer(gsub("mu_beta\\[|\\]", "", axis)))

beta_summary <- beta_long %>%
  group_by(axis) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )

ggplot(beta_summary, aes(x = mean, y = factor(axis))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_point(size = 3) +
  labs(
    x = "Effect on emergence",
    y = "Latent climate axis (W)",
    title = "Emergence response to latent climate axes"
  ) +
  theme_minimal()

###### link PCA axes to variables:
Lambda_df <- as.data.frame(Lambda)
Lambda_df$climate_var <- 1:nrow(Lambda_df)

Lambda_long <- Lambda_df %>%
  pivot_longer(-climate_var,
               names_to = "axis",
               values_to = "loading")

ggplot(Lambda_long,
       aes(x = loading, y = factor(climate_var))) +
  geom_col() +
  facet_wrap(~ axis, scales = "free_x") +
  labs(
    x = "Loading",
    y = "Climate variable",
    title = "How original variables load onto latent axes"
  ) +
  theme_minimal()

#β (mu_beta) → which axes matter
#Λ (Lambda) → what those axes represent biologically
#Λ × β (climate_effects) → which original variables matter