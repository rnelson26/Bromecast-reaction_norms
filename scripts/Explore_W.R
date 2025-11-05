############## Integrated Reaction Norm Model ########
######## Explore W ##########
######## code by Becca Nelson ###############################
############# created 3-25-25 ######################
############# Last modified: 11-3-25 ##########################

###### biplot of W ############
library(cmdstanr)
library(posterior)   
library(ggplot2)
library(dplyr)

## first run models in checking convergence code 


## average over chains and split into two columns 
## combine chains first and then take the mean

# mean posterior  across iterations and chains
W_means <- apply(draws_array, 3, mean)

## reformat into different columns
W_df <- tibble(
  param = names(W_means),
  value = W_means
) %>%
  mutate(
    i = as.integer(str_extract(param, "(?<=\\[)\\d+")),
    j = as.integer(str_extract(param, "(?<=,)\\d+(?=\\])"))
  ) %>%
  pivot_wider(
    id_cols = i,
    names_from = j,
    names_prefix = "W_col",
    values_from = value
  ) %>%
  arrange(i)

## visualize results
ggplot(W_df, aes(x = W_col1, y = W_col2)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = i), vjust = -0.8, size = 3) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Posterior mean PC1",
    y = "Posterior mean PC2",
  )

ggplot(W_df, aes(x = 0, y = 0, xend = W_col1, yend = W_col2)) +
  geom_segment(arrow = arrow(length = unit(0.15, "cm")), color = "darkred") +
  geom_text(aes(x = W_col1, y = W_col2, label = i), vjust = -0.5, size = 3) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Posterior mean PC1",
    y = "Posterior mean PC2",
  )


##### with climate 

W_df <- W_df %>%
  mutate(site_year = rownames(X_emg_SOS))  

X_df <- as.data.frame(X_emg_SOS)
X_df$i <- 1:nrow(X_df)  
X_df$site <- rownames(X_df)  

W_df_full <- W_df %>%
  left_join(X_df, by = c("i"))


ggplot(W_df_full, aes(x = W_col1, y = W_col2, color = prcp.Spr)) +
  geom_point(size = 3) +
  geom_text(aes(label = site_year), vjust = -0.7, size = 2.5) +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  labs(
    x = "Posterior mean PC1",
    y = "Posterior mean PC2",
    color = "Spring precipitation (scaled)",
  )

###########  climate and W biplot #######
# Make W and X into matrices
W_mat <- as.matrix(W_df[, c("W_col1", "W_col2")])  # 83 x 2
X_mat <- as.matrix(X_emg_SOS)                      # 83 x 19

# Compute loadings: regression coefficients of X on W
loadings <- t(solve(t(W_mat) %*% W_mat) %*% t(W_mat) %*% X_mat)  # 19 x 2
loadings_df <- as.data.frame(loadings)
loadings_df$variable <- colnames(X_emg_SOS)
colnames(loadings_df)[1:2] <- c("PC1", "PC2")


library(ggrepel)
arrowscale <- 10
loadings_df <- loadings_df %>%
  mutate(PC1 = PC1 * arrowscale,
         PC2 = PC2 * arrowscale)

ggplot(W_df_full, aes(x = W_col1, y = W_col2)) +
  # Posterior mean site points
  geom_point(size = 3, color = "steelblue") +
  geom_text_repel(aes(label = site_year), size = 3,
                  max.overlaps = Inf) +
  
  # Climate variable arrows
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "darkred") +
  geom_text_repel(data = loadings_df,
                  aes(x = PC1, y = PC2, label = variable),
                  color = "darkred",
                  size = 3) +
  
  theme_minimal(base_size = 14) +
  labs(
    x = "Posterior PC1",
    y = "Posterior PC2",
  )




####### climate W vs original PCA ######
W_draws <- posterior::as_draws_matrix(fit_emg$draws("W"))
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
