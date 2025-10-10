#### Integrated Reaction Norm Model ########
######## Check model convergence and priors ##########
######## code by Becca Nelson and Justin Van Ee ###############################
############# created 3-25-25 ######################
############# Last modified: 10-10-25 ##########################

### load data 
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")


mod_fec <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/fecundity_model_checking.stan")

mod_rep <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/reproduced_model_checking.stan")
mod_emg <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/emerged_model_checking.stan")


# Pathfinder init
pathfinder_fit <- mod$pathfinder(data = config$data, init = 0, num_paths = 1)
init_list <- pathfinder_fit$draws(format = "list")

fit <- mod$sample(
  data = config$data,
  init = init_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000,
  seed = 123
)




####### Fit stan model #########

# Fit using cmdrstan 
### full models
mod <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/ztnb_glm.random.predict.stan")
#mod_fit <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/ztnb_glm.random.predict.fit.stan")
mod_rep <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_reproduced.stan")
mod_emg <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_survived.stan")

### submodels for reproduced
mod_rep_nogene <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_reproduced_nogene.stan")
mod_rep_nocomp <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_reproduced_nocomp.stan")
mod_rep_nointer <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_reproduced_nointer.stan")
mod_rep_clim <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_reproduced_climateonly.stan")

## submodels for emerged
mod_emg_nogene <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_emerged_nogene.stan")
mod_emg_nocomp <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_emerged_nocomp.stan")
mod_emg_nointer <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_emerged_nointer.stan")
mod_emg_clim <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/binomial_glm_emerged_climateonly.stan")

### null models
mod_null_emg <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/null_model_emerged.stan")
mod_null_rep <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/null_model_reproduced.stan")
mod_null_fec <- cmdstan_model("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/null_model_fecundity.stan")
# Extract posterior samples
posterior <- fit$draws(variables = c("theta","beta", "sigma", "W", "zeta", "mu_test", "mu_train", "W_soil"))

posterior_rep <- fit_rep$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))

posterior_emg_full <- fit_emg_full$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))

#posterior_emg_fall <- fit_emg_fall$draws(variables = c("beta", "sigma", "W", "zeta", "p_test", "p_train", "W_soil"))

#
# Traceplots for diagnostics
#

# theta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("theta"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("theta"), n_warmup = iter_warmup)
p + facet_text(size = 15)


# beta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("beta[20,1]","beta[20,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# gamma
#color_scheme_set("mix-blue-pink")
#p <- mcmc_trace(posterior,  pars = paste0("gamma[", 1:6, "]"), n_warmup = iter_warmup)
#p + facet_text(size = 15)

# sigma
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = paste0("sigma[", 1:ncol(X), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = paste0("sigma[", 1:ncol(X_emg_fall), "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# zeta
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = paste0("zeta[", 1:q_X, "]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# W
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)


color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("W[1,1]","W[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# W soil
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("W_soil[1,1]","W_soil[1,2]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

# mu
color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu_train[191]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("mu_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("mu_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)



color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_rep,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_full,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("p_train[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(posterior_emg_fall,  pars = c("p_test[199]"), n_warmup = iter_warmup)
p + facet_text(size = 15)

###### posterior means of random effects #######
library(posterior)
#posterior <- fit$draws(variables = c("theta","beta", "sigma", "W", "zeta", "mu_test", "mu_train", "W_soil"))
draws <- fit$draws()
draws_df <- as_draws_df(draws)



### Genotype random intercepts: scaled and centered
beta_0_raw <- draws_df %>%
  dplyr::select(starts_with("beta_0_raw"))

zeta_0 <- draws_df$zeta_0

# Scale
beta_0_scaled <- beta_0_raw * zeta_0

# Center across genotypes *within each draw*
beta_0_centered <- beta_0_scaled - rowMeans(beta_0_scaled)

# Summarize
beta_0_means <- beta_0_centered %>%
  summarise(across(everything(), list(
    mean = ~mean(.),
    q5 = ~quantile(., 0.05),
    q95 = ~quantile(., 0.95)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Genotype", "stat"),
    names_pattern = "(.*)_(mean|q5|q95)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

hist(beta_0_means$mean, main = "Distribution of Genotype Means",
     xlab = "Mean", breaks = 20)
mean(beta_0_means$mean)
range(beta_0_means$mean)
sum(abs(beta_0_means$mean) > 0.05)


### Site-year random effects: scaled and centered
site_year_raw <- draws_df %>%
  dplyr::select(starts_with("site_year_effect_train_raw"))

sigma_sy <- draws_df$sigma_site_year

site_year_scaled <- site_year_raw * sigma_sy
site_year_centered <- site_year_scaled - rowMeans(site_year_scaled)

site_year_means <- site_year_centered %>%
  summarise(across(everything(), list(
    mean = ~mean(.),
    q5 = ~quantile(., 0.05),
    q95 = ~quantile(., 0.95)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("SiteYear", "stat"),
    names_pattern = "(.*)_(mean|q5|q95)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

hist(site_year_means$mean, main = "Distribution of Site-Year Means",
     xlab = "Mean", breaks = 20)
mean(site_year_means$mean)
range(site_year_means$mean)
sum(abs(site_year_means$mean) > 0.05)


### Plot random effects: scaled and centered
eta_plot_raw <- draws_df %>%
  dplyr::select(starts_with("eta_plot_raw"))

sigma_plot <- draws_df$sigma_plot

eta_plot_scaled <- eta_plot_raw * sigma_plot
eta_plot_centered <- eta_plot_scaled - rowMeans(eta_plot_scaled)

eta_plot_means <- eta_plot_centered %>%
  summarise(across(everything(), list(
    mean = ~mean(.),
    q5 = ~quantile(., 0.05),
    q95 = ~quantile(., 0.95)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Plot", "stat"),
    names_pattern = "(.*)_(mean|q5|q95)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

hist(eta_plot_means$mean, main = "Distribution of Plot Means",
     xlab = "Mean", breaks = 20)
range(eta_plot_means$mean)
sum(abs(eta_plot_means$mean) > 0.05)

mean(eta_plot_means$mean)



## visualize random effect means

beta_0_means %>%
  ggplot(aes(x = reorder(Genotype, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_classic() +
  labs(x = "Genotype", y = "Genotype Intercept (mean ± 90% CI)") +
  coord_flip()

site_year_means %>%
  ggplot(aes(x = reorder(SiteYear, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_classic() +
  labs(x = "Site-Year", y = "Site-Year Effect (mean ± 90% CI)") +
  coord_flip()


eta_plot_means %>%
  ggplot(aes(x = reorder(Plot, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_classic() +
  labs(x = "Plot", y = "Plot Effect (mean ± 90% CI)") +
  coord_flip()

##### what we would expect from the prior
rnorm(1000, 0, 1) * sigma_sy  



### explore parameters ##############
library(posterior)
posterior_df <- as_draws_df(posterior)
names(posterior_df)

summary %>%
  filter(variable %in% c("beta_annual", "beta_neighbors", "beta_perennial", "beta_shrub")) %>%
  ggplot(aes(x = variable, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_minimal() +
  labs(y = "Estimate", title = " with 90% Credible Interval")




beta_summary <- posterior_df %>%
  dplyr::select(starts_with("beta")) %>%
  summarise(across(everything(), list(mean = ~mean(.), q5 = ~quantile(., 0.05), q95 = ~quantile(., 0.95))))


beta_long <- beta_summary %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(.*)")

ggplot(beta_long, aes(x = variable, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_minimal() +
  labs(y = "Estimate", title = "Parameter Estimates with 90% Credible Interval")



# Select all W coefficients

W_summary <- posterior_df %>%
  dplyr::select(matches("W\\[.*\\]")) %>%  # Match all W[i,j] values
  summarise(across(everything(), list(mean = ~mean(.), q5 = ~quantile(., 0.05), q95 = ~quantile(., 0.95))))

# Reshape data into long format for plotting
W_long <- W_summary %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(.*)")  # Separate variable name from summary stat


ggplot(W_long, aes(x = variable, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0.2) +
  theme_minimal() +
  labs(y = "Estimate", title = "Parameter Estimates with 90% Credible Interval")

#Lambda_df <- data.frame(loading = unlist(Lambda))
#Lambda_df$climate_variable <- rownames(Lambda_df)
#Lambda_rep$climate_variable <- rownames(Lambda_rep)



# Reshape to long format
library(reshape2)
library(reshape2)
Lambda_long <- melt(Lambda, varnames = c("climate_variable", "PC"), value.name = "loading")

# Heatmap
ggplot(Lambda_long, aes(x = PC, y = climate_variable, fill = loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  labs(title = "PCA Loadings: Climate variables on W axes",
       x = "Principal Component",
       y = "Climate Variable")

# Bar plot of loadings
ggplot(Lambda_long, aes(x = reorder(climate_variable, loading), y = loading, fill = loading > 0)) +
  geom_col() +
  facet_wrap(~ PC, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Contribution of Climate Variables to Each PCA Axis",
    x = "Climate Variable",
    y = "Loading"
  ) +
  theme(legend.position = "none")










#PC1 loads negatively on most temperature variables and positively on precipitation/SWE: temperature–moisture tradeoff axis.

#PC2 loads heavily on Fall/Winter precipitation/SWE and prcp.Sum (negatively): a seasonal moisture pattern axis.

### soil
library(reshape2)
Lambda_soil_long <- melt(Lambda_soil, varnames = c("soil_variable", "PC"), value.name = "loading")


## heat map
ggplot(Lambda_soil_long, aes(x = PC, y = soil_variable, fill = loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_size = 12) +
  labs(title = "PCA Loadings: Soil variables on W axes",
       x = "Principal Component",
       y = "Soil Variable")

# Bar plot of loadings
ggplot(Lambda_soil_long, aes(x = reorder(soil_variable, loading), y = loading, fill = loading > 0)) +
  geom_col() +
  facet_wrap(~ PC, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Contribution of Soil Variables to Each PCA Axis",
    x = "Soil Variable",
    y = "Loading"
  ) +
  theme(legend.position = "none")

#### means
beta_draws <- posterior_df %>% 
  dplyr::select(starts_with("beta["))

beta_names <- colnames(beta_draws)
beta_idx <- stringr::str_match(beta_names, "beta\\[(\\d+),(\\d+)\\]") %>% as.data.frame()
colnames(beta_idx) <- c("full", "row", "col")
beta_idx$row <- as.integer(beta_idx$row)
beta_idx$col <- as.integer(beta_idx$col)

n_g <- max(beta_idx$row)
q_X_beta <- max(beta_idx$col)
stopifnot(q_X == q_X_beta)

beta_mean <- matrix(NA, nrow = n_g, ncol = q_X)
for (i in seq_along(beta_names)) {
  r <- beta_idx$row[i]
  c <- beta_idx$col[i]
  beta_mean[r, c] <- mean(beta_draws[[i]])
}



