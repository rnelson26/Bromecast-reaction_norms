################# Bromecast: 02.Prepare Stan Data Emergence ##########################
############# created 3-25-25 ######################
############# Last modified: 9-24-25 ##########################
######## Prepares stan_data objects for emergence ################################

# Full emerged Stan data
stan_data_emg_full <- list(
  n_X = nrow(X_emg_SOS),
  n_X_soil = nrow(X_soil_emg),
  p_X = ncol(X_emg_SOS),
  s_X = ncol(X_soil_emg),
  q_X = ncol(Lambda_emg_SOS),
  q_X_soil = ncol(Lambda_soil_emg),
  X = X_emg_SOS,
  X_soil = X_soil_emg,
  Lambda = Lambda_emg_SOS,
  Lambda_soil = Lambda_soil_emg,
  n_g = length(unique(genotype_plant_train_emg)),
  K = K_all, #K_common_garden just common garden genotypes, K_all from landscape genomics
  n_plot = max(training_df_emg$plot_index),
  n_site_year = length(unique(c(training_df_emg$site_year, testing_df_emg$site_year))),
  
  # Training data
  n_train = nrow(training_df_emg),
  e_train = training_df_emg$e_train,
  idx_plant_train = idx_plant_train_emg,
  idx_plant_train_site = idx_plant_train_site_emg,
  genotype_plant_train = genotype_plant_train_emg,
  neighbors_train = training_df_emg$neighbors.s,
  annual_train = training_df_emg$annual.s,
  perennial_train = training_df_emg$perennial.s,
  shrub_train = training_df_emg$shrub.s,
  plot_index_train = training_df_emg$plot_index,
  n_site_year_train = length(unique(as.integer(as.factor(training_df_emg$site_year)))),
  site_year_id_train = training_df_emg$idx,
  
  # Testing data
  n_test = nrow(testing_df_emg),
  idx_plant_test = idx_plant_test_emg,
  idx_plant_test_site = idx_plant_test_site_emg,
  genotype_plant_test = genotype_plant_test_emg,
  neighbors_test = testing_df_emg$neighbors.s,
  annual_test = testing_df_emg$annual.s,
  perennial_test = testing_df_emg$perennial.s,
  shrub_test = testing_df_emg$shrub.s,
  site_year_id_test = testing_df_emg$idx,
  plot_index_test = rep(0, nrow(testing_df_emg)),
  n_site_year_test = length(unique(as.integer(as.factor(testing_df_emg$site_year))))
)

# Submodels
stan_data_nocomp_emg <- stan_data_emg_full
stan_data_nocomp_emg[c("neighbors_train","neighbors_test",
                       "annual_train","annual_test",
                       "perennial_train","perennial_test",
                       "shrub_train","shrub_test")] <- NULL

stan_data_intra_emg <- stan_data_emg_full
stan_data_intra_emg[c("annual_train","annual_test",
                      "perennial_train","perennial_test",
                      "shrub_train","shrub_test")] <- NULL

stan_data_nogen_emg <- stan_data_emg_full
stan_data_nogen_emg[c("n_g","K","genotype_plant_train","genotype_plant_test")] <- NULL

stan_data_climate_only_emg <- stan_data_emg_full
stan_data_climate_only_emg[c("n_g","K","genotype_plant_train","genotype_plant_test",
                             "neighbors_train","neighbors_test",
                             "annual_train","annual_test",
                             "perennial_train","perennial_test",
                             "shrub_train","shrub_test")] <- NULL
