################# Bromecast: 07.Prepare Stan Data Reproduced ##########################
############# created 3-31-25 ######################
############# Last modified: 4-23-26 ##########################
######## Prepares stan_data objects for reproduced ################################


# Full reproduction Stan data
stan_data_rep_full <- list(
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
  n_g = 121,
  K = K_Michael_PD,
  n_plot = max(training_df_emg$plot_index),
  n_transect = max(training_df_emg$transect_index),
  n_site_year = length(unique(c(training_df_emg$site_year, testing_df_emg$site_year))),
  
  # Training data
  n_train = nrow(training_df_emg),
  e_train = training_df_emg$e_train,
  r_train = training_df_emg$r_train,
  idx_plant_train = idx_plant_train_emg,
  idx_plant_train_site = idx_plant_train_site_emg,
  genotype_plant_train = genotype_plant_train_emg,
  neighbors_train = training_df_emg$neighbors.s,
  annual_train = training_df_emg$annual.s,
  perennial_train = training_df_emg$perennial.s,
  shrub_train = training_df_emg$shrub.s,
  plot_index_train = training_df_emg$plot_index,
  transect_index_train = training_df_emg$transect_index,
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
  n_plot_test = 0,
  n_transect_test = max(testing_df_emg$transect_index),
  plot_index_test = rep(0, nrow(testing_df_emg)),
  transect_index_test = testing_df_emg$transect_index,
  n_site_year_test = length(unique(as.integer(as.factor(testing_df_emg$site_year))))
)

# Submodels
stan_data_nocomp_rep <- stan_data_rep_full
stan_data_nocomp_rep[c("neighbors_train","neighbors_train_full",
                       "neighbors_test","neighbors_test_full",
                       "annual_train","annual_train_full",
                       "annual_test","annual_test_full",
                       "perennial_train","perennial_train_full",
                       "perennial_test","perennial_test_full",
                       "shrub_train","shrub_train_full",
                       "shrub_test","shrub_test_full")] <- NULL

stan_data_intra_rep <- stan_data_rep_full
stan_data_intra_rep[c("annual_train","annual_train_full","annual_test","annual_test_full",
                      "perennial_train","perennial_train_full","perennial_test","perennial_test_full",
                      "shrub_train","shrub_train_full","shrub_test","shrub_test_full")] <- NULL

stan_data_nogen_rep <- stan_data_rep_full
stan_data_nogen_rep[c("n_g","K","genotype_plant_train","genotype_plant_train_full",
                      "genotype_plant_test","genotype_plant_test_full")] <- NULL

stan_data_climate_only_rep <- stan_data_rep_full
stan_data_climate_only_rep[c("n_g","K","genotype_plant_train","genotype_plant_train_full",
                             "genotype_plant_test","genotype_plant_test_full",
                             "neighbors_train","neighbors_train_full",
                             "neighbors_test","neighbors_test_full",
                             "annual_train","annual_train_full",
                             "annual_test","annual_test_full",
                             "perennial_train","perennial_train_full",
                             "perennial_test","perennial_test_full",
                             "shrub_train","shrub_train_full",
                             "shrub_test","shrub_test_full")] <- NULL
