
################# Bromecast: 04.Prepare Stan Data Fecundity ##########################
############# created 3-25-25 ######################
############# Last modified: 7-29-25 ##########################
######## Prepares stan_data objects for fecundity ################################



# Full fecundity Stan data
stan_data_fec_full <- list(
  n_X = nrow(X_SOS),
  n_X_soil = nrow(X_soil),
  p_X = ncol(X_SOS),
  s_X = ncol(X_soil),
  q_X = ncol(Lambda_SOS),
  q_X_soil = ncol(Lambda_soil),
  X = X_SOS,
  X_soil = X_soil,
  Lambda = Lambda_SOS,
  Lambda_soil = Lambda_soil,
  n_g = length(unique(genotype_plant_train)),
  K = K_common_garden,
  n_plot = max(training_df$plot_index),
  n_site_year = length(unique(c(training_df$site_year, testing_df$site_year))),
  
  # Full emerged data for posterior checks
  n_X_full = nrow(X_emg_SOS),
  n_X_soil_full = nrow(X_soil_emg),
  p_X_full = ncol(X_emg_SOS),
  s_X_full = ncol(X_soil_emg),
  q_X_full = ncol(Lambda_emg_SOS),
  q_X_soil_full = ncol(Lambda_soil_emg),
  X_full = X_emg_SOS,
  X_soil_full = X_soil_emg,
  Lambda_full = Lambda_emg_SOS,
  Lambda_soil_full = Lambda_soil_emg,
  n_g_full = length(unique(genotype_plant_train_emg)),
  n_plot_full = max(training_df_emg$plot_index),
  n_site_year_full = length(unique(c(training_df_emg$site_year, testing_df_emg$site_year))),
  
  # Training data
  n_train = nrow(training_df),
  y_train = y,
  idx_plant_train = idx_plant_train,
  idx_plant_train_site = idx_plant_train_site,
  genotype_plant_train = genotype_plant_train,
  neighbors_train = training_df$neighbors.s,
  annual_train = training_df$annual.s,
  perennial_train = training_df$perennial.s,
  shrub_train = training_df$shrub.s,
  plot_index_train = training_df$plot_index,
  n_site_year_train = length(unique(as.integer(as.factor(training_df$site_year)))),
  site_year_id_train = training_df$idx,
  
  # Training full (emerged)
  n_train_full = nrow(training_df_emg),
  idx_plant_train_full = idx_plant_train_emg,
  idx_plant_train_site_full = idx_plant_train_site_emg,
  genotype_plant_train_full = genotype_plant_train_emg,
  neighbors_train_full = training_df_emg$neighbors.s,
  annual_train_full = training_df_emg$annual.s,
  perennial_train_full = training_df_emg$perennial.s,
  shrub_train_full = training_df_emg$shrub.s,
  plot_index_train_full = training_df_emg$plot_index,
  n_site_year_train_full = length(unique(as.integer(as.factor(training_df_emg$site_year)))),
  site_year_id_train_full = training_df_emg$idx,
  
  # Testing data
  n_test = nrow(testing_df),
  idx_plant_test = idx_plant_test,
  idx_plant_test_site = idx_plant_test_site,
  genotype_plant_test = genotype_plant_test,
  neighbors_test = testing_df$neighbors.s,
  annual_test = testing_df$annual.s,
  perennial_test = testing_df$perennial.s,
  shrub_test = testing_df$shrub.s,
  site_year_id_test = testing_df$idx,
  plot_index_test = rep(0, nrow(testing_df)),
  n_site_year_test = length(unique(as.integer(as.factor(testing_df$site_year)))),
  
  # Testing full (emerged)
  n_test_full = nrow(testing_df_emg),
  idx_plant_test_full = idx_plant_test_emg,
  idx_plant_test_site_full = idx_plant_test_site_emg,
  genotype_plant_test_full = genotype_plant_test_emg,
  neighbors_test_full = testing_df_emg$neighbors.s,
  annual_test_full = testing_df_emg$annual.s,
  perennial_test_full = testing_df_emg$perennial.s,
  shrub_test_full = testing_df_emg$shrub.s,
  site_year_id_test_full = testing_df_emg$idx,
  plot_index_test_full = rep(0, nrow(testing_df_emg)),
  n_site_year_test_full = length(unique(as.integer(as.factor(testing_df_emg$site_year))))
)

# Submodels
stan_data_nocomp_fec <- stan_data_fec_full
stan_data_nocomp_fec[c("neighbors_train","neighbors_train_full",
                       "neighbors_test","neighbors_test_full",
                       "annual_train","annual_train_full",
                       "annual_test","annual_test_full",
                       "perennial_train","perennial_train_full",
                       "perennial_test","perennial_test_full",
                       "shrub_train","shrub_train_full",
                       "shrub_test","shrub_test_full")] <- NULL

stan_data_intra_fec <- stan_data_fec_full
stan_data_intra_fec[c("annual_train","annual_train_full",
                      "annual_test","annual_test_full",
                      "perennial_train","perennial_train_full",
                      "perennial_test","perennial_test_full",
                      "shrub_train","shrub_train_full",
                      "shrub_test","shrub_test_full")] <- NULL

stan_data_nogen_fec <- stan_data_fec_full
stan_data_nogen_fec[c("n_g","K","genotype_plant_train","genotype_plant_train_full",
                      "genotype_plant_test","genotype_plant_test_full")] <- NULL

stan_data_climate_only_fec <- stan_data_fec_full
stan_data_climate_only_fec[c("n_g","K","genotype_plant_train","genotype_plant_train_full",
                             "genotype_plant_test","genotype_plant_test_full",
                             "neighbors_train","neighbors_train_full",
                             "neighbors_test","neighbors_test_full",
                             "annual_train","annual_train_full",
                             "annual_test","annual_test_full",
                             "perennial_train","perennial_train_full",
                             "perennial_test","perennial_test_full",
                             "shrub_train","shrub_train_full",
                             "shrub_test","shrub_test_full")] <- NULL

