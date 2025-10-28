################# Bromecast: 06.Prepare Stan Data Emergence ##########################
############# created 3-25-25 ######################
############# Last modified: 10-22-25 ##########################
######## Prepares stan_data objects for emergence ################################

# Full emerged Stan data


stan_data_emg_full <- list(
  # dimensions 
  n_X = nrow(X_emg_SOS),
  n_X_soil = nrow(X_soil_emg),
  p_X = ncol(X_emg_SOS),
  s_X = ncol(X_soil_emg),
  q_X = ncol(Lambda_emg_SOS),
  q_X_soil = ncol(Lambda_soil_emg),
  
  # climate and soil 
  X = X_emg_SOS,
  X_soil = X_soil_emg,
  Lambda = Lambda_emg_SOS,
  Lambda_soil = Lambda_soil_emg,
  
  # genotype info
  n_g = 121,   # str(K_all_filtered)
  K = K_all_filtered, ## need to adjust so that it is only the genotypes actually used 
  
  # plot and site-year
  n_plot = max(training_df_emg$plot_id),
  n_site_year = max(c(training_df_emg$site_year_id, testing_df_emg$site_year_id)),
  
  # training data
  n_train = nrow(training_df_emg),
  e_train = training_df_emg$e_train,
  idx_plant_train = training_df_emg$plant_id, ### individual plant id within each site-year
  idx_plant_train_site = training_df_emg$site_id, ### site id
  genotype_plant_train = training_df_emg$genotype_id,## genotype id
  neighbors_train = training_df_emg$neighbors.s,
  annual_train = training_df_emg$annual.s,
  perennial_train = training_df_emg$perennial.s,
  shrub_train = training_df_emg$shrub.s,
  plot_index_train = training_df_emg$plot_id, ## plot id 
  n_site_year_train = length(unique(training_df_emg$site_year_id)),
  site_year_id_train = training_df_emg$site_year_id, ## site year id
  
  # testing data
  n_test = nrow(testing_df_emg),
  idx_plant_test = testing_df_emg$plant_id,
  idx_plant_test_site = testing_df_emg$site_id,
  genotype_plant_test = testing_df_emg$genotype_id,
  neighbors_test = testing_df_emg$neighbors.s,
  annual_test = testing_df_emg$annual.s,
  perennial_test = testing_df_emg$perennial.s,
  shrub_test = testing_df_emg$shrub.s,
  site_year_id_test = testing_df_emg$site_year_id,
  plot_index_test = testing_df_emg$plot_id,  # should all be 0 for satellite sites
  n_site_year_test = length(unique(testing_df_emg$site_year_id))
)

# submodels
stan_data_nocomp_emg <- stan_data_emg_full
stan_data_nocomp_emg[c(
  "neighbors_train", "neighbors_test",
  "annual_train", "annual_test",
  "perennial_train", "perennial_test",
  "shrub_train", "shrub_test"
)] <- NULL

stan_data_intra_emg <- stan_data_emg_full
stan_data_intra_emg[c(
  "annual_train", "annual_test",
  "perennial_train", "perennial_test",
  "shrub_train", "shrub_test"
)] <- NULL

stan_data_nogen_emg <- stan_data_emg_full
stan_data_nogen_emg[c(
  "n_g", "K", "genotype_plant_train", "genotype_plant_test"
)] <- NULL

stan_data_climate_only_emg <- stan_data_emg_full
stan_data_climate_only_emg[c(
  "n_g", "K", "genotype_plant_train", "genotype_plant_test",
  "neighbors_train", "neighbors_test",
  "annual_train", "annual_test",
  "perennial_train", "perennial_test",
  "shrub_train", "shrub_test"
)] <- NULL

# checks
cat("✅ Emergence Stan data constructed\n")
cat("Training plants:", stan_data_emg_full$n_train, 
    "| Testing plants:", stan_data_emg_full$n_test, "\n")
cat("Site-years (train/test/all):", 
    stan_data_emg_full$n_site_year_train, "/", 
    stan_data_emg_full$n_site_year_test, "/", 
    stan_data_emg_full$n_site_year, "\n")

