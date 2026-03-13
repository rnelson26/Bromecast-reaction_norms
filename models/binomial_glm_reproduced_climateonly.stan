data {
  int<lower=1> n_train;
  int<lower=1> n_test;
  array[n_train] int<lower=0, upper=1> r_train;

  int<lower=1> n_site_year;
  int<lower=1> n_site_year_train;
  int<lower=1> n_site_year_test;

  array[n_train] int<lower=1> idx_plant_train;
  array[n_train] int<lower=1> idx_plant_train_site;
  array[n_test] int<lower=1> idx_plant_test;
  array[n_test] int<lower=1> idx_plant_test_site;

  array[n_train] int<lower=0> plot_index_train;
  array[n_test] int<lower=0> plot_index_test;

  array[n_train] int<lower=1, upper=n_site_year_train> site_year_id_train;
  array[n_test] int<lower=n_site_year_train + 1, upper=n_site_year> site_year_id_test;

  int<lower=1> n_X;
  int<lower=1> p_X;
  int<lower=1> q_X;
  int<lower=1> q_X_soil;
  int<lower=1> n_plot;
  int<lower=1> n_X_soil;
  int<lower=1> s_X;

  matrix[n_X, p_X] X;
  matrix[p_X, q_X] Lambda;
  matrix[s_X, q_X_soil] Lambda_soil;
  matrix[n_X_soil, s_X] X_soil;

  // Full train/test for predictions
  int<lower=0> n_train_full;
  int<lower=0> n_X_full;
  int<lower=0> p_X_full;
  int<lower=0> n_X_soil_full;
  int<lower=0> s_X_full;
  int<lower=0> q_X_full;
  int<lower=0> q_X_soil_full;

  matrix[n_X_full, p_X_full] X_full;
  matrix[n_X_soil_full, s_X_full] X_soil_full;
  matrix[p_X_full, q_X_full] Lambda_full;
  matrix[s_X_full, q_X_soil_full] Lambda_soil_full;

  array[n_train_full] int<lower=1> idx_plant_train_full;     
  array[n_train_full] int<lower=1> idx_plant_train_site_full;
  array[n_train_full] int<lower=1> site_year_id_train_full;
  array[n_train_full] int<lower=0> plot_index_train_full;

  int<lower=0> n_test_full;
  array[n_test_full] int<lower=1> idx_plant_test_full;  
  array[n_test_full] int<lower=1> idx_plant_test_site_full;
  array[n_test_full] int<lower=1> site_year_id_test_full;
  array[n_test_full] int<lower=0> plot_index_test_full;
}

parameters {
  vector<lower=0, upper=pi()/2>[p_X] u_sigma;
  vector<lower=0, upper=pi()/2>[s_X] u_sigma_soil;
  vector<lower=0, upper=pi()/2>[q_X] u_zeta;
  vector<lower=0, upper=pi()/2>[q_X_soil] u_zeta_soil;

  matrix[n_X, q_X] W;
  matrix[n_X_soil, q_X_soil] W_soil;

  vector[q_X] beta;       // climate latent coefficients
  vector[q_X_soil] beta_soil;  // soil latent coefficients

  vector[n_site_year_train] site_year_effect_train_raw;
  real<lower=0> sigma_site_year;
  vector[n_plot] eta_plot_raw;
  real<lower=0> sigma_plot;
  real alpha;
}

transformed parameters {
  vector<lower=0>[p_X] sigma;
  vector<lower=0>[s_X] sigma_soil;
  vector<lower=0>[q_X] zeta;
  vector<lower=0>[q_X_soil] zeta_soil;

  matrix[n_X, q_X] W_scaled;
  matrix[n_X_soil, q_X_soil] W_soil_scaled;

  vector[n_site_year_train] site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
  vector[n_site_year_train] site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);

  vector[n_plot] eta_plot = sigma_plot * eta_plot_raw;
  vector[n_plot] eta_plot_centered = eta_plot - mean(eta_plot);

  for (j in 1:p_X) sigma[j] = tan(u_sigma[j]);
  for (j in 1:s_X) sigma_soil[j] = tan(u_sigma_soil[j]);
  for (l in 1:q_X) zeta[l] = tan(u_zeta[l]);
  for (l in 1:q_X_soil) zeta_soil[l] = tan(u_zeta_soil[l]);

  for (i in 1:n_X) W_scaled[i] = X[i] * Lambda;
  for (i in 1:n_X_soil) W_soil_scaled[i] = X_soil[i] * Lambda_soil;
}


model {
  // Priors
  to_vector(W) ~ normal(0, 1);
  to_vector(W_soil) ~ normal(0, 1);

  site_year_effect_train_raw ~ normal(0, 100);
  sigma_site_year ~ normal(0, 100);
  eta_plot_raw ~ normal(0, 100);
  sigma_plot ~ normal(0, 100);
  alpha ~ normal(0, 100);

  beta ~ normal(0, 1);
  beta_soil ~ normal(0, 1);

  // Likelihood for training
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    int s = site_year_id_train[i];

    real logit_p = alpha
                  + dot_product(W_scaled[idx], beta)
                  + dot_product(W_soil_scaled[site], beta_soil)
                  + site_year_effect_train_scaled_centered[s];

    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];

    r_train[i] ~ bernoulli_logit(logit_p);
  }
}

generated quantities {
  vector[n_train] p_train;
  array[n_train] int r_train_pred;
  vector[n_test] p_test;
  array[n_test] int r_test_pred;

  vector[n_train_full] p_train_full;
  array[n_train_full] int r_train_full;

  vector[n_test_full] p_test_full;
  array[n_test_full] int r_test_full;
  
    vector[n_train] p_train_fixed;
  array[n_train] int r_train_pred_fixed;

  vector[n_train_full] p_train_full_fixed;
  array[n_train_full] int r_train_full_fixed;

  // Project latent climate/soil projections for full data
  matrix[n_X_full, q_X] W_full;
  matrix[n_X_soil_full, q_X_soil_full] W_soil_full;

  for (i in 1:n_X_full) W_full[i] = X_full[i] * Lambda_full;
  for (i in 1:n_X_soil_full) W_soil_full[i] = X_soil_full[i] * Lambda_soil_full;

  // Training predictions
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    int s = site_year_id_train[i];

    real logit_p = alpha
                  + dot_product(W_scaled[idx], beta)
                  + dot_product(W_soil_scaled[site], beta_soil)
                  + site_year_effect_train_scaled_centered[s];

    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];

    p_train[i] = inv_logit(logit_p);
    r_train_pred[i] = bernoulli_logit_rng(logit_p);
    
  }
  
  // Fixed effects only 
    for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    
    
    real site_year_noise = normal_rng(0, sigma_site_year);

    real plot_noise =
   (plot_index_train[i] == 0)
       ? 0
     : normal_rng(0, sigma_plot);
    
    real mu_fixed_base = alpha
                       + dot_product(W_scaled[idx], beta)
                       + dot_product(W_soil_scaled[site], beta_soil)+ site_year_noise +
    plot_noise;

    p_train_fixed[i] = inv_logit(mu_fixed_base);
    r_train_pred_fixed[i] = bernoulli_logit_rng(mu_fixed_base);
  }

  // Testing predictions
  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int site = idx_plant_test_site[i];
    real site_year_noise = normal_rng(0, sigma_site_year);

    real logit_p = alpha
                  + dot_product(W_scaled[idx], beta)
                  + dot_product(W_soil_scaled[site], beta_soil)
                  + site_year_noise;

    if (plot_index_test[i] != 0)
      logit_p += eta_plot_centered[plot_index_test[i]];

    p_test[i] = inv_logit(logit_p);
    r_test_pred[i] = bernoulli_logit_rng(logit_p);
  }

  // Full training predictions
  for (i in 1:n_train_full) {
    int idx = idx_plant_train_full[i];
    int site = idx_plant_train_site_full[i];
    int s = site_year_id_train_full[i];

    real site_year_effect = s <= n_site_year_train
                           ? site_year_effect_train_scaled_centered[s]
                           : normal_rng(0, sigma_site_year);

    real logit_p = alpha
                  + dot_product(W_full[idx], beta)
                  + dot_product(W_soil_full[site], beta_soil)
                  + site_year_effect;

    if (plot_index_train_full[i] != 0)
      logit_p += eta_plot_centered[plot_index_train_full[i]];

    p_train_full[i] = inv_logit(logit_p);
    r_train_full[i] = bernoulli_logit_rng(logit_p);
    
  }
  
for (i in 1:n_train_full) {
  int idx = idx_plant_train_full[i];
  int site = idx_plant_train_site_full[i];
  
  
  real site_year_noise = normal_rng(0, sigma_site_year);

  real plot_noise =
   (plot_index_train_full[i] == 0)
       ? 0
     : normal_rng(0, sigma_plot);

  real mu_fixed = alpha
                            + dot_product(W_full[idx], beta)
                            + dot_product(W_soil_full[site], beta_soil)  + site_year_noise +
    plot_noise;

  p_train_full_fixed[i] = inv_logit(mu_fixed);
  r_train_full_fixed[i] = bernoulli_logit_rng(mu_fixed);
}


  // Full testing predictions
  for (i in 1:n_test_full) {
    int idx = idx_plant_test_full[i];
    int site = idx_plant_test_site_full[i];
    int s = site_year_id_test_full[i];
    real site_year_noise = normal_rng(0, sigma_site_year);

    real logit_p = alpha
                  + dot_product(W_full[idx], beta)
                  + dot_product(W_soil_full[site], beta_soil)
                  + site_year_noise;

    if (plot_index_test_full[i] != 0)
      logit_p += eta_plot_centered[plot_index_test_full[i]];

    p_test_full[i] = inv_logit(logit_p);
    r_test_full[i] = bernoulli_logit_rng(logit_p);
  }
}
