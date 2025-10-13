data {
  int<lower=1> n_train;
  int<lower=1> n_test;
  array[n_train] int<lower=0, upper=1> e_train;

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
  int<lower=1> n_plot;
  int<lower=1> n_X_soil;
  int<lower=1> s_X;

  matrix[n_X, p_X] X;
  /// same comment as in other file
  /// you should have a different number of PCs for climate versus soil.
  matrix[p_X, q_X] Lambda;
  matrix[s_X, q_X] Lambda_soil;
  matrix[n_X_soil, s_X] X_soil;
}

parameters {
  /// no convergence issues? If so, pi()/2 is good
  vector<lower=0, upper=pi()/2>[p_X] u_sigma;
  vector<lower=0, upper=pi()/2>[s_X] u_sigma_soil;
  vector<lower=0, upper=pi()/2>[q_X] u_zeta;

  matrix[n_X, q_X] W;
  matrix[n_X_soil, q_X] W_soil;
  /// yes, same slopes and intercepts across all genotypes because we are ingoring genotype 
  vector[q_X] beta;       // climate latent coefficients
  vector[q_X] beta_soil;  // soil latent coefficients

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

  matrix[n_X, q_X] W_scaled;
  matrix[n_X_soil, q_X] W_soil_scaled;

  vector[n_site_year_train] site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
  vector[n_site_year_train] site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);

  vector[n_plot] eta_plot = sigma_plot * eta_plot_raw;
  vector[n_plot] eta_plot_centered = eta_plot - mean(eta_plot);
  
  /// Here is where you would put prior for beta and beta_soil 
  /// for (l in 1:q_X) beta[l] = zeta[l] * beta_raw[l]; 
  /// or this is in model block 
  /// beta[l] ~ normal(0, zeta[l]); where zeta[l] is sd and zeta[l]^2 is variance

  for (j in 1:p_X) sigma[j] = tan(u_sigma[j]);
  for (j in 1:s_X) sigma_soil[j] = tan(u_sigma_soil[j]);
  for (l in 1:q_X) zeta[l] = tan(u_zeta[l]);
  /// you had n_X for both X and X_soil in the previous "full" model
  /// here you have n_X and n_X_soil 
  /// check which one is correct. 
  /// is there soil and climate data at the same number of sites or do they differ? 
  /// if one is indexed in time and the other is not then they definetely will differ, right? 
  /// Also, I don't understand this part below 
  for (i in 1:n_X) W_scaled[i] = X[i] * Lambda;
  for (i in 1:n_X_soil) W_soil_scaled[i] = X_soil[i] * Lambda_soil;
}

model {
  // Priors
  to_vector(W) ~ normal(0, 1);
  to_vector(W_soil) ~ normal(0, 1);
  
  /// Where is the probabilistic PCA part? 
  /// i.e., target += normal_lpdf(X[i, j] | dot_product(Lambda[j, ], W[i, ]), sigma[j]); 

  site_year_effect_train_raw ~ normal(0, 1);
  sigma_site_year ~ normal(0, 1);
  eta_plot_raw ~ normal(0, 1);
  sigma_plot ~ normal(0, 1);
  alpha ~ normal(0, 1);

  beta ~ normal(0, 1);
  beta_soil ~ normal(0, 1);

  // Training likelihood
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

    e_train[i] ~ bernoulli_logit(logit_p);
  }
}

generated quantities {
  vector[n_train] p_train;
  array[n_train] int e_train_pred;
  vector[n_test] p_test;
  array[n_test] int e_test_pred;

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
    e_train_pred[i] = bernoulli_logit_rng(logit_p);
  }

  // Test predictions
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
    e_test_pred[i] = bernoulli_logit_rng(logit_p);
  }
}
