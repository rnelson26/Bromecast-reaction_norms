data {
  int<lower=1> n_train;
  int<lower=1> n_test;
  array[n_train] int<lower=0, upper=1> e_train;
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
  
  array[n_train] int<lower=0> transect_index_train;
  array[n_test] int<lower=0> transect_index_test;

  array[n_train] int<lower=1, upper=n_site_year_train> site_year_id_train;
  array[n_test] int<lower=n_site_year_train + 1, upper=n_site_year> site_year_id_test;

  int<lower=1> n_X;
  int<lower=1> p_X;
  int<lower=1> q_X;
  int<lower=1> q_X_soil;
  int<lower=1> n_plot;
  int<lower=1> n_transect;
  int<lower=1> n_X_soil;
  int<lower=1> s_X;
  
  int<lower=0> n_plot_test;
  int<lower=1> n_transect_test;

  matrix[n_X, p_X] X;
  matrix[p_X, q_X] Lambda;
  matrix[s_X, q_X_soil] Lambda_soil;
  matrix[n_X_soil, s_X] X_soil;
}

parameters {
  vector<lower=0, upper=pi()/2>[p_X] u_sigma;
  vector<lower=0, upper=pi()/2>[s_X] u_sigma_soil;

  matrix[n_X, q_X] W;
  matrix[n_X_soil, q_X_soil] W_soil;

  vector[q_X] beta;       // climate latent coefficients
  vector[q_X_soil] beta_soil;  // soil latent coefficients

  vector[n_site_year_train] site_year_effect_train_raw;
  real<lower=0> sigma_site_year;
  
  vector[n_plot] eta_plot_raw;
  real<lower=0> sigma_plot;
  
  vector[n_transect] eta_transect_raw;
  real<lower=0> sigma_transect;
  
  real alpha;
}

transformed parameters {
  vector<lower=0>[p_X] sigma;
  vector<lower=0>[s_X] sigma_soil;

  vector[n_site_year_train] site_year_effect_train_scaled;
  vector[n_site_year_train] site_year_effect_train_scaled_centered;
  
  vector[n_plot] eta_plot;
  vector[n_plot] eta_plot_centered;
  
  vector[n_transect] eta_transect;
  vector[n_transect] eta_transect_centered;

//apply transformations 
  for (j in 1:p_X)
    sigma[j] = tan(u_sigma[j]);

  for (j in 1:s_X)
    sigma_soil[j] = tan(u_sigma_soil[j]);
  
  site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
  site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);

  eta_plot = sigma_plot * eta_plot_raw;
  eta_plot_centered = eta_plot - mean(eta_plot);
  
  eta_transect = sigma_transect * eta_transect_raw;
  eta_transect_centered = eta_transect - mean(eta_transect);
}


model {
  // Priors
  to_vector(W) ~ normal(0, 1);
  to_vector(W_soil) ~ normal(0, 1);

  // Likelihood for training
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    int s = site_year_id_train[i];

    real logit_p = alpha
                  + dot_product(W[idx, ], beta)
                  + dot_product(W_soil[site, ], beta_soil)
                  + site_year_effect_train_scaled_centered[s];

    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];
      
    if (transect_index_train[i] != 0)
      logit_p += eta_transect_centered[transect_index_train[i]];

    if (e_train[i] == 1)
  target += bernoulli_logit_lpmf(r_train[i] | logit_p);
  }
  site_year_effect_train_raw ~ normal(0, 1);
  sigma_site_year ~ normal(0, 1.5);
  eta_plot_raw ~ normal(0, 1);
  sigma_plot ~ normal(0, 1.5);
  eta_transect_raw ~ normal(0, 1);
  sigma_transect ~ normal(0, 1.5);
  alpha ~ normal(0, 1.5);

  beta ~ normal(0, 1);
  beta_soil ~ normal(0, 1);
  
     // likelihood for climate and W
for (i in 1:n_X) {
  for (j in 1:p_X) {
    target += normal_lpdf(X[i, j] | dot_product(Lambda[j, ], W[i, ]), sigma[j]);
  }
}

// likelihood for soil and W
for (i in 1:n_X_soil) {
  for (j in 1:s_X) {
    target += normal_lpdf(X_soil[i, j] | dot_product(Lambda_soil[j, ], W_soil[i, ]), sigma_soil[j]);
  }
  }
}

generated quantities {
  vector[n_train] p_train;
  vector[n_test] p_test;
  
  array[n_train] int r_train_pred;
  array[n_train] int r_train_pred2;
  array[n_test] int r_test_pred;
  array[n_test] int r_test_pred2;
  
  vector[n_train] p_train_fixed;
  
  array[n_train] int r_train_pred_fixed;
  array[n_train] int r_train_pred_fixed2;

  // Training predictions
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    int s = site_year_id_train[i];

    real logit_p = alpha
                  + dot_product(W[idx, ], beta)
                  + dot_product(W_soil[site, ], beta_soil)
                  + site_year_effect_train_scaled_centered[s];

    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];
      
    if (transect_index_train[i] != 0)
      logit_p += eta_transect_centered[transect_index_train[i]];

    p_train[i] = inv_logit(logit_p);
    r_train_pred[i] = bernoulli_logit_rng(logit_p);
    r_train_pred2[i] = bernoulli_logit_rng(logit_p);
  }
  
  // simulate spatial random effects for training data
vector[n_site_year_train] site_year_noise_train;
vector[n_plot] plot_noise_train;
vector[n_transect] transect_noise_train;

for (j in 1:n_site_year_train)
  site_year_noise_train[j] = normal_rng(0, sigma_site_year);

for (j in 1:n_plot)
  plot_noise_train[j] = normal_rng(0, sigma_plot);

for (j in 1:n_transect)
  transect_noise_train[j] = normal_rng(0, sigma_transect);

// center random effects consistent with model structure
site_year_noise_train -= mean(site_year_noise_train);
plot_noise_train -= mean(plot_noise_train);
transect_noise_train -= mean(transect_noise_train);
//x−=y is equal to x=x−y

  // Fixed effects only 
    for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    
  real site_year_noise =
    site_year_noise_train[site_year_id_train[i]];

  // ? : equivalent to if else
  real plot_noise =
    (plot_index_train[i] == 0)
    ? 0
    : plot_noise_train[plot_index_train[i]];

  real transect_noise =
    (transect_index_train[i] == 0)
    ? 0
    : transect_noise_train[transect_index_train[i]];
    
    real mu_fixed_base = alpha
                       + dot_product(W[idx,], beta)
                       + dot_product(W_soil[site,], beta_soil)
                       + site_year_noise 
                       + plot_noise 
                       + transect_noise;

    p_train_fixed[i] = inv_logit(mu_fixed_base);
    r_train_pred_fixed[i] = bernoulli_logit_rng(mu_fixed_base);
    r_train_pred_fixed2[i] = bernoulli_logit_rng(mu_fixed_base);
  }

// simulate spatial random effects for testing
vector[n_site_year_test] site_year_noise_test;
vector[n_transect_test] transect_noise_test;

for (j in 1:n_site_year_test)
  site_year_noise_test[j] = normal_rng(0, sigma_site_year);

for (j in 1:n_transect_test)
  transect_noise_test[j] = normal_rng(0, sigma_transect);

site_year_noise_test -= mean(site_year_noise_test);
transect_noise_test -= mean(transect_noise_test);

  // Testing predictions
  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int site = idx_plant_test_site[i];
    
    int site_id = site_year_id_test[i] - n_site_year_train;

  real site_year_noise = site_year_noise_test[site_id];

  real plot_noise = 0;

  real transect_noise =
    transect_noise_test[transect_index_test[i]];

    real logit_p = alpha
                  + dot_product(W[idx,], beta)
                  + dot_product(W_soil[site,], beta_soil)
                  + site_year_noise
                  + plot_noise
                  + transect_noise;

    p_test[i] = inv_logit(logit_p);
    r_test_pred[i] = bernoulli_logit_rng(logit_p);
    r_test_pred2[i] = bernoulli_logit_rng(logit_p);
  }
}
