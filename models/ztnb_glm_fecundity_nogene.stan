functions {
  // Log-likelihood for zero-truncated Negative Binomial
  real zt_negbinom_lpmf(int y, real mu, real theta) {
    return neg_binomial_2_lpmf(y | mu, theta) - log1m_exp(neg_binomial_2_lpmf(0 | mu, theta));
  }

  int ztnb_rng(real mu, real theta) {
    real log_p0 = neg_binomial_2_lpmf(0 | mu, theta);
    real u = uniform_rng(exp(log_p0), 1);
    int y = 1;
    real log_cdf = neg_binomial_2_lpmf(1 | mu, theta);

    while (exp(log_cdf) < u && y < 10000) {
      y += 1;
      log_cdf = log_sum_exp(log_cdf, neg_binomial_2_lpmf(y | mu, theta));
    }
    if (y >= 10000) y = 1;
    return y;
  }
}

data {
  int<lower=1> n_train;
  int<lower=1> n_test;

  int<lower=1> n_site_year;
  int<lower=1> n_site_year_train;
  int<lower=1> n_site_year_test;
  
  array[n_train] int<lower=1> idx_plant_train;
  array[n_train] int<lower=1> idx_plant_train_site;
  array[n_test] int<lower=1> idx_plant_test;
  array[n_test] int<lower=1> idx_plant_test_site;

  array[n_train] int<lower=0> y_train;
  vector[n_train] neighbors_train;
  vector[n_train] annual_train;
  vector[n_train] perennial_train;
  vector[n_train] shrub_train;
  array[n_train] int<lower=0> plot_index_train;

  vector[n_test] neighbors_test;
  vector[n_test] annual_test;
  vector[n_test] perennial_test;
  vector[n_test] shrub_test;
  array[n_test] int<lower=0> plot_index_test;

  array[n_train] int<lower=1, upper=n_site_year_train> site_year_id_train;
  array[n_test] int<lower=n_site_year_train + 1, upper=n_site_year> site_year_id_test;

  int<lower=1> n_X;
  int<lower=1> p_X;
  int<lower=1> q_X;
  int<lower=1> q_X_soil;
  int<lower=1> n_g;
  int<lower=1> n_plot;
  int<lower=1> n_X_soil;
  int<lower=1> s_X;

  matrix[n_X, p_X] X; 
  matrix[p_X, q_X] Lambda;
  matrix[s_X, q_X_soil] Lambda_soil;
  matrix[n_X_soil, s_X] X_soil;
  matrix[n_g, n_g] K;

  int<lower=1> n_train_full;
  int<lower=1> n_test_full;
  int<lower=1> n_site_year_train_full;
  int<lower=1> n_site_year_full;

  array[n_train_full] int<lower=1> idx_plant_train_full;
  array[n_train_full] int<lower=1> idx_plant_train_site_full;
  array[n_train_full] int<lower=1, upper=n_site_year_train_full> site_year_id_train_full;
  vector[n_train_full] neighbors_train_full;
  vector[n_train_full] annual_train_full;
  vector[n_train_full] perennial_train_full;
  vector[n_train_full] shrub_train_full;
  array[n_train_full] int<lower=0> plot_index_train_full;

  array[n_test_full] int<lower=1> idx_plant_test_full;
  array[n_test_full] int<lower=1> idx_plant_test_site_full;
  vector[n_test_full] neighbors_test_full;
  vector[n_test_full] annual_test_full;
  vector[n_test_full] perennial_test_full;
  vector[n_test_full] shrub_test_full;
  array[n_test_full] int<lower=0> plot_index_test_full;
  array[n_test_full] int<lower=n_site_year_train_full + 1, upper=n_site_year_full> site_year_id_test_full;
}

parameters {
  vector[q_X] beta;        // climate coefficients
  vector[q_X_soil] beta_soil;   // soil coefficients
  vector<lower=0, upper=pi()/2>[p_X] u_sigma;
  vector<lower=0, upper=pi()/2>[s_X] u_sigma_soil;
  vector<lower=0, upper=pi()/2>[q_X] u_zeta;
  vector<lower=0, upper=pi()/2>[q_X_soil] u_zeta_soil;
  
  matrix[n_X, q_X] W;
  matrix[n_X_soil, q_X_soil] W_soil;

  real beta_neighbors;
  real beta_annual;
  real beta_perennial;
  real beta_shrub;

  real<lower=0, upper=10000> theta;
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

  vector[n_site_year_train] site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
  vector[n_site_year_train] site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);

  vector[n_plot] eta_plot = sigma_plot * eta_plot_raw;
  vector[n_plot] eta_plot_centered = eta_plot - mean(eta_plot);

  for (j in 1:p_X) sigma[j] = tan(u_sigma[j]);
  for (j in 1:s_X) sigma_soil[j] = tan(u_sigma_soil[j]);
  for (l in 1:q_X) zeta[l] = tan(u_zeta[l]);
  for (l in 1:q_X_soil) zeta_soil[l] = tan(u_zeta_soil[l]);
}
}

model {
  theta ~ gamma(1, 0.1);
  to_vector(W) ~ normal(0, 1);
  to_vector(W_soil) ~ normal(0, 1);

  site_year_effect_train_raw ~ normal(0, 1);
  sigma_site_year ~ normal(0, 1);
  eta_plot_raw ~ normal(0, 1);
  sigma_plot ~ normal(0, 1);
  alpha ~ normal(0, 1);

  beta_neighbors ~ normal(0, 1);
  beta_annual ~ normal(0, 1);
  beta_perennial ~ normal(0, 1);
  beta_shrub ~ normal(0, 1);

  beta ~ normal(0, 1);
  beta_soil ~ normal(0, 1);

  // Zero-truncated NB likelihood
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];  
    real mu_base = alpha + dot_product(W[idx], beta) + dot_product(W_soil[idx_site], beta_soil) +
                   site_year_effect_train_scaled_centered[site_year_id_train[i]] +
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial_train[i] +
                   beta_shrub * shrub_train[i];

    real mu = (plot_index_train[i] == 0 ? exp(mu_base) : exp(mu_base + eta_plot_centered[plot_index_train[i]]));
    target += zt_negbinom_lpmf(y_train[i] | mu, theta);
  }

  // Likelihoods for latent climate and soil matrices
  for (i in 1:n_X)
    for (j in 1:p_X)
      target += normal_lpdf(X[i,j] | dot_product(Lambda[j,], W[i,]), sigma[j]);

  for (i in 1:n_X_soil)
    for (j in 1:s_X)
      target += normal_lpdf(X_soil[i,j] | dot_product(Lambda_soil[j,], W_soil[i,]), sigma_soil[j]);
}

generated quantities {
  real mu_cap = 1e10;

  vector[n_train] mu_train;
  vector[n_test] mu_test;
  vector[n_train_full] mu_train_full;
  vector[n_test_full] mu_test_full;

  array[n_train] int y_train_pred;
  array[n_test] int y_test_pred;
  array[n_train_full] int y_train_pred_full;
  array[n_test_full] int y_test_pred_full;

  // Training
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    real mu_base = alpha + dot_product(W[idx], beta) + dot_product(W_soil[site], beta_soil) +
                   site_year_effect_train_scaled_centered[site_year_id_train[i]] +
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial
