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
    if (y >= 10000) y = 1;  // Safety fallback
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
  array[n_train] int<lower=0> plot_index_train;
  array[n_test] int<lower=0> plot_index_test;

  array[n_train] int<lower=1, upper=n_site_year_train> site_year_id_train;
  array[n_test] int<lower=n_site_year_train + 1, upper=n_site_year> site_year_id_test;

  int<lower=1> n_X;
  int<lower=1> p_X;
  int<lower=1> q_X;
  int<lower=1> n_g;
  int<lower=1> n_plot;
  int<lower=1> n_X_soil;
  int<lower=1> s_X;

  matrix[n_X, p_X] X;
  matrix[p_X, q_X] Lambda;
  matrix[s_X, q_X] Lambda_soil;
  matrix[n_X_soil, s_X] X_soil;
  matrix[n_g, n_g] K;

  int<lower=1> n_train_full;
  int<lower=1> n_test_full;
  int<lower=1> n_site_year_train_full;
  int<lower=1> n_site_year_full;

  array[n_train_full] int<lower=1> idx_plant_train_full;
  array[n_train_full] int<lower=1> idx_plant_train_site_full;
  array[n_train_full] int<lower=1, upper=n_site_year_train_full> site_year_id_train_full;
  array[n_train_full] int<lower=0> plot_index_train_full;

  array[n_test_full] int<lower=1> idx_plant_test_full;
  array[n_test_full] int<lower=1> idx_plant_test_site_full;
  array[n_test_full] int<lower=0> plot_index_test_full;
  array[n_test_full] int<lower=n_site_year_train_full + 1, upper=n_site_year_full> site_year_id_test_full;
}

parameters {
  vector[q_X] beta;       // climate latent coefficients
  vector[q_X] beta_soil;  // soil latent coefficients

  vector<lower=0, upper=pi()/2>[p_X] u_sigma;
  vector<lower=0, upper=pi()/2>[s_X] u_sigma_soil;
  vector<lower=0, upper=pi()/2>[q_X] u_zeta; 

  matrix[n_X, q_X] W;
  matrix[n_X_soil, q_X] W_soil;
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

  vector[n_site_year_train] site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
  vector[n_site_year_train] site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);
  vector[n_plot] eta_plot = sigma_plot * eta_plot_raw;
  vector[n_plot] eta_plot_centered = eta_plot - mean(eta_plot);

  for (j in 1:p_X) sigma[j] = tan(u_sigma[j]);
  for (j in 1:s_X) sigma_soil[j] = tan(u_sigma_soil[j]);
  for (l in 1:q_X) zeta[l] = tan(u_zeta[l]);
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

  beta ~ normal(0, 1);
  beta_soil ~ normal(0, 1);

  // Training likelihood
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];

    real mu_base = alpha
                   + dot_product(W[idx], beta)
                   + dot_product(W_soil[site], beta_soil)
                   + site_year_effect_train_scaled_centered[site_year_id_train[i]];
    if (plot_index_train[i] != 0) mu_base += eta_plot_centered[plot_index_train[i]];

    target += zt_negbinom_lpmf(y_train[i] | exp(mu_base), theta);
  }

  // Climate/soil likelihoods for W projections
  for (i in 1:n_X) for (j in 1:p_X)
    target += normal_lpdf(X[i, j] | dot_product(Lambda[j, ], W[i, ]), sigma[j]);

  for (i in 1:n_X_soil) for (j in 1:s_X)
    target += normal_lpdf(X_soil[i, j] | dot_product(Lambda_soil[j, ], W_soil[i, ]), sigma_soil[j]);
}

generated quantities {
  real mu_cap = 10000;

  vector[n_train] mu_train;
  vector[n_test] mu_test;
  vector[n_train_full] mu_train_full;
  vector[n_test_full] mu_test_full;

  array[n_train] int y_train_pred;
  array[n_test] int y_test_pred;
  array[n_train_full] int y_train_pred_full;
  array[n_test_full] int y_test_pred_full;

  // Training predictions
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int site = idx_plant_train_site[i];
    real mu_base = alpha + dot_product(W[idx], beta) + dot_product(W_soil[site], beta_soil)
                   + site_year_effect_train_scaled_centered[site_year_id_train[i]];
    if (plot_index_train[i] != 0) mu_base += eta_plot_centered[plot_index_train[i]];

    mu_train[i] = exp(fmin(mu_base, mu_cap));
    y_train_pred[i] = ztnb_rng(mu_train[i], theta);
  }

  // Test predictions
  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int site = idx_plant_test_site[i];
    real site_year_noise = normal_rng(0, sigma_site_year);
    real mu_base = alpha + dot_product(W[idx], beta) + dot_product(W_soil[site], beta_soil)
                   + site_year_noise;
    if (plot_index_test[i] != 0) mu_base += eta_plot_centered[plot_index_test[i]];

    mu_test[i] = exp(fmin(mu_base, mu_cap));
    y_test_pred[i] = ztnb_rng(mu_test[i], theta);
  }

  // Full training predictions
  for (i in 1:n_train_full) {
    int idx = idx_plant_train_full[i];
    int site = idx_plant_train_site_full[i];
    real site_year_effect = site_year_id_train_full[i] <= n_site_year_train
                            ? site_year_effect_train_scaled_centered[site_year_id_train_full[i]]
                            : normal_rng(0, sigma_site_year);

    real mu_base = alpha + dot_product(W[idx], beta) + dot_product(W_soil[site], beta_soil)
                   + site_year_effect;
    if (plot_index_train_full[i] != 0) mu_base += eta_plot_centered[plot_index_train_full[i]];

    mu_train_full[i] = exp(fmin(mu_base, mu_cap));
    y_train_pred_full[i] = ztnb_rng(mu_train_full[i], theta);
  }

  // Full test predictions
  for (i in 1:n_test_full) {
    int idx = idx_plant_test_full[i];
    int site = idx_plant_test_site_full[i];
    real site_year_noise = normal_rng(0, sigma_site_year);
    real mu_base = alpha + dot_product(W[idx], beta) + dot_product(W_soil[site], beta_soil)
                   + site_year_noise;
    if (plot_index_test_full[i] != 0) mu_base += eta_plot_centered[plot_index_test_full[i]];

    mu_test_full[i] = exp(fmin(mu_base, mu_cap));
    y_test_pred_full[i] = ztnb_rng(mu_test_full[i], theta);
  }
}

