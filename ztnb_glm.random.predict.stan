functions {
  // Log-likelihood for a zero-truncated Negative Binomial
  real zt_negbinom_lpmf(int y, real mu, real theta) {
    return neg_binomial_2_lpmf(y | mu, theta) - log1m_exp(neg_binomial_2_lpmf(0 | mu, theta));
  }
}
data {
  int<lower=1> n_train;
  int<lower=1> n_test;

  // Define the total number of site_years first
  int<lower=1> n_site_year;  // Total number of site_years (training + testing)
  int<lower=1> n_site_year_train;  // Number of training site_years  
  int<lower=1> n_site_year_test;   // Number of test site_years
  
  array[n_train] int<lower=1> idx_plant_train;
  array[n_train] int<lower=1> idx_plant_train_site;
  array[n_train] int<lower=1> genotype_plant_train;
  array[n_test] int<lower=1> idx_plant_test;
  array[n_test] int<lower=1> idx_plant_test_site;
  array[n_test] int<lower=1> genotype_plant_test;

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

  // Correct bounds for site_year arrays to use n_site_year

array[n_train] int<lower=1, upper=n_site_year_train> site_year_id_train;  // Training set
array[n_test] int<lower=n_site_year_train + 1, upper=n_site_year> site_year_id_test; // Test set

  int<lower=1> n_X;
  int<lower=1> p_X;
  int<lower=1> q_X;
  int<lower=1> n_g;
  int<lower=1> n_plot;
  int<lower=1> n_X_soil;
  int<lower=1> s_X;

  matrix[n_X, p_X] X; // full climate space 
  matrix[p_X, q_X] Lambda;
  matrix[s_X, q_X] Lambda_soil;
  matrix[n_X_soil, s_X] X_soil;
  matrix[n_g, n_g] K;
  
  // === Full training data ===
  int<lower=1> n_X_full;
int<lower=1> p_X_full;
int<lower=1> q_X_full;
matrix[n_X_full, p_X_full] X_full;
matrix[p_X_full, q_X_full] Lambda_full;

int<lower=1> n_X_soil_full;
int<lower=1> s_X_full;
matrix[n_X_soil_full, s_X_full] X_soil_full;
matrix[s_X_full, q_X_full] Lambda_soil_full;
int<lower=1> n_train_full;
array[n_train_full] int<lower=1> idx_plant_train_full;
array[n_train_full] int<lower=1> idx_plant_train_site_full;
array[n_train_full] int<lower=1> genotype_plant_train_full;
array[n_train_full] int<lower=1, upper=n_site_year_train> site_year_id_train_full;
vector[n_train_full] neighbors_train_full;
vector[n_train_full] annual_train_full;
vector[n_train_full] perennial_train_full;
vector[n_train_full] shrub_train_full;
array[n_train_full] int<lower=0> plot_index_train_full;

// === Full testing data ===
int<lower=1> n_test_full;
array[n_test_full] int<lower=1> idx_plant_test_full;
array[n_test_full] int<lower=1> idx_plant_test_site_full;
array[n_test_full] int<lower=1> genotype_plant_test_full;
vector[n_test_full] neighbors_test_full;
vector[n_test_full] annual_test_full;
vector[n_test_full] perennial_test_full;
vector[n_test_full] shrub_test_full;
array[n_test_full] int<lower=0> plot_index_test_full;

}


parameters {
  matrix[n_g, q_X] beta_raw;
  vector<lower=0, upper=1.57079632679>[p_X] u_sigma;
  vector<lower=0, upper=1.57079632679>[s_X] u_sigma_soil;
  vector<lower=0, upper=1.57079632679>[q_X] u_zeta;
  matrix[n_X, q_X] W;
  matrix[n_X_soil, q_X] W_soil;
  real beta_neighbors;
  real beta_annual;
  real beta_perennial;
  real beta_shrub;
  real<lower=0, upper=10000> theta; // Dispersion parameter for the negative binomial
vector[n_site_year_train] site_year_effect_train_raw;
real<lower=0> sigma_site_year;
vector[n_plot] eta_plot_raw;
real<lower=0> sigma_plot;
real alpha; // Global intercept

// Random intercepts for genotype  
 vector[n_g] beta_0_raw; //genotype_intercept
 real<lower=0, upper=pi()/2> u_zeta_0; //sigma_genotype_intercept
}

transformed parameters {
  vector<lower=0>[p_X] sigma;
  vector<lower=0>[q_X] zeta;
  real<lower=0> zeta_0 = 5 * tan(u_zeta_0);
   vector<lower=0>[s_X] sigma_soil;
  matrix[n_g, q_X] beta;
vector[n_site_year_train] site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
  vector[n_plot] eta_plot;
  vector[n_site_year_train] site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);
eta_plot = sigma_plot * eta_plot_raw;
 vector[n_plot] eta_plot_centered = eta_plot - mean(eta_plot);
vector[n_g] beta_0;

  for (j in 1:p_X)
    sigma[j] = tan(u_sigma[j]);

  for (j in 1:s_X)
      sigma_soil[j] = tan(u_sigma_soil[j]);

  for (l in 1:q_X)
    zeta[l] = tan(u_zeta[l]);

  for (l in 1:q_X) {
    beta[, l] = cholesky_decompose(K) * (sqrt(zeta[l]) * beta_raw[, l]);
  }
   beta_0 = zeta_0 * (cholesky_decompose(K) * beta_0_raw);  
   vector[n_g] beta_0_centered = beta_0 - mean(beta_0);
}

 //use same structure for genotype random intercepts, start normal 0,1 and then get decomposed here with K and square root of variance parameter 


model {
  for (l in 1:q_X) {
    beta_raw[, l] ~ normal(0, 1);
  }
  theta ~ gamma(1, 0.1);
  //gamma ~ normal(0, 1);
  to_vector(W) ~ normal(0, 1);
  to_vector(W_soil) ~ normal(0, 1);

  // Likelihood for zero-truncated negative binomial
  for (i in 1:n_train) {
  int idx = idx_plant_train[i];
  int idx_genotype = genotype_plant_train[i];
  int idx_site = idx_plant_train_site[i];  
  real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +  dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                 site_year_effect_train_scaled_centered[site_year_id_train[i]] +  
                 //beta[idx_genotype, 1] augment matrix for genotype intercepts index by genotype to get variable intercepts for genotype
                 beta_neighbors * neighbors_train[i] +
                 beta_annual * annual_train[i] +
                 beta_perennial * perennial_train[i] +
                 beta_shrub * shrub_train[i] +
                beta_0_centered[idx_genotype]; // Adding genotype-specific random intercept

  real mu;
  if (plot_index_train[i] == 0)
    mu = exp(mu_base);
  else
    mu = exp(mu_base + eta_plot_centered[plot_index_train[i]]);

  target += zt_negbinom_lpmf(y_train[i] | mu, theta);
}


      // Priors for random intercept of site_year_effect and for common garden plot random effect
site_year_effect_train_raw ~ normal(0, 1);
sigma_site_year ~ normal(0, 1);
eta_plot_raw ~ normal(0, 1);
sigma_plot ~ normal(0, 1); 
beta_0_raw ~ normal(0, 1); 
alpha ~ normal(0, 1);



// Priors for competition
beta_neighbors ~ normal(0, 1);
beta_annual ~ normal(0, 1);
beta_perennial ~ normal(0, 1);
beta_shrub ~ normal(0, 1);

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
  vector[n_test] mu_test;
  vector[n_train] mu_train;
  vector[n_train] mu_train_fixed;
  vector[n_test] mu_test_fixed;
  array[n_train] int y_train_pred;
  array[n_test] int y_test_pred;
  array[n_train] int y_train_pred_fixed;
  array[n_test] int y_test_pred_fixed;

  // NEW full predictions
  vector[n_train_full] mu_train_full;
  vector[n_test_full] mu_test_full;
  array[n_train_full] int y_train_pred_full;
  array[n_test_full] int y_test_pred_full;

  // Training predictions
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];
    int idx_genotype = genotype_plant_train[i];
    real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +
                   dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                   beta_0_centered[idx_genotype] +
                   site_year_effect_train_scaled_centered[site_year_id_train[i]] +
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial_train[i] +
                   beta_shrub * shrub_train[i];

    if (plot_index_train[i] == 0)
      mu_train[i] = exp(mu_base);
    else
      mu_train[i] = exp(mu_base + eta_plot_centered[plot_index_train[i]]);

    int y_sample = neg_binomial_2_rng(mu_train[i], theta);
    while (y_sample == 0)
      y_sample = neg_binomial_2_rng(mu_train[i], theta);
    y_train_pred[i] = y_sample;
  }

  // Test predictions
  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int idx_site = idx_plant_test_site[i];
    int idx_genotype = genotype_plant_test[i];

    real site_year_sample = normal_rng(0, sigma_site_year);

    real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +
                   dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                   beta_0_centered[idx_genotype] +
                   site_year_sample +
                   beta_neighbors * neighbors_test[i] +
                   beta_annual * annual_test[i] +
                   beta_perennial * perennial_test[i] +
                   beta_shrub * shrub_test[i];

    if (plot_index_test[i] == 0)
      mu_test[i] = exp(mu_base);
    else
      mu_test[i] = exp(mu_base + eta_plot_centered[plot_index_test[i]]);

    int y_sample = neg_binomial_2_rng(mu_test[i], theta);
    while (y_sample == 0)
      y_sample = neg_binomial_2_rng(mu_test[i], theta);
    y_test_pred[i] = y_sample;
  }

  // Fixed effects predictions (no plot or site-year noise)
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];
    int idx_genotype = genotype_plant_train[i];
    real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +
                   dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial_train[i] +
                   beta_shrub * shrub_train[i];
    mu_train_fixed[i] = exp(mu_base);

    int y_sample = neg_binomial_2_rng(mu_train_fixed[i], theta);
    while (y_sample == 0)
      y_sample = neg_binomial_2_rng(mu_train_fixed[i], theta);
    y_train_pred_fixed[i] = y_sample;
  }

  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int idx_site = idx_plant_test_site[i];
    int idx_genotype = genotype_plant_test[i];
    real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +
                   dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                   beta_neighbors * neighbors_test[i] +
                   beta_annual * annual_test[i] +
                   beta_perennial * perennial_test[i] +
                   beta_shrub * shrub_test[i];
    mu_test_fixed[i] = exp(mu_base);

    int y_sample = neg_binomial_2_rng(mu_test_fixed[i], theta);
    while (y_sample == 0)
      y_sample = neg_binomial_2_rng(mu_test_fixed[i], theta);
    y_test_pred_fixed[i] = y_sample;
  }

  // ==== Full dataset predictions ====
  matrix[n_train_full, q_X_full] W_train_full;
matrix[n_train_full, q_X_full] W_soil_train_full;
matrix[n_test_full, q_X_full] W_test_full;
matrix[n_test_full, q_X_full] W_soil_test_full;


  for (i in 1:n_train_full) {
    int idx = idx_plant_train_full[i];
    int idx_site = idx_plant_train_site_full[i];
    int idx_genotype = genotype_plant_train_full[i];
    real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +
                   dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                   beta_0_centered[idx_genotype] +
                   site_year_effect_train_scaled_centered[site_year_id_train_full[i]] +
                   beta_neighbors * neighbors_train_full[i] +
                   beta_annual * annual_train_full[i] +
                   beta_perennial * perennial_train_full[i] +
                   beta_shrub * shrub_train_full[i];
    if (plot_index_train_full[i] == 0)
      mu_train_full[i] = exp(mu_base);
    else
      mu_train_full[i] = exp(mu_base + eta_plot_centered[plot_index_train_full[i]]);

    int y_sample = neg_binomial_2_rng(mu_train_full[i], theta);
    while (y_sample == 0)
      y_sample = neg_binomial_2_rng(mu_train_full[i], theta);
    y_train_pred_full[i] = y_sample;
  }

  for (i in 1:n_test_full) {
    int idx = idx_plant_test_full[i];
    int idx_site = idx_plant_test_site_full[i];
    int idx_genotype = genotype_plant_test_full[i];
    real site_year_noise = normal_rng(0, sigma_site_year);

    real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +
                   dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                   site_year_noise +
                   beta_0_centered[idx_genotype] +
                   beta_neighbors * neighbors_test_full[i] +
                   beta_annual * annual_test_full[i] +
                   beta_perennial * perennial_test_full[i] +
                   beta_shrub * shrub_test_full[i];

    if (plot_index_test_full[i] == 0)
      mu_test_full[i] = exp(mu_base);
    else
      mu_test_full[i] = exp(mu_base + eta_plot_centered[plot_index_test_full[i]]);

    int y_sample = neg_binomial_2_rng(mu_test_full[i], theta);
    while (y_sample == 0)
      y_sample = neg_binomial_2_rng(mu_test_full[i], theta);
    y_test_pred_full[i] = y_sample;
  }
}


