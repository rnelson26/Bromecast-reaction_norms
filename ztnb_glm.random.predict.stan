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
  array[n_train] int<lower=1> genotype_plant_train;
  array[n_test] int<lower=1> idx_plant_test;
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
 // array[n_train] int<lower=1, upper=n_site_year> site_year_id_train;  // Correct bounds
 // array[n_test] int<lower=1, upper=n_site_year> site_year_id_test;    // Correct bounds

array[n_train] int<lower=1, upper=n_site_year_train> site_year_id_train;  // Training set
array[n_test] int<lower=n_site_year_train + 1, upper=n_site_year> site_year_id_test; // Test set

  int<lower=1> n_X;
  int<lower=1> p_X;
  int<lower=1> q_X;
  int<lower=1> n_g;
  int<lower=1> n_plot;

  matrix[n_X, p_X] X;
  matrix[p_X, q_X] Lambda;
  matrix[n_g, n_g] K;
}


parameters {
  matrix[n_g, q_X] beta_raw;
  vector<lower=0, upper=1.57079632679>[p_X] u_sigma;
  vector<lower=0, upper=1.57079632679>[q_X] u_zeta;
  matrix[n_X, q_X] W;
  real beta_neighbors;
  real beta_annual;
  real beta_perennial;
  real beta_shrub;
  real<lower=0, upper=10000> theta; // Dispersion parameter for the negative binomial
vector[n_site_year_train] site_year_effect_train_raw;
vector[n_site_year_test] site_year_effect_test_raw;
real<lower=0> sigma_site_year;
vector[n_plot] eta_plot_raw;
real<lower=0> sigma_plot;

}

transformed parameters {
  vector<lower=0>[p_X] sigma;
  vector<lower=0>[q_X] zeta;
  matrix[n_g, q_X] beta;
vector[n_site_year_train] site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
vector[n_site_year_test] site_year_effect_test_scaled = sigma_site_year * site_year_effect_test_raw;
  vector[n_plot] eta_plot;
eta_plot = sigma_plot * eta_plot_raw;

  for (j in 1:p_X)
    sigma[j] = tan(u_sigma[j]);

  for (l in 1:q_X)
    zeta[l] = tan(u_zeta[l]);

  for (l in 1:q_X) {
    beta[, l] = cholesky_decompose(K) * (sqrt(zeta[l]) * beta_raw[, l]);
  }
}

model {
  for (l in 1:q_X) {
    beta_raw[, l] ~ normal(0, 1);
  }
  theta ~ gamma(1, 0.1);
  //gamma ~ normal(0, 1);
  to_vector(W) ~ normal(0, 1);

  // Likelihood for zero-truncated negative binomial
  for (i in 1:n_train) {
  int idx = idx_plant_train[i];
  int idx_genotype = genotype_plant_train[i];
  real mu_base = dot_product(W[idx, ], beta[idx_genotype, ]) +
                 site_year_effect_train_scaled[site_year_id_train[i]] +
                 beta_neighbors * neighbors_train[i] +
                 beta_annual * annual_train[i] +
                 beta_perennial * perennial_train[i] +
                 beta_shrub * shrub_train[i];

  real mu;
  if (plot_index_train[i] == 0)
    mu = exp(mu_base);
  else
    mu = exp(mu_base + eta_plot[plot_index_train[i]]);

  target += zt_negbinom_lpmf(y_train[i] | mu, theta);
}


      // Priors for random intercept of site_year_effect and for common garden plot random effect
site_year_effect_train_raw ~ normal(0, 1);
site_year_effect_test_raw ~ normal(0, 1);
sigma_site_year ~ normal(0, 1);
eta_plot_raw ~ normal(0, 1);
sigma_plot ~ normal(0, 1); 

// Priors for competition
beta_neighbors ~ normal(0, 1);
beta_annual ~ normal(0, 1);
beta_perennial ~ normal(0, 1);
beta_shrub ~ normal(0, 1);


  // Priors for X data
  for (i in 1:n_X) {
    for (j in 1:p_X) {
      target += normal_lpdf(X[i, j] | dot_product(Lambda[j, ], W[i, ]), sigma[j]);
    }
  }
}

generated quantities {
  vector[n_test] mu_test;
  vector[n_train] mu_train;

  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int idx_genotype = genotype_plant_test[i];
real mu_base = dot_product(W[idx, ], beta[idx_genotype, ]) +
               site_year_effect_test_scaled[site_year_id_test[i] - n_site_year_train] + 
               beta_neighbors * neighbors_test[i] +
               beta_annual * annual_test[i] +
               beta_perennial * perennial_test[i] +
               beta_shrub * shrub_test[i];


    if (plot_index_test[i] == 0)
      mu_test[i] = exp(mu_base);
    else
      mu_test[i] = exp(mu_base + eta_plot[plot_index_test[i]]);
  }

  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_genotype = genotype_plant_train[i];
    real mu_base = dot_product(W[idx, ], beta[idx_genotype, ]) +
                   site_year_effect_train_scaled[site_year_id_train[i]] +
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial_train[i] +
                   beta_shrub * shrub_train[i];

    if (plot_index_train[i] == 0)
      mu_train[i] = exp(mu_base);
    else
      mu_train[i] = exp(mu_base + eta_plot[plot_index_train[i]]);
  }
}
