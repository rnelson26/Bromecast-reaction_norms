data {
  int<lower=1> n_train;
  int<lower=1> n_test;
  array[n_train] int<lower=0, upper=1> e_train;

  // Define the total number of site_years first
  int<lower=1> n_site_year;
  int<lower=1> n_site_year_train;
  int<lower=1> n_site_year_test;

  array[n_train] int<lower=1> idx_plant_train;
  array[n_train] int<lower=1> genotype_plant_train;
  array[n_train] int<lower=1> idx_plant_train_site;
  array[n_test] int<lower=1> idx_plant_test;
  array[n_test] int<lower=1> genotype_plant_test;
  array[n_test] int<lower=1> idx_plant_test_site;


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
}

parameters {
  matrix[n_g, q_X] beta_raw;
  vector<lower=0, upper=1.57079632679>[p_X] u_sigma;
  vector<lower=0, upper=1.57079632679>[s_X] u_sigma_soil;
  vector<lower=0, upper=1.57079632679>[q_X] u_zeta;
  matrix[n_X, q_X] W;
  matrix[n_X, q_X] W_soil;
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
  vector[n_site_year_train] site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);
  vector[n_plot] eta_plot;
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
  //gamma ~ normal(0, 1);
  to_vector(W) ~ normal(0, 1);
  to_vector(W_soil) ~ normal(0, 1);

  // Likelihood for bernoulli binomial
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];  
    int idx_genotype = genotype_plant_train[i];
    
    real logit_p = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) + 
                   dot_product(W_soil[idx, ], beta[idx_genotype, ]) + dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                   site_year_effect_train_scaled_centered[site_year_id_train[i]] + 
                   beta_0_centered[idx_genotype];
  
    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];
      e_train[i] ~ bernoulli_logit(logit_p);
}


      // Priors for random intercept of site_year_effect and for common garden plot random effect
site_year_effect_train_raw ~ normal(0, 1);
sigma_site_year ~ normal(0, 1);
eta_plot_raw ~ normal(0, 1);
sigma_plot ~ normal(0, 1); 
beta_0_raw ~ normal(0, 1); 
alpha ~ normal(0, 1);


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
  // Fitted train/test sets
vector[n_train] p_train;
array[n_train] int e_train_pred;
vector[n_test] p_test;
array[n_test] int e_test_pred;



  // Fixed-effects-only predictions
vector[n_train] p_train_fixed;
array[n_train] int e_train_pred_fixed;

  // Training data predictions
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];
    int idx_genotype = genotype_plant_train[i];

    real logit_p = alpha
                 + dot_product(W[idx], beta[idx_genotype])
                 + dot_product(W_soil[idx_site], beta[idx_genotype])
                 + site_year_effect_train_scaled_centered[site_year_id_train[i]]
                 + beta_0_centered[idx_genotype];

    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];

    p_train[i] = inv_logit(logit_p);
    e_train_pred[i] = bernoulli_logit_rng(logit_p);

    // Fixed effects only (no site/plot random effects)
    real mu_fixed_base = alpha
                       + dot_product(W[idx], beta[idx_genotype])
                       + dot_product(W_soil[idx_site], beta[idx_genotype]);
                   
              
    p_train_fixed[i] = inv_logit(mu_fixed_base);
    e_train_pred_fixed[i] = bernoulli_logit_rng(mu_fixed_base);
  }

  // Testing data predictions (with noise for new site-years)
  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int idx_site = idx_plant_test_site[i];
    int idx_genotype = genotype_plant_test[i];
    real site_year_noise = normal_rng(0, sigma_site_year);

    real logit_p = alpha
                 + dot_product(W[idx], beta[idx_genotype])
                 + dot_product(W_soil[idx_site], beta[idx_genotype])
                 + site_year_noise
                 + beta_0_centered[idx_genotype];

    if (plot_index_test[i] != 0)
      logit_p += eta_plot_centered[plot_index_test[i]];

    p_test[i] = inv_logit(logit_p);
    e_test_pred[i] = bernoulli_logit_rng(logit_p);
  }
 }
