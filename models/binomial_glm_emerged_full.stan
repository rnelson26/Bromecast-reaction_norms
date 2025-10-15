data {
    int<lower=1> n_train;
    int<lower=1> n_test;
    array[n_train] int<lower=0, upper=1> e_train;

  // Define the total number of site_years first
  int<lower=1> n_site_year;  // Total number of site_years (training + testing)
  int<lower=1> n_site_year_train;  // Number of training site_years  
  int<lower=1> n_site_year_test;   // Number of test site_years
  
  array[n_train] int<lower=1> idx_plant_train;
  array[n_train] int<lower=1> genotype_plant_train;
  array[n_train] int<lower=1> idx_plant_train_site;
  array[n_test] int<lower=1> idx_plant_test;
  array[n_test] int<lower=1> genotype_plant_test;
  array[n_test] int<lower=1> idx_plant_test_site;

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
  int<lower=1> q_X_soil;
  int<lower=1> n_g;
  int<lower=1> n_plot;
  int<lower=1> n_X_soil;
  int<lower=1> s_X;

  matrix[n_X, p_X] X; // full climate space 
  matrix[p_X, q_X] Lambda;
  matrix[s_X, q_X_soil] Lambda_soil;
  matrix[n_X_soil, s_X] X_soil;
  matrix[n_g, n_g] K;
}


parameters {
  matrix[n_g, q_X] beta_raw;
  vector[q_X] mu_beta; 
  matrix[n_g, q_X_soil] beta_soil_raw;
  vector[q_X_soil] mu_beta_soil; 
  vector<lower=0, upper=1.57079632679>[p_X] u_sigma; //pi/2 = 1.57, stablizes half-cauchy in stan
  vector<lower=0, upper=1.57079632679>[s_X] u_sigma_soil;
  vector<lower=0, upper=1.57079632679>[q_X] u_zeta;
  vector<lower=0, upper=1.57079632679>[q_X_soil] u_zeta_soil;
  matrix[n_X, q_X] W;
  matrix[n_X_soil, q_X_soil] W_soil;
  real beta_neighbors;
  real beta_annual;
  real beta_perennial;
  real beta_shrub;
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
  vector<lower=0>[s_X] sigma_soil;
  vector<lower=0>[q_X] zeta;
  real<lower=0> zeta_0 = 5 * tan(u_zeta_0);
  vector<lower=0>[q_X_soil] zeta_soil;
  matrix[n_g, q_X] beta;
  matrix[n_g, q_X_soil] beta_soil;
vector[n_site_year_train] site_year_effect_train_scaled = sigma_site_year * site_year_effect_train_raw;
  vector[n_site_year_train] site_year_effect_train_scaled_centered = site_year_effect_train_scaled - mean(site_year_effect_train_scaled);
  vector[n_plot] eta_plot;
eta_plot = sigma_plot * eta_plot_raw;
vector[n_plot] eta_plot_centered = eta_plot - mean(eta_plot);
vector[n_g] beta_0;
vector[n_g] beta_0_centered;  

  for (j in 1:p_X)
    sigma[j] = tan(u_sigma[j]);

  for (j in 1:s_X)
      sigma_soil[j] = tan(u_sigma_soil[j]);
//zeta is a multivariate normal prior -- might be able to change back and see if it still convergences, zerta is cauchi, and beta is multivariate normal or clarify in comments 
  for (l in 1:q_X)
    zeta[l] = tan(u_zeta[l]);
    
for (l in 1:q_X_soil)
  zeta_soil[l] = tan(u_zeta_soil[l]);


  for (l in 1:q_X) {
    beta[, l] = mu_beta[l] + cholesky_decompose(K) * (sqrt(zeta[l]) * beta_raw[, l]);
  }
   beta_0 = zeta_0 * (cholesky_decompose(K) * beta_0_raw);  
   beta_0_centered = beta_0 - mean(beta_0);
   
   for (l in 1:q_X_soil) {
  beta_soil[, l] = mu_beta_soil[l] + cholesky_decompose(K) * (sqrt(zeta_soil[l]) * beta_soil_raw[, l]);
}
}


 //use same structure for genotype random intercepts, start normal 0,1 and then get decomposed here with K and square root of variance parameter 


model {
    
for (l in 1:q_X) {
  beta_raw[, l] ~ normal(0, 1); //fixed, do not change 
  mu_beta[l] ~ normal(0, 100);
}
for (l in 1:q_X_soil) {
  beta_soil_raw[, l] ~ normal(0, 1); //fixed, do not change 
  mu_beta_soil[l] ~ normal(0, 100);
}

  to_vector(W) ~ normal(0, 1); //fixed, do not change
  to_vector(W_soil) ~ normal(0, 1); //fixed, do not change 

  // Likelihood for bernoulli binomial
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];  
    int idx_genotype = genotype_plant_train[i];
    //separate a beta_soil[idx_genotype] with separate prior indentiical to beta for climate with prior defined similarly for beta_soil
    real logit_p = alpha + dot_product(W[idx, ], beta[idx_genotype, ])  + dot_product(W_soil[idx_site, ], beta_soil[idx_genotype, ]) +
                   site_year_effect_train_scaled_centered[site_year_id_train[i]] + 
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial_train[i] +
                   beta_shrub * shrub_train[i] +
                   beta_0_centered[idx_genotype];
  
    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];
      e_train[i] ~ bernoulli_logit(logit_p);
}


      // Priors for random intercept of site_year_effect and for common garden plot random effect
      // try changing priors to 0, 100 and see if it affects model and if it is needed. if not stable say used strong priors. can't change multivariate normal random variables, z beta/cholesky part, all below you can back off and widen  
site_year_effect_train_raw ~ normal(0, 1);
sigma_site_year ~ normal(0, 1);
eta_plot_raw ~ normal(0, 1);
sigma_plot ~ normal(0, 1); 
beta_0_raw ~ normal(0, 1); //do not change this prior 
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
  vector[n_test] p_test;
  vector[n_train] p_train;
  array[n_train] int e_train_pred;
  array[n_test] int e_test_pred;

  vector[n_train] p_train_fixed;
  array[n_train] int e_train_pred_fixed;

  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];
    int idx_genotype = genotype_plant_train[i];

    // --- Full model with random effects ---
    real logit_p = alpha + 
                   dot_product(W[idx, ], beta[idx_genotype, ]) + 
                   dot_product(W_soil[idx_site, ], beta_soil[idx_genotype, ]) +
                   site_year_effect_train_scaled_centered[site_year_id_train[i]] +
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial_train[i] +
                   beta_shrub * shrub_train[i] +
                   beta_0_centered[idx_genotype];

    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];

    p_train[i] = inv_logit(logit_p);
    e_train_pred[i] = bernoulli_logit_rng(logit_p);

    // --- Fixed effects only (no random effects) ---
    real mu_fixed = alpha + 
                    dot_product(W[idx, ], beta[idx_genotype, ]) +
                    dot_product(W_soil[idx_site, ], beta_soil[idx_genotype, ]) +
                    beta_neighbors * neighbors_train[i] +
                    beta_annual * annual_train[i] +
                    beta_perennial * perennial_train[i] +
                    beta_shrub * shrub_train[i];

    p_train_fixed[i] = inv_logit(mu_fixed);
    e_train_pred_fixed[i] = bernoulli_logit_rng(mu_fixed);
  }

  for (i in 1:n_test) {
    int idx = idx_plant_test[i];
    int idx_site = idx_plant_test_site[i];  
    int idx_genotype = genotype_plant_test[i];
    real site_year_noise = normal_rng(0, sigma_site_year);

    real logit_p = alpha + 
                   dot_product(W[idx, ], beta[idx_genotype, ]) + 
                   dot_product(W_soil[idx_site, ], beta_soil[idx_genotype, ]) +
                   site_year_noise +
                   beta_neighbors * neighbors_test[i] +
                   beta_annual * annual_test[i] +
                   beta_perennial * perennial_test[i] +
                   beta_shrub * shrub_test[i] +
                   beta_0_centered[idx_genotype];

    if (plot_index_test[i] != 0)
      logit_p += eta_plot_centered[plot_index_test[i]];

    p_test[i] = inv_logit(logit_p);
    e_test_pred[i] = bernoulli_logit_rng(logit_p);
  }
}
