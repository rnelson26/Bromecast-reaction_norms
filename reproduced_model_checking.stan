//version of reproduced full for model checking of priors (no generated quantities)
data {
  int<lower=1> n_train;
  int<lower=1> n_test;
  array[n_train] int<lower=0, upper=1> r_train;

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

  // Full training (emergence) data for prediction
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
  array[n_train_full] int<lower=1> genotype_plant_train_full;
  array[n_train_full] int<lower=1> site_year_id_train_full;
  vector[n_train_full] neighbors_train_full;
  vector[n_train_full] annual_train_full;
  vector[n_train_full] perennial_train_full;
  vector[n_train_full] shrub_train_full;
  array[n_train_full] int<lower=0> plot_index_train_full;

  // Full testing (emergence) data for prediction
  int<lower=0> n_test_full;
  array[n_test_full] int<lower=1> idx_plant_test_full;  
  array[n_test_full] int<lower=1> genotype_plant_test_full;
  array[n_test_full] int<lower=1> idx_plant_test_site_full;
  array[n_test_full] int<lower=1> site_year_id_test_full;
  vector[n_test_full] neighbors_test_full;
  vector[n_test_full] annual_test_full;
  vector[n_test_full] perennial_test_full;
  vector[n_test_full] shrub_test_full;
  array[n_test_full] int<lower=0> plot_index_test_full;
}

parameters {
  matrix[n_g, q_X] beta_raw;
  vector[q_X] mu_beta; 
  matrix[n_g, q_X_soil] beta_soil_raw;
  vector[q_X_soil] mu_beta_soil; 
  vector<lower=0, upper=1.57079632679>[p_X] u_sigma;
  vector<lower=0, upper=1.57079632679>[s_X] u_sigma_soil;
  vector<lower=0, upper=1.57079632679>[q_X] u_zeta;
  vector<lower=0, upper=1.57079632679>[q_X] u_zeta_soil;
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

  for (l in 1:q_X)
    zeta[l] = tan(u_zeta[l]);
    
  for (l in 1:q_X_soil)
  zeta_soil[l] = tan(u_zeta_soil[l]);

 for (l in 1:q_X) {
    beta[, l] = mu_beta[l] + cholesky_decompose(K) * (zeta[l] * beta_raw[, l]);
  }
   beta_0 = zeta_0 * (cholesky_decompose(K) * beta_0_raw);  
   beta_0_centered = beta_0 - mean(beta_0);
   
   for (l in 1:q_X_soil) {
  beta_soil[, l] = mu_beta_soil[l] + cholesky_decompose(K) * (zeta_soil[l] * beta_soil_raw[, l]);
}
}
 //use same structure for genotype random intercepts, start normal 0,1 and then get decomposed here with K and square root of variance parameter 


model {
  for (l in 1:q_X) {
    beta_raw[, l] ~ normal(0, 1);
    mu_beta[l] ~ normal(0, 100);
  }

for (l in 1:q_X_soil) {
  beta_soil_raw[, l] ~ normal(0, 1); //fixed, do not change 
  mu_beta_soil[l] ~ normal(0, 100);
}

u_sigma ~ uniform(0, 1.57079632679); //added priors for sigma and zeta 
u_sigma_soil ~ uniform(0, 1.57079632679);
u_zeta ~ uniform(0, 1.57079632679);
u_zeta_soil ~ uniform(0, 1.57079632679);

to_vector(W) ~ normal(0, 1);
to_vector(W_soil) ~ normal(0, 1);

  // Likelihood for bernoulli binomial
  for (i in 1:n_train) {
    int idx = idx_plant_train[i];
    int idx_site = idx_plant_train_site[i];  
    int idx_genotype = genotype_plant_train[i];
    
    real logit_p = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) + 
                   dot_product(W_soil[idx_site, ], beta_soil[idx_genotype, ]) +
                   site_year_effect_train_scaled_centered[site_year_id_train[i]] + 
                   beta_neighbors * neighbors_train[i] +
                   beta_annual * annual_train[i] +
                   beta_perennial * perennial_train[i] +
                   beta_shrub * shrub_train[i] +
                   beta_0_centered[idx_genotype];
  
    if (plot_index_train[i] != 0)
      logit_p += eta_plot_centered[plot_index_train[i]];
 
      r_train[i] ~ bernoulli_logit(logit_p);
}


      // Priors for random intercept of site_year_effect and for common garden plot random effect
site_year_effect_train_raw ~ normal(0, 100);
sigma_site_year ~ normal(0, 100);
eta_plot_raw ~ normal(0, 100);
sigma_plot ~ normal(0, 100); 
beta_0_raw ~ normal(0, 100); 
alpha ~ normal(0, 100);


// Priors for competition
beta_neighbors ~ normal(0, 100);
beta_annual ~ normal(0, 100);
beta_perennial ~ normal(0, 100);
beta_shrub ~ normal(0, 100);

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

