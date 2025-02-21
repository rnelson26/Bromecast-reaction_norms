


data {
  int<lower=1> K_genotype;  // Number of genotypes
  int<lower=0> N;           // Number of observations
  int P;                    // Number of predictors (no constraint)
  
  // Declare site_id and plot_id arrays without constraints in the data block
  int site_id[N];           // Array of site group identifiers
  int plot_id[N];           // Array of plot group identifiers
}


parameters {
  real alpha;  // Intercept
  vector[P] beta;  // Coefficients for fixed effects (including interactions)

  real<lower=0> sigma_site;  // Standard deviation for random intercepts of site
  real<lower=0> sigma_plot;  // Standard deviation for random intercepts of plot
  real<lower=0> sigma_genotype;  // Standard deviation for random intercepts of genotype
  real<lower=0> phi;  // Dispersion parameter (inverse of overdispersion) for Negative Binomial

  // Unique standard deviations for each random slope
  real<lower=0> sigma_genotype_slope_neighbors;     // Standard deviation for random slope of neighbors
  real<lower=0> sigma_genotype_slope_temp;      // Standard deviation for random slope of temp
  real<lower=0> sigma_genotype_slope_vwc; // Standard deviation for random slope of vwc

  // Random intercepts for site, plot, and genotype
  vector[K_site] site_intercepts;  // Random intercepts for site
  vector[K_plot] plot_intercepts;  // Random intercepts for plot
  vector[K_genotype] genotype_intercepts;  // Random intercepts for genotype

  // Random slopes for genotype (for temp, vwc, neighbors, and climate distance)
  matrix[K_genotype, 3] genotype_slopes;  // Slopes for temp, vwc, and neighbors for each genotype
}

model {
  // Likelihood: Negative Binomial distribution for seed_count with log link
  for (n in 1:N) {
    // Compute the log of the mean (mu) for the Negative Binomial model
    real log_mu = alpha + dot_product(X[n], beta) + 
      site_intercepts[site_id[n]] + 
      plot_intercepts[plot_id[n]] + 
      genotype_intercepts[genotype_id[n]] + 
      genotype_slopes[genotype_id[n], 1] * X[n, 1] +  // Random slope for neighbors
      genotype_slopes[genotype_id[n], 2] * X[n, 2] +  // Random slope for temp
      genotype_slopes[genotype_id[n], 3] * X[n, 3] ;   // Random slope for vwc

    // Negative Binomial likelihood with log link: exp(log_mu) gives the mean (mu)
    seed_count[n] ~ neg_binomial_2(exp(log_mu), phi);  // Correct Negative Binomial with log link
  }

  // Priors
  alpha ~ normal(0, 5);  // Prior for the intercept
  beta ~ normal(0, 5);  // Prior for the fixed effects

  sigma_site ~ normal(0, 1);  // Prior for the standard deviation of random intercepts for site
  sigma_plot ~ normal(0, 1);  // Prior for the standard deviation of random intercepts for plot
  sigma_genotype ~ normal(0, 1);  // Prior for the standard deviation of random intercepts for genotype
  phi ~ gamma(1, 1);  // Prior for the dispersion parameter (phi), gamma prior ensures it is positive

  // Priors for unique standard deviations of random slopes
  sigma_genotype_slope_neighbors ~ normal(0, 5);     
  sigma_genotype_slope_temp ~ normal(0, 5);      
  sigma_genotype_slope_vwc ~ normal(0, 5); 

  // Random intercepts: site, plot, genotype
  site_intercepts ~ normal(0, sigma_site);  // Random intercepts for site
  plot_intercepts ~ normal(0, sigma_plot);  // Random intercepts for plot
  genotype_intercepts ~ normal(0, sigma_genotype);  // Random intercepts for genotype

  // Random slopes for genotype (independent normal distributions with unique standard deviations)
  for (g in 1:K_genotype) {
    genotype_slopes[g, 1] ~ normal(0, sigma_genotype_slope_neighbors);     // Random slope for neighbors
    genotype_slopes[g, 2] ~ normal(0, sigma_genotype_slope_temp);      // Random slope for temp
    genotype_slopes[g, 3] ~ normal(0, sigma_genotype_slope_vwc); // Random slope for vwc
    }
}

generated quantities {
  vector[N] seed_count_pred;
  vector[N] log_likelihood_values;
  
  for (n in 1:N) {
    // Compute the log of the mean (mu) for the Negative Binomial model
    real log_mu = alpha + dot_product(X[n], beta) + 
      site_intercepts[site_id[n]] + 
      plot_intercepts[plot_id[n]] + 
      genotype_intercepts[genotype_id[n]] + 
      genotype_slopes[genotype_id[n], 1] * X[n, 1] +  // Random slope for neighbors
      genotype_slopes[genotype_id[n], 2] * X[n, 2] +  // Random slope for temp
      genotype_slopes[genotype_id[n], 3] * X[n, 3] ;   // Random slope for vwc

    // Predicted seed count from the model, using Negative Binomial distribution
    seed_count_pred[n] = neg_binomial_2_rng(exp(log_mu), phi);  // Predicted values with Negative Binomial distribution
    
    // Log-likelihood for each observation, which can be used in LOO
    log_likelihood_values[n] = neg_binomial_2_lpmf(seed_count[n] | exp(log_mu), phi);
  }
}

