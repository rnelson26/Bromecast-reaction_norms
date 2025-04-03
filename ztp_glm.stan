data {
  int<lower=1> n;                 // Number of observations 
  int<lower=1> n_g;               // Number of genotypes 
  int<lower=1> n_X;               // Number of observations of bioclimatic variables   
  int<lower=1> p_X;               // Number of bioclimatic variables 
  int<lower=1> q_X;               // Number of latent factors
  int<lower=1> p_V;               // Number of treatments + 1 for intercept
  array[31] int<lower=1> idx_sites;// Vector that connects sites to position in bioclim, was originally set to 4 
  array[n] int<lower=1> idx_plant;// Vector that says what site each plant is from
  array[n] int<lower=1> genotype_plant;// Vector that says what genotype each plant is from
  array[n] int<lower=1> y;        // Zero-truncated Poisson data (y_i >= 1)
  matrix[n, p_V] V;               // Covariate matrix (n obs, p_V treatments)
  matrix[n_X, p_X] X;             // Observed data matrix (n_E sites, p_W variables)
  matrix[p_X, q_X] Lambda;        // Provided factor loadings matrix
  matrix[n_g, n_g] K;             // Kinship matrix
}

parameters {
  matrix[n_g, q_X] beta;          // Regression coefficients PCs of bioclimatic variables varying by genotype
  vector[p_V] gamma;              // Regression coefficients treatments 
  vector<lower=0>[p_X] sigma;     // Diagonal elements of covariance matrix Sigma
  vector<lower=0>[q_X] zeta;      // Genetic Variances 
  matrix[n_X, q_X] W;             // Probabilistic PCs 
}

model {
  // Priors
  for (l in 1:q_X) {
    zeta[l] ~ cauchy(0, 1);
    beta[,l] ~ multi_normal(rep_vector(0, n_g), zeta[l] * K);
  }
  gamma ~ normal(0, 10);
  sigma ~ cauchy(0, 2.5);
  
  // Likelihood for zero-truncated Poisson
  for (i in 1:n) {
    int idx = idx_sites[idx_plant[i]];
    int idx_genotype = genotype_plant[i];
    real lambda = exp(dot_product(W[idx, ]', beta[idx_genotype,]) + dot_product(V[i, ]', gamma));
    target += poisson_lpmf(y[i] | lambda) - log1m_exp(-lambda); // Zero-truncation adjustment (we should do mean correction)
  }

  // Likelihood for X
  for (i in 1:n_X) {
    for (j in 1:p_X) {
      X[i, j] ~ normal(dot_product(Lambda[j, ], W[i, ]), sigma[j]);
    }
  }
}
