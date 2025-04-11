data {
  int<lower=1> n;
  int<lower=1> n_s;
  int<lower=1> n_g;
  int<lower=1> n_X;
  int<lower=1> p_X;
  int<lower=1> q_X;
  int<lower=1> p_V;
  array[n] int<lower=1> idx_plant;
  array[n] int<lower=1> genotype_plant;
  array[n] real y;
  matrix[n_X, p_X] X;
  matrix[p_X, q_X] Lambda;
  matrix[n_g, n_g] K;
  matrix[n, p_V] V;
}

parameters {
  matrix[n_g, q_X] beta_raw;
  vector<lower=0, upper=1.57079632679>[p_X] u_sigma;    // ~ Uniform(0, π/2)
  vector<lower=0, upper=1.57079632679>[q_X] u_zeta;     // ~ Uniform(0, π/2)
  real<lower=0, upper=1.57079632679> u_tau;             // ~ Uniform(0, π/2)
  matrix[n_X, q_X] W;
  vector[p_V] gamma;
}

transformed parameters {
  vector<lower=0>[p_X] sigma;
  vector<lower=0>[q_X] zeta;
  real<lower=0> tau;
  matrix[n_g, q_X] beta;

  // Apply tan transformations
  for (j in 1:p_X)
    sigma[j] = tan(u_sigma[j]);

  for (l in 1:q_X)
    zeta[l] = tan(u_zeta[l]);

  tau = tan(u_tau);

  for (l in 1:q_X) {
    beta[, l] = cholesky_decompose(K) * (sqrt(zeta[l]) * beta_raw[, l]);
  }
}

model {
  for (l in 1:q_X)
    beta_raw[, l] ~ normal(0, 1);

  gamma ~ normal(0, 1);
  to_vector(W) ~ normal(0, 1);

  for (i in 1:n) {
    int idx = idx_plant[i];
    int idx_genotype = genotype_plant[i];
    real mu = dot_product(W[idx, ], beta[idx_genotype, ]) + dot_product(V[i], gamma); 
    target += normal_lpdf(y[i] | mu, 1 / sqrt(tau));
  }

  for (i in 1:n_X) {
    for (j in 1:p_X) {
      target += normal_lpdf(X[i, j] | dot_product(Lambda[j, ], W[i, ]), sigma[j]);
    }
  }
}
