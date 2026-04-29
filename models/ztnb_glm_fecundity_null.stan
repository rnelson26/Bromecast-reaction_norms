functions {
  // Log-likelihood for zero-truncated Negative Binomial
  real zt_negbinom_lpmf(int y, real mu, real theta) {
    return neg_binomial_2_lpmf(y | mu, theta)
           - log1m_exp(neg_binomial_2_lpmf(0 | mu, theta));
  }
  
  // Simple rejection sampler for zero-truncated negative binomial 
  int ztnb_rng(real mu, real theta) {
    int y = 0;
    while (y == 0) {
      y = neg_binomial_2_rng(mu, theta);
    }
    return y;
  }
}

data {
  int<lower=1> n_train;
  int<lower=1> n_test;
  
  array[n_train] int<lower=0> y_train;  // fecundity observations
}

parameters {
  real alpha;                  // Global intercept on log scale
  real<lower=0> theta;  // Dispersion parameter
}

model {
  alpha ~ normal(0, 1.5);       // Prior on intercept
  theta ~ gamma(1, 0.1);      // Prior on dispersion

  for (i in 1:n_train) {
    real mu = exp(alpha);

    if (y_train[i] > 0)
      target += zt_negbinom_lpmf(y_train[i] | mu, theta);
  }
}

generated quantities {
  vector[n_train] mu_train;
  vector[n_test] mu_test;

  array[n_train] int y_train_pred;
  array[n_train] int y_train_pred2;
  
  array[n_test] int y_test_pred;
  array[n_test] int y_test_pred2;

  real mu = exp(alpha);

  for (i in 1:n_train) {
    mu_train[i] = mu;
    int y_sim = 0;
    while (y_sim == 0)
      y_sim = neg_binomial_2_rng(mu, theta);
    y_train_pred[i] = y_sim;
    y_train_pred2[i] = y_sim;
  }

  for (i in 1:n_test) {
    mu_test[i] = mu;
    int y_sim = 0;
    while (y_sim == 0)
      y_sim = neg_binomial_2_rng(mu, theta);
    y_test_pred[i] = y_sim;
    y_test_pred2[i] = y_sim;
  }
}
