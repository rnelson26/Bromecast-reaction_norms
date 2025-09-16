data {
  int<lower=0> n_train;
  int<lower=0> n_test;
  array[n_train] int<lower=0, upper=1> r_train;
}

parameters {
  real alpha;  
}

model {
  alpha ~ normal(0, 1);
  r_train ~ bernoulli_logit(alpha);
}

generated quantities {
  vector[n_train] p_train;
  array[n_train] int r_train_pred;

  vector[n_test] p_test;
  array[n_test] int r_test_pred;

  for (i in 1:n_train) {
    p_train[i] = inv_logit(alpha);
    r_train_pred[i] = bernoulli_logit_rng(alpha);
  }

  for (i in 1:n_test) {
    p_test[i] = inv_logit(alpha);
    r_test_pred[i] = bernoulli_logit_rng(alpha);
  }
}

