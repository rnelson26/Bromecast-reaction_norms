data {
  int<lower=0> n_train;
  int<lower=0> n_test;
  array[n_train] int<lower=0, upper=1> r_train;
  array[n_train] int<lower=0, upper=1> e_train;
}

parameters {
  real alpha;  
}

model {
  alpha ~ normal(0, 1.5);
for (i in 1:n_train) {
  if (e_train[i] == 1) {
    target += bernoulli_logit_lpmf(r_train[i] | alpha);
  }
}
 }

generated quantities {
  vector[n_train] p_train;
  array[n_train] int r_train_pred;
  array[n_train] int r_train_pred2;

  vector[n_test] p_test;
  array[n_test] int r_test_pred;
  array[n_test] int r_test_pred2;

  for (i in 1:n_train) {
    p_train[i] = inv_logit(alpha);
    r_train_pred[i] = bernoulli_logit_rng(alpha);
    r_train_pred2[i] = bernoulli_logit_rng(alpha);
  }

  for (i in 1:n_test) {
    p_test[i] = inv_logit(alpha);
    r_test_pred[i] = bernoulli_logit_rng(alpha);
    r_test_pred2[i] = bernoulli_logit_rng(alpha);
  }
}

