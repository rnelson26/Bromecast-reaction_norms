data {
  int<lower=0> n_train;
  int<lower=0> n_test;
  array[n_train] int<lower=0, upper=1> e_train;
}

parameters {
  real alpha;  
}

model {
  alpha ~ normal(0, 1.5);
  e_train ~ bernoulli_logit(alpha);
}

generated quantities {
  vector[n_train] p_train;
  vector[n_test] p_test;
  
  array[n_train] int e_train_pred;
  array[n_train] int e_train_pred2;  
  array[n_test] int e_test_pred;
  array[n_test] int e_test_pred2; 

  for (i in 1:n_train) {
    p_train[i] = inv_logit(alpha);
    e_train_pred[i] = bernoulli_logit_rng(alpha);
    e_train_pred2[i] = bernoulli_logit_rng(alpha);
  }

  for (i in 1:n_test) {
    p_test[i] = inv_logit(alpha);
    e_test_pred[i] = bernoulli_logit_rng(alpha);
    e_test_pred2[i] = bernoulli_logit_rng(alpha); 
  }
}

