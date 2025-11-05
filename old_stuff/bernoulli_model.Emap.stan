data {
  int<lower=0> N;                  
  array[N] int<lower=0, upper=1> y; 
  array[N] real MAPscaled;         
}
parameters {
  real alpha;  
  real beta_map; 
}
model {
  alpha ~ normal(0, 5);
  beta_map ~ normal(0, 5);
  for (i in 1:N) {
    y[i] ~ bernoulli_logit(alpha + beta_map * MAPscaled[i]);
  }
}
