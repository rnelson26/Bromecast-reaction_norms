
data {
  int<lower=0> N;
 array[N] int<lower=0, upper=1> y;
}
parameters {
  real theta;
}
model {
  theta ~ beta(1,1);
  y ~ bernoulli(theta);
}

