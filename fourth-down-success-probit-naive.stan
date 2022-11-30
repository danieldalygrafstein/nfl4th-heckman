data {
  int<lower=1> N; //number of observations
  int<lower=1> K; //number of predictors
  // covariates
  matrix[N, K] X; // predictors
  array[N] int<lower=0, upper=1> y; // outcome
  vector[K] beta_mean;
  vector[K] beta_scale;
}

parameters {
  real beta_0;
  vector[K] beta;
}

transformed parameters{
  vector[N] mu;
  mu = Phi_approx(beta_0 + X*beta);
}

model {

  // normal priors
  beta_0 ~ normal(0, 0.5);

  for(i in 1:K){
  beta[i] ~ normal(beta_mean[i], beta_scale[i]);
  }
  // log-likelihood
  for(i in 1:N){
    y[i] ~ bernoulli(mu[i]);
  }

}

generated quantities {
  // log-likelihood of each obs
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = bernoulli_lpmf(y[i] | mu[i]);
  }
}

