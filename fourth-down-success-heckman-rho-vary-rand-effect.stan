functions{
// Approximation from:
//  Boys, R.J., 1989. Algorithm AS R80: A remark on Algorithm AS 76: An
//    integral useful in calculating noncentral t and bivariate normal
//    probabilities. Journal of the Royal Statistical Society. Series C (Applied
//    Statistics), 38(3), pp.580-582. https://www.jstor.org/stable/2347755
  real binormal_cdf(real z1, real z2, real rho) {
  if (z1 != 0 || z2 != 0) {
    real denom = fabs(rho) < 1.0 ? sqrt((1 + rho) * (1 - rho)) : not_a_number();
    real a1 = (z2 / z1 - rho) / denom;
    real a2 = (z1 / z2 - rho) / denom;
    real product = z1 * z2;
    real delta = product < 0 || (product == 0 && (z1 + z2) < 0);
    return 0.5 * (Phi_approx(z1) + Phi_approx(z2) - delta) - owens_t(z1, a1) - owens_t(z2, a2);
  }
  return 0.25 + asin(rho) / (2 * pi());
}
}

data {
  int<lower=1> N; // number of observations
  int<lower=1> K; //number of outcome predictors
  int<lower=1> M; //number of instrumental variables
  int<lower=1, upper=N> N_y;
  // covariates
  matrix[N, K] X; //selection and outcome predictors
  matrix[N, M] Z;  //selection predictors
  // responses
  array[N] int<lower=0, upper=1> R; // missingness indicator
  array[N_y] int<lower=0, upper=1> Y; // outcome 
  // indexes
  int G[N]; //indicator whether selection probability is 1
  int<lower=1> num_cor_vars;
  int<lower=1> n_rho;
  matrix[n_rho, num_cor_vars] C;
  int<lower=1> num_coaches;
  int<lower=1, upper=num_coaches> coach[N];

}
parameters {
  real beta_r_0;
  real beta_y_0;
  vector[K] beta_y_x;
  vector[K] beta_r_x;
  vector[M] beta_r_z;
  vector[num_cor_vars] beta_c; 
  vector[num_coaches] beta_l;
  real<lower=0> sigma_l_squared;
}

transformed parameters{
  vector<lower=-1,upper=1>[n_rho] rho;
  rho = tanh(C*beta_c);

  real<lower=0> sigma_l;
  sigma_l = sqrt(sigma_l_squared);
}

model {

  int ny = 1;
  int nrho = 1;
  
  // normal (truncated) priors
  beta_r_0 ~ normal(0, 1);
  beta_y_0 ~ normal(0, 1);

  for(i in 1:K){
    beta_y_x[i] ~ normal(0, 0.5);
    beta_r_x[i] ~ normal(0, 0.5);
  }

  for(i in 1:M){
    beta_r_z[i] ~ normal(0, 0.5);
  }

  for(i in 1:num_cor_vars){
    beta_c[i] ~ normal(0, 0.5);
  }

  sigma_l_squared ~ normal(0, 1);

  for(i in 1:num_coaches){
    beta_l[i] ~ normal(0, sigma_l);
  }

  // log-likelihood
  for(i in 1:N){
    if(R[i] == 0){
       target += log(Phi_approx(-(beta_r_0 + X[i]*beta_r_x + Z[i]*beta_r_z + beta_l[coach[i]])));
    }
    else{
      if(G[i] == 0){
        if(Y[ny] == 0){
          target += log(binormal_cdf(-(beta_y_0 + X[i]*beta_y_x), (beta_r_0 + X[i]*beta_r_x + Z[i]*beta_r_z + beta_l[coach[i]]), -rho[nrho]));  
        } 
        else{
          target += log(binormal_cdf((beta_y_0 + X[i]*beta_y_x), (beta_r_0 + X[i]*beta_r_x + Z[i]*beta_r_z + beta_l[coach[i]]), rho[nrho])); 
        }
        nrho += 1;
      }
      // if prob of selection is 1, then have F(Y, inf) = F(Y) marginal normal dist of Y
      else{
        if(Y[ny] == 0){
          target += log(Phi_approx(-(beta_y_0 + X[i]*beta_y_x)));
        }
        else{
          target += log(Phi_approx(beta_y_0 + X[i]*beta_y_x));
        }
      }
     ny += 1;
    }
  }
}