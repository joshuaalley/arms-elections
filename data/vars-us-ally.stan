// Joshua Alley 
// Varying slopes model of allied responses to election proximity


data {
  int<lower = 0> N; // number of observations
  vector[N] y; // outcome 
  int<lower = 1> P; // number of regressors 
  matrix[N, P] X; // fixed regressors matrix
  int<lower = 1> J; // number of groups for varying slopes
  int<lower = 1, upper = J> elec[N]; // election indicator
  int<lower = 1> L; // number of varying slopes
  matrix[N, L] Z; // varying slopes regressors
}


parameters {
  real<lower=0> sigma;
  real<lower = 2> nu; // degrees of freedom in t-distribution of outcome
  real alpha; // overall intercept 
  vector[P] gamma; // regressors
  
  matrix[J, L] mu_beta; // mean of alliance-level coefficients
  vector<lower = 0>[L] tau_beta; // mean of theta par in multivariate distribution
  matrix[L, J] z_beta; // for non-centered Cholesky factorization
  cholesky_factor_corr[L] L_Omega_beta; // for non-centered Cholesky factorization
}

transformed parameters {
  matrix[J, L] beta; // effect of alliance char on treaty participation across groups

  // varying slopes in alliance-level regression parameters beta
  beta = mu_beta + (diag_pre_multiply(tau_beta, L_Omega_beta) * z_beta)';


}


model {
  
  sigma ~ normal(0, .5);
  nu ~ gamma(2, 0.1); // Prior for degrees of freedom in t-dist
  alpha ~ normal(0, 1); 
  gamma ~ normal(0, .5); 
  
  
  to_vector(z_beta) ~ normal(0, .15);
  L_Omega_beta ~ lkj_corr_cholesky(2);
  tau_beta ~ normal(0, .15);
  to_vector(mu_beta) ~ normal(0, .15);
  
  
  for(i in 1:N){
  y ~ student_t(nu, alpha + X[i, ]*gamma + Z[i, ]*beta'[, elec[i]], sigma);
  }
  
}

