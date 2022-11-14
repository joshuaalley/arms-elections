// Joshua Alley
// ML model of US arms deals as a function of 
// recipient characteristics and state defense contracts

data {
  int<lower = 0> N;
  array[N] int<lower = 0> y;
  int<lower = 1> K;  // number of country/deal-level variables
  matrix[N, K] X;  // country/deal-level design matrix
  int<lower = 1> C; // number of countries
  array[N] int<lower = 1, upper = C> cntry; // country index

  int<lower = 1> S; // number of state-year obs
  matrix[N, S] Z; // state-year intercepts index
  int<lower = 1> L; // number of state-year variables
  matrix[S, L] G; // state-year variables matrix
  
  // old indexing attempt
  //int<lower = 1> T; // number of years- deals level
  //array[N] int<lower = 1, upper = T> yeard; // year index: deals
  //array[S] int<lower = 1, upper = T> yearc; // year index: contracts
}

parameters {
  vector[K] beta;  // population-level effects
  real alpha;  // overall intercept
  vector[C] alpha_cntry_std; // country varying intercepts: standardized for NC
  real<lower = 0> sigma_cntry; // sd of country intercepts
  vector[S] alpha_stateyr_std; // standardized state-year intercepts
  //vector[S] alpha_stateyr; // state-year varying intercepts
  vector[S] mu_stateyr; // state-year varying intercepts means
  real<lower = 0> sigma_stateyr; // sd of state-year intercepts
}

transformed parameters {
  vector[N] mu;
  vector[C] alpha_cntry; // country varying intercepts
  vector[S] alpha_stateyr; // state-year varying intercepts
  
  // country varying intercepts
  alpha_cntry = 0 + sigma_cntry * alpha_cntry_std; // non-centered parameterization,
                                      // where alpha_cntry ~ N(0, sigma_cntry)
                                      
  // state-year parameters                                    
  for(i in 1:S)
  alpha_stateyr[i] = mu_stateyr[i] + sigma_stateyr * alpha_stateyr_std[i]; // also non-centered
  
  // initialize linear predictor term
    mu = alpha_cntry[cntry] + 
                  Z * alpha_stateyr + 
                  X * beta;

 }

model {
  // likelihood 
    for (n in 1:N) {
      target += poisson_log_lpmf(y[n] | mu[n]);
    }
  // priors including constants
  target += student_t_lpdf(alpha | 3, 0.7, 2.5);
  target += normal_lpdf(beta | 0, 1);
  target += normal_lpdf(alpha_cntry_std | 0, 1);
  target += normal_lpdf(sigma_cntry | 0, 1);
  target += normal_lpdf(alpha_stateyr_std | 0, 1);
  target += normal_lpdf(sigma_stateyr | 0, 1);
  target += normal_lpdf(mu_stateyr | 0, 1);
}

generated quantities{
  
  vector[N] y_pred; // posterior pred for checking
  
  for(i in 1:N){
  y_pred[i] = poisson_log_rng(mu[i]);
  }
  
}


