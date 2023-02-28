// Joshua Alley
// ML model of US arms deals as a function of 
// recipient characteristics and state defense contracts

data {
  int<lower = 0> N;
  int<lower = 1> S; // number of state-year obs
  array[N] int<lower = 0> y_arms; // deliveries outcome: country-year
  array[S] real<lower = 0> y_ob; // state-level contracts
  int<lower = 1> K;  // number of country/deal-level variables
  matrix[N, K] X;  // country/deal-level design matrix
  int<lower = 1> C; // number of countries
  array[N] int<lower = 1, upper = C> cntry; // country index

  matrix[N, S] Z; // state-year intercepts index
  int<lower = 1> L; // number of state-year variables
  matrix[S, L] G; // state-year variables matrix
  
}

parameters {
  vector[K] beta;  // population-level effects- arms
  vector[L] lambda; // state-year level effects
  real alpha;  // overall intercept
  vector[C] alpha_cntry_std; // country varying intercepts: standardized for NC
  real<lower = 0> sigma_cntry; // sd of country intercepts
  real<lower = 0> sigma_stateyr; // sd of state-year parameters
  //real<lower = 4> nu_ob; // d.f. for state-year parameters
}

transformed parameters {
  vector[C] alpha_cntry; // country varying intercepts
  vector[S] mu_stateyr; // state-year parameter means
  
    
  // linear predictor for arms
    mu_stateyr = G * lambda;
  
  // country varying intercepts
  alpha_cntry = 0 + sigma_cntry * alpha_cntry_std; // non-centered parameterization,
                                      // where alpha_cntry ~ N(0, sigma_cntry)
                                    
  

 }

model {

  vector[N] mu;
  
  
  // likelihood: contracts- t distributed
      for (s in 1:S) {
      target += normal_lpdf(y_ob[s] | mu_stateyr[s], sigma_stateyr);
    }
    
  // initialize linear predictor term: arms
    mu = alpha_cntry[cntry] + 
                  Z * mu_stateyr + // brings in arms contracts
                  X * beta;

  // likelihood: arms exports- poisson
    for (n in 1:N) {
      target += poisson_log_lpmf(y_arms[n] | mu[n]);
    }
  // priors including constants
  target += student_t_lpdf(alpha | 3, 0.7, 2.5);
  target += normal_lpdf(beta | 0, 1);
  target += normal_lpdf(lambda | 0, 1);
  target += normal_lpdf(alpha_cntry_std | 0, 1);
  target += normal_lpdf(sigma_cntry | 0, 1);
  target += normal_lpdf(sigma_stateyr | 0, 1);
  //target += gamma_lpdf(nu_ob | 2, 0.1);
  
}

generated quantities{
  
  // vector[N] y_pred; // posterior pred for checking
  // 
  // for(i in 1:N){
  // y_pred[i] = poisson_log_rng(mu[i]);
  // }
  
}


