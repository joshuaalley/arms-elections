// Joshua Alley
// ML model of US arms deals as a function of 
// recipient characteristics and state defense contracts

data {
  int<lower = 0> N; // number of arms deal level obs
  int<lower = 1> S; // number of state-year obs
  array[N] int<lower = 0> y_arms; // deliveries outcome: country-year
  array[S] real y_ob; // state-level contracts
  
  int<lower = 1> K;  // number of country/deal-level variables
  matrix[N, K] X;  // country/deal-level design matrix
  
  int<lower = 1> T; // number of years
  matrix[N, T] Z; // year intercepts index
  //array[N] int<lower = 1, upper = T> year_arms; // year index- arms eq
  array[S] int<lower = 1, upper = T> year_ob; // year index- contracts
  
  int<lower = 1> L; // number of state-year variables
  matrix[S, L] G; // state-year variables matrix
  int<lower = 1> M; // number of electoral competition variables 
  matrix[S, M] H; // matrix of state-year electoral competition variables
  
}

parameters {
  vector[K] beta;  // population-level effects- arms
  vector[L] lambda; // state-year level effects
  real alpha;  // overall intercept

  real<lower = 0> sigma_stateyr; // sd of state-year outcome
  real<lower = 3> nu_ob; // d.f. for state-year outcome
  
  matrix[T, M] mu_gamma; // mean of state-year coefficients
  vector<lower = 0>[M] tau_gamma; // mean of theta par in multivariate distribution 
  matrix[M, T] z_gamma; // for non-centered Cholesky factorization 
  cholesky_factor_corr[M] L_Omega_gamma; // for non-centered Cholesky factorization 
  
  vector[M] rho; // impact of deals on means of eta- corr between 
}

transformed parameters {
  vector[N] mu_arms; // state-year parameter means
  vector[T] agg_deals;
  vector[S] mu_ob;
  // matrix[T, M] mu_gamma; // mean of alliance-level coefficients
  matrix[T, M] gamma; // state-year competition effects
  
    
  // linear predictor for arms deals
    mu_arms = X * beta;
    
  // aggregate deals by year
    agg_deals = Z' * mu_arms;
    
  // predict mean of gamma pars with deals
  // for (m in 1:M){
  //   mu_gamma[, m] = agg_deals * rho[m];
  // }
  
  // multivariate implementation of gamma 
    gamma = mu_gamma + (diag_pre_multiply(tau_gamma, L_Omega_gamma) * z_gamma)';
                                      
  // linear predictor term: contracts
    mu_ob = G * lambda + 
          (H * gamma') * rep_vector(1.0, T); // sums the product of H and the gammas
          // brings in arms contracts
                                    
 }

model {


  
    // likelihood: arms exports- poisson
    for (n in 1:N) {
      target += poisson_log_lpmf(y_arms[n] | mu_arms[n]);
    }
    

  
  // likelihood: contracts- normal
      for (s in 1:S) {
      target += student_t_lpdf(y_ob[s] | mu_ob, sigma_stateyr, nu_ob);
    }
    


  // priors including constants
  alpha ~ student_t(3, 0.7, 2.5);
  beta ~ normal(0, 1);
  lambda ~ normal(0, 1);
  rho ~ normal(0, 1);
  to_vector(z_gamma) ~ normal(0, .5);
  L_Omega_gamma ~ lkj_corr_cholesky(2);
  tau_gamma ~ normal(0, .25); 
  to_vector(mu_gamma) ~ normal(0, 1);

  sigma_stateyr ~ normal(0, 1);
  nu_ob ~ gamma(2, 0.1);
  
}

generated quantities{
  
  // vector[N] y_pred; // posterior pred for checking
  // 
  // for(i in 1:N){
  // y_pred[i] = poisson_log_rng(mu[i]);
  // }
  
}


