// Joshua Alley
// ML model of US arms deals as a function of 
// recipient characteristics and state defense contracts

data {
  int<lower = 0> N; // number of arms deal level obs
  int<lower = 1> S; // number of state-year obs
  array[N] int<lower = 0> y_arms; // deliveries outcome: country-year
  array[S] real<lower = 0> y_ob; // state-level contracts
  
  int<lower = 1> K;  // number of country/deal-level variables
  matrix[N, K] X;  // country/deal-level design matrix
  //int<lower = 1> C; // number of countries
  //array[N] int<lower = 1, upper = C> cntry; // country index

  int<lower = 1> T; // number of years
  matrix[N, T] Z; // year intercepts index
  
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
  //real<lower = 4> nu_ob; // d.f. for state-year outcome
  
  matrix[T, M] mu_gamma; // mean of alliance-level coefficients
  vector<lower = 0>[M] tau_gamma; // mean of theta par in multivariate distribution 
  matrix[M, T] z_gamma; // for non-centered Cholesky factorization 
  cholesky_factor_corr[M] L_Omega_gamma; // for non-centered Cholesky factorization 
  
  //real[M] rho; // impact of deals on means of eta- corr between 
}

transformed parameters {
  //vector[C] alpha_cntry; // country varying intercepts
  vector[N] mu_arms; // state-year parameter means
  vector[T] agg_deals;
  vector[S] mu_ob;
  matrix[T, M] gamma; // state-year competition effects
  
    
  // linear predictor for arms deals
    mu_arms = X * beta;
    
  // aggregate deals by year
    agg_deals = Z' * mu_cntry;
  
  // country varying intercepts
  //alpha_cntry = 0 + sigma_cntry * alpha_cntry_std; // non-centered parameterization,
                                      // where alpha_cntry ~ N(0, sigma_cntry)
  // multivariate implementation of gamma 
    gamma = mu_gamma + (diag_pre_multiply(tau_gamma, L_Omega_gamma) * z_gamma)';
                                      
  // linear predictor term: contracts
    mu_ob = G * lambda + 
          H * gamma; // brings in arms contracts
                                    
 }

model {


  
    // likelihood: arms exports- poisson
    for (n in 1:N) {
      target += poisson_log_lpmf(y_arms[n] | mu_arms[n]);
    }
    

  
  // likelihood: contracts- normal
      for (s in 1:S) {
      target += normal_lpdf(y_ob[s] | mu_ob, sigma_stateyr);
    }
    


  // priors including constants
  target += student_t_lpdf(alpha | 3, 0.7, 2.5);
  target += normal_lpdf(beta | 0, 1);
  target += normal_lpdf(lambda | 0, 1);
  to_vector(z_gamma) ~ normal(0, .5);
  L_Omega_gamma ~ lkj_corr_cholesky(2);
  tau_gamma ~ normal(0, .25); 
  to_vector(mu_gamma) ~ normal(0, .5);
  //target += normal_lpdf(alpha_cntry_std | 0, 1);
  //target += normal_lpdf(sigma_cntry | 0, 1);
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


