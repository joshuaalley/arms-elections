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
  
  matrix[N, S] Z; // state-year index
  
  int<lower = 1> L; // number of state-year variables
  matrix[S, L] G; // state-year variables matrix
  int<lower = 1> M; // number of contract variance predictors  
  matrix[S, M] H; // matrix of state-year contract variance predictors  
  
}

parameters {
  vector[K] beta;  // population-level effects- arms
  real alpha_arms;  // overall intercept- arms 
  vector[L] lambda; // state-year level effects
  vector[M] gamma; // state-year variance effects 
  real alpha_ob;  // overall intercept- contracts

  real<lower = 2> nu_ob; // d.f. for state-year outcome
  
  real rho; // impact of deals on means of eta- corr between 
}

transformed parameters {
  vector[N] mu_arms; // state-year parameter means
  vector[S] agg_deals;
  vector[S] mu_ob;
  
    
  // linear predictor for arms deals
    mu_arms = alpha_arms + X * beta;
    
  // aggregate deals by year
    agg_deals = Z' * mu_arms;
                                      
  // linear predictor term: contracts
    mu_ob = alpha_ob + G * lambda; 
                                    
 }

model {

    vector[S] sigma_stateyr = rep_vector(0.0, S);; // sd of state-year outcome
  
  // likelihood: arms exports- poisson
    for (n in 1:N) {
      target += poisson_log_lpmf(y_arms[n] | mu_arms[n]);
    }
    
    
  // predict contract variance  
     sigma_stateyr = H * gamma + agg_deals * rho;
     sigma_stateyr = exp(sigma_stateyr);
  
  // likelihood: contracts- student
      for (s in 1:S) {
      target += student_t_lpdf(y_ob[s] | mu_ob[s], sigma_stateyr[s], nu_ob);
    }
    


  // priors including constants
  alpha_ob ~ student_t(3, 15, 5);
  alpha_arms ~ student_t(3, .75, 2);
  beta ~ normal(0, 1);
  lambda ~ normal(0, 1);
  rho ~ normal(0, 1);
  gamma ~ normal(0, .5);

  nu_ob ~ gamma(2, 0.1);
  
}

generated quantities{
  
  // vector[N] y_pred; // posterior pred for checking
  // 
  // for(i in 1:N){
  // y_pred[i] = poisson_log_rng(mu[i]);
  // }
  
}


