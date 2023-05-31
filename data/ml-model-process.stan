// Joshua Alley
// ML model of US defense contracts as a function of 
// arms deals- estimate both parts of the model

data {
  int<lower = 0> N; // number of arms deal level obs
  int<lower = 0> S; // number of state-year obs
  array[N] int<lower = 0> y_arms; // deliveries outcome: country-year
  array[S] real y_ob; // state-level contracts
  
  int<lower = 1> K;  // number of country/deal-level variables
  matrix[N, K] X;  // country/deal-level design matrix
  
  matrix[S, N] Z; // state-year index
  
  int<lower = 1> st; // numer of states 
  int<lower = 1, upper = st> state[S]; // state index
  array[S] real lag_y_ob; // lagged contracts
  int<lower = 1> L; // number of state-year variables
  matrix[S, L] G; // state-year variables matrix
  vector<lower = 0, upper = 1>[S] gwot; // gwot measure for interaction
  
  
}

transformed data{
  
  // sparse matrix represenation of Z
  vector[rows(csr_extract_w(Z))] w;
  int v[size(csr_extract_v(Z))]; 
  int u[size(csr_extract_u(Z))]; 
  
  // Implement the transformations   
  w = csr_extract_w(Z);
  v = csr_extract_v(Z);
  u = csr_extract_u(Z); 
  
}

parameters {
  vector[K] beta;  // population-level effects- arms
  real alpha_arms;  // overall intercept- arms 
  vector[L] lambda; // state-year level effects
  real alpha_ob;  // overall intercept- contracts
  real sigma_stateyr; // sd of state-year outcome
  real theta; // lagged DV coef

  real<lower = 2> nu_ob; // d.f. for state-year outcome
  
  vector[2] rho; // impact of deals on contracts- both inter terms
  vector[st] alpha_state; //
}

transformed parameters {
  vector[N] mu_arms; // state-year parameter means
  vector[S] agg_deals;
  vector[S] mu_ob;
  
    
  // linear predictor for arms deals
    mu_arms = alpha_arms + X * beta;
    
  // aggregate deals by year
    agg_deals = csr_matrix_times_vector(S, N, w, v, u, mu_arms);
                                      
  // linear predictor term: contracts
    mu_ob = alpha_ob + alpha_state[state] +
    lag_y_ob * theta +
    G * lambda + agg_deals * rho[1] + 
    (agg_deals .* gwot) * rho[2]; 
                                    
 }

model {
  
  // likelihood: arms exports- poisson
    for (n in 1:N) {
      target += poisson_log_lpmf(y_arms[n] | mu_arms[n]);
    }
    

  
  // likelihood: contracts- student
      for (s in 1:S) {
      target += student_t_lpdf(y_ob[s] | nu_ob, mu_ob[s], sigma_stateyr);
    }
    


  // priors including constants
  alpha_ob ~ student_t(3, 15, 5);
  alpha_arms ~ student_t(3, .75, 2);
  alpha_state ~ normal(0, 15);
  beta ~ std_normal();
  lambda ~ std_normal();
  rho ~ std_normal();
  sigma_stateyr ~ std_normal(); 
  theta ~ normal(0, .5); 

  nu_ob ~ gamma(2, 0.1);
  
}

generated quantities{
  
  // vector[N] y_pred; // posterior pred for checking
  // 
  // for(i in 1:N){
  // y_pred[i] = poisson_log_rng(mu[i]);
  // }
  
}


