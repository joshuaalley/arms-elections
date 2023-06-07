// Joshua Alley
// ML model of US defense contracts as a function of 
// arms deals- estimate both parts of the model

data {
  int<lower = 0> N; // number of arms deal level obs
  int<lower = 0> S; // number of state-year obs
  array[N] int<lower = 0> y_arms; // deliveries outcome: country-year
  array[S] real y_ob; // state-level contracts
  //vector[S] lag_y_ob; // state-level contracts LDV
  
  int<lower = 1> K;  // number of country/deal-level variables
  matrix[N, K] X;  // country/deal-level design matrix
  
  matrix[S, N] Z; // state-year index
  
  int<lower = 1> st; // numer of states 
  int<lower = 1, upper = st> state[S]; // state index
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
  real<lower = 0> sigma_ob; // sd of state-year outcome

  real<lower = 2> nu_ob; // d.f. for state-year outcome
  
  vector[2] rho; // impact of deals on contracts- both inter terms
  vector[st] alpha_state_std; // 
  //vector[st] theta; // lagged DV coef
  real<lower = 0> sigma_st; // sd for intercept and slope

}

transformed parameters {
  vector[N] mu_arms; // state-year parameter means
  vector[S] agg_deals;
  vector[S] mu_ob;
  vector[st] alpha_state;
    
    
  // state-intercepts
  alpha_state = 0 + sigma_st * alpha_state_std; 
    
  // linear predictor for arms deals
    mu_arms = alpha_arms + X * beta;
    
  // aggregate deals by year
    agg_deals = csr_matrix_times_vector(S, N, w, v, u, mu_arms);
    
    

  // linear predictor term: contracts
  for(s in 1:S){
   mu_ob[s] = alpha_ob + 
    alpha_state[state[s]] + 
    //theta * y[s-1] +
    G[s] * lambda + agg_deals[s] * rho[1] + 
    (agg_deals[s] .* gwot[s]) * rho[2];
  }
                                    
 }

model {
  
  
  // likelihood: arms exports- poisson
    for (n in 1:N) {
      target += poisson_log_lpmf(y_arms[n] | mu_arms[n]);
    }
    

  
  // likelihood: contracts- student
      for (s in 1:S) {
      target += student_t_lpdf(y_ob[s] | 
      nu_ob, 
      mu_ob[s],
    sigma_ob);
    }
    


  // priors including constants
  alpha_ob ~ student_t(3, 0, 1);
  alpha_arms ~ student_t(3, .75, 2);
  beta ~ std_normal();
  lambda ~ normal(0, 2);
  rho ~ std_normal();
  sigma_ob ~ std_normal(); 
  //theta ~ normal(0, .45); 
  alpha_state_std ~ std_normal();
  sigma_st ~ normal(0, 1.5);
  nu_ob ~ gamma(2, 0.1);
  
}

generated quantities{

  
}


