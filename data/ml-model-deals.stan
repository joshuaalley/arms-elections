// Joshua Alley
// ML model of US arms deals as a function of 
// recipient characteristics and state defense contracts

// Zero-inflated negative binomial functions from brms
functions {
  /* zero-inflated negative binomial log-PDF of a single response 
   * Args: 
   *   y: the response value 
   *   mu: mean parameter of negative binomial distribution
   *   phi: shape parameter of negative binomial distribution
   *   zi: zero-inflation probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_neg_binomial_lpmf(int y, real mu, real phi, 
                                       real zi) { 
    if (y == 0) { 
      return log_sum_exp(bernoulli_lpmf(1 | zi), 
                         bernoulli_lpmf(0 | zi) + 
                         neg_binomial_2_lpmf(0 | mu, phi)); 
    } else { 
      return bernoulli_lpmf(0 | zi) +  
             neg_binomial_2_lpmf(y | mu, phi); 
    } 
  } 
  /* zero-inflated negative binomial log-PDF of a single response 
   * logit parameterization of the zero-inflation part
   * Args: 
   *   y: the response value 
   *   mu: mean parameter of negative binomial distribution
   *   phi: shape parameter of negative binomial distribution
   *   zi: linear predictor for zero-inflation part
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_neg_binomial_logit_lpmf(int y, real mu, 
                                             real phi, real zi) { 
    if (y == 0) { 
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi), 
                         bernoulli_logit_lpmf(0 | zi) + 
                         neg_binomial_2_lpmf(0 | mu, phi)); 
    } else { 
      return bernoulli_logit_lpmf(0 | zi) +  
             neg_binomial_2_lpmf(y | mu, phi); 
    } 
  }
  /* zero-inflated negative binomial log-PDF of a single response 
   * log parameterization for the negative binomial part
   * Args: 
   *   y: the response value 
   *   eta: linear predictor for negative binomial distribution 
   *   phi: shape parameter of negative binomial distribution
   *   zi: zero-inflation probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_neg_binomial_log_lpmf(int y, real eta, 
                                           real phi, real zi) { 
    if (y == 0) { 
      return log_sum_exp(bernoulli_lpmf(1 | zi), 
                         bernoulli_lpmf(0 | zi) + 
                         neg_binomial_2_log_lpmf(0 | eta, phi)); 
    } else { 
      return bernoulli_lpmf(0 | zi) +  
             neg_binomial_2_log_lpmf(y | eta, phi); 
    } 
  } 
  /* zero-inflated negative binomial log-PDF of a single response
   * log parameterization for the negative binomial part
   * logit parameterization of the zero-inflation part
   * Args: 
   *   y: the response value 
   *   eta: linear predictor for negative binomial distribution 
   *   phi: shape parameter of negative binomial distribution
   *   zi: linear predictor for zero-inflation part 
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real zero_inflated_neg_binomial_log_logit_lpmf(int y, real eta, 
                                                 real phi, real zi) { 
    if (y == 0) { 
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi), 
                         bernoulli_logit_lpmf(0 | zi) + 
                         neg_binomial_2_log_lpmf(0 | eta, phi)); 
    } else { 
      return bernoulli_logit_lpmf(0 | zi) +  
             neg_binomial_2_log_lpmf(y | eta, phi); 
    } 
  }
  // zero_inflated negative binomial log-CCDF and log-CDF functions
  real zero_inflated_neg_binomial_lccdf(int y, real mu, real phi, real hu) { 
    return bernoulli_lpmf(0 | hu) + neg_binomial_2_lccdf(y | mu, phi);
  }
  real zero_inflated_neg_binomial_lcdf(int y, real mu, real phi, real hu) { 
    return log1m_exp(zero_inflated_neg_binomial_lccdf(y | mu, phi, hu));
  }
}


data {
  int<lower = 0> N;
  array[N] int<lower = 0> y;
  int<lower = 1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower = 1> C; // number of countries
  array[N] int<lower = 1, upper = C> cntry; // country index
}

parameters {
  vector[K] b;  // population-level effects
  real alpha;  // overall intercept
  real<lower = 0> shape;  // shape parameter
  real<lower = 0,upper = 1> zi;  // zero-inflation probability
  vector[C] alpha_cntry_std; // country varying intercepts
  real<lower = 0> sigma_cntry; // sd of country intercepts
}

transformed parameters {
  vector[C] alpha_cntry; // country varying intercepts
  alpha_cntry = 0 + sigma_cntry * alpha_cntry_std; // non-centered parameterization,
                                                          // where alpha_state ~ N(0, sigma_state)


 }

model {
  // likelihood including constants
    // initialize linear predictor term
    vector[N] mu = alpha + alpha_cntry[cntry] + X * b;
    for (n in 1:N) {
      target += zero_inflated_neg_binomial_log_lpmf(y[n] | mu[n], shape, zi);
    }
  // priors including constants
  target += student_t_lpdf(alpha | 3, 0.7, 2.5);
  target += gamma_lpdf(shape | 0.01, 0.01);
  target += beta_lpdf(zi | 1, 1);
  target += normal_lpdf(b | 0, 1);
  target += normal_lpdf(alpha_cntry_std | 0, 1);
  target += normal_lpdf(sigma_cntry | 0, 1);
}

