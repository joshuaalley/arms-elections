// generated with brms 2.16.3
functions {
  /* hurdle lognormal log-PDF of a single response 
   * Args: 
   *   y: the response value 
   *   mu: mean parameter of the lognormal distribution 
   *   sigma: sd parameter of the lognormal distribution
   *   hu: hurdle probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real hurdle_lognormal_lpdf(real y, real mu, real sigma, real hu) { 
    if (y == 0) { 
      return bernoulli_lpmf(1 | hu); 
    } else { 
      return bernoulli_lpmf(0 | hu) +  
             lognormal_lpdf(y | mu, sigma); 
    } 
  }
  /* hurdle lognormal log-PDF of a single response
   * logit parameterization of the hurdle part
   * Args: 
   *   y: the response value 
   *   mu: mean parameter of the lognormal distribution 
   *   sigma: sd parameter of the lognormal distribution
   *   hu: linear predictor for the hurdle part 
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
  real hurdle_lognormal_logit_lpdf(real y, real mu, real sigma, real hu) { 
    if (y == 0) { 
      return bernoulli_logit_lpmf(1 | hu); 
    } else { 
      return bernoulli_logit_lpmf(0 | hu) +  
             lognormal_lpdf(y | mu, sigma); 
    } 
  } 
  // hurdle lognormal log-CCDF and log-CDF functions 
  real hurdle_lognormal_lccdf(real y, real mu, real sigma, real hu) { 
    return bernoulli_lpmf(0 | hu) + lognormal_lccdf(y | mu, sigma); 
  }
  real hurdle_lognormal_lcdf(real y, real mu, real sigma, real hu) { 
    return log1m_exp(hurdle_lognormal_lccdf(y | mu, sigma, hu));
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> K_hu;  // number of population-level effects
  matrix[N, K_hu] X_hu;  // population-level design matrix
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  int Kc_hu = K_hu - 1;
  matrix[N, Kc_hu] Xc_hu;  // centered version of X_hu without an intercept
  vector[Kc_hu] means_X_hu;  // column means of X_hu before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
  for (i in 2:K_hu) {
    means_X_hu[i - 1] = mean(X_hu[, i]);
    Xc_hu[, i - 1] = X_hu[, i] - means_X_hu[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
  vector[Kc_hu] b_hu;  // population-level effects
  real Intercept_hu;  // temporary intercept for centered predictors
}
transformed parameters {
}
model {
  // likelihood including constants
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    // initialize linear predictor term
    vector[N] hu = Intercept_hu + Xc_hu * b_hu;
    for (n in 1:N) {
      target += hurdle_lognormal_logit_lpdf(Y[n] | mu[n], sigma, hu[n]);
    }
  // priors including constants
  target += normal_lpdf(b | 0, 1);
  target += student_t_lpdf(Intercept | 3, -2.3, 2.5);
  target += normal_lpdf(sigma | 0, 1)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += normal_lpdf(b_hu | 0, 1);
  target += logistic_lpdf(Intercept_hu | 0, 1);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_hu_Intercept = Intercept_hu - dot_product(means_X_hu, b_hu);
}
