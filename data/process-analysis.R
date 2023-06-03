# Joshua Alley
# generative model of process from contracts to exports via arms

# data largely from contracts-analysis.R 
 

# model, data, and fit
# compile stan code
process.mod <- cmdstan_model(stan_file = "data/ml-model-process.stan",
                           cpp_options = list(stan_threads = TRUE))


# fit model 
fit.process <- process.mod$sample(
  data = process.data,
  chains = 4, 
  parallel_chains = 4,
  threads_per_chain = 1,
  seed = 12,
  max_treedepth = 20, 
  adapt_delta = .99,
  refresh = 200
)

# save fit model
fit.process$save_object(file = "data/ml-model-process-fit.RDS")


#  diagnose 
fit.process$cmdstan_diagnose()

diagnostics <- fit.process$diagnostic_summary()
print(diagnostics)

# draws
draws <- fit.process$draws(format = "df")

# n/eff ratios and posterior ACFs 
ratios <- neff_ratio(fit.process)
mcmc_neff(ratios, size = 2)

mcmc_acf(draws, pars = "alpha_ob")
mcmc_acf(draws, pars = "alpha_arms")

# parallel coordinates plot
mcmc_parcoord(fit.process$draws("beta"))
mcmc_parcoord(fit.process$draws("rho"))



# state-year level parameters
mcmc_intervals(fit.process$draws("lambda")) +
  scale_y_discrete(labels = colnames(process.data$G))


mcmc_intervals(fit.process$draws("beta")) +
  scale_y_discrete(labels = colnames(process.data$X))

# competition
mcmc_intervals(fit.process$draws("rho"))

# country-year level parameters for arms
mcmc_intervals(fit.process$draws("beta")) +
  scale_y_discrete(labels = colnames(process.data$X))

# country-year level parameters for arms
mcmc_intervals(fit.process$draws("theta")) 

# state varying intercepts
mcmc_intervals(fit.process$draws("alpha_ob")) 
mcmc_intervals(fit.process$draws("alpha_state")) 
mcmc_intervals(fit.process$draws("sigma_st")) 
mcmc_intervals(fit.process$draws("sigma_ob")) 

