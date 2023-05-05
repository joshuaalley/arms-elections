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
  adapt_delta = .9,
  refresh = 200
)

 # save fit model
fit.process$save_object(file = "data/ml-model-process-fit.RDS")


#  diagnose 
fit.process$cmdstan_diagnose()

diagnostics <- fit.process$diagnostic_summary()
draws <- fit.process$draws(format = "df")

print(diagnostics)
# n/eff ratios and posterior ACFs 
ratios <- neff_ratio(fit.process)
mcmc_neff(ratios, size = 2)

mcmc_acf(draws, pars = "alpha")
#mcmc_acf(draws, pars = "alpha_cntry[12]")

# parallel coordinates plot
mcmc_parcoord(fit.process$draws("beta"))

# look at parameter estimates: start with alpha_stateyr
draws.state.yr <- select(draws, starts_with("mu_ob")) 
mcmc_intervals(draws.state.yr) # messy, but shows variation

# get summary 
state.yr.int <- mcmc_intervals_data(draws.state.yr) %>%
  bind_cols(
    select(state.data.ml, state, year,
           state.year.txt,
           ln_ngdp, diff_vote_share,
           time_to_pelec, time_to_selec)
  )

# plot
ggplot(state.yr.int, aes(x = year, y = m,
                         color = factor(time_to_pelec))) +
  geom_point(position = "jitter")


# state-year level parameters
mcmc_intervals(draws, regex_pars = "lambda\\[[1-9]\\]") +
  scale_y_discrete(labels = colnames(process.data$G))

# competition
draws.gamma <- select(draws, starts_with("gamma"))
mcmc_intervals(draws.gamma) +
  scale_y_discrete(labels = colnames(process.data$H))

# country-year level parameters
draws.beta <- select(draws, starts_with("beta"))
mcmc_intervals(draws.beta) +
  scale_y_discrete(labels = colnames(process.data$X))


