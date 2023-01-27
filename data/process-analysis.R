# Joshua Alley
# generative model of process from contracts to exports via arms

# data largely from contracts-analysis.R 
 

# model, data, and fit
# compile stan code
process.mod <- cmdstan_model(stan_file = "data/ml-model-process.stan",
                           cpp_options = list(stan_threads = TRUE))

# state-year data
state.yr.proc <- select(as.data.frame(state.yr.final), -ln_obligations) %>% 
                   mutate(
                     vote_pelec = diff_vote_share * time_to_pelec
                   )

# data 
process.data <- list(
  N = nrow(us.arms.deals),
  y_arms = us.arms.deals$deals,
  X = us.arms.deals[, 4:(ncol(us.arms.deals) - 2)],
  K = ncol(us.arms.deals[, 4:(ncol(us.arms.deals) - 2)]),
  cntry = us.arms.deals$cntry.index,
  C = max(us.arms.deals$cntry.index),
  S = nrow(state.data.ml),
  y_ob = state.data.ml$ln_obligations,
  Z = as.matrix(state.yr.idmat),
  T = max(state.data.ml$year.id),
  G = state.yr.proc,
  L = ncol(state.yr.proc)
)

# fit model 
fit.process <- process.mod$sample(
  data = process.data,
  chains = 4, 
  parallel_chains = 4,
  threads_per_chain = 1,
  seed = 12,
  max_treedepth = 15,
  refresh = 200
)

# save fit model
fit.process$save_object(file = "data/ml-model-process-fit.RDS")
