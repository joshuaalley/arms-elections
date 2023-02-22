# Joshua Alley
# generative model of process from contracts to exports via arms

# data largely from contracts-analysis.R 
 

# model, data, and fit
# compile stan code
process.mod <- cmdstan_model(stan_file = "data/ml-model-process.stan",
                           cpp_options = list(stan_threads = TRUE))

# state-year data
state.yr.proc <- select(as.data.frame(state.yr.final), -ln_obligations) 
# %>%
#                    mutate(
#                      # pivot proximity- rescaled and
#                      pivot_prox = pivot_prox / 10,
#                      # this is top 10- pivot_prox is in absolute value
#                      #pivot_prox = ifelse(pivot_prox <= 5, 1, 0),
#                      # interacted with election timing
#                      #prox_pelec = pivot_prox * time_to_pelec
#                    )

# separate data for allies/not- easy first cut
us.arms.deals.all <- us.arms.deals %>%
                      filter(atop_defense == 1) %>%
                      select(-atop_defense)

state.yr.idmat.all <- left_join(
  select(us.arms.deals.all, ccode, year),
  select(state.data.ml, year, state.year.txt)
) %>%
  group_by(ccode, year, state.year.txt) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n == 1) %>% 
  mutate(
    present = 1, # to fill dummies
  ) %>%
  pivot_wider( # wider 
    id_cols = c(ccode, year),
    names_from = "state.year.txt",
    values_from = "present"
  ) %>%
  select(-c(ccode, year))
state.yr.idmat.all[is.na(state.yr.idmat.all)] <- 0

# data 
process.data <- list(
  N = nrow(us.arms.deals.all),
  y_arms = us.arms.deals.all$deals,
  X = us.arms.deals.all[, 5:(ncol(us.arms.deals.all) - 3)],
  K = ncol(us.arms.deals.all[, 5:(ncol(us.arms.deals.all) - 3)]),
  cntry = us.arms.deals.all$cntry.index,
  C = max(us.arms.deals.all$cntry.index),
  S = nrow(state.data.ml),
  y_ob = state.data.ml$ln_obligations,
  Z = as.matrix(state.yr.idmat.all),
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


#  diagnose 
fit.process$cmdstan_diagnose()

diagnostics <- fit.process$diagnostic_summary()
draws <- fit.process$draws(format = "df")

print(diagnostics)
# n/eff ratios and posterior ACFs 
ratios <- neff_ratio(fit.process)
mcmc_neff(ratios, size = 2)

mcmc_acf(draws, pars = "alpha")
mcmc_acf(draws, pars = "alpha_cntry[12]")

# parallel coordinates plot
mcmc_parcoord(fit.process$draws("lambda"))
mcmc_parcoord(fit.process$draws("alpha_cntry"))
mcmc_parcoord(fit.process$draws("mu_stateyr"))

# look at parameter estimates: start with alpha_stateyr
draws.state.yr <- select(draws, starts_with("mu_stateyr")) 
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

# country-year level parameters
mcmc_intervals(draws, regex_pars = "beta\\[[\\[1-9]\\]") +
  scale_y_discrete(labels = colnames(process.data$X))



# model, data, and fit
# compile stan code
process.mod.full <- cmdstan_model(stan_file = "data/ml-model-process-full.stan",
                             cpp_options = list(stan_threads = TRUE))

# data 
process.data.full <- list(
  N = nrow(us.arms.deals),
  y_arms = us.arms.deals$deals,
  y_trade = us.arms.deals$change_ln_exports,
  X = us.arms.deals[, 5:(ncol(us.arms.deals) - 2)],
  K = ncol(us.arms.deals[, 5:(ncol(us.arms.deals) - 2)]),
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
fit.process.full <- process.mod.full$sample(
  data = process.data.full,
  chains = 4, 
  parallel_chains = 4,
  threads_per_chain = 1,
  seed = 12,
  max_treedepth = 15,
  refresh = 200
)

# save fit model
fit.process.full$save_object(file = "data/ml-process-full-fit.RDS")

fit.process.full$cmdstan_diagnose()
