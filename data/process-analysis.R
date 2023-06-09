# Joshua Alley
# generative model of process from contracts to exports via arms

# data largely from process-data-clean 
 

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

# competition
mcmc_intervals(fit.process$draws("rho"))

# country-year level parameters for arms
mcmc_intervals(fit.process$draws("beta")) +
  scale_y_discrete(labels = colnames(process.data$X))

# country-year level parameters for arms
mcmc_intervals(fit.process$draws("theta")) 

# state varying intercepts
mcmc_intervals(fit.process$draws("alpha_ob")) 
mcmc_intervals(fit.process$draws("alpha_state")) +
  scale_y_discrete(labels =  state.indices$state)
mcmc_intervals(fit.process$draws("sigma_st")) 
mcmc_intervals(fit.process$draws("sigma_ob")) 



### Why doesn't this have the same association as the deals model? 
# summing fitted values by year smooths out variance in total deals
# essentially an aggregation problem 

# id all years 
yr.idmat.all  <- left_join(
  select(us.deals.comp, ccode, year),
  select(state.data, year),
  multiple = "all") %>%
  distinct() %>%
  group_by(ccode, year) %>%
  summarise(present = dplyr::n(), 
            .groups = "drop") %>%
  mutate(
    obs = paste(ccode, year, sep = "-")
  ) %>%
  pivot_wider( # wider 
    id_cols = c("obs"),
    names_from = c("year"),
    values_from = "present"
  ) %>%
  select(-c(obs))
yr.idmat.all[is.na(yr.idmat.all)] <- 0


# grab predictions 
fit.deals <- posterior_epred(pois.deals)
fit.deals <- as.data.frame(t(fit.deals))

fit.deals$year <- us.deals.comp$year

# sample 10 draws
cols <- sample(seq(1, 4000, by = 1), size = 10, replace = FALSE)
cols
fit.deals.sam <- fit.deals[, cols]


# transform
fit.deals.yr <- t(as.matrix(yr.idmat.all)) %*% as.matrix(fit.deals.sam)
dim(fit.deals.yr)
fit.deals.yr <- as.data.frame(fit.deals.yr)
fit.deals.yr$year <- as.numeric(rownames(fit.deals.yr))
summary(fit.deals.yr)
summary(fit.deals.sam)


deals.year <- us.deals.comp %>% group_by(year) %>%
  summarize(
    total_deals = sum(deals, na.rm = TRUE)
  ) %>% 
  mutate(
    lag_total_deals = lag(total_deals),
    change_total_deals = total_deals - lag_total_deals
  ) 

ggplot(deals.year, aes(x = year, y = total_deals)) + geom_line()
ggplot(deals.year, aes(x = year, y = change_total_deals)) + geom_line()


# pivot long and plot
fit.deals.long <- fit.deals.yr %>%
  left_join(select(deals.year, year, total_deals)) %>%
  pivot_longer(-year) %>%
  left_join(elections.data)

ggplot(fit.deals.long, aes(x = year, y = value,
                           group = name)) + geom_line()

# test deals and time to elec
deals.year.elec <- left_join(select(deals.year, year, total_deals),
                             elections.data)
t.test(deals.year.elec$total_deals ~ deals.year.elec$election)


### brm-multiple 

# add predicted draws 
state.data.dpred <- lapply(1:10, function(x) state.data)
for(i in 1:10){
  state.data.dpred[[i]] <- state.data.dpred[[i]] %>% 
    left_join(
      select(fit.deals.yr, year, i)
    )
  colnames(state.data.dpred[[i]])[ncol(state.data.dpred[[i]])] <- "pred_deals"
  # rescale to help model
  state.data.dpred[[i]] <- state.data.dpred[[i]] %>%
    group_by(state) %>%
    mutate(
      # pred_deals = arm::rescale(pred_deals),
      lag_pred_deals = lag(pred_deals),
      change_pred_deals = pred_deals - lag_pred_deals,
    ) %>% 
    ungroup()
}



comp.dist.mult <- brm(bf(ln_obligations ~  
                           (1 + lag_ln_obligations | state) +
                           pred_deals*swing + time_to_elec + 
                           rep_pres  + gwot +
                           poptotal + ln_ngdp,
                         center = FALSE),
                      family = student(),
                      prior = c(
                        set_prior("normal(0, 2)", class = "b"),
                        set_prior("normal(0, 2)", class = "sd")
                      ),
                      data = state.data.dpred[[1]],
                      chains = 4,
                      cores = 4,
                      backend = "cmdstanr",
                      control = list(
                        adapt_delta = .99,
                        max_treedepth = 20)
) 
summary(comp.dist.mult)
plot_slopes(comp.dist.mult, variables = "pred_deals", by = "gwot")
plot_slopes(comp.dist.mult, by = "pred_deals", variables = "gwot")

# fit model 
# doesn't show individual model progress- ignore chain 1 warning at start
comp.dist.mult <- brm_multiple(bf(ln_obligations ~ 
                                    (1 + lag_ln_obligations | state) +
                                    pred_deals*swing + gwot +
                                    rep_pres  + time_to_elec +
                                    poptotal + ln_ngdp),
                               family = student(),
                               prior = c(
                                 set_prior("normal(0, 2)", class = "b"),
                                 set_prior("normal(0, 2)", class = "sd")
                               ),
                               data = state.data.dpred,
                               chains = 4,
                               cores = 4,
                               backend = "cmdstanr",
                               control = list(
                                 adapt_delta = .99,
                                 max_treedepth = 20)
) 
summary(comp.dist.mult)
