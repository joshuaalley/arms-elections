# Joshua Alley
# Defense contract cycles analysis



# super coarse regression of arms: lag contracts is positive 
arms.ts.reg <- lm(us_arms ~ lag_us_arms +
                         lag_all_contracts,
                       data = contracts.data.clean)
summary(arms.ts.reg)


# elections and contracts: expected negative coef
contracts.ts.reg <- lm(all_contracts ~ lag_all_contracts +
                         time_to_elec,
                       data = contracts.data.clean)
summary(contracts.ts.reg)


# summarize by election proximity- look within cycles
contracts.elec.agg <- ggplot(contracts.data.clean, aes(x = time_to_elec,
                                 color = factor(elec.cycle),
                                 y = all_contracts)) +
                      geom_point() +
                      geom_line() +
                      scale_x_reverse() +
                      scale_color_manual(values = wes_palette("Darjeeling1")) +
                     labs(y = "Total Prime Contracts",
                        x = "Years to Presidential Election",
                        title = "Aggregate Defense Contracting",
                        color = "Election Cycle")
contracts.elec.agg

# cycles by type of contract
contracts.data.key <- select(contracts.data.clean,
                             year, time_to_elec, missile_space,
                             aircraft, vehicles, arms,
                             electronics, ships, elec.cycle) %>% 
                      pivot_longer(
                        -c(year, time_to_elec, elec.cycle),
                        names_to = "allocation",
                        values_to = "value"
                      )

# plot over time
ggplot(contracts.data.key, aes(x =  year,
                                 y = value,
                                 group = allocation,
                                 color = allocation)) +
  geom_line() +
  labs(y = "Total Defense Contracts",
       x = "Year",
       title = "Defense Contracting Allocations over time")

contracts.elec.sector <- ggplot(contracts.data.key, aes(x = time_to_elec,
                                                        color = factor(elec.cycle),
                                                        y = value)) +
                          geom_point() +
                          geom_line() +
                          scale_x_reverse() +
                          facet_wrap(~ allocation, scales = "free_y",
                                     labeller = labeller(allocation = 
                                         c("aircraft" = "Aircraft",
                                         "missile_space" = "Missiles & Space",
                                         "electronics" = "Electronics",
                                         "ships" = "Ships",
                                         "vehicles" = "Vehicles",
                                         "arms"= "Weapons & Ammo"))
                                     ) +
                         scale_color_manual(values = wes_palette("Darjeeling1")) +
                         labs(y = "Total Prime Contracts",
                              x = "Years to Presidential Election",
                              title = "Sectoral Defense Contracting",
                              color = "Election Cycle")
contracts.elec.sector

# combine sectoral and aggregate plots
grid.arrange(contracts.elec.agg, contracts.elec.sector,
             nrow = 1,
             layout_matrix = rbind(c(1, 1, 2, 2, 2),
                                   c(1, 1, 2, 2, 2),
                                   c(1, 1, 2, 2, 2)))
contract.cycles <- arrangeGrob(contracts.elec.agg, contracts.elec.sector,
                               nrow = 1,
                               layout_matrix = rbind(c(1, 1, 2, 2, 2),
                                                     c(1, 1, 2, 2, 2),
                                                     c(1, 1, 2, 2, 2)))
ggsave("figures/contract-cycles.png", contract.cycles, height = 6, width = 8)




# add contract allocations to prior panel models
us.trade.contract <- left_join(us.trade.ally, select(contracts.data.clean,
                                                     year, lag_all_contracts, non_arms, 
                                                     arms_cont,
                                                     lag_arms_cont, lag_non_arms,
                                                     aircraft, vehicles, missile_space,
                                                     arms, 
                                                     ships,
                                                     lag_missile_space,
                                                     lag_aircraft, lag_vehicles, 
                                                     lag_arms, 
                                                     lag_ships)) %>% 
                                        filter(year >= 2002) # clarify observations


# model exports to allies 
ally.exports.contract <- rlm(change_ln_exports ~ 
                           lag_arms_cont*time_to_elec +
                           lag_non_arms*time_to_elec +
                            rep_pres +
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           change_gdp_o + change_gdp_d + Distw + eu_member +
                            Comlang,
                          data = filter(us.trade.contract, atop_defense == 1))
summary(ally.exports.contract)


# model exports to allies- specific sectors
ally.exports.sector <- lm(change_ln_exports ~ 
                             aircraft +
                             vehicles +
                             arms +
                             ships +
                             missile_space +
                             non_arms +
                             lag_aircraft +
                             lag_vehicles +
                             lag_arms +
                             lag_ships +
                             lag_missile_space +
                             lag_non_arms +
                               xm_qudsest2 +  cowmidongoing + dyadigos +
                               change_gdp_d + Distw + eu_member +
                               Comlang,
                             data = filter(us.trade.contract, atop_defense == 1))
summary(ally.exports.sector)


# model exports to non-allies 
nonally.exports.contract <- rlm(change_ln_exports ~ 
                               lag_arms_cont*time_to_elec +
                               lag_non_arms*time_to_elec +
                               rep_pres +
                               xm_qudsest2 +  cowmidongoing + dyadigos +
                               change_gdp_o + change_gdp_d + Distw + eu_member +
                               Comlang,
                             data = filter(us.trade.contract, atop_defense == 0))
summary(nonally.exports.contract)



### Do contracts lead into more international orders?
# model in cmdstanr- can't write ML model in brms


# state component
state.data.ml <- select(state.data, state, year,
                        ln_obligations, 
                        time_to_selec, time_to_pelec,
                        ln_ngdp) %>% 
  distinct() %>% 
  group_by(state) %>%
  mutate( # lag obligations- one year to NA 
    lag_ln_obligations = lag(ln_obligations),
    state.year.txt = paste0(state, ".", year)
  ) %>% # remove missing for STAN
  drop_na() 
state.data.ml$state.year <- state.data.ml %>%
  group_by(state, year) %>%
  group_indices()
state.data.ml$year.id <- state.data.ml %>%
  group_by(year) %>%
  group_indices()

# clean up ordering
state.data.ml <- state.data.ml %>%
  select(state, year, state.year.txt, state.year, year.id,
         ln_obligations, lag_ln_obligations,
         everything()) %>%
  mutate(
    intercept = 1
  ) %>% 
  filter(year <= 2014)
class(state.data.ml) <- "data.frame"


# state data for analysis 
state.yr.final <- state.data.ml %>%
  select(intercept,
         ln_obligations, lag_ln_obligations,
         time_to_pelec, time_to_selec, ln_ngdp) 
# rescale by 2sd 
state.yr.final[, 2:ncol(state.yr.final)] <- apply(state.yr.final[, 2:ncol(state.yr.final)], 2,
       function(x) arm::rescale(x,
                                binary.inputs = "0/1"))
# matrix for stan
state.yr.final <- as.matrix(state.yr.final)




# orders data  
us.arms.deals <- us.arms.cat %>%
                  group_by(ccode, year) %>%
                  summarize(
                    deals = sum(deals, na.rm = TRUE),
                    .groups = "keep"
                  ) %>% 
                  right_join(select(us.trade.ally,
                          ccode, year,
                          atop_defense,
                          xm_qudsest2, cowmidongoing, dyadigos,
                          change_gdp_o, change_gdp_d, 
                          Distw, eu_member)) %>%
                 filter(year %in% state.data.ml$year)
# rescale for model input
us.arms.deals[, 4:ncol(us.arms.deals)] <- lapply(us.arms.deals[, 4:ncol(us.arms.deals)],
                                              function(x) arm::rescale(x,
                                                  binary.inputs = "0/1"))
# no deals are NA, make zero
us.arms.deals$deals[is.na(us.arms.deals$deals)] <- 0
# drop missing- cow and other key controls only through 2014
us.arms.deals <- drop_na(us.arms.deals)
class(us.arms.deals) <- "data.frame"

# group indices 
us.arms.deals$cntry.index <- us.arms.deals %>% group_by(ccode) %>%
                              group_indices()
us.arms.deals$year.id <- us.arms.deals %>% group_by(year) %>%
  group_indices()


ggplot(us.arms.deals, aes(x = deals)) + geom_histogram()


# create a matrix to index when state-year obs apply 
# (double-indexing failed)
state.yr.idmat <- left_join(
    select(us.arms.deals, ccode, year),
    select(state.data.ml, year, state.year.txt)
   ) %>%
  mutate(
    present = 1, # to fill dummies
  ) %>%
  pivot_wider( # wider 
    id_cols = c(ccode, year),
    names_from = "state.year.txt",
    values_from = "present"
  ) %>%
  select(-c(ccode, year))
state.yr.idmat[is.na(state.yr.idmat)] <- 0

# grab ZINB code from brms
# make_stancode(deals ~ atop_defense, 
#               family = zero_inflated_negbinomial(),
#                 data = us.arms.deals)


# compile stan code
deals.mod <- cmdstan_model(stan_file = "data/ml-model-deals.stan",
                           cpp_options = list(stan_threads = TRUE))


# data 
deals.data <- list(
               N = nrow(us.arms.deals),
               y = us.arms.deals$deals,
               X = us.arms.deals[, 4:ncol(us.arms.deals) - 2],
               K = ncol(us.arms.deals[, 4:ncol(us.arms.deals) - 2]),
               cntry = us.arms.deals$cntry.index,
               C = max(us.arms.deals$cntry.index),
               S = nrow(state.data.ml),
               Z = as.matrix(state.yr.idmat),
               T = max(state.data.ml$year.id),
               G = state.yr.final,
               L = ncol(state.yr.final)
         )

# fit model 
fit.deals <- deals.mod$sample(
  data = deals.data,
  chains = 4, 
  parallel_chains = 4,
  threads_per_chain = 1,
  seed = 12,
  max_treedepth = 20,
  refresh = 200
)
# diagnose 
fit.deals$cmdstan_diagnose()

diagnostics <- fit.deals$sampler_diagnostics(format = "cmdstanr_draws_format")
draws <- fit.deals$draws(format = "df")

ratios <- neff_ratio(fit.deals)
mcmc_neff(ratios, size = 2)

mcmc_nuts_treedepth(diagnostics)
mcmc_nuts_energy(diagnostics)

mcmc_acf(draws, pars = "alpha")
mcmc_acf(draws, pars = "alpha_cntry[55]")

mcmc_parcoord(fit.deals$draws("alpha_cntry"))


# posterior prediction
ppc_dens_overlay(y = us.arms.deals$deals,
                 yrep = as.matrix(select(draws, starts_with("y_pred")))[1:50, ])

ppc_hist(y = us.arms.deals$deals,
                 yrep = as.matrix(select(draws, starts_with("y_pred")))[1:5, ])





