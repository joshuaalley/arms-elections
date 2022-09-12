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


# summarize by election proximity
contracts.elec.agg <- ggplot(contracts.data.clean, aes(x =  factor(time_to_elec,
                                             ordered = TRUE,
                                             levels = c("3", "2",
                                                        "1", "0")),
                                 y = all_contracts)) +
                      geom_boxplot(outlier.shape = NA) +
                     labs(y = "Total Prime Contracts",
                        x = "Years to Presidential Election",
                        title = "Aggregate Defense Contracting")
contracts.elec.agg

# cycles by type of contract
contracts.data.key <- select(contracts.data.clean,
                             year, time_to_elec, missile_space,
                             aircraft, vehicles, arms,
                             electronics, ships) %>% 
                      pivot_longer(
                        -c(year, time_to_elec),
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

contracts.elec.sector <- ggplot(contracts.data.key, aes(x =  factor(time_to_elec,
                                           ordered = TRUE,
                                           levels = c("3", "2",
                                                      "1", "0")),
                               y = value)) +
                          facet_wrap(~ allocation, scales = "free_y",
                                     labeller = labeller(allocation = 
                                         c("aircraft" = "Aircraft",
                                         "missile_space" = "Missiles & Space",
                                         "electronics" = "Electronics",
                                         "ships" = "Ships",
                                         "vehicles" = "Vehicles",
                                         "arms"= "Weapons & Ammo"))
                                     ) +
                          geom_boxplot(outlier.shape = NA) +
                         labs(y = "Total Prime Contracts",
                              x = "Years to Presidential Election",
                              title = "Sectoral Defense Contracting")
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
                                                     lag_ships))


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


# model exports to allies 
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
# model this with ML in cmdstanr- can't write this in brms

# start with simple aggregates by state before translating to sectors
state.data.allcont <- select(state.data, ln_obligations, 
                             time_to_selec, time_to_pelec,
                             poptotal, ln_ngdp, state, year)


# orders: very simple model 
us.arms.deals <- us.arms.cat %>%
                  group_by(ccode, year) %>%
                  summarize(
                    deals = sum(deals, na.rm = TRUE),
                    .groups = "keep"
                  ) %>% 
                  right_join(select(us.trade.ally,
                          ccode, year,
                          atop_defense, cold_war,
                          xm_qudsest2, cowmidongoing, dyadigos,
                          change_gdp_o, change_gdp_d, 
                          Distw, eu_member)) 
us.arms.deals[, 4:ncol(us.arms.deals)] <- lapply(us.arms.deals[, 4:ncol(us.arms.deals)],
                                              function(x) arm::rescale(x,
                                                  binary.inputs = "0/1"))
# no deals are NA, make zero
us.arms.deals$deals[is.na(us.arms.deals$deals)] <- 0
us.arms.deals <- drop_na(us.arms.deals)
class(us.arms.deals) <- "data.frame"

# group indices 
us.arms.deals$cntry.index <- us.arms.deals %>% group_by(ccode) %>%
                              group_indices()


ggplot(us.arms.deals, aes(x = deals)) + geom_histogram()


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
               X = us.arms.deals[, 4:ncol(us.arms.deals) - 1],
               K = ncol(us.arms.deals[, 4:ncol(us.arms.deals) - 1]),
               cntry = us.arms.deals$cntry.index,
               C = max(us.arms.deals$cntry.index)
         )

# fit model 
fit.deals <- deals.mod$sample(
  data = deals.data,
  chains = 4, 
  parallel_chains = 4,
  threads_per_chain = 1,
  seed = 12,
  refresh = 200
)
# diagnose 
fit.deals$cmdstan_diagnose()




