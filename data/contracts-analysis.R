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
                             air, vehicles, weapons_ammo,
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
                                         c("air" = "Aircraft",
                                         "missile_space" = "Missiles & Space",
                                         "electronics" = "Electronics",
                                         "ships" = "Ships",
                                         "vehicles" = "Vehicles",
                                         "weapons_ammo"= "Weapons & Ammo"))
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
                                                     air, vehicles, missile_space,
                                                     weapons_ammo, 
                                                     ships,
                                                     lag_missile_space,
                                                     lag_air, lag_vehicles, 
                                                     lag_weapons_ammo, 
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
                             air +
                             vehicles +
                             weapons_ammo +
                             ships +
                             missile_space +
                             non_arms +
                             lag_air +
                             lag_vehicles +
                             lag_weapons_ammo +
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





# model arms transfers to allies 
arms.ex.cont <- left_join(us.arms.comp, select(contracts.data.clean,
                                               year, lag_all_contracts, non_arms, 
                                               arms_cont,
                                               lag_arms_cont, lag_non_arms,
                                               air, vehicles, missile_space,
                                               weapons_ammo, 
                                               ships,
                                               lag_missile_space,
                                               lag_air, lag_vehicles, 
                                               lag_weapons_ammo, 
                                               lag_ships))


ally.arms.contract <- rlm(us_arms ~ lag_us_arms +
                           arms_cont +
                            lag_arms_cont + 
                            non_arms +
                            lag_non_arms +
                               rep_pres +
                               xm_qudsest2 +  cowmidongoing + dyadigos +
                               change_gdp_d + Distw + eu_member +
                               Comlang + pred_nz_arms,
                             data = filter(arms.ex.cont, nz_us_arms == 1 &
                                             atop_defense == 1))
summary(ally.arms.contract)


# specific sectors
ally.arms.sector <- rlm(us_arms ~ lag_us_arms +
                            air +
                            vehicles +
                            weapons_ammo +
                            ships +
                            missile_space +
                            non_arms +
                            lag_air +
                            lag_vehicles +
                            lag_weapons_ammo +
                            lag_ships +
                            lag_missile_space +
                            lag_non_arms +
                            xm_qudsest2 +  cowmidongoing + dyadigos +
                            change_gdp_d + Distw + eu_member +
                            Comlang + pred_nz_arms,
                          data = filter(arms.ex.cont, nz_us_arms == 1 &
                                          atop_defense == 1))
summary(ally.arms.sector)


# model arms  to non-allies 
nonally.arms.contract <- lm(us_arms ~ lag_us_arms + 
                                  lag_arms_cont +
                                  lag_non_arms +
                                  rep_pres +
                                  xm_qudsest2 +  cowmidongoing + dyadigos +
                                  change_gdp_o + change_gdp_d + Distw + eu_member +
                                  Comlang + pred_nz_arms,
                                data = filter(arms.ex.cont, nz_us_arms == 1 &
                                                atop_defense == 0))
summary(nonally.arms.contract)


# model arms  to non-allies 
nonally.arms.sector <- lm(us_arms ~ lag_us_arms + 
                            air +
                            vehicles +
                            weapons_ammo +
                            ships +
                            missile_space +
                            non_arms +
                            lag_air +
                            lag_vehicles +
                            lag_weapons_ammo +
                            lag_ships +
                            lag_missile_space +
                            lag_non_arms +
                              rep_pres +
                              xm_qudsest2 +  cowmidongoing + dyadigos +
                              change_gdp_o + change_gdp_d + Distw + eu_member +
                              Comlang + pred_nz_arms,
                            data = filter(arms.ex.cont, nz_us_arms == 1 &
                                            atop_defense == 0))
summary(nonally.arms.sector)





# load 1976 to 2003 data: market level
contracts.data.76 <- read_dta("data/carril-duggan-market_V1.dta") 

contracts.76.yr <- contracts.data.76 %>%
  group_by(actfy) %>%
  summarize(
    obligations = sum(mktdollars, na.rm = TRUE)
  ) %>% 
  mutate( # obligations in billions
    obligations = obligations / 1000000000
  ) %>%
  rename(
    year = actfy
  )


# plot
ggplot(contracts.76.yr, aes(x = year, y = obligations)) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  xlim(1976, 2004) +
  geom_line()   
