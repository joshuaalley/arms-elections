# Joshua Alley
# Defense contract cycles analysis


# load exports and time-series it 
us.arms.year <- us.arms.sipri %>% 
                 group_by(year) %>%
                 summarize(
                   us_arms = sum(us_arms, na.rm = TRUE)
                 ) %>%
                 ungroup() %>%
                 mutate(
                   lag_us_arms = lag(us_arms)
                 )

# same with some key trade variables
us.trade.year <- us.trade.ally %>%
                  group_by(year) %>%
                  summarize(
                    GDP_o = max(GDP_o, na.rm = TRUE),
                    change_gdp_o = max(GDP_o, na.rm = TRUE),
                    total_exports = sum(ln_exports, na.rm = TRUE)
                  ) %>%
                  ungroup() %>%
                  mutate(
                    lag_total_exports = lag(total_exports)
                  )
us.trade.year[us.trade.year == -Inf] <- NA

# load contracts data
contracts.data <- read.csv("data/contracts-data.csv") %>%
      filter(year < 2020) # some 2020 obs from 2019- cut


# annual by program
contracts.data.yr <- contracts.data %>%
  group_by(year) %>% 
  summarize(
    obligations = sum(fed.obligation, na.rm = TRUE)
  ) %>% 
  mutate( # obligations in billions
    obligations = obligations / 1000000000
  )

# plot
ggplot(contracts.data.yr, aes(x = year, y = obligations)) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  xlim(2000, 2020) +
  geom_line()

# annual by program
contracts.data.pyr <- contracts.data %>%
                      group_by(year, program) %>% 
                      summarize(
                        obligations = sum(fed.obligation, na.rm = TRUE)
                      ) %>% 
                      mutate( # obligations in billions
                        obligations = obligations / 1000000000
                      )

# plot
ggplot(contracts.data.pyr, aes(x = year, y = obligations)) +
  facet_wrap(~ program, scales = "free_y") +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  xlim(2000, 2020) +
  geom_line()

# pivot wider
contracts.data.pyr$program[contracts.data.pyr$program == ""] <- "Unknown"
contracts.data.wide <- contracts.data.pyr %>%
                        pivot_wider(id_cols = "year",
                                    names_from = "program",
                                    values_from = "obligations")
colnames(contracts.data.wide) <- c("year", "unknown", "air_engines", "airframes",
                                   "all_others", "ammunition", "building",
                                   "combat_vehicles", "construct", "construct_equip",
                                   "electronics", "materials_equip", "medical",
                                   "missile_space", "noncom_vehicles", "other_air",
                                   "other_fuel", "petrol", "photo", "production",
                                   "containers_handling", "services", "ships",
                                   "subsistence", "textiles", "railway", "weapons")

# create summary categories and add other info
contracts.data.clean <- contracts.data.wide %>%
                         rowwise() %>%
                         mutate(
                           all_contracts = sum(c_across(unknown:weapons))
                         ) %>%
                         ungroup() %>%
                         left_join(us.arms.year) %>%
                         left_join(elections.data) %>%
                         left_join(us.trade.year) %>%
                         mutate(
                          air = air_engines + airframes + other_air,
                          lag_air = lag(air),
                          vehicles = combat_vehicles + noncom_vehicles,
                          lag_vehicles = lag(vehicles),
                          weapons_ammo = ammunition + weapons + missile_space,
                          lag_weapons_ammo = lag(weapons_ammo),
                          transport = railway + other_fuel + petrol,
                          lag_transport = lag(transport),
                          arms_cont = air + ships + vehicles + weapons_ammo,
                          lag_arms_cont = lag(arms_cont),
                          non_arms = all_contracts - air - ships - vehicles - weapons_ammo,
                          lag_non_arms = lag(non_arms),
                          lag_ships = lag(ships),
                          lag_all_contracts = lag(all_contracts),
                          change_all_contracts = all_contracts - lag_all_contracts
                          ) 

# super coarse regression of arms: lag contracts is positive 
arms.ts.reg <- lm(us_arms ~ lag_us_arms +
                         lag_all_contracts,
                       data = contracts.data.clean)
summary(arms.ts.reg)



# super coarse regression of arms: lag contracts is positive 
exports.ts.reg <- lm(total_exports ~ lag_total_exports +
                    lag_all_contracts*time_to_elec,
                  data = contracts.data.clean)
summary(exports.ts.reg)

# elections and contracts
contracts.ts.reg <- lm(all_contracts ~ lag_all_contracts +
                         time_to_elec,
                       data = contracts.data.clean)
summary(contracts.ts.reg)

# plot over time
ggplot(contracts.data.clean, aes(x =  year,
                                 y = all_contracts)) +
  geom_line() +
  labs(y = "Total Defense Contracts",
       x = "Year",
       title = "Defense Contracting over time")

# summarize by election proximity
ggplot(contracts.data.clean, aes(x =  factor(time_to_elec,
                                             ordered = TRUE,
                                             levels = c("3", "2",
                                                        "1", "0")),
                                 y = all_contracts)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = "Total Defense Contracts",
       x = "Years to Presidential Election",
       title = "Defense Contracting Levels by Election Proximity",
       subtitle = "2000-2019")


# cycles by type of contract
contracts.data.key <- select(contracts.data.clean,
                             year, time_to_elec,
                             air, vehicles, weapons_ammo,
                             non_arms, ships) %>% 
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

ggplot(contracts.data.key, aes(x =  factor(time_to_elec,
                                           ordered = TRUE,
                                           levels = c("3", "2",
                                                      "1", "0")),
                               y = value)) +
  facet_wrap(~ allocation, scales = "free_y") +
  geom_boxplot(outlier.shape = NA) +
  labs(y = "Total Contract Value",
       x = "Years to Presidential Election",
       title = "Defense Contracting Levels by Election Proximity",
       subtitle = "2000-2019")


# add allocations to prior panel models
us.trade.contract <- left_join(us.trade.ally, select(contracts.data.clean,
                                                    year, lag_all_contracts,
                                                    lag_arms_cont, lag_non_arms,
                                                    air, vehicles, weapons_ammo,
                                                    non_arms, ships))


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
                                               year, lag_all_contracts,
                                               lag_arms_cont, lag_non_arms,
                                               air, vehicles, weapons_ammo,
                                               non_arms, ships))


ally.arms.contract <- rlm(us_arms ~ lag_us_arms +
                               lag_arms_cont*time_to_elec +
                               lag_non_arms*time_to_elec +
                               rep_pres +
                               xm_qudsest2 +  cowmidongoing + dyadigos +
                               change_gdp_o + change_gdp_d + Distw + eu_member +
                               Comlang + pred_nz_arms,
                             data = filter(arms.ex.cont, nz_us_arms == 1 &
                                             atop_defense == 1))
summary(ally.arms.contract)


# model arns  to non-allies 
nonally.arms.contract <- rlm(us_arms ~ lag_us_arms + 
                                  lag_arms_cont*time_to_elec +
                                  lag_non_arms*time_to_elec +
                                  rep_pres +
                                  xm_qudsest2 +  cowmidongoing + dyadigos +
                                  change_gdp_o + change_gdp_d + Distw + eu_member +
                                  Comlang + pred_nz_arms,
                                data = filter(arms.ex.cont, nz_us_arms == 1 &
                                                atop_defense == 0))
summary(nonally.arms.contract)
