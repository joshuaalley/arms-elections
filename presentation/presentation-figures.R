# Joshua Alley
# presentation figures

library(ggcarly)

# first cycles figure
ggplot(us.trade.total, aes(x = factor(time_to_elec,
                                      ordered = TRUE,
                                      levels = c("3", "2",
                                                 "1", "0")), y = value)) +
  facet_wrap(~ trade,
             labeller = labeller(trade = c("total_exports_change" = "Exports",
                                           "total_imports_change" = "Imports",
                                           "total_trade_change" = "Total Trade"))) +
  geom_boxplot(outlier.shape = NA) +
  ylim(-10, 10) +
  labs(y = "Annual\n Trade\n Change",
       x = "Years to Presidential Election") +
  theme_carly_presents()
ggsave("presentation/us-trade-cycles.png", height = 6, width = 8)


# marginal effects of alliances
ggplot(us.elec.pred, aes(y = fit, 
                         x = time_to_elec,
                         group = factor(atop_defense),
                         color = factor(atop_defense))) +
  facet_wrap(~ outcome) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = lwr, ymax = upr),
                  position = position_dodge(width = .1)) +
  scale_color_grey("Defense Pact", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(y = "Predicted\n Outcome",
       x = "Years to Presidential Election") +
  theme_carly_presents()
ggsave("presentation/us-elec-pred.png", height = 8, width = 10)


# arms predictions
ggplot(us.arms.res[[2]], aes(y = fit, 
                             x = time_to_elec,
                             group = factor(atop_defense),
                             color = factor(atop_defense))) +
  scale_x_reverse() + # decreasing time to election
  #geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = lwr, ymax = upr),
                  position = position_dodge(width = .1)) +
  scale_color_grey("Defense Pact", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Predicted Arms Exports",
       y = "Log Arms\n Exports",
       x = "Years to Presidential Election") + 
  theme_carly_presents()
ggsave("presentation/us-arms-pred.png", height = 8, width = 10)



# total defense contracts
ggplot(contracts.data.clean, aes(x =  factor(time_to_elec,
                                             ordered = TRUE,
                                             levels = c("3", "2",
                                                        "1", "0")),
                                 y = all_contracts)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = "Total\n Prime\n Contracts",
       x = "Years to Presidential Election",
       title = "Aggregate Defense Contracting") +
  theme_carly_presents()
ggsave("presentation/contract-cycles.png", height = 8, width = 10)



# defense sectors: appendix
ggplot(contracts.data.key, aes(x =  factor(time_to_elec,
                                           ordered = TRUE,
                                           levels = c("3", "2",
                                                      "1", "0")),
                               y = value)) +
  facet_wrap(~ allocation, scales = "free_y",
             labeller = labeller(allocation = 
                                   c("air" = "Aircraft",
                                     "missile_space" = "Missiles & Space",
                                     "non_arms" = "Other",
                                     "ships" = "Ships",
                                     "vehicles" = "Vehicles",
                                     "weapons_ammo"= "Weapons & Ammo"))
  ) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = "Total\n Prime \n Contracts",
       x = "Years to Presidential Election",
       title = "Sectoral Defense Contracting") +
  theme_carly_presents()
ggsave("presentation/contract-cycles-sector.png", height = 8, width = 10)
