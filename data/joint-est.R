# Joshua Alley
# joint model with arms deals and contracts


### state data with arms deals
state.data.deals <- left_join(state.data, 
                              select(arms.deals.year,
                                     year, deals)) %>%
  group_by(state) %>% 
  mutate(
    lag_deals = lag(deals),
    change_deals = deals - lag_deals,
    deals_rs = arm::rescale(deals),
    lag_deals_rs = arm::rescale(lag_deals)
  ) %>%
  filter(state != "District of Columbia")


# look at state-level contracts
ggplot(state.data.deals, aes(x = year, y = ln_obligations,
                             group = state)) +
  geom_line()

ggplot(state.data.deals, aes(x = year, y = ln_obligations)) +
  facet_wrap(~ state, nrow = 10) +
  geom_line() +
  labs(
    x = "Year",
    y = "Log Defense Contracts",
    title = "State Defense Contracts: 2001-2020"
  )
ggsave("appendix/state-dynamics.png", height = 8, width = 10)


# no dynamics
deals.state <- brm(bf(ln_obligations ~ 
                        deals*swing +
                        gwot + time_to_elec + 
                        rep_pres  +
                        poptotal + ln_ngdp,
                      center = FALSE),
                   family = student(),
                   prior = c(
                     set_prior("normal(0, 2)", class = "b")
                   ),
                   data = state.data.deals,
                   cores = 4,
                   backend = "cmdstanr",
                   control = list(
                     adapt_delta = .99,
                     max_treedepth = 20)
) 
summary(deals.state)
plot_slopes(deals.state, variable = "deals", by = "swing")
plot_slopes(deals.state, variable = "swing", by = "deals")

# add state VI and LDV 
comp.deals <- brm(bf(ln_obligations ~ 
                            (1 + lag_ln_obligations | state) +
                            deals*swing +
                            gwot + time_to_elec + 
                            rep_pres  +
                            poptotal + ln_ngdp,
                          center = FALSE),
                       family = student(),
                       prior = c(
                         set_prior("normal(0, 2)", class = "b"),
                         set_prior("normal(0, 2)", class = "sd")
                       ),
                       data = state.data.deals,
                       cores = 4,
                       backend = "cmdstanr",
                       control = list(
                         adapt_delta = .99,
                         max_treedepth = 20)
) 
summary(comp.deals)
slope.deals <- plot_slopes(comp.deals, variable = "deals", by = "swing") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(`0` = "No", `1` = "Yes")) +
  labs(
    title = "Marginal Impact of Arms Deals",
    x = "Swing State",
    y = "Impact of Increasing Arms Deals"
  )
slope.deals

slope.swing <- plot_slopes(comp.deals, variable = "swing", by = "deals") +
  geom_hline(yintercept = 0) +
  labs(
    title = "Marginal Impact of Swing States",
    x = "Total Arms Deals",
    y = "Impact of Swing Status"
  )
slope.swing

pred.cont <- predictions(comp.deals, 
                        newdata = datagrid(model = comp.deals, 
                                           swing = c(0, 1),
                          deals = seq(from = min(state.data.deals$deals),
                                      to = max(state.data.deals$deals),
                                      by = 1),
                          state = "Wisconsin")
                        )
pred.cont.plot <- ggplot(pred.cont, aes(x = deals, y = estimate,
                      fill = factor(swing))) +
  geom_line() +
  scale_fill_grey(labels = c(`0` = "Not Swing", `1` = "Swing")) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .5) +
  labs(x = "Arms Deals",
       fill = "Electoral\nCompetition",
       y = "Predicted Log Defense Contracts",
       title = "Predicted Contracts")
pred.cont.plot

# combine it all 
grid.arrange(slope.deals, slope.swing,
             pred.cont.plot,
             layout_matrix = rbind(c(1, 2),
                                   c(3, 3)))
plot.state.inter <- arrangeGrob(slope.deals, slope.swing,
                                 pred.cont.plot,
                                 layout_matrix = rbind(c(1, 2),
                                                       c(3, 3)))
ggsave("figures/deals-swing-me.png", plot.state.inter,
       height = 8, width = 10)



# state varying coefs
coefs.joint <- coef(comp.deals) 
coefs.joint[["state"]]



# estimates from the interaction 
coef.joint <- get_estimates(comp.deals)


# nice term labels
coef.joint$term <- str_remove(coef.joint$term, "b_")
coef.joint$var <- coef.names.map.state[coef.joint$term]
coef.joint$var <- factor(coef.joint$var, ordered = T,
                         levels = coef.names.map.state)

# plot it 
ggplot(drop_na(coef.joint, var), aes(y = fct_rev(var), x = estimate)) +
  facet_wrap(~ group, scales = "free") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low,
                      xmax = conf.high),
                  linewidth = 1.5,
                  size = .75) +
    labs(
    x = "Estimate and 95% Credible Interval",
    y = "Variable",
    title = "Arms Deals and Defense Contracting",
    subtitle = "2001-2020"
  ) +
  theme(
    axis.text=element_text(size=11),
    axis.title=element_text(size=13),
    title = element_text(size = 15),
    strip.text = element_text(size = 9)
  )
ggsave("figures/coef-joint-state.png", height = 6, width = 8)


# summarize state intercepts and LDV estimates 
coefs.var.state <- bind_rows("Intercept" = as.data.frame(coefs.joint$state[, , 1]),
                             "Lag Contracts" = as.data.frame(coefs.joint$state[, , 2]),
                             .id = "Variable") 
coefs.var.state$state <- gsub("\\..*","", row.names(coefs.var.state))
# order for plotting 
coefs.var.state <- coefs.var.state %>%
  group_by(Variable) %>%
  arrange(Estimate, .by_group = TRUE) 
coefs.var.state$state <- factor(coefs.var.state$state, ordered = TRUE,
                                levels = coefs.var.state$state[1:50])

ggplot(coefs.var.state, aes(y = state, x = Estimate)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5)) +
  labs(
    y = "State",
    title = "State Varying Intercepts and Temporal Autocorrelation"
  )
ggsave("appendix/state-pars.png", height = 6, width = 8)



# check if negative on swing in main figure is down to GWOT- triple
# add state VI and LDV 
comp.deals.gwot <- brm(bf(ln_obligations ~ 
                       (1 + lag_ln_obligations | state) +
                       deals*swing*gwot +
                       time_to_elec + 
                       rep_pres  +
                       poptotal + ln_ngdp,
                     center = FALSE),
                  family = student(),
                  prior = c(
                    set_prior("normal(0, 2)", class = "b"),
                    set_prior("normal(0, 2)", class = "sd")
                  ),
                  data = state.data.deals,
                  cores = 4,
                  backend = "cmdstanr",
                  control = list(
                    adapt_delta = .99,
                    max_treedepth = 20)
) 
summary(comp.deals.gwot)
slope.deals.gwot <- plot_slopes(comp.deals.gwot, variable = "deals", 
                                by = c("swing", "gwot")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(`0` = "No", `1` = "Yes")) +
  labs(
    title = "Marginal Impact of Arms Deals",
    x = "Swing State",
    y = "Impact of Increasing Arms Deals"
  )
slope.deals.gwot

slope.swing.gwot <- plot_slopes(comp.deals.gwot, variable = "swing", 
                           by = c("deals", "gwot")) +
  geom_hline(yintercept = 0) +
  scale_color_grey(guide = "none") +
  scale_fill_grey(labels = c(`0` = "No", `1` = "Yes")) +
  labs(
    title = "Marginal Impact of Swing States\nand Global War on Terror",
    x = "Total Arms Deals",
    fill = "Global War\nOn Terror", 
    y = "Impact of Swing Status"
  ) 
slope.swing.gwot
ggsave("appendix/swing-gwot.png", height = 6, width = 8)
