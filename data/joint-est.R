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
    lag_deals_rs = arm::rescale(lag_deals),
    obligations_rs = obligations / (sum(obligations, na.rm = T)),
    lag_obligations_rs = lag(obligations_rs)
  ) %>%
  filter(state != "District of Columbia")

summary(state.data.deals$obligations_rs)

# look at state-level contracts
ggplot(state.data.deals, aes(x = year, y = obligations,
                             group = state)) +
  geom_line()

ggplot(state.data.deals, aes(x = year, y = obligations)) +
  facet_wrap(~ state, nrow = 10, scales = "free_y") +
  geom_line() +
  labs(
    x = "Year",
    y = "Defense Contracts",
    title = "State Defense Contracts: 2001-2020"
  )
ggsave("appendix/state-dynamics.png", height = 10, width = 12)


# outcome plot
ggplot(state.data.deals, aes(x = ln_obligations)) + geom_histogram()
ggplot(state.data.deals, aes(x = obligations)) + geom_histogram()
ggplot(state.data.deals, aes(x = obligations_rs)) + geom_histogram()
ggplot(state.data.deals, aes(x = change_obligations)) + geom_histogram()
ggplot(state.data.deals, aes(x = change_ln_obligations)) + geom_histogram()


# no dynamics- really struggles when added
# ordbeta reg for transformed outcomes

deals.state <- ordbetareg(obligations_rs ~
                      # LDV and intercept uncorrelated 
                        (1 + lag_obligations_rs || state) +
                        deals*swing + core +
                        gwot + time_to_elec + 
                        rep_pres  +
                        poptotal + ln_ngdp,
                  true_bounds = c(0, 1),
                   data = state.data.deals,
                   cores = 4,
                   backend = "cmdstanr"
) 
summary(deals.state)
pp_check(deals.state)
plot_slopes(deals.state, conf_level = .90, variable = "deals", by = "swing")
plot_slopes(deals.state, conf_level = .90, variable = "swing", by = "deals")


margin.deals <- marginaleffects(deals.state,
                                conf_level = .90,
                                variables = "deals", by = "swing")
margin.deals

# back to outcome scale
# comparisons with post transformation
deals.est <- comparisons(deals.state,
               variables = "deals",
               by = "swing",
               conf_level = .90,
               transform_pre = "dydx",
               transform_post = 
                 function(x) x * sum(state.data.deals$obligations, na.rm = T))
deals.est

swing.est <- comparisons(deals.state,
                         variables = "swing",
                         by = "deals",
                         conf_level = .90,
                         transform_pre = "dydx",
                         transform_post = 
                           function(x) x * sum(state.data.deals$obligations, na.rm = T))

# plot everything
slope.deals <- ggplot(deals.est, aes(x = factor(swing), y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1, linewidth = 2) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(`0` = "No", `1` = "Yes")) +
  labs(
    title = "Marginal Impact of Arms Deals",
    x = "Swing State",
    y = "Impact of Increasing Arms Deals"
  )
slope.deals

slope.swing <-  ggplot(swing.est, aes(x = deals, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .5) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Marginal Impact of Swing States",
    x = "Total Arms Deals",
    y = "Impact of Swing Status"
  )
slope.swing

pred.cont <- predictions(deals.state, conf.level = .9, 
                        newdata = datagrid(model = deals.state, 
                                           swing = c(0, 1),
                          deals = seq(from = min(state.data.deals$deals),
                                      to = max(state.data.deals$deals),
                                      by = 1),
                          state = "Wisconsin")
                        )
pred.cont.plot <- ggplot(pred.cont, aes(x = deals, y = estimate,
                      fill = factor(swing))) +
  geom_line(linewidth = 2) +
  scale_fill_grey(labels = c(`0` = "Not Swing", `1` = "Swing")) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .5) +
  labs(x = "Arms Deals",
       fill = "Electoral\nCompetition",
       y = "Predicted Defense Contracts",
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
coefs.joint <- coef(deals.state) 
coefs.joint[["state"]]



# coefficients
coef.joint <- get_estimates(deals.state)


# nice term labels
coef.joint$term <- str_remove(coef.joint$term, "b_")
coef.joint$var <- coef.names.map.state[coef.joint$term]
coef.joint$var <- factor(coef.joint$var, ordered = T,
                         levels = coef.names.map.state)

# plot it 
ggplot(drop_na(coef.joint, var), aes(y = fct_rev(var), x = estimate)) +
  #facet_wrap(~ group, scales = "free") +
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
ggsave("appendix/coef-joint-state.png", height = 6, width = 8)


# state ldv estimates
ldv.state <- avg_slopes(deals.state, 
                        variables = "lag_obligations_rs", 
                        type = "response",
                        by = "state") %>%
                select(
                  estimate, conf.low, conf.high, state,
                ) %>%
                rename(
                  state = state,
                  Estimate = estimate,
                  Q2.5 = conf.low,
                  Q97.5 = conf.high
                )
state.intercepts <- as.data.frame(coefs.joint$state[, , 1])
state.intercepts$state <- gsub("\\..*","", row.names(state.intercepts))
  

# summarize state intercepts and LDV estimates 
coefs.var.state <- bind_rows("Intercept" = state.intercepts,
                             "Lag Contracts" = ldv.state,
                             .id = "Variable") 

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



# robustness check- hurdle model

# use a lognormal hurdle instead 
deals.state.hurdle <- brm(bf(obligations ~ 
                               (1 + lag_obligations| state) +
                               deals*swing + 
                               gwot + time_to_elec + 
                               rep_pres  +
                               poptotal + ln_ngdp,
                             hu ~ ln_ngdp,
                             center = FALSE),
                          family = hurdle_lognormal(),
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
summary(deals.state.hurdle)
pp_check(comp.deals)



# check if negative on swing in main figure is down to GWOT- triple
# add state VI and LDV 
comp.deals.gwot <- brm(bf(obligations ~ 
                       (1 + lag_obligations | state) +
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



### rough calculation of how arms deals feed defense contracts ### 

# aggregate total deals by year, all else equal
# different kinds of alliances- from prediction 

pred.data.hyp <- pois.deals.est[[2]]
glimpse(pred.data.hyp)

pred.deals.elec <- pred.data.hyp %>%
                    group_by(time_to_elec) %>% 
                    summarize(
                      total_deals = sum(estimate),
                      total_low = sum(conf.low),
                      total_high = sum(conf.high)
                    )
# implies one more deal across four hypothetical dyads- 
# driven by allies with median polyarchy or less 
pred.deals.elec

# scale it for all states- 2001 on 
us.deals.comp.2000 <- us.deals.comp %>% 
                      filter(year >= 2001) %>%
                      ungroup() %>%
                        mutate(
                          cycle = case_when(year <= 2000 ~ 2000,
                                                     year <= 2004 ~ 2004,
                                                     year <= 2008 ~ 2008,
                                                     year <= 2012 ~ 2012,
                                                     year <= 2016 ~ 2016,
                                                     year > 2016 ~ 2020))
glimpse(us.deals.comp.2000)

# predictions from observed data
pred.deals.all <- predictions(pois.deals,
                              newdata = us.deals.comp.2000) %>%
  group_by(time_to_elec, cycle) %>% 
  summarize(
    year = mean(year), 
    deals = sum(estimate),
    deals_low = sum(conf.low),
    deals_high = sum(conf.high),
    .groups = "keep"
  )
pred.deals.all 

ggplot(pred.deals.all, aes(x = time_to_elec, y = deals,
                           color = factor(cycle))) +
  geom_line(linewidth = 1) +
  # geom_pointrange(aes(ymin = total_low,
  #                     ymax = total_high),
  #                 position = position_dodge(width = .5)) +
  scale_x_reverse()

# get data for states 
swing.data.pdeals <- state.data.deals %>%
                      filter(swing == 1 & year >= 2005) %>%
                      select(-deals) %>%
                      left_join(pred.deals.all)

# calculate predictions for observed swing states 
pred.state.sw <- predictions(comp.deals,
                              newdata = swing.data.pdeals)

ggplot(pred.state.sw, aes(x = time_to_elec, y = estimate,
                          color = factor(cycle))) +
  facet_wrap(~ state, scales = "free_y") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high)) +
  scale_x_reverse()

ggplot(pred.state.sw, aes(x = time_to_elec, y = estimate)) +
  facet_wrap(cycle ~ state, scales = "free_y") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high)) +
  scale_x_reverse()


# calculation for all swing states
pred.sw.agg <- pred.state.sw %>%
                 group_by(time_to_elec, cycle) %>%
                 summarize(
                   estimate = sum(estimate),
                   conf.low = sum(conf.low),
                   conf.high = sum(conf.high),
                   .groups = "keep"
                 )   

ggplot(pred.sw.agg, aes(x = time_to_elec, y = estimate)) +
  facet_wrap(~ cycle, scales = "free_y") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high)) +
  scale_x_reverse()

# then calculate for states that are permanent swing states
# grab coefs.var.state thetas and VIs 