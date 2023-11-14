# Joshua Alley
# Raw data for contracts


# generate a list of swing states
swing.list <- select(state.data, state, swing, year) %>%
  filter(swing == 1) %>%
  group_by(state) %>%
  summarize(
    Start = min(year),
    End = max(year)
  ) %>%
  rename(State = state)


swing.list.tab <- datasummary_df(swing.list, fmt = 0,
                                 output = "flextable")
swing.list.tab
flextable::save_as_image(swing.list.tab, "appendix/swing-list.png")





### state data with arms deals
state.data.deals <- left_join(state.data, 
                              select(arms.deals.year,
                                     year, deals)) %>%
  group_by(state) %>% 
  filter(state != "District of Columbia")  %>% 
  mutate(
    lag_deals = lag(deals),
    change_deals = deals - lag_deals,
    deals_rs = arm::rescale(deals),
    lag_deals_rs = arm::rescale(lag_deals)
  ) %>%
  group_by(year) %>%
  mutate(   
    sum_ob = sum(obligations, na.rm = TRUE),
    obligations_rs = obligations / sum_ob
  ) %>%
  group_by(state) %>%
  mutate(
    lag_obligations_rs = lag(obligations_rs)
  )

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



# summarize
swing.sum <- state.data.deals %>%
                   group_by(swing, time_to_elec) %>%
                   summarize(
                     obligations = median(obligations, na.rm = TRUE),
                     .groups = "keep"
                   )

ggplot(swing.sum, aes(x = time_to_elec,
                           y = obligations,
                           fill = factor(swing))) +
  scale_x_reverse() +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_grey(start = .5, end = .2,
                  labels = c(`0` = "No",
                              `1` = "Yes")) +
  labs(
    x = "Years to Election",
    y = "Median Contracts",
    fill = "Swing",
    title = "Median Contracts by Election Timing"
  )
ggsave("appendix/cont-swing-raw.png", height = 4, width = 6)


# summarize
swing.sum.deals <- state.data.deals %>%
  mutate(
    high.deals = ifelse(deals >= fivenum(deals)[2], "High Deals", 
                        "Low Deals")
  ) %>%
  group_by(swing, time_to_elec, high.deals) %>%
  summarize(
    obligations = median(obligations, na.rm = TRUE),
    .groups = "keep"
  )

ggplot(swing.sum.deals, aes(x = time_to_elec,
                           y = obligations,
                           fill = factor(swing))) +
  facet_wrap(~ high.deals) +
  scale_x_reverse() +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_grey(start = .5, end = .2,
                  labels = c(`0` = "No",
                             `1` = "Yes")) +
  labs(
    x = "Years to Election",
    y = "Median Contracts",
    fill = "Swing",
    title = "Median Contracts, Deals, and Election Timing"
  )



# summary table:
sum.data.cont <- select(ungroup(state.data.deals),
                        obligations, lag_obligations,
                          deals, swing, core,
                          gwot, time_to_elec,
                          rep_pres,
                          poptotal, ln_ngdp)
colnames(sum.data.cont) <- coef.names.map.state[colnames(sum.data.cont)]
datasummary_skim(data = sum.data.cont,
                 output = "latex",
                 histogram = FALSE)
