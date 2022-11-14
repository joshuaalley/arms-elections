# Joshua Alley
# state exports in election years


### exports by state ### 

# model state exports
summary(state.exports.dyad$value)

# plot exports
ggplot(state.exports.dyad, aes(x = value)) + geom_histogram()
#ggplot(state.exports.dyad, aes(x = lamw_state_exports)) + geom_histogram()
ggplot(state.exports.dyad, aes(x = ln_state_exports)) + geom_histogram()
ggplot(state.exports.dyad, aes(x = ihs_state_exports)) + geom_histogram()
# long right tail- outliers and skewed
# no 0s- so no hurdle model here
sum(state.exports.dyad$value == 0)
# work in logs
quantile(state.exports.dyad$ihs_state_exports)
quantile(state.exports.dyad$ln_state_exports)




# complete data on key dimensions 
state.exports.dyad.comp <- select(state.exports.dyad,
                                  state, ccode, year, 
                                  value, lag_close_state, lag_diff_vote,
                                  #time_to_elec,
                                  pivot_prox, election,
                                  lag_election, lead_election,
                                  lag_poptotal, election, lead_election,
                                  atop_defense, lag_ln_ngdp,
                                  lag_ln_rgdpe, lag_pop, lag_xr, lag_csh_g) %>%
  filter(election == 1) %>%
  mutate(
    fin_crisis = ifelse(year == 2007 | year == 2008 |
                                year == 2009,
                              1, 0),
    reelection_bush = ifelse(year == 2004 |
                               year == 2003, 1, 0),
    reelection_obama = ifelse(year == 2012 |
                                year == 2011, 1, 0),
    reelection_trump = ifelse(year == 2020 |
                                year == 2019, 1, 0),
    dem_incumbent = ifelse(year == 2012 | year == 2016, 
                           1, 0),
    ln_state_exports = log(value),
    lag_ln_exports = lag(ln_state_exports),
    change_ln_exports = ln_state_exports - lag_ln_exports,
  ) %>%
  drop_na()

# create a presidential variable
state.exports.dyad.comp$president <- pres.full(state.exports.dyad.comp)



# rescale IVs by 2sd 
state.exports.dyad.comp[, 9:21] <- lapply(state.exports.dyad.comp[, 9:21],
                                          function(x)
                                            arm::rescale(x, binary.inputs = "0/1"))
# year dummies
state.exports.dyad.comp <- dummy_cols(state.exports.dyad.comp, select_columns = "year")


state.exports.dyad.comp$dyad.id <- group_indices(state.exports.dyad.comp, state, ccode) 
state.exports.dyad.comp$cntry.id <- group_indices(state.exports.dyad.comp, ccode) 
state.exports.dyad.comp$state.id <- group_indices(state.exports.dyad.comp, state) 



# outcome distribution
summary(state.exports.dyad.comp$value)
ggplot(state.exports.dyad.comp, aes(x = ln_state_exports)) + geom_histogram()


# simple weighted model:
lm.state.exports <- rlm(ln_state_exports ~ lag_ln_exports +
                         atop_defense*lag_diff_vote +
                       reelection_bush + reelection_obama + reelection_trump +
                         lag_poptotal + lag_ln_ngdp + fin_crisis +
                         lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                       data = state.exports.dyad.comp)
summary(lm.state.exports)



# pivot proximity instead: 
lm.state.exports.prox <- rlm(ln_state_exports ~ lag_ln_exports +
                              atop_defense*pivot_prox +
                              lag_poptotal + lag_ln_ngdp + fin_crisis +
                              reelection_bush + reelection_obama + reelection_trump +
                              lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                            data = state.exports.dyad.comp)
summary(lm.state.exports.prox)


# dyad robust se 
state.exports.dr <- dyadRobust(lm(ln_state_exports ~ lag_ln_exports +
                                    atop_defense*lag_diff_vote +
                                    reelection_bush + reelection_obama + reelection_trump +
                                    lag_poptotal + lag_ln_ngdp + fin_crisis +
                                    lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                                  weights = lm.state.exports$w,
                                  data = state.exports.dyad.comp),
                               dat = state.exports.dyad.comp,
                               dyadid = "dyad.id",
                               egoid = "state",
                               alterid = "ccode")

# dyad robust se
state.exports.prox.dr <- dyadRobust(lm(ln_state_exports ~ lag_ln_exports +
                                         atop_defense*pivot_prox +
                                         reelection_bush + reelection_obama + reelection_trump +
                                         lag_poptotal + lag_ln_ngdp + fin_crisis +
                                         lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                                       weights = lm.state.exports.prox$w,
                                       data = state.exports.dyad.comp),
                                    dat = state.exports.dyad.comp,
                                    dyadid = "dyad.id",
                                    egoid = "state",
                                    alterid = "ccode")

# clean up dr results
state.est <- bind_rows(
  dr.clean(state.exports.dr),
  dr.clean(state.exports.prox.dr)
) %>% # cut FE and intercept terms
  filter(str_detect(variable, "dyad.id", negate = T)) %>%
  filter(str_detect(variable, "Intercept", negate = T))

# nice names for plotting
coef.names.state = c("(Intercept)" = "Intercept",
                     "lag_ln_exports" = "Lag Ln(Exports)",
                     "atop_defense" = "Alliance",
                     "lag_diff_vote" = "Prior Election Vote Difference",
                     "pivot_prox" = "Pivot State Proximity",
                     "atop_defense:lag_diff_vote" = "Alliance x Vote Difference",
                     "atop_defense:pivot_prox" = "Alliance x Pivot Proximity",
                     "reelection_bush" = "Bush Reelection",
                     "reelection_obama" = "Obama Reelection",
                     "reelection_trump" = "Trump Reelection",
                     "lag_poptotal" = "Lag State Population", 
                     "lag_ln_ngdp" = "Lag Ln(State GDP)", 
                     "fin_crisis" = "Great Recession",
                     "lag_ln_rgdpe" = "Lag LN(Destination GDP)",
                     "lag_pop" = "Destination Population",
                     "lag_xr" = "Destination Exchange Rate",
                     "lag_csh_g" = "Destination Government Spending")
state.est$variable <- coef.names.state[state.est$variable]

# model names
model.names.state <- c("state.exports.dr" = "Vote Difference",
                       "state.exports.prox.dr" = "Pivot Proximity")
state.est$model <- model.names.state[state.est$model]



# plot results
ggplot(state.est, aes(y = factor(variable, ordered = T,
                                 levels = rev(coef.names.state)),
                      x = coef,
                      group = model,
                      color = model)) +
  geom_vline(xintercept = 0) +
  #xlim(-0.3, 0.3) +
  geom_pointrange(aes(
    xmin = coef - 1.96*se,
    xmax = coef + 1.96*se),
    position = position_dodge(width = 1)
  ) +
  scale_color_grey() +
  labs(x = "Estimate",
       y = "Term",
       color = "Model")


# plot ME 
# level of exports 
state.ex.vote <- marginaleffects(lm.state.exports,
                                 vcov = state.exports.dr$Vhat,
                                 variables = "atop_defense",
                                 newdata = typical(lag_diff_vote = 
                                                     c(seq(from = 0.0, to = .48, by = .01))))
# level of exports 
state.ex.prox <- marginaleffects(lm.state.exports.prox,
                                 vcov = state.exports.prox.dr$Vhat,
                                 variables = "atop_defense",
                                 newdata = typical(pivot_prox = 
                                                     c(seq(from = 0, to = 30, by = 1))))

# plot all three margins
plot.vdiff.ex  <- ggplot(state.ex.vote, 
                         aes(y = dydx, x = lag_diff_vote,
                             ymin = dydx - 1.96*std.error,
                             ymax = dydx + 1.96*std.error)) +       
  geom_line(size = 1) +
  geom_ribbon(alpha = .1) +
  labs(y = "Estimated Marginal Effect of Alliance",
       x = "Prior Election Vote Difference",
       title = "Vote Difference")
plot.vdiff.ex
# plot imports
plot.prox.ex  <- ggplot(state.ex.prox, 
                        aes(y = dydx, x = pivot_prox,
                            ymin = dydx - 1.96*std.error,
                            ymax = dydx + 1.96*std.error)) +
  #geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_ribbon(alpha = .1) +
  labs(y = "Estimated Marginal Effect of Alliance",
       x = "Proximity to Electoral College Pivot",
       title = "Pivot Proximity")
plot.prox.ex


# plot marginal effects
grid.arrange(plot.vdiff.ex, plot.prox.ex, nrow = 1)
me.all.state <- arrangeGrob(plot.vdiff.ex, plot.prox.ex, nrow = 1)




# brms model
bf.exports.state <- brmsformula(ln_state_exports ~ lag_ln_exports +
                                  lag_atop_defense*lag_diff_vote + 
                                  reelection_bush + reelection_obama +
                                  reelection_trump + 
                                  lag_poptotal + 
                                  lag_rgdpe + lag_pop + lag_xr + lag_csh_g +
                                  (1 | state) + (1 | ccode),
                                center = TRUE) +
  student()
exports.priors.state <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "sigma"),
  set_prior("normal(0, 1)", class = "sd")
) 

# fit the model
brm.state.exports <- brm(bf.exports.state, 
                         data = state.exports.dyad.comp,
                         prior = exports.priors.state,
                         iter = 2000, warmup = 1000,
                         chains = 4, cores = 4,
                         backend = "cmdstanr",
                         control = list(max_treedepth = 20))
summary(brm.state.exports, prob = .9)





