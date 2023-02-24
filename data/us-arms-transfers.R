# Joshua Alley
# Analyze arms deals 


### model arms deals 

# total deals- summarize at country-year level and add covariates
us.deals <- us.arms.cat %>%
                group_by(ccode, year) %>%
                select(
                  country, ccode, year, deals
                ) %>%
                summarize(
                  deals = sum(deals, na.rm = TRUE),
                  .groups = "keep"
                ) %>%
                right_join(select(us.trade.ally,
                                               ccode, year,
                                               atop_defense, ally, ally_democ,
                                               cold_war, democ_bin,
                                               xm_qudsest2, cowmidongoing,
                                               dyadigos,
                                               rep_pres, time_to_elec, 
                                               eu_member, ln_gdp_d,
                                               ln_pop_d, ln_distw,
                                               change_gdp_d, Comlang,
                                               Contig, Evercol)) %>%
               filter(year >= 1950) %>%
               mutate(
                 nz_deals = ifelse(deals > 0, 1, 0)
               ) %>% # pakistan/east pak duplicate gives warning- drop
               distinct()
# NA from right join- move to zero
us.deals$deals[is.na(us.deals$deals)] <- 0
us.deals$nz_deals[is.na(us.deals$nz_deals)] <- 0
# complete cases
us.deals.comp <- drop_na(us.deals)

# poisson model of deals 
pois.deals <- brm(deals ~ #time_to_elec*ally_democ +
                    time_to_elec*ally*democ_bin +
                  cold_war + eu_member +
                   cowmidongoing + dyadigos +
                  rep_pres + 
                  ln_gdp_d + 
                  ln_pop_d + ln_distw + 
                    Comlang,
                  family = poisson(link = "log"),
                  backend = "cmdstanr",
                  cores = 4,
                  data = us.deals.comp)
summary(pois.deals)
plot_cme(pois.deals, variables = "time_to_elec", condition = "ally")
plot_cme(pois.deals, condition = "time_to_elec", variables = c("ally", "democ_bin")) +
  scale_x_reverse() # decreasing time to election

# poisson model predictions 
pois.deals.est <- me.us.elec(pois.deals, data = us.deals.comp)  

# nice labeller
democ.all.labs <- labeller(democ_bin = c(`1` = "Democracy", `0` = "Nondemocracy"),
                             ally = c(`1` = "US Ally", `0` = "Not Ally"))

pred.us.deals <- ggplot(pois.deals.est[[2]], aes(y = estimate, 
                         x = time_to_elec,
                         group = factor(ally),
                         color = factor(ally))) +
  facet_wrap(~ democ_bin, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
pred.us.deals
ggsave("figures/us-arms-plots.png", us.arms.plots, height = 6, width = 8)

# marginal effects w/ same style- less helpful
me.us.deals <- ggplot(pois.deals.est[[1]], 
                              aes(y = estimate, 
                                x = time_to_elec,
                                group = term,
                                color = term)) +
  facet_wrap(~ ally + democ_bin, labeller = democ.all.labs) +
  scale_x_reverse() + # decreasing time to election +
  geom_line(linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .4) +
  scale_color_grey("Defense Pact", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Marginal Impact of Alliance on Arms Deals",
       y = "Marginal Effect of Allaince",
       x = "Years to Presidential Election")
me.us.deals

# combine and export
grid.arrange(pred.us.deals, me.us.deals, nrow = 1)
us.arms.plots <- arrangeGrob(pred.us.deals, me.us.deals, nrow = 1)


# negative binomial deals
# poisson model of deals 
nb.deals <- brm(deals ~  time_to_elec*ally*democ_bin +
                    cold_war + 
                    eu_member +
                    cowmidongoing + dyadigos +
                    rep_pres + 
                    ln_gdp_d + 
                    ln_pop_d + ln_distw + 
                    Comlang,
                  family = negbinomial(link = "log",
                                       link_shape = "log"),
                 cores = 4,
                  prior = c(prior(normal(0, .5), class = "b")),
                  backend = "cmdstanr",
                  data = us.deals.comp)
summary(nb.deals)
plot_cme(nb.deals, variables = "time_to_elec", condition = "ally")
plot_cme(nb.deals, condition = "time_to_elec", variables = c("ally", "democ_bin"))



# Negative binomial model predictions 
nb.deals.est <- me.us.elec(nb.deals, data = us.deals.comp)  

pred.us.deals <- ggplot(nb.deals.est[[2]], aes(y = estimate, 
                                                 x = time_to_elec,
                                                 group = factor(ally),
                                                 color = factor(ally))) +
  facet_wrap(~ democ_bin) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
pred.us.deals

# marginal effects w/ same style
me.us.deals <- ggplot(nb.deals.est[[1]], 
                      aes(y = estimate, 
                          x = time_to_elec,
                          group = term,
                          color = term)) +
  facet_wrap(~ ally + democ_bin) +
  scale_x_reverse() + # decreasing time to election +
  geom_line(linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .4) +
  scale_color_grey("Defense Pact", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Marginal Impact of Alliance on Arms Deals",
       y = "Marginal Effect of Allaince",
       x = "Years to Presidential Election")
me.us.deals


# combine and export
modelplot(list(pois.deals, nb.deals),
          coef_map = coef.names.map)






### Arms transfers: SIRPI TIV
# log normal hurdle model
us.arms.comp <- select(us.trade.ally,
                       ccode, year,
                       us_arms, lag_us_arms, nz_us_arms,
                       change_us_arms,
                       time_to_elec, near_elec,
                       atop_defense, 
                       rep_pres, cold_war,
                       xm_qudsest2,  cowmidongoing, dyadigos,
                       GDP_o, GDP_d, Distw, eu_member,
                       change_gdp_o, change_gdp_d,
                       Comlang, Contig, Evercol) %>%
  mutate(
    election_defense = time_to_elec*atop_defense
  ) %>%
  drop_na() %>%
  ungroup()

us.arms.comp[, 10:ncol(us.arms.comp)-1] <- apply(us.arms.comp[, 10:ncol(us.arms.comp)-1],
                                                 2, function(x) 
                                                   arm::rescale(x, 
                                                                binary.inputs = "0/1"))

# time since arms transfer event
us.arms.comp <- us.arms.comp %>% 
  mutate(tmpG = cumsum(c(FALSE, as.logical(diff(nz_us_arms))))) %>%
  group_by(ccode) %>%
  mutate(tmp_a = c(0, diff(year)) * !nz_us_arms) %>%
  group_by(tmpG) %>%
  mutate(time_tr = cumsum(tmp_a)) %>%
  ungroup() %>%
  select(-c(tmp_a, tmpG))

us.arms.comp$time_tr <- parameters::demean(us.arms.comp, "time_tr", "ccode")$time_tr_within  

us.arms.comp <- us.arms.comp %>%
  mutate(
    time_tr2 =  time_tr^2,
    time_tr3 = time_tr^3
  )

# non-zero arms
us.arms.nz <- glm(nz_us_arms ~ 
                    atop_defense + cold_war +
                    rep_pres + eu_member +
                    xm_qudsest2 +  cowmidongoing + dyadigos +
                    GDP_o + GDP_d + Distw + 
                    Contig + Comlang + Evercol +
                    time_tr + time_tr2 + time_tr3,
                  data = us.arms.comp,
                  family = binomial(link = "logit"))
summary(us.arms.nz)

table(us.arms.comp$nz_us_arms)
table(us.arms.comp$cold_war)
table(us.arms.comp$nz_us_arms, us.arms.comp$cold_war)


# predicted prob of non-zero arms
us.arms.comp$pred_nz_arms <- predict(us.arms.nz, type = "response")
ggplot(us.arms.comp, aes(x = pred_nz_arms)) + geom_histogram()
ggplot(us.arms.comp, aes(x = pred_nz_arms,
                         group = factor(nz_us_arms),
                         fill = factor(nz_us_arms))) + geom_histogram()

# arms trade models
us.arms.ex <- lm(us_arms ~ lag_us_arms +
                   time_to_elec*atop_defense + 
                   rep_pres + cold_war +
                   xm_qudsest2 +  cowmidongoing + dyadigos +
                   GDP_o + GDP_d + Distw + eu_member +
                   Comlang + Contig + Evercol + pred_nz_arms,
                 data = filter(us.arms.comp, nz_us_arms == 1))
summary(us.arms.ex)

# changes in arms exports: gives odd results on alliance constituent term
us.arms.chex <- rlm(change_us_arms ~ 
                      time_to_elec*atop_defense + 
                      rep_pres + cold_war +
                      xm_qudsest2 +  cowmidongoing + dyadigos +
                      change_gdp_o + change_gdp_d + Distw + eu_member +
                      Comlang + Contig + Evercol +
                      pred_nz_arms,
                    data = filter(us.arms.comp, nz_us_arms == 1))
summary(us.arms.chex)

# ME and predicted values
us.arms.res <- me.us.elec(model = us.arms.ex,
                          data = filter(us.arms.comp, nz_us_arms == 1))


# results 
# tabulate logit and robust regression
modelplot(list("Non-Zero Arms Transfer: Logit" = us.arms.nz, 
               "Arms Transfers: Robust Reg" = us.arms.ex),
          coef_map =  coef.names.map)
modelsummary(list("Non-Zero Arms Transfer: Logit" = us.arms.nz, 
                  "Arms Transfers: OLS" = us.arms.ex),
             fmt = 2,
             coef_map =  coef.names.map,
             estimate = "{estimate}",
             statistic = "({conf.low}, {conf.high})",
             gof_omit = "^(?!Num)",
             output = "latex")

# combine predictions 
us.arms.pred <- bind_rows(
  "All Years" = us.arms.res[[2]],
  .id = "time"
)

# plot
pred.usarms <- ggplot(us.arms.res[[2]], aes(y = estimate, 
                                            x = time_to_elec,
                                            group = factor(atop_defense),
                                            color = factor(atop_defense))) +
  scale_x_reverse() + # decreasing time to election
  #geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("Defense Pact", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Exports",
       y = "Predicted Log Arms Exports",
       x = "Years to Presidential Election")
pred.usarms

# combine marginal effects  
us.arms.me <- bind_rows(
  "All Years" = us.arms.res[[1]],
  .id = "time"
)

# plot
me.usarms <- ggplot(us.arms.res[[1]], aes(y = estimate, 
                                          x = time_to_elec)) +
  scale_x_reverse() +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(
    ymin = conf.low,
    ymax = conf.high),
    position = position_dodge(width = .1)
  ) +
  labs(title = "Marginal Impact of Alliance on Arms Transfers",
       y = "Estimated Marginal Effect of Alliance",
       x = "Years to Presidential Election")
me.usarms

# combine and export
grid.arrange(pred.usarms, me.usarms, nrow = 2)
us.arms.plots <- arrangeGrob(pred.usarms, me.usarms, nrow = 2)
ggsave("figures/us-arms-plots.png", us.arms.plots, height = 6, width = 8)


# Cold War vs not
# arms trade models
us.arms.ex.cw <- rlm(us_arms ~ lag_us_arms +
                       time_to_elec*atop_defense + 
                       rep_pres +
                       xm_qudsest2 +  cowmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw + eu_member +
                       Comlang + Contig + Evercol + pred_nz_arms,
                     maxit = 40,
                     data = filter(us.arms.comp, cold_war == 1))
summary(us.arms.ex.cw)


# not cold war 
# arms trade models
us.arms.ex.ncw <- rlm(us_arms ~ lag_us_arms +
                        time_to_elec*atop_defense + 
                        rep_pres +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw + eu_member +
                        Comlang + Contig + Evercol + pred_nz_arms,
                      maxit = 40,
                      data = filter(us.arms.comp, cold_war == 0))
summary(us.arms.ex.ncw)


