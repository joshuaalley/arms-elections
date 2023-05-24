# Joshua Alley
# Analyze arms deals 



### raw by country
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
                    v2x_polyarchy2, 
                    rep_pres, time_to_elec, 
                    eu_member, ln_rgdp,
                    ln_pop, ln_distw,
                    Comlang,
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



# time-series of deals
ggplot(filter(us.deals, ally == 1 & !is.na(democ_bin)), 
       aes(x = factor(time_to_elec,
                      ordered = TRUE,
                      levels = c("3", "2",
                                 "1", "0")),
           y = deals,
           color = factor(democ_bin))) +
  facet_wrap(~ ccode) +
  geom_boxplot(outlier.shape = NA) 


# line plots in latin america
# time-series of deals
ggplot(filter(us.deals, ally == 1 & !is.na(democ_bin) &
                ccode < 200), 
       aes(x = year,
           y = deals,
           color = factor(democ_bin))) +
  facet_wrap(~ ccode) +
  geom_point(aes(shape = factor(time_to_elec))) 

# line plots in Asia/ME
# time-series of deals
ggplot(filter(us.deals, ally == 1 & !is.na(democ_bin) &
                ccode > 600), 
       aes(x = year,
           y = deals,
           color = factor(democ_bin))) +
  facet_wrap(~ ccode) +
  geom_point(aes(shape = factor(time_to_elec))) 


### model arms deals 

# poisson model of deals 
pois.deals <- brm(deals ~ 
                    time_to_elec*ally*v2x_polyarchy2 +
                    cold_war + 
                    eu_member +
                    rep_pres + 
                    ln_rgdp + 
                    ln_pop + ln_distw + 
                    Comlang,
                  family = poisson(link = "log"),
                  backend = "cmdstanr",
                  prior = c(prior(normal(0, .5), class = "b")),
                  cores = 4,
                  data = us.deals.comp)
summary(pois.deals)
plot_cme(pois.deals, variables = "time_to_elec", condition = "ally")
plot_cme(pois.deals, condition = "time_to_elec", variables = c("ally", "v2x_polyarchy2")) +
  scale_x_reverse() # decreasing time to election

# poisson model predictions 
pois.deals.est <- me.us.elec(pois.deals, data = us.deals.comp)  

# nice labeller
democ.all.labs <- labeller(democ_bin = c(`1` = "Democracy", `0` = "Nondemocracy"),
                           v2x_polyarchy2 = c(`0.013` = "Minimum Democracy",
                                              `0.186` = "1st Quartile\nDemocracy",
                                              `0.39` = "Median Democracy",
                                              `0.745` = "3rd Quartile\nDemocracy",
                                              `0.924` = "Maximum Democracy"),
                           ally = c(`1` = "US Ally", `0` = "Not Ally"))

pred.us.deals <- ggplot(pois.deals.est[[2]], aes(y = estimate, 
                                                 x = time_to_elec,
                                                 group = factor(ally),
                                                 color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy2, labeller = democ.all.labs) + 
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
       x = "Years to Presidential Election") +
  theme(
    axis.text=element_text(size=11),
    axis.title=element_text(size=13),
    title = element_text(size = 15),
    legend.title = element_text(size = 13),
    strip.text = element_text(size = 9)
  )
pred.us.deals
ggsave("figures/us-arms-plots.png", height = 6, width = 8)


# test difference 
# democracy at minimum 
pois.comp.dmin <- pois.deals.est[[2]] %>%
                     filter(
                       ally == 1 & v2x_polyarchy2 == 0.013) 

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.dmin$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))


# democracy at 1st q  
pois.comp.d1q <- pois.deals.est[[2]] %>%
  filter(
    ally == 1 & v2x_polyarchy2 == 0.186) 

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.d1q$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))


# democracy at median  
pois.comp.dmed <- pois.deals.est[[2]] %>%
  filter(
    ally == 1 & v2x_polyarchy2 == 0.390) 

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.dmed$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))



# democracy at 3rd quartile
pois.comp.d3q <- pois.deals.est[[2]] %>%
  filter(
    ally == 1 & v2x_polyarchy2 == 0.745) 

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.d3q$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))
 



# marginal effects w/ same style- less helpful
# don't need a line and points- need a grid of democ/time to elec, 
# split by ally/not
ggplot(pois.deals.est[[1]], 
                              aes(x = time_to_elec, 
                                y = v2x_polyarchy2,
                                fill = estimate)) +
  #facet_wrap(~ ally, labeller = democ.all.labs) +
  scale_x_reverse() + # decreasing time to election +
  geom_tile() + 
  scale_fill_viridis_b() +
  labs(title = "Marginal Impact of Alliance on Arms Deals",
       y = "Partner Democracy",
       x = "Years to Presidential Election")


me.us.deals <- ggplot(pois.deals.est[[1]], aes(y = estimate, 
                                x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy2, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  labs(title = "Marginal Effect of Alliance",
       y = "Marginal Effect of Alliance",
       x = "Years to Presidential Election") +
  theme(
    axis.text=element_text(size=11),
    axis.title=element_text(size=13),
    title = element_text(size = 15),
    strip.text = element_text(size = 9)
  )
me.us.deals

# combine and export
grid.arrange(pred.us.deals, me.us.deals, nrow = 1)
us.arms.plots <- arrangeGrob(pred.us.deals, me.us.deals, nrow = 1)



# check poisson with OLS model of deals 
ols.deals <- brm(deals ~  
                  time_to_elec*ally*v2x_polyarchy2 +
                  cold_war + 
                  eu_member +
                  rep_pres + 
                  ln_rgdp + 
                  ln_pop + ln_distw + 
                  Comlang,
                  family = gaussian(link = "identity"),
                 cores = 4,
                  prior = c(prior(normal(0, .5), class = "b")),
                  backend = "cmdstanr",
                data = us.deals.comp)
summary(ols.deals)
plot_cme(ols.deals, variables = "time_to_elec", condition = "ally")
plot_cme(ols.deals, condition = "time_to_elec", variables = c("ally", "v2x_polyarchy2"))


# check estimates: predictions 
ols.deals.est <- me.us.elec(ols.deals, data = us.deals.comp)  


ggplot(ols.deals.est[[2]], aes(y = estimate, 
                              x = time_to_elec,
                             group = factor(ally),
                            color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy2, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals: OLS",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election") +
  theme(
    axis.text=element_text(size=11),
    axis.title=element_text(size=13),
    title = element_text(size = 15),
    legend.title = element_text(size = 13),
    strip.text = element_text(size = 9)
  )
ggsave("appendix/deals-pred-ols.png", height = 6, width = 8)


# test difference 
key.ols.draws <- as.data.frame(ols.deals.est[[4]][, pois.comp.dmed$rowid])
colnames(key.ols.draws) <- c("a", "b", "c", "d")
hypothesis(key.ols.draws, c("a > b", "b > c", "c > d"))


### Arms transfers: SIRPI TIV
# log normal hurdle model
us.arms.comp <- select(us.trade.ally,
                       ccode, year,
                       us_arms, lag_us_arms, nz_us_arms,
                       change_us_arms, v2x_polyarchy2, 
                       time_to_elec, near_elec,
                       atop_defense, ally, democ_bin, 
                       rep_pres, cold_war,
                      cowmidongoing, dyadigos,
                       ln_rgdp, ln_distw, eu_member,
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
                    ally + cold_war +
                    rep_pres + eu_member +
                    v2x_polyarchy2 +  cowmidongoing + dyadigos +
                    ln_rgdp + ln_distw + 
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
                   time_to_elec*ally*v2x_polyarchy2 + 
                   rep_pres + cold_war +
                   cowmidongoing + dyadigos +
                   ln_rgdp + ln_distw + eu_member +
                   Comlang + Contig + Evercol + pred_nz_arms,
                 data = filter(us.arms.comp, nz_us_arms == 1))
summary(us.arms.ex)

# changes in arms exports: gives odd results on alliance constituent term
us.arms.chex <- rlm(change_us_arms ~ 
                      time_to_elec*ally*v2x_polyarchy2 +
                      rep_pres + cold_war +
                      v2x_polyarchy2 +  cowmidongoing + dyadigos +
                      ln_rgdp + ln_distw + eu_member +
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
                                            group = factor(ally),
                                            color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy2, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Transfers",
       y = "Arms Transfers Value",
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
  facet_wrap(~ v2x_polyarchy2, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  labs(title = "Marginal Effect of Alliance on Arms Transfers",
       y = "Marginal Effect of Alliance",
       x = "Years to Presidential Election")
me.usarms

# combine and export
grid.arrange(pred.usarms, me.usarms, nrow = 2)
us.arms.plots <- arrangeGrob(pred.usarms, me.usarms, nrow = 2)


# Cold War vs not
# arms trade models
us.arms.ex.cw <- rlm(us_arms ~ lag_us_arms +
                       time_to_elec*ally*v2x_polyarchy2 + 
                       rep_pres +
                       cowmidongoing + dyadigos +
                       ln_rgdp + ln_distw + eu_member +
                       Comlang + Contig + Evercol + pred_nz_arms,
                     maxit = 40,
                     data = filter(us.arms.comp, cold_war == 1))
summary(us.arms.ex.cw)


# not cold war 
# arms trade models
us.arms.ex.ncw <- rlm(us_arms ~ lag_us_arms +
                        time_to_elec*ally*v2x_polyarchy2 + 
                        rep_pres + 
                        cowmidongoing + dyadigos +
                        ln_rgdp + ln_distw + eu_member +
                        Comlang + Contig + Evercol + pred_nz_arms,
                      maxit = 40,
                      data = filter(us.arms.comp, cold_war == 0))
summary(us.arms.ex.ncw)



# ME and predicted values
us.arms.res.ncw <- me.us.elec(model = us.arms.ex.ncw,
                          data = filter(us.arms.comp, cold_war == 0))

ggplot(us.arms.res.ncw[[2]], aes(y = estimate, 
                             x = time_to_elec,
                             group = factor(ally),
                             color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy2) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Transfers",
       y = "Arms Transfers Value",
       x = "Years to Presidential Election")
