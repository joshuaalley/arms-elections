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
                    cold_war, gwot, democ_bin,
                    v2x_polyarchy, cowmidongoing,
                    rep_pres, time_to_elec, 
                    eu_member, ln_rgdp,
                    ln_pop, ln_distw,
                    Comlang,
                    Contig, Evercol)) %>%
  filter(year >= 1949) %>%
  group_by(ccode) %>%
  mutate(
    nz_deals = ifelse(deals > 0, 1, 0)
  ) %>% # pakistan/east pak duplicate gives warning- drop
  distinct() 
# NA from right join- move to zero
us.deals$deals[is.na(us.deals$deals)] <- 0
us.deals$nz_deals[is.na(us.deals$nz_deals)] <- 0
# complete cases
us.deals.comp <- drop_na(us.deals) %>%
                  group_by(ccode) %>%
                  mutate(
                    lag_deals = lag(deals),
                    change_deals = deals - lag(deals),
                  )

# check for labels
fivenum(us.deals.comp$v2x_polyarchy)

# nice labeller
democ.all.labs <- labeller(democ_bin = c(`1` = "Democracy", `0` = "Nondemocracy"),
                           v2x_polyarchy = c(`0.012` = "Minimum\nDemocracy",
                                             `0.176` = "1st Quartile\nDemocracy",
                                             `0.363` = "Median\nDemocracy",
                                             `0.731` = "3rd Quartile\nDemocracy",
                                             `0.926` = "Maximum\nDemocracy"),
                           ally = c(`1` = "US Ally", `0` = "Not Ally"))


# time-series of deals
ggplot(us.deals, 
       aes(x = year,
           y = deals,
           group = ccode)) +
  geom_line()


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


# aggregate summary 
poly.sum <- fivenum(us.deals$v2x_polyarchy)
poly.sum
us.deals.sum <- us.deals %>%
        ungroup() %>% # otherwise case_when is super slow 
        mutate(
          democ_grp = case_when(
            v2x_polyarchy <= poly.sum[2] ~ "1st Quartile",
            v2x_polyarchy > poly.sum[2] &
            v2x_polyarchy <= poly.sum[3] ~ "2nd Quartile",
            v2x_polyarchy > poly.sum[3] &
            v2x_polyarchy <= poly.sum[4] ~ "3rd Quartile",
            v2x_polyarchy > poly.sum[4] ~ "4th Quartile"
          )
        ) %>% 
        group_by(
          democ_grp, time_to_elec, atop_defense
        ) %>%
        summarize(
          deals = sum(deals, na.rm = TRUE),
          n = n(),
          deals.state = deals / n,
          .groups = "keep"
        ) %>%
        drop_na()
table(us.deals.sum$democ_grp)


ggplot(us.deals.sum, aes(x = time_to_elec,
                         y = deals.state,
                         color = factor(atop_defense))) +
  facet_wrap(~ democ_grp) +
  scale_x_reverse() +
  scale_color_grey(
  start = 0.7,
  end = 0.1,
   labels = c(`0` = "No", `1` = "Yes")) +
  geom_point() +
  geom_line() +
  labs(x = "Time to Election",
       y = "Deals per Country in Group",
       color = "Alliance")
ggsave("appendix/deals-democ-raw.png", height = 6, width = 8)



### model arms deals 

pois.deals.cycle <- brm(bf(deals ~ 
                            time_to_elec + ally
                          + v2x_polyarchy +
                            cold_war + gwot +
                            eu_member +
                            rep_pres + 
                            ln_rgdp + cowmidongoing +
                            ln_pop + ln_distw + 
                            Comlang,
                          hu ~ ally + v2x_polyarchy + cowmidongoing + ln_rgdp,
                          center = FALSE),
                       family = hurdle_poisson(),
                       backend = "cmdstanr",
                       prior = c(prior(normal(0, .5), class = "b")),
                       cores = 4,
                       refresh = 500,
                       data = us.deals.comp)
summary(pois.deals.cycle)

pois.cycle.pred <- predictions(pois.deals.cycle, conf_level = .9,
                              newdata = datagrid(model = pois.deals.ally,
                                                 time_to_elec = c(0, 1, 2, 3)))
ggplot(pois.cycle.pred, aes(y = estimate, 
                           x = time_to_elec)) +
  scale_x_reverse() + # decreasing time to election
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  labs(title = "Elections and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election") 
ggsave("appendix/elec-pred-deals.png", height = 6, width = 8)

# hurdle poisson model of deals: democracy and time to election
pois.deals.democ <- brm(bf(deals ~ 
                            time_to_elec*v2x_polyarchy +
                            ally +
                            cold_war + gwot +
                            eu_member +
                            rep_pres + 
                            ln_rgdp + cowmidongoing +
                            ln_pop + ln_distw + 
                            Comlang,
                          hu ~ ally + v2x_polyarchy + cowmidongoing + ln_rgdp,
                          center = FALSE),
                       family = hurdle_poisson(),
                       backend = "cmdstanr",
                       prior = c(prior(normal(0, .5), class = "b")),
                       cores = 4,
                       refresh = 500,
                       data = us.deals.comp)
summary(pois.deals.democ)

pois.democ.pred <- predictions(pois.deals.democ, conf_level = .9,
                              newdata = datagrid(model = pois.deals.democ,
                                                 time_to_elec = c(0, 1, 2, 3),
                                                 v2x_polyarchy = fivenum))

ggplot(pois.democ.pred, aes(y = estimate, 
                           x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs,
             ncol = 5) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  labs(title = "Elections, Democracy, and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("figures/democ-deals-pred.png", height = 6, width = 8)


# full fitted draws
# full fitted draws
fit.democ <- posterior_epred(pois.deals.democ, 
                           newdata = datagrid(model = pois.deals.democ,
                                              time_to_elec = c(0, 1, 2, 3),
                                              v2x_polyarchy = fivenum)) 

pois.comp.dmin <- pois.democ.pred  %>%
             filter(
      v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[1]) 

key.pois.draws <- as.data.frame(fit.democ[, pois.comp.dmin$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))

# hurdle poisson model of deals: ally and time to election
pois.deals.ally <- brm(bf(deals ~ 
                       time_to_elec*ally
                      + v2x_polyarchy +
                       cold_war + gwot +
                       eu_member +
                       rep_pres + 
                       ln_rgdp + cowmidongoing +
                       ln_pop + ln_distw + 
                       Comlang,
                     hu ~ ally + v2x_polyarchy + cowmidongoing + ln_rgdp,
                     center = FALSE),
                  family = hurdle_poisson(),
                  backend = "cmdstanr",
                  prior = c(prior(normal(0, .5), class = "b")),
                  cores = 4,
                  refresh = 500,
                  data = us.deals.comp)
summary(pois.deals.ally)

plot_slopes(pois.deals.ally, variables = c("ally"), 
            by = "time_to_elec",
            conf_level = .9)

plot_slopes(pois.deals.ally, by = c("ally"), 
            variables = "time_to_elec",
            conf_level = .9)

pois.ally.pred <- predictions(pois.deals.ally, conf_level = .9,
            newdata = datagrid(model = pois.deals.ally,
                               time_to_elec = c(0, 1, 2, 3),
                               ally = c(0, 1)))
ggplot(pois.ally.pred, aes(y = estimate, 
                                x = time_to_elec)) +
  facet_wrap(~ ally, scales = "free_y") +
  scale_x_reverse() + # decreasing time to election
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



# poisson model of deals 
pois.deals <- brm(bf(deals ~ 
                    time_to_elec*ally*v2x_polyarchy +
                    cold_war + gwot +
                    eu_member +
                    rep_pres + 
                    ln_rgdp + cowmidongoing +
                    ln_pop + ln_distw + 
                    Comlang,
                    hu ~ v2x_polyarchy + ally + cowmidongoing + ln_rgdp,
                    center = FALSE),
                  family = hurdle_poisson(),
                  backend = "cmdstanr",
                  prior = c(prior(normal(0, .5), class = "b")),
                  cores = 4,
                  refresh = 500,
                  data = us.deals.comp)
summary(pois.deals)

# posterior predictive check
pp_check(pois.deals, type = "rootogram", 
         style = "hanging") +
  labs(title = "Posterior Predictive Check: Arms Deals")
ggsave("appendix/pp-check-deals.png", height = 6, width = 8)


# poisson model predictions 
pois.deals.est <- me.us.elec(pois.deals, data = us.deals.comp)  


pred.us.deals <- ggplot(pois.deals.est[[2]], aes(y = estimate, 
                                x = time_to_elec,
                                group = factor(ally),
                                color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs,
             ncol = 5) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections, Democracy, Alliances and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election") 
pred.us.deals
ggsave("figures/us-arms-plots.png", height = 6, width = 8)



# test difference 
# democracy at minimum 
pois.comp.dmin <- pois.deals.est[[2]] %>%
                     filter(
                       ally == 1 & 
                      v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[1]) 

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.dmin$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))


# democracy at 1st q  
pois.comp.d1q <- pois.deals.est[[2]] %>%
  filter(
    ally == 1 & 
      v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[2])

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.d1q$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))


# democracy at median  
pois.comp.dmed <- pois.deals.est[[2]] %>%
  filter(
    ally == 1 & 
      v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[3]) 

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.dmed$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))



# democracy at 3rd quartile: this is where it stops
pois.comp.d3q <- pois.deals.est[[2]] %>%
  filter(
    ally == 1 & 
      v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[4])

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.d3q$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("a > b", "b > c", "c > d"))
hypothesis(key.pois.draws, c("a > d"))
 



# marginal effects w/ same style- less helpful
# don't need a line and points- need a grid of democ/time to elec, 
# split by ally/not
ggplot(pois.deals.est[[1]], 
                              aes(x = time_to_elec, 
                                y = v2x_polyarchy,
                                fill = estimate)) +
  scale_x_reverse() + # decreasing time to election +
  geom_tile() + 
  scale_fill_viridis_b() +
  labs(title = "Marginal Impact of Alliance on Arms Deals",
       y = "Partner Democracy",
       x = "Years to Presidential Election")


me.us.deals <- ggplot(pois.deals.est[[1]], aes(y = estimate, 
                                x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  labs(title = "Marginal Effect of Alliance",
       y = "Marginal Effect of Alliance",
       x = "Years to Presidential Election") 
me.us.deals

# combine and export
grid.arrange(pred.us.deals, me.us.deals, nrow = 1)


# check poisson with OLS model of deals 
ols.deals <- brm(deals ~  
                  time_to_elec*ally*v2x_polyarchy +
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
plot_cme(ols.deals, condition = "time_to_elec", variables = c("ally", "v2x_polyarchy"))


# check estimates: predictions 
ols.deals.est <- me.us.elec(ols.deals, data = us.deals.comp)  


ggplot(ols.deals.est[[2]], aes(y = estimate, 
                              x = time_to_elec,
                             group = factor(ally),
                            color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs) + 
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
       x = "Years to Presidential Election")
ggsave("appendix/deals-pred-ols.png", height = 6, width = 8)


# test difference 
key.ols.draws <- as.data.frame(ols.deals.est[[4]][, pois.comp.dmed$rowid])
colnames(key.ols.draws) <- c("a", "b", "c", "d")
hypothesis(key.ols.draws, c("a > b", "b > c", "c > d"))



# poisson 
pc.deals <- brm(deals ~  
                   time_to_elec*ally*v2x_polyarchy +
                   cold_war + 
                   eu_member +
                   rep_pres + 
                   ln_rgdp + 
                   ln_pop + ln_distw + 
                   Comlang,
                 family = poisson(),
                 cores = 4,
                 prior = c(prior(normal(0, .5), class = "b")),
                 backend = "cmdstanr",
                 data = us.deals.comp)


# check estimates: predictions 
pc.deals.est <- me.us.elec(pc.deals, data = us.deals.comp)  

ggplot(pc.deals.est[[2]], aes(y = estimate, 
                               x = time_to_elec,
                               group = factor(ally),
                               color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals: Poisson",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("appendix/deals-pred-pois.png", height = 6, width = 8)


# test difference 
key.pc.draws <- as.data.frame(pc.deals.est[[4]][, pois.comp.dmed$rowid])
colnames(key.pc.draws) <- c("a", "b", "c", "d")
hypothesis(key.pc.draws, c("a > b", "b > c", "c > d"))


# zero-inflated poisson 
zip.deals <- brm(deals ~  
                   time_to_elec*ally*v2x_polyarchy +
                   cold_war + 
                   eu_member +
                   rep_pres + 
                   ln_rgdp + 
                   ln_pop + ln_distw + 
                   Comlang,
                 family = zero_inflated_poisson(),
                 cores = 4,
                 prior = c(prior(normal(0, .5), class = "b")),
                 backend = "cmdstanr",
                 data = us.deals.comp)


# check estimates: predictions 
zip.deals.est <- me.us.elec(zip.deals, data = us.deals.comp)  


ggplot(zip.deals.est[[2]], aes(y = estimate, 
                               x = time_to_elec,
                               group = factor(ally),
                               color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals: Zero-Inflated Poisson",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("appendix/deals-pred-zip.png", height = 6, width = 8)


# test difference 
key.zip.draws <- as.data.frame(zip.deals.est[[4]][, pois.comp.dmed$rowid])
colnames(key.zip.draws) <- c("a", "b", "c", "d")
hypothesis(key.zip.draws, c("a > b", "b > c", "c > d"))



### Show predictions with standard poisson and negative binomial
# negative binomial 
nb.deals <- brm(deals ~  
                   time_to_elec*ally*v2x_polyarchy +
                   cold_war + 
                   eu_member +
                   rep_pres + 
                   ln_rgdp + 
                   ln_pop + ln_distw + 
                   Comlang,
                 family = negbinomial(),
                 cores = 4,
                 prior = c(prior(normal(0, .5), class = "b")),
                 backend = "cmdstanr",
                 data = us.deals.comp)


# posterior predictive check
pp_check(pc.deals, type = "rootogram", 
         style = "hanging") +
  labs(title = "Poisson Posterior Predictive Check: Arms Deals")
ggsave("appendix/pois-pp-check.png", height = 6, width = 8)


pp_check(nb.deals, type = "rootogram", 
         style = "hanging") +
  labs(title = "Negative Binomial Posterior Predictive Check: Arms Deals")
ggsave("appendix/nb-pp-check.png", height = 6, width = 8)



# table with model coefficients for appendix
# nice names
coef.names.deals.brm = c("b_time_to_elec" = "Years to Election",
                   "b_v2x_polyarchy" = "Polyarchy",
                   "b_ally" = "US Ally",
                   "v2x_polyarchy" = "Polyarchy",
                   "ally" = "US Ally",
                   "b_time_to_elec × v2x_polyarchy" = "Polyarchy x Years to Election",
                   "b_time_to_elec × ally" = "Ally x Years to Election",
                   "b_ally × v2x_polyarchy" = "Ally x Polyarchy",
                   "b_time_to_elec × ally × v2x_polyarchy" = "Ally x Years to Election\nx Polyarchy",
                   "b_cowmidongoing" = "Ongoing MID",
                   "b_ln_pop" = "Log Population",
                   "b_ln_rgdp" = "Log GDP",
                   "b_ln_distw" = "Log Distance",
                   "b_eu_member" = "EU Member",
                   "b_gwot" = "Global War on Terror",
                   "b_cold_war" = "Cold War",
                   "b_rep_pres" = "Republican President",
                   "b_Comlang" = "Common Language",
                   "b_Intercept" = "Intercept",
                   "b_hu_v2x_polyarchy" = "Hurdle: Polyarchy",
                   "b_hu_ally" = "Hurdle: US Ally",
                   "b_hu_ln_rgdp" = "Hurdle: Log GDP",
                   "b_hu_cowmidongoing" = "Hurdle: Ongoing MID",
                   "b_hu_Intercept" = "Hurdle: Intercept")

pois.models <- list(pois.deals.cycle, pois.deals.democ, pois.deals)
names(pois.models) <- c("Generic Cycle", "Regime Cycle", "Regime and Ally Cycle")
modelsummary(pois.models,
             #output = "latex",
             output = "appendix/deals-reg-tabs.tex", 
             gof_map = "none",
             conf.level = .9,
             longtable = TRUE,
             fmt = fmt_significant(2),
             coef_rename = coef.names.deals.brm,
             statistic = "({conf.low}, {conf.high})",
             #notes = list('90\\% Credible Intervals in parentheses.'),
             title = "\\label{tab:pois-regs}: Coefficient estimates from hurdle Poisson models of US arms deals.") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position")) %>%
  footnote(general = "90% Credible Intervals in parentheses.")

# robustness check models
modelsummary(list(ols.deals, pois.deals, zip.deals),
             gof_map = "none")
