# Joshua Alley
# Analysis of exports from US to allies around elections 


# look at the raw data
ggplot(us.trade.ally, aes(x = ln_us_trade)) + geom_histogram()
ggplot(us.trade.ally, aes(x = change_lnus_trade)) + geom_histogram()
ggplot(us.trade.ally, aes(x = growth_lnus_trade)) + geom_histogram()
ggplot(us.trade.ally, aes(x = trade_balance)) + geom_histogram()
ggplot(us.trade.ally, aes(x = ln_exports)) + geom_histogram()

ggplot(us.trade.ally, aes(x = year,
                          y = us_exports)) + 
  facet_wrap(~ ccode) +
  geom_line()

ggplot(us.trade, aes(x = us_imports)) + geom_histogram()
ggplot(us.trade, aes(x = ln_imports)) + geom_histogram()

# look at raw by election
ggplot(us.trade.ally, aes(x = factor(time_to_elec), y = ln_exports,
                          color = mean_leader_supp)) +
  geom_boxplot() +
    geom_jitter(alpha = .5) +
  scale_color_gradientn(
    colours = wes_palette("Zissou1", 100, type = "continuous")) 

# look at trade balance by election
ggplot(us.trade.ally, aes(x = factor(time_to_elec), y = trade_balance,
                          color = mean_leader_supp)) +
  geom_boxplot() +
  geom_jitter()  +
  scale_color_gradientn(
    colours = wes_palette("Zissou1", 100, type = "continuous")
    ) 

# look at trade changes by election 
ggplot(us.trade.ally, aes(x = factor(time_to_elec), y = change_lnus_trade,
                          color = mean_leader_supp)) +
  geom_boxplot(outlier.alpha = 0.0) +
  geom_jitter()  +
  scale_color_gradientn(
    colours = wes_palette("Zissou1", 100, type = "continuous")
  ) 

# look at trade growth by election 
ggplot(us.trade.ally, aes(x = factor(time_to_elec), y = change_lnus_trade,
                          color = mean_leader_supp)) +
  geom_boxplot(outlier.alpha = 0.0) +
  geom_jitter()  +
  scale_color_gradientn(
    colours = wes_palette("Zissou1", 100, type = "continuous")
  ) 

# split commitment by low/high
us.trade.ally$high_leader_supp <- factor(ifelse(us.trade.ally$mean_leader_supp > 
                                    mean(us.trade.ally$mean_leader_supp, 
                                         na.rm = T),
                                         1, 0))
us.trade.ally$country <- countrycode(sourcevar = us.trade.ally$ccode,
                                     origin = "cown",
                                     destination = "country.name")

# plot trade over time
ggplot(us.trade.ally, aes(x = year, y = ihs_trade_balance,
                          group = high_leader_supp,
                          color = factor(time_to_elec))) +
  geom_jitter()

# elections timing
# time to election
ggplot(us.trade.ally, aes(x = factor(time_to_elec), 
                          y = trade_balance)) +
  facet_wrap(~ country, scales = "free") +
  geom_boxplot()



# complete cases to get use rlm weights with OLS FE 
# approx OLS FE for models w/ changes
us.trade.ally.comp <- us.trade.ally %>%
                       drop_na(
                         time_to_elec, mean_leader_supp,
                           incumbent,
                           lag_latency_pilot, lag_rivalry_thompson, 
                           adv_signal_last3, 
                           xm_qudsest2, 
                           cowmidongoing, dyadigos,
                           change_gdp_o, change_gdp_d, Distw,
                           Comlang, Contig, Evercol,
                         change_lnus_trade, change_lnus_exports,
                         change_lnus_imports
                       )



### estimate models
### Look at trade balance 
# trade balance
us.balance.all <- rlm(ihs_trade_balance ~ lag_ihs_balance +
                        time_to_elec*mean_leader_supp +
                        incumbent + xm_qudsest2 +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + 
                        cowmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw +
                        Comlang + Contig + Evercol,
                      maxit = 40,
                      data = us.trade.ally.comp)
summary(us.balance.all)


# change trade balance
us.chbalance <- rlm(ihs_change_balance ~
                     time_to_elec*mean_leader_supp +
                     incumbent + xm_qudsest2 + 
                     lag_latency_pilot + lag_rivalry_thompson +
                     adv_signal_last3 + 
                     cowmidongoing + dyadigos +
                     change_gdp_o + change_gdp_d +
                     Comlang + Contig + Distw + Evercol,
                    maxit = 40,
                   data = us.trade.ally.comp)
summary(us.chbalance)


me.supp.balance <- plot_cme(us.chbalance,
                            effect = "mean_leader_supp",
                            condition = "time_to_elec") +
  geom_hline(yintercept = 0) +
  labs(
    x = "Time to Election",
    y = "Marginal Effect of Change in Leader Support",
    title = "Annual Trade Balance"
  )
me.supp.balance


# change trade balance: 2012
summary(rlm(ihs_change_balance ~
                      mean_leader_supp +
                      xm_qudsest2 + 
                      dyadigos +
                      change_gdp_d +
                      Comlang + Contig + Distw + Evercol,
                    data = filter(us.trade.ally, year == 2012)))

# trade balance w/ fe 
us.balance.all.fe <- lm(ihs_change_balance ~
                          time_to_elec*mean_leader_supp +
                          incumbent +
                          lag_latency_pilot + lag_rivalry_thompson +
                          adv_signal_last3 + xm_qudsest2 + 
                          cowmidongoing + dyadigos +
                          change_gdp_o + change_gdp_d + 
                          factor(ccode),
                        weights = us.chbalance$w,  
                        data = us.trade.ally.comp)
summary(us.balance.all.fe)

me.supp.balance.fe <- plot_cme(us.balance.all.fe,
                            effect = "mean_leader_supp",
                            condition = "time_to_elec") +
  geom_hline(yintercept = 0) +
  labs(
    x = "Time to Election",
    y = "Marginal Effect of Change in Leader Support",
    title = "Trade Balance Changes (Partner FE)"
  )
me.supp.balance.fe


# ols w/o any dyad corrections: US trade changes
us.trade.all <- rlm(change_lnus_trade ~
                       time_to_elec*mean_leader_supp +
                       incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       cowmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + + 
                      factor(ccode),
                     data = us.trade.ally.comp,
                     maxit = 40)
summary(us.trade.all)

me.supp.trade <- plot_cme(us.trade.all,
                    effect = "mean_leader_supp",
                    condition = "time_to_elec") +
                  geom_hline(yintercept = 0) +
                  labs(
                    x = "Time to Election",
                    y = "Marginal Effect of Avg. Leader Support",
                    title = "Annual Change in Total Trade"
                  )
me.supp.trade


# ols w/o any dyad corrections: US exports
us.exports.all <- rlm(ln_us_exports ~ lag_ln_exports + lag_ln_imports +
                        time_to_elec*mean_leader_supp +
                        incumbent +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + xm_qudsest2 + 
                        cowmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + + 
                        factor(ccode),
                      maxit = 40,
                       data = us.trade.ally.comp)
summary(us.exports.all)

me.supp.exports <- plot_cme(us.exports.all,
                           effect = "mean_leader_supp",
                           condition = "time_to_elec") +
                    geom_hline(yintercept = 0) +
                    labs(
                      x = "Time to Election",
                      y = "Marginal Effect of Avg. Leader Support",
                      title = "US Exports to Allies"
                    )
me.supp.exports


# rlm w/o any dyad corrections: change in US exports
us.exports.ch <- rlm(change_lnus_exports ~ 
                        time_to_elec*mean_leader_supp +
                       incumbent +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + xm_qudsest2 + 
                        cowmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw +
                        Comlang + Contig + Evercol,
                      maxit = 40,
                      data = us.trade.ally.comp)
summary(us.exports.ch)

# most relevant
me.supp.change <- plot_cme(us.exports.ch,
                           effect = "mean_leader_supp",
                           condition = "time_to_elec") +
                   geom_hline(yintercept = 0) +                    
                   labs(
                     x = "Time to Election",
                     y = "Marginal Effect of Avg. Leader Support",
                     title = "Change in US Exports"
                    )
me.supp.change

# flip it- time to election
plot_cme(us.exports.ch,
         effect = "time_to_elec",
         condition = "mean_leader_supp") +
  geom_hline(yintercept = 0) +                    
  labs(
    y = "Marginal Effect of Time to Election",
    x = "Avg. Leader Support",
    title = "Change in US Exports"
  )


# ols w/o any dyad corrections: change in US exports w/ fixed effects
us.exports.ch.fe <- lm(change_lnus_exports ~ 
                       time_to_elec*mean_leader_supp +
                         incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       cowmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d +
                       factor(ccode),
                      weights = us.exports.ch$w,
                     data = us.trade.ally.comp)
summary(us.exports.ch.fe)

me.supp.change.fe <- plot_cme(us.exports.ch.fe,
                           effect = "mean_leader_supp",
                           condition = "time_to_elec") +
                      geom_hline(yintercept = 0) +
                      labs(
                        x = "Time to Election",
                        y = "Marginal Effect of Avg. Leader Support",
                          title = "Change in US Exports (FE)"
                         )
me.supp.change.fe



# ols w/o any dyad corrections: US imports
# model in changes: lagged DV suggests a unit root
us.imports.all <- rlm(change_lnus_imports ~ 
                       time_to_elec*mean_leader_supp +
                        incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       cowmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + 
                      factor(ccode),
                     data = us.trade.ally.comp)
summary(us.imports.all)

me.supp.imports <- plot_cme(us.imports.all,
                     effect = "mean_leader_supp",
                     condition = "time_to_elec") +
                    geom_hline(yintercept = 0) +
                    labs(
                      x = "Time to Election",
                      y = "Marginal Effect of Avg. Leader Support",
                      title = "Change in US Imports"
                       )
me.supp.imports


# SUR exports and imports
us.trade.sur <- systemfit(list(
  change_lnus_exports ~ 
    time_to_elec*mean_leader_supp +
    incumbent +
    lag_latency_pilot + lag_rivalry_thompson +
    adv_signal_last3 + xm_qudsest2 + 
    cowmidongoing + dyadigos +
    change_gdp_o + change_gdp_d + Distw +
    Comlang + Contig + Evercol,
  change_lnus_imports ~ 
    time_to_elec*mean_leader_supp +
    incumbent +
    lag_latency_pilot + lag_rivalry_thompson +
    adv_signal_last3 + xm_qudsest2 + 
    cowmidongoing + dyadigos +
    change_gdp_o + change_gdp_d + Distw +
    Comlang + Contig + Evercol),
  data = us.trade.ally.comp
)
# residual correlations are very strong
summary(us.trade.sur)





### plot key coefficient estimates
# nice names for plotting
coef.names.map.us = c(
                   "mean_leader_supp" = "Change Leader Support",
                   "time_to_elec" = "Years to Election",
                   "time_to_elec:mean_leader_supp" = "Years to Election x Change Leader Support",
                   "incumbent" = "Incumbent",
                   "xm_qudsest2" = "Allied Democracy",
                   "GDP_o" = "US GDP",
                   "change_gdp_o" = "Change US GDP",
                   "GDP_d" = "Ally GDP",
                   "change_gdp_d" = "Change Ally GDP",
                   "Distw" = "Pop. Weighted Distance",
                   "Contig" = "Contiguous",
                   "Comlang" = "Common Language",
                   "Evercol" = "Former Colony",
                   "cowmidongoing" = "Ongoing MID",
                   "dyadigos" = "Shared IGOs",
                   "lag_latency_pilot" = "Lag Ally Latency",
                   "lag_rivalry_thompson" = "Lag Rivalry",
                   "adv_signal_last3" = "Prior Adversary Signal",
                   "lag_trade_balance" = "Lag Trade Balance")

us.model.list <- list(us.balance.all, us.balance.all.fe,
                      us.trade.all,
                      us.exports.ch,
                      us.imports.all)
names(us.model.list) <- c("US Trade Balance", "Change Trade Balance (FE)",
                          "US Trade",
                          "Change US Exports",
                          "Change US Imports")


modelsummary(us.model.list,
             "figures/us-model-coefs.tex",
          statistic = c("({conf.low}, {conf.high})"),
          coef_map = coef.names.map.us,
          coef_omit = c("ccode"),
          gof_map = list(
            list("raw" = "nobs", "clean" = "N", "fmt" = 0)))


# marginal effects
grid.arrange(me.supp.balance, me.supp.balance.fe,
             me.supp.trade, 
             me.supp.change,
             me.supp.imports,
             nrow = 2)
us.me.plots <- arrangeGrob(me.supp.balance, me.supp.balance.fe,
                           me.supp.trade, 
                           me.supp.change,
                           me.supp.imports,
                           nrow = 2)
#ggsave("figures/me-plots-us.png", us.me.plots,
#       height = 7, width = 9)



# complete data and rescaled 
brm.us.balance <- select(ungroup(us.trade.ally.comp),
                              ccode, year, trade_balance, 
                                time_to_elec, mean_leader_supp,
                                lag_latency_pilot, lag_rivalry_thompson,
                                adv_signal_last3, xm_qudsest2,  dyadigos,
                                change_gdp_o, change_gdp_d, Distw,
                                Comlang, Contig, Evercol)
# rescale IVs by 2sd 
brm.us.balance[, 5:16] <- lapply(brm.us.balance[, 5:16],
                                          function(x)
                                            arm::rescale(x, binary.inputs = "0/1"))



# brms model of balance as dyadic clustering does not work
bf.balance.all <- brmsformula(trade_balance ~ 
                                time_to_elec*mean_leader_supp +
                                lag_latency_pilot + lag_rivalry_thompson +
                                adv_signal_last3 + xm_qudsest2 +  dyadigos +
                                change_gdp_o + change_gdp_d + Distw +
                                Comlang + Contig + Evercol +
                                (1 | ccode) + (1 | year),
                              center = FALSE) +
                                student()
balance.priors <- c(
                   set_prior("normal(0, .25)", class = "b"),
                   #set_prior("normal(0, .5)", class = "Intercept"),
                   set_prior("normal(0, .5)", class = "sigma"),
                   set_prior("normal(0, .5)", class = "sd")
) 

# fit the model
brm.ally.balance <- brm(bf.balance.all, 
                     data = brm.us.balance,
                     prior = balance.priors,
                     iter = 2000, warmup = 1000,
                     chains = 4, cores = 4,
                     threads = 2,
                     backend = "cmdstanr",
                     control = list(
                     max_treedepth = 20))
summary(brm.ally.balance, prob = .9)
mcmc_plot(brm.ally.balance, "hist_by_chain", pars = "sigma")
mcmc_plot(brm.ally.balance, "intervals", pars = "b_",
          prob = .9)
mcmc_plot(brm.ally.balance, "intervals", 
          pars = c("b_mean_leader_supp",
                   "b_time_to_elec",
                   "b_time_to_elec:mean_leader_supp"),
          prob = .9)

# conditional effects
plot(conditional_effects(brm.ally.balance,
                         effects = "mean_leader_supp:time_to_elec",
                         int_conditions = list(time_to_elec = 
                                       seq(from = 0, to = 3, by = 1)),
                         method = "posterior_epred",
                         prob = .9,
                         surface = TRUE,
                         theme = "bw"),
     ask = FALSE)
plot(conditional_effects(brm.ally.balance), ask = FALSE)



# export us trade to iso3c codes
unique(countrycode(us.trade.ally$ccode, origin = "cown", destination = "iso3c"))

# add to text file 
# define text file
us.iso3c <- file("data/us-ally-iso3c.txt")
# write blocks to text
writeLines(unlist(unique(countrycode(us.trade.ally$ccode, origin = "cown", destination = "iso3c"))),
           con = us.iso3c,
           sep = ";")
close(us.iso3c)
