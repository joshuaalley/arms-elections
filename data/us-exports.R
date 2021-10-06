# Joshua Alley
# Analysis of exports from US to allies around elections 


# get us exports
us.trade.ally <- filter(dyadic.mp.ally, 
                   ccode == 2) %>%
            ungroup() %>%
            rename(
              us_imports = IPTOT_o,
              us_exports = XPTOT_o,
              us.code = ccode,
              ccode = ccode2
            ) %>%
            left_join(promises.data) %>%
            mutate(
              lag_us_exports = lag(us_exports),
              lag_us_imports = lag(us_imports),
              change_us_exports = us_exports - lag_us_exports,
              change_us_imports = us_imports - lag_us_imports,
              ln_trade = log(FLOW + 1),
              lag_ln_trade = lag(ln_trade),
              change_us_trade = (FLOW - lag(FLOW)) / 1000000000,
              ihs_change_trade = asinh(change_us_trade),
              ln_exports = log(us_exports), 
              ln_imports = log(us_imports), 
              lag_ln_exports = lag(ln_exports),
              lag_ln_imports = lag(ln_imports),
            ) %>%
            left_join(select(promises.annual, year, 
                      president, time_to_elec)) %>%
            group_by(president, ccode) %>%
            mutate(
             # running mean stat
              total_statements = rollapply(statements_americas, 2, sum,
                               align ='right', fill = 0),
              ln_total_statements = log(total_statements + 1)
             )
us.trade.ally$dyad.id <- group_indices(us.trade.ally, us.code, ccode) 

# look at the raw data
ggplot(us.trade.ally, aes(x = ln_trade)) + geom_histogram()
ggplot(us.trade.ally, aes(x = change_us_trade)) + geom_histogram()
ggplot(us.trade.ally, aes(x = us_exports)) + geom_histogram()
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


# ols w/o any dyad corrections: US trade changes
us.trade.all <- rlm(change_us_trade ~
                       time_to_elec*mean_leader_supp +
                       incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       gmlmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + Distw +
                       Comlang + Contig + Evercol,
                     data = us.trade.ally,
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
us.exports.all <- rlm(us_exports ~ lag_us_exports + lag_us_imports +
                        time_to_elec*mean_leader_supp +
                        incumbent +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + xm_qudsest2 + 
                        gmlmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + Distw +
                       Comlang + Contig + Evercol,
                      maxit = 40,
                       data = us.trade.ally)
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
us.exports.ch <- rlm(change_us_exports ~ 
                        time_to_elec*mean_leader_supp +
                       incumbent +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + xm_qudsest2 + 
                        gmlmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw +
                        Comlang + Contig + Evercol,
                      maxit = 40,
                      data = us.trade.ally)
summary(us.exports.ch)

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


# ols w/o any dyad corrections: change in US exports w/ fixed effects
us.exports.ch.fe <- robustbase::lmrob(change_us_exports ~ 
                       time_to_elec*mean_leader_supp +
                         incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       gmlmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d +
                       factor(ccode),
                       fast.s.large.n = Inf,
                       trace.lev = 4,
                       setting = "KS2014",
                       singular.ok = TRUE,
                     data = us.trade.ally)
summary(us.exports.ch.fe)

me.supp.change.fe <- plot_cme(us.exports.ch.fe,
                           effect = "mean_leader_supp",
                           condition = "time_to_elec") +
                      geom_hline(yintercept = 0) +
                      labs(
                        x = "Time to Election",
                        y = "Marginal Effect of Avg. Leader Support",
                          title = "Change in US Exports (OLS & FE)"
                         )
me.supp.change.fe



# ols w/o any dyad corrections: US imports
us.imports.all <- rlm(change_us_imports ~ 
                       time_to_elec*mean_leader_supp +
                        incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       gmlmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + Distw +
                       Comlang + Contig + Evercol,
                     data = us.trade.ally)
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
  change_us_exports ~ 
    time_to_elec*mean_leader_supp +
    incumbent +
    lag_latency_pilot + lag_rivalry_thompson +
    adv_signal_last3 + xm_qudsest2 + 
    gmlmidongoing + dyadigos +
    change_gdp_o + change_gdp_d + Distw +
    Comlang + Contig + Evercol,
  change_us_imports ~ 
    time_to_elec* mean_leader_supp +
    incumbent +
    lag_latency_pilot + lag_rivalry_thompson +
    adv_signal_last3 + xm_qudsest2 + 
    gmlmidongoing + dyadigos +
    change_gdp_o + change_gdp_d + Distw +
    Comlang + Contig + Evercol),
  data = us.trade.ally
)
# residual correlations are very strong
summary(us.trade.sur)


# trade balance
us.balance.all <- rlm(trade_balance ~ lag_trade_balance +
                       time_to_elec*mean_leader_supp +
                        incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       gmlmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + Distw +
                       Comlang + Contig + Evercol,
                     data = us.trade.ally)
summary(us.balance.all)

plot_cme(us.balance.all,
         effect = "mean_leader_supp",
         condition = "time_to_elec") +
  geom_hline(yintercept = 0)




### plot key coefficient estimates
# nice names for plotting
coef.names.map.us = c(
                   "mean_leader_supp" = "Avg Leader Support",
                   "time_to_elec" = "Years to Election",
                   "time_to_elec:mean_leader_supp" = "Years to Election x Avg Leader Support",
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
                   "gmlmidongoing" = "Ongoing MID",
                   "dyadigos" = "Shared IGOs",
                   "lag_latency_pilot" = "Lag Ally Latency",
                   "lag_rivalry_thompson" = "Lag Rivalry",
                   "adv_signal_last3" = "Prior Adversary Signal",
                   "lag_us_exports" = "Lag Exports",
                   "lag_us_imports" = "Lag Imports")

us.model.list <- list(us.trade.all, us.exports.all,
                      us.exports.ch,
                      us.imports.all)
names(us.model.list) <- c("Change US Trade",
                          "US Exports",
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
grid.arrange(me.supp.trade, me.supp.exports,
             me.supp.change, 
             me.supp.imports,
             nrow = 2)
us.me.plots <- arrangeGrob(me.supp.exports,
                me.supp.change,
                me.supp.imports,
                nrow = 2)
ggsave("figures/me-plots-us.png", us.me.plots,
       height = 7, width = 9)



# complete data and rescaled 
brm.us.exports <- select(ungroup(us.trade.ally),
                              ccode, year, change_us_exports, 
                                time_to_elec, mean_leader_supp,
                                lag_latency_pilot, lag_rivalry_thompson,
                                adv_signal_last3, xm_qudsest2,  dyadigos,
                                change_gdp_o, change_gdp_d, Distw,
                                Comlang, Contig, Evercol)
# rescale IVs by 2sd 
brm.us.exports[, 5:16] <- lapply(brm.us.exports[, 5:16],
                                          function(x)
                                            arm::rescale(x, binary.inputs = "0/1"))



# brms model of exports as dyadic clustering does not work
bf.exports.all <- brmsformula(change_us_exports ~ 
                                time_to_elec*mean_leader_supp +
                                lag_latency_pilot + lag_rivalry_thompson +
                                adv_signal_last3 + xm_qudsest2 +  dyadigos +
                                change_gdp_o + change_gdp_d + Distw +
                                Comlang + Contig + Evercol +
                                (1 | ccode) + (1 | year),
                              center = TRUE) +
                                student()
exports.priors <- c(
                   set_prior("normal(0, .5)", class = "b"),
                   set_prior("normal(0, 1)", class = "Intercept"),
                   set_prior("normal(0, 1)", class = "sigma"),
                   set_prior("normal(0, 1)", class = "sd")
) 

# fit the model
brm.ally.exports <- brm(bf.exports.all, 
                     data = brm.us.exports,
                     prior = exports.priors,
                     iter = 2000, warmup = 1000,
                     chains = 4, cores = 4,
                     threads = 2,
                     backend = "cmdstanr",
                     control = list(
                     max_treedepth = 20))
summary(brm.ally.exports, prob = .9)
mcmc_plot(brm.ally.exports, "hist_by_chain", pars = "sigma")
mcmc_plot(brm.ally.exports, "intervals", pars = "b_",
          prob = .9)
mcmc_plot(brm.ally.exports, "intervals", 
          pars = c("b_mean_leader_supp",
                   "b_time_to_elec",
                   "b_time_to_elec:mean_leader_supp"),
          prob = .9)

# conditional effects
plot(conditional_effects(brm.ally.exports,
                         effects = "mean_leader_supp:time_to_elec",
                         int_conditions = list(time_to_elec = 
                                       seq(from = 0, to = 3, by = 1)),
                         method = "posterior_epred",
                         prob = .9,
                         surface = TRUE,
                         theme = "bw"),
     ask = FALSE)
plot(conditional_effects(brm.ally.exports), ask = FALSE)



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
