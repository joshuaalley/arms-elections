# Joshua Alley
# Analysis of exports by all leading states




# plot variables
# GDP is non-stationary
ggplot(dyadic.mp.ally, aes(x = year, y = GDP_d)) +
  facet_wrap(~ ccode2, scales = "free") +
  geom_line()
ggplot(dyadic.mp.ally, aes(x = year, y = ln_gdp_d)) +
  facet_wrap(~ ccode2, scales = "free") +
  geom_line()

# exports are also not stationary in almost any panel
ggplot(dyadic.mp.ally, aes(x = year, y = exports,
                           color = factor(ccode),
                           group = factor(ccode))) +
  facet_wrap(~ ccode2) +
  geom_line()
# in logs too 
ggplot(dyadic.mp.ally, aes(x = year, y = ln_exports,
                           color = factor(ccode),
                           group = factor(ccode))) +
  facet_wrap(~ ccode2) +
  geom_line()


# ols w/o any dyad corrections: MP exports
ggplot(dyadic.mp.ally, aes(x = exports)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = ln_exports)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = change_exports)) + geom_histogram()


# check missing data
dyadic.mp.ally %>% select(exports, lag_exports, imports,
                          election, mean_leader_supp,
                          lag_election, lead_election, incumbent,
                          ln_gdp_o, ln_gdp_d, ln_distw,
                          xm_qudsest2, cowmidongoing, dyadigos) %>%
  naniar::vis_miss()

# complete cases of dyad data
dyad.mp.comp <- function(data){
  out <- data %>% drop_na(exports, lag_exports, lag_imports,
    election, mean_leader_supp,
    lag_election, lead_election, incumbent,
    xm_qudsest2,  cowmidongoing, dyadigos,
    GDP_o, GDP_d, Distw,
    Comlang, Contig, Evercol)
}
# complete cases of dyad data: changes
dyad.mp.comp.ch <- function(data){
  out <- data %>% drop_na(change_ln_exports, change_ln_imports,
                          election, mean_leader_supp,
                          lag_election, lead_election, incumbent,
                          xm_qudsest2,  cowmidongoing, dyadigos,
                          change_gdp_o, change_gdp_d, Distw,
                          Comlang, Contig, Evercol)
}



# model w/o transformation
mp.exports.all <- lm(exports ~ lag_exports + lag_imports +
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  cowmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyad.mp.comp(dyadic.mp.ally))
summary(mp.exports.all)


# model w/o transformation: robust
mp.exports.rlm <- rlm(exports ~ lag_exports + lag_imports +
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                      maxit = 40,
                     data = dyad.mp.comp(dyadic.mp.ally))
summary(mp.exports.rlm)
qqnorm(mp.exports.all$residuals)
qqline(mp.exports.all$residuals)


# model in logs
mp.exports.ln <- rlm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  cowmidongoing + dyadigos +
                       ln_gdp_o + ln_gdp_d + ln_distw +
                       Comlang + Contig + Evercol,
                     data = dyadic.mp.ally)
summary(mp.exports.ln)
qqnorm(mp.exports.ln$residuals)
qqline(mp.exports.ln$residuals)


# change total trade 
mp.trade <- lm(change_trade ~ 
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  cowmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + 
                      factor(dyad.id),
                     data = dyad.mp.comp.ch(dyadic.mp.ally))
summary(mp.trade)
qqnorm(mp.trade$residuals)
qqline(mp.trade$residuals)



# correct se: add rlm weights to OLS
mp.trade.dr <- dyadRobust(lm(change_trade ~ 
                               election*mean_leader_supp +
                               lag_election + lead_election + incumbent +
                               xm_qudsest2 +  cowmidongoing + dyadigos +
                               change_gdp_o + change_gdp_d +
                               factor(dyad.id),
                               weights = mp.trade$w,
                               data = dyad.mp.comp.ch(dyadic.mp.ally)),
                            dat = dyad.mp.comp.ch(dyadic.mp.ally),
                            dyadid = "dyad.id",
                            egoid = "ccode1",
                            alterid = "ccode2")


# fixed effects
# nickell bias here- long T helps, but T=N at best
mp.exports.fe <- lm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                      election*mean_leader_supp +
                      lag_election + lead_election + incumbent +
                      xm_qudsest2 +  cowmidongoing + dyadigos +
                      ln_gdp_o + ln_gdp_d +  
                     factor(dyad.id),
  data = dyadic.mp.ally)
summary(mp.exports.fe)


### model changes, given high autocorrelation and interest in dyad FE
mp.chexports.all <- rlm(change_ln_exports ~ 
                          election*mean_leader_supp +
                          lag_election + lead_election + incumbent +
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          change_gdp_o + change_gdp_d + factor(dyad.id),
                      data = dyad.mp.comp.ch(dyadic.mp.ally))
summary(mp.chexports.all)
qqnorm(mp.chexports.all$residuals)
qqline(mp.chexports.all$residuals)
plot(mp.chexports.all$residuals, mp.chexports.all$w)


# robust se
mp.exports.dr <- dyadRobust(lm(change_ln_exports ~ 
                                   election*mean_leader_supp +
                                   lag_election + lead_election + incumbent +
                                   xm_qudsest2 +  cowmidongoing + dyadigos +
                                   change_gdp_o + change_gdp_d + 
                                   factor(dyad.id),
                                 weights = mp.chexports.all$w,
                                 data = dyad.mp.comp.ch(dyadic.mp.ally)),
                               dat = dyad.mp.comp.ch(dyadic.mp.ally),
                               dyadid = "dyad.id",
                               egoid = "ccode1",
                               alterid = "ccode2")



# ols w/o any dyad corrections: imports
ggplot(dyadic.mp.ally, aes(x = imports)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = ln_imports)) + geom_histogram()

mp.imports.all <- rlm(ln_imports ~ lag_ln_imports +
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  cowmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyadic.mp.ally)
summary(mp.imports.all)


# changes
mp.imports.ch <- rlm(change_ln_imports ~ 
                        election*mean_leader_supp +
                        lag_election + lead_election + incumbent +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + 
                       factor(dyad.id),
                      data = dyad.mp.comp.ch(dyadic.mp.ally))
summary(mp.imports.ch)


# correct se + dyad FE
mp.imports.dr <- dyadRobust(lm(change_ln_imports ~ 
                                 election*mean_leader_supp +
                                 lag_election + lead_election + incumbent +
                                 xm_qudsest2 +  cowmidongoing + dyadigos +
                                 change_gdp_o + change_gdp_d + 
                                 factor(dyad.id),
                               weight = mp.imports.ch$w,
                               data = dyad.mp.comp.ch(dyadic.mp.ally)),
                            dat = dyad.mp.comp.ch(dyadic.mp.ally),
                            dyadid = "dyad.id",
                            egoid = "ccode1",
                            alterid = "ccode2")


# trade balance
ggplot(dyadic.mp.ally, aes(x = trade_balance)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = change_ihs_balance)) + geom_histogram()

mp.balance.all <- rlm(change_ihs_balance ~ 
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  cowmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + 
                        factor(dyad.id),
                      maxit = 40,
                     data = dyad.mp.comp.ch(dyadic.mp.ally))
summary(mp.balance.all)
qqnorm(mp.balance.all$residuals)
qqline(mp.balance.all$residuals)


# robust se
mp.balance.dr <- dyadRobust(lm(change_ihs_balance ~ 
                                 election*mean_leader_supp +
                                 lag_election + lead_election + incumbent +
                                 xm_qudsest2 +  cowmidongoing + dyadigos +
                                 change_gdp_o + change_gdp_d + 
                                 factor(dyad.id),
                               weights = mp.balance.all$w,
                               data = dyad.mp.comp.ch(dyadic.mp.ally)),
                            dat = dyad.mp.comp.ch(dyadic.mp.ally),
                            dyadid = "dyad.id",
                            egoid = "ccode1",
                            alterid = "ccode2")




### present results ###

# create a dataframe w/ coefficient estimates
mp.est <- bind_rows(
     dr.clean(mp.trade.dr),
     dr.clean(mp.imports.dr),
     dr.clean(mp.exports.dr),
     dr.clean(mp.balance.dr),
  ) %>% # cut FE and intercept terms
  filter(str_detect(variable, "dyad.id", negate = T)) %>%
  filter(str_detect(variable, "Intercept", negate = T))

# nice names for plotting
coef.names.map = c("lag_exports" = "Lag Exports",
                   "lag_imports" = "Lag Imports",
                   "lag_trade_balance" = "Lag Trade Balance",
                   "election" = "Election",
                   "mean_leader_supp" = "Change Leader Support", 
                   "election:mean_leader_supp" = "Election x Change Leader Support",
                   "lag_election" = "Lag Election",
                   "lead_election" = "Lead Election", 
                   "incumbent" = "Incumbent",
                   "xm_qudsest2" = "Allied Democracy",
                   "GDP_o" = "Major Power GDP",
                   "change_gdp_o" = "Change Major Power GDP",
                   "GDP_d" = "Ally GDP",
                   "change_gdp_d" = "Change Ally GDP",
                   "Distw" = "Pop. Weighted Distance)",
                   "Contig" = "Contiguous",
                   "Comlang" = "Common Language",
                   "Evercol" = "Former Colony",
                   "cowmidongoing" = "Ongoing MID",
                   "dyadigos" = "Shared IGOs",
                   "lag_latency_pilot" = "Lag Ally Latency",
                   "lag_rivalry_thompson" = "Lag Rivalry",
                   "adv_signal_last3" = "Prior Adversary Signal",
                   "time_to_elec" = "Years to Election",
                   "time_to_elec:mean_leader_supp" = "Years to Election x Change Leader Support")
mp.est$variable <- coef.names.map[mp.est$variable]

# model names
model.names.map <- c("mp.trade.dr" = "Change Ln(Trade)",
                     "mp.imports.dr" = "Change Ln(Imports)",
                     "mp.exports.dr" = "Change Ln(Exports)",
                     "mp.balance.dr" = "Change Trade Balance")
mp.est$model <- model.names.map[mp.est$model]


# plot results
ggplot(mp.est, aes(y = factor(variable, ordered = T,
                           levels = rev(coef.names.map)),
                   x = coef
                   )) +
   facet_grid(~ model, scales = "free_x") +
   geom_vline(xintercept = 0) +
   geom_pointrange(aes(
     xmin = coef - 1.96*se,
     xmax = coef + 1.96*se),
     position = position_dodge(width = 1)
     ) +
  labs(x = "Estimate",
       y = "Term",
       color = "Model")
ggsave("figures/mp-model-coefs.png", height = 8, width = 12)


# interaction terms only
ggplot(filter(mp.est, variable == "Election" | 
                variable == "Change Leader Support" |
                variable == "Election x Change Leader Support"),
        aes(y = variable, x = coef)) +
    facet_grid(~ model, scales = "free_x") +
     geom_vline(xintercept = 0) +
     geom_pointrange(aes(
       xmin = coef - 1.96*se,
       xmax = coef + 1.96*se),
       position = position_dodge(width = 1)
       ) +
    scale_color_grey() +
    labs(x = "Estimate",
         y = "Term",
         color = "Model")


# marginal effects

# marginal effects with elections changing and all others fixed to typical 
# typical as function
# mean_leader_supp = seq(-2, 3, .25)
typical.func <- function(x){
  dat <- typical(model = x, election = c(0, 1),
                 mean_leader_supp = seq(-2, 3, .25)) 
   dat$lag_election <- 0
   dat$lead_election <- 0
  dat$incumbent <- 0
  dat 
}


predictions(mp.trade, newdata = typical.func(mp.trade))


# need this to replace revised vcov
# change in trade 
me.leader.supp <- marginaleffects(mp.trade,
                vcov = mp.trade.dr$Vhat,
                variables = "mean_leader_supp",
                newdata = typical.func(mp.trade))
# change in imports
me.imports.supp <- marginaleffects(mp.imports.ch,
                                  vcov = mp.imports.dr$Vhat,
                                  variables = "mean_leader_supp",
                                  newdata = typical.func(mp.imports.ch))
# changes in exports
me.ch.exports <- marginaleffects(mp.chexports.all,
                                 vcov = mp.exports.dr$Vhat,
                                 variables = "mean_leader_supp",
                                 newdata = typical.func(mp.chexports.all))

# trade balance
me.balance <- marginaleffects(mp.balance.all,
                                   vcov = mp.balance.dr$Vhat,
                                   variables = "mean_leader_supp",
                                newdata = typical.func(mp.balance.all))

plot.me <- function(model, label){
plot.out <- ggplot(model, aes(y = dydx, 
                 x = factor(election))) +
                 geom_hline(yintercept = 0) +
                 geom_pointrange(aes(
                ymin = dydx - 1.96*std.error,
                ymax = dydx + 1.96*std.error
                )) +
              scale_x_discrete("Election", labels = c(`0` = "No", `1` = "Yes")) +
             labs(y = "Estimated Marginal Effect of Leader Support",
              title = label)
plot.out
}

# plot all three margins
plot.me.tr  <- plot.me(model = me.leader.supp, label = "Change Log Trade")
plot.me.tr
# plot imports
plot.me.imp  <- plot.me(model = me.imports.supp, label = "Change Log Imports")
plot.me.imp
# changes
plot.ch.ex  <- plot.me(model = me.ch.exports, label = "Change Log Exports")
plot.ch.ex
# trade balance
plot.balance  <- plot.me(model = me.balance, label = "Change Log Trade Balance")
plot.balance



# combine 
grid.arrange(plot.me.tr, 
             plot.ch.ex, 
             plot.me.imp,
             plot.balance,
             nrow = 2)
me.plots.mp <- arrangeGrob(plot.me.tr, 
                            plot.ch.ex, 
                            plot.me.imp,
                            plot.balance,
                            nrow = 2)
ggsave("appendix/me-plots-mp.png", me.plots.mp, height = 8, width = 10)



# plot predictions
plot.pred <- function(model, label){
  plot.out <- ggplot(model, aes(y = predicted, 
                                x = mean_leader_supp,
                                group = factor(election),
                                color = factor(election))) +
    geom_hline(yintercept = 0) +
    geom_line(lwd = 1.5) +
    scale_color_grey("Election", 
                         labels = c(`0` = "No", `1` = "Yes")) +
    labs(y = paste("Predicted", label),
         title = label)
  plot.out
}
plot.pred(model = me.leader.supp, label = "Change Log Trade")
plot.pred(model = me.ch.exports, label = "Change Log Exports")
plot.pred(model = me.imports.supp, label = "Change Log Imports")
plot.pred(model = me.balance, label = "Change Trade Balance")


# predictions
trade.pred <- bind_cols(as.data.frame(predict(mp.trade, 
                      newdata = typical.func(mp.trade),
                     interval = "confidence")),
                     typical.func(mp.trade))
exports.pred <- bind_cols(as.data.frame(predict(mp.chexports.all, 
                        newdata = typical.func(mp.chexports.all),
                      interval = "confidence")),
                      typical.func(mp.chexports.all))
imports.pred <- bind_cols(as.data.frame(predict(mp.imports.ch, 
                         newdata = typical.func(mp.imports.ch),
                        interval = "confidence")),
                        typical.func(mp.imports.ch))
balance.pred <- bind_cols(as.data.frame(predict(mp.balance.all, 
                        newdata = typical.func(mp.balance.all),
                        interval = "confidence")),
                        typical.func(mp.balance.all))

# combine
mp.pred <- bind_rows(trade.pred, exports.pred, imports.pred, balance.pred)
mp.pred$outcome <- c(rep("Change Log Trade", 42),
                     rep("Change Log Exports", 42),
                     rep("Change Log Imports", 42),
                     rep("Change Trade Balance", 42))

ggplot(mp.pred, aes(y = fit, 
                  x = mean_leader_supp,
                  group = factor(election),
                  fill = factor(election))) +
  facet_wrap(~ outcome) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
                alpha = .5) +
  # geom_pointrange(aes(ymin = lwr, ymax = upr),
  #                 position = position_dodge(width = .1)) +
  scale_fill_grey("Election", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(y = "Predicted Outcome",
       x = "Mean Leader Support")
ggsave("figures/pred-mp-trade.png", height = 6, width = 8)


