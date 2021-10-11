# Joshua Alley
# Analysis of exports by all leading states




# plot variables
# GDP is non-stationary
ggplot(dyadic.mp.ally, aes(x = year, y = GDP_o)) +
  facet_wrap(~ ccode) +
  geom_line()
ggplot(dyadic.mp.ally, aes(x = year, y = ln_gdp_o)) +
  facet_wrap(~ ccode) +
  geom_line()

# exports are also not stationary in almost any panel
ggplot(dyadic.mp.ally, aes(x = year, y = XPTOT_o,
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
ggplot(dyadic.mp.ally, aes(x = XPTOT_o)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = ln_exports)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = change_exports)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = ihs_change_exports)) + geom_histogram()


# check missing data
dyadic.mp.ally %>% select(XPTOT_o, lag_exports, IPTOT_o,
                          election, mean_leader_supp,
                          lag_election, lead_election, incumbent,
                          ln_gdp_o, ln_gdp_d, ln_distw,
                          xm_qudsest2, gmlmidongoing, dyadigos) %>%
  naniar::vis_miss()

# complete cases of dyad data
dyad.mp.comp <- function(data){
  out <- data %>% drop_na(XPTOT_o, lag_exports, lag_imports,
    election, change_leader_supp,
    lag_election, lead_election, incumbent,
    xm_qudsest2,  gmlmidongoing, dyadigos,
    GDP_o, GDP_d, Distw,
    Comlang, Contig, Evercol)
}


# model w/o transformation
mp.exports.all <- lm(XPTOT_o ~ lag_exports + lag_imports +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyad.mp.comp(dyadic.mp.ally))
summary(mp.exports.all)


# model w/o transformation: robust
mp.exports.rlm <- rlm(XPTOT_o ~ lag_exports + lag_imports +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyad.mp.comp(dyadic.mp.ally))
summary(mp.exports.rlm)
qqnorm(mp.exports.all$residuals)
qqline(mp.exports.all$residuals)


# correct se: add rlm weights to OLS
mp.exports.dr <- dyadRobust(lm(XPTOT_o ~ lag_exports + lag_imports +
                                 election*change_leader_supp +
                                 lag_election + lead_election + incumbent +
                                 xm_qudsest2 +  gmlmidongoing + dyadigos +
                                 GDP_o + GDP_d + Distw +
                                 Comlang + Contig + Evercol,
                               weights = mp.exports.rlm$w,
                               data = dyad.mp.comp(dyadic.mp.ally)),
                            dat = dyad.mp.comp(dyadic.mp.ally),
                            dyadid = "dyad.id",
                            egoid = "ccode",
                            alterid = "ccode2")
# model in logs
mp.exports.ln <- rlm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                       ln_gdp_o + ln_gdp_d + ln_distw +
                       Comlang + Contig + Evercol,
                     data = dyadic.mp.ally)
summary(mp.exports.ln)
qqnorm(mp.exports.ln$residuals)
qqline(mp.exports.ln$residuals)


### given skewness, box-cox it
# box-cox exports
bc.exports <- boxcox(XPTOT_o ~ lag_exports + lag_imports +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyadic.mp.ally,
                     lambda = seq(-1, 1, length = 200))
lambda.ex <- bc.exports$x[which.max(bc.exports$y)]

# box-cox imports
bc.imports <- boxcox(IPTOT_o ~ lag_exports + lag_imports +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyadic.mp.ally,
                     lambda = seq(-1, 1, length = 200))
lambda.im <- bc.imports$x[which.max(bc.imports$y)]


# create box-cox variables with lags
dyadic.mp.ally <- dyadic.mp.ally %>%
  group_by(ccode, ccode2) %>%
  mutate(
    bc_exports = ((XPTOT_o^lambda.ex - 1) / lambda.ex),
    lag_bc_exports = lag(bc_exports),
    change_bc_exports = bc_exports - lag_bc_exports,
    bc_imports = ((IPTOT_o^lambda.im - 1) / lambda.im),
    lag_bc_imports = lag(bc_imports),
    change_bc_imports = bc_imports - lag_bc_imports
  )



# model w/ bc transformed exports
mp.exports.bc <- rlm( bc_exports ~ 
                      lag_exports + lag_imports +
                      election*change_leader_supp +
                      lag_election + lead_election + incumbent +
                      xm_qudsest2 +  gmlmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                      Comlang + Contig + Evercol,
                    data = dyadic.mp.ally)
summary(mp.exports.bc)
qqnorm(mp.exports.bc$residuals)
qqline(mp.exports.bc$residuals)




# fixed effects
# nickell bias here- long T helps, but T=N at best
mp.exports.fe <- lm(XPTOT_o ~ 
                      lag_exports + lag_imports +
                      election*change_leader_supp +
                      lag_election + lead_election + incumbent +
                      xm_qudsest2 +  gmlmidongoing + dyadigos +
                      ln_gdp_o + ln_gdp_d + ln_distw +
                      Comlang + Contig + Evercol  
                    + factor(dyad.id),
  data = dyadic.mp.ally)
summary(mp.exports.fe)


### model changes, given unit root
# complete cases of dyad data
dyad.mp.comp.ch <- function(data){
  out <- data %>% drop_na(change_exports,
                          election, change_leader_supp,
                          lag_election, lead_election, incumbent,
                          xm_qudsest2,  gmlmidongoing, dyadigos,
                          change_gdp_o, change_gdp_d, Distw,
                          Comlang, Contig, Evercol)
}

mp.chexports.all <- rlm(change_ln_exports ~ 
                          election*change_leader_supp +
                          lag_election + lead_election + incumbent +
                          xm_qudsest2 +  gmlmidongoing + dyadigos +
                          change_gdp_o + change_gdp_d + + Distw +
                         Comlang + Contig + Evercol,
                      data = dyad.mp.comp.ch(dyadic.mp.ally))
summary(mp.chexports.all)
qqnorm(mp.chexports.all$residuals)
qqline(mp.chexports.all$residuals)
plot(mp.chexports.all$residuals, mp.chexports.all$w)


# robust se
mp.chexports.dr <- dyadRobust(lm(change_ln_exports ~ 
                                   election*change_leader_supp +
                                   lag_election + lead_election + incumbent +
                                   xm_qudsest2 +  gmlmidongoing + dyadigos +
                                   change_gdp_o + change_gdp_d + + Distw +
                                   Comlang + Contig + Evercol,
                                 weights = mp.chexports.all$w,
                                 data = dyad.mp.comp.ch(dyadic.mp.ally)),
                               dat = dyad.mp.comp.ch(dyadic.mp.ally),
                               dyadid = "dyad.id",
                               egoid = "ccode",
                               alterid = "ccode2")


# changes w/ FE 
mp.chexports.fe <- lm(change_ln_exports ~ 
                          election*change_leader_supp +
                        lag_election + lead_election + incumbent +
                        xm_qudsest2 +  gmlmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d +
                        + factor(dyad.id),
                      weights = mp.chexports.all$w,
                        data = dyad.mp.comp.ch(dyadic.mp.ally))
summary(mp.chexports.fe)


# robust se
mp.chexports.fe.dr <- dyadRobust(mp.chexports.fe,
                               dat = dyad.mp.comp.ch(dyadic.mp.ally),
                               dyadid = "dyad.id",
                               egoid = "ccode",
                               alterid = "ccode2")


# changes w/ IHS: induces major left skew
mp.chexports.all.ihs <- rlm(ihs_change_exports ~ 
                          election*change_leader_supp +
                          lag_election + lead_election + incumbent +
                          xm_qudsest2 +  gmlmidongoing + dyadigos +
                          change_gdp_o + change_gdp_d + Distw +
                          Comlang + Contig + Evercol,
                        data = dyadic.mp.ally)
summary(mp.chexports.all.ihs)
qqnorm(mp.chexports.all.ihs$residuals)
qqline(mp.chexports.all.ihs$residuals)



# ols w/o any dyad corrections: imports
ggplot(dyadic.mp.ally, aes(x = IPTOT_o)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = ln_imports)) + geom_histogram()
ggplot(dyadic.mp.ally, aes(x = bc_imports)) + geom_histogram()

mp.imports.all <- rlm(IPTOT_o ~ lag_imports +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyadic.mp.ally)
summary(mp.imports.all)


# correct se
mp.imports.dr <- dyadRobust(lm(IPTOT_o ~ lag_imports +
                                 election*change_leader_supp +
                                 lag_election + lead_election + incumbent +
                                 xm_qudsest2 +  gmlmidongoing + dyadigos +
                                 GDP_o + GDP_d + Distw +
                                 Comlang + Contig + Evercol,
                               weight = mp.imports.all$w,
                               data = dyad.mp.comp(dyadic.mp.ally)),
                            dat = dyad.mp.comp(dyadic.mp.ally),
                            dyadid = "dyad.id",
                            egoid = "ccode",
                            alterid = "ccode2")


# model w/ transformed imports
mp.imports.bc <- rlm(bc_imports ~ 
                       lag_exports + lag_imports +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                       GDP_o + GDP_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyadic.mp.ally)
summary(mp.imports.bc)
qqnorm(mp.imports.bc$residuals)


# SUR exports and imports
mp.trade.sur <- systemfit(list(
  change_ln_exports ~ 
    election*change_leader_supp +
    lag_election + lead_election + incumbent +
    xm_qudsest2 +  gmlmidongoing + dyadigos +
    change_gdp_o + change_gdp_d + Distw +
    Comlang + Contig + Evercol,
  change_ln_imports ~ 
    election*change_leader_supp +
    lag_election + lead_election + incumbent +
    xm_qudsest2 +  gmlmidongoing + dyadigos +
    change_gdp_o + change_gdp_d + Distw +
    Comlang + Contig + Evercol),
  data = dyadic.mp.ally
)
# residual correlations are weaker than expected
summary(mp.trade.sur)


# trade balance
ggplot(dyadic.mp.ally, aes(x = trade_balance)) + geom_histogram()

mp.balance.all <- rlm(trade_balance ~ lag_trade_balance +
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 +  gmlmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw +
                       Comlang + Contig + Evercol,
                     data = dyad.mp.comp.ch(dyadic.mp.ally))
summary(mp.balance.all)
qqnorm(mp.balance.all$residuals)
qqline(mp.balance.all$residuals)


# robust se
mp.balance.dr <- dyadRobust(lm(trade_balance ~ lag_trade_balance +
                                 election*change_leader_supp +
                                 lag_election + lead_election + incumbent +
                                 xm_qudsest2 +  gmlmidongoing + dyadigos +
                                 change_gdp_o + change_gdp_d + Distw +
                                 Comlang + Contig + Evercol,
                               weights = mp.balance.all$w,
                               data = dyad.mp.comp.ch(dyadic.mp.ally)),
                            dat =dyad.mp.comp.ch(dyadic.mp.ally),
                            dyadid = "dyad.id",
                            egoid = "ccode",
                            alterid = "ccode2")

# trade balance w/ FE: need changes
mp.balance.all.fe <- lm(change_trade_balance ~
                       election*change_leader_supp +
                       lag_election + lead_election + incumbent +
                         xm_qudsest2 +  gmlmidongoing + dyadigos +
                         change_gdp_o + change_gdp_d +
                       factor(dyad.id),
                     data = dyadic.mp.ally)
summary(mp.balance.all.fe)


mp.balance.dr.fe <- dyadRobust(mp.balance.all.fe,
                            dat = dyadic.mp.ally,
                            dyadid = "dyad.id",
                            egoid = "ccode",
                            alterid = "ccode2")



### present results ###
# create dataframe with model results- all cluster robust SE

# create a dataframe w/ coefficient estimates
mp.est <- bind_rows(
     dr.clean(mp.exports.dr),
     dr.clean(mp.imports.dr),
     dr.clean(mp.chexports.dr),
     dr.clean(mp.chexports.fe.dr),
     dr.clean(mp.balance.dr),
     dr.clean(mp.balance.dr.fe)
  ) %>% # cut FE and intercept terms
  filter(str_detect(variable, "dyad.id", negate = T)) %>%
  filter(str_detect(variable, "Intercept", negate = T))

# nice names for plotting
coef.names.map = c("lag_exports" = "Lag Exports",
                   "lag_imports" = "Lag Imports",
                   "lag_trade_balance" = "Lag Trade Balance",
                   "election" = "Election",
                   "change_leader_supp" = "Change Leader Support", 
                   "election:change_leader_supp" = "Election x Change Leader Support",
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
                   "gmlmidongoing" = "Ongoing MID",
                   "dyadigos" = "Shared IGOs",
                   "lag_latency_pilot" = "Lag Ally Latency",
                   "lag_rivalry_thompson" = "Lag Rivalry",
                   "adv_signal_last3" = "Prior Adversary Signal",
                   "time_to_elec" = "Years to Election",
                   "time_to_elec:change_leader_supp" = "Years to Election x Change Leader Support")
mp.est$variable <- coef.names.map[mp.est$variable]

# model names
model.names.map <- c("mp.exports.dr" = "Exports",
                     "mp.imports.dr" = "Imports",
                     "mp.chexports.dr" = "Change Exports",
                     "mp.chexports.fe.dr" ="Change Exports\n & Dyad FE",
                     "mp.balance.dr" = "Trade Balance",
                     "mp.balance.dr.fe" = "Change Trade Balance\n & Dyad FE")
mp.est$model <- model.names.map[mp.est$model]


# plot results
ggplot(mp.est, aes(y = factor(variable, ordered = T,
                           levels = rev(coef.names.map)),
                   x = coef,
                   #group = model,
                   )) +
   facet_wrap(~ model, scales = "free") +
   geom_vline(xintercept = 0) +
   geom_pointrange(aes(
     xmin = coef - 1.96*se,
     xmax = coef + 1.96*se),
     position = position_dodge(width = 1)
     ) +
  #scale_color_grey() +
  labs(x = "Estimate",
       y = "Term",
       color = "Model")
#ggsave("figures/mp-model-coefs.png", height = 6, width = 8)

# interaction terms only
ggplot(filter(mp.est, variable == "Election" | 
                variable == "Change Leader Support" |
                variable == "Election x Change Leader Support"),
        aes(y = variable, x = coef,
           group = model,
           color = model)) +
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
# need this to emplace revised vcov
# level of exports 
me.leader.supp <- marginaleffects(mp.exports.rlm,
                vcov = mp.exports.dr$Vhat,
                variables = "change_leader_supp",
                newdata = typical(election = c(0, 1)))
# level of imports
me.imports.supp <- marginaleffects(mp.imports.all,
                                  vcov = mp.imports.dr$Vhat,
                                  variables = "change_leader_supp",
                                  newdata = typical(election = c(0, 1)))
# changes in exports
me.ch.exports <- marginaleffects(mp.chexports.all,
                                 vcov = mp.chexports.dr$Vhat,
                                 variables = "change_leader_supp",
                                 newdata = typical(election = c(0, 1)))

# changes in exports w/ fixed effects
me.chfe.exports <- marginaleffects(mp.chexports.fe,
                                 vcov = mp.chexports.fe.dr$Vhat,
                                 variables = "change_leader_supp",
                                 newdata = typical(election = c(0, 1)))

# trade balance
me.balance <- marginaleffects(mp.balance.all,
                                   vcov = mp.balance.dr$Vhat,
                                   variables = "change_leader_supp",
                                   newdata = typical(election = c(0, 1)))
# trade balance w/ fixed effects
me.balance.fe <- marginaleffects(mp.balance.all.fe,
                              vcov = mp.balance.dr.fe$Vhat,
                              variables = "change_leader_supp",
                              newdata = typical(election = c(0, 1)))

plot.me <- function(model, label){
plot.out <- ggplot(model, aes(y = dydx, x = factor(election))) +
                 geom_hline(yintercept = 0) +
                 geom_pointrange(aes(
                ymin = dydx - 1.96*std.error,
                ymax = dydx + 1.96*std.error
                )) +
              scale_x_discrete("Election", labels = c(`0` = "No", `1` = "Yes")) +
             labs(y = "Estimated Marginal Effect of Leader Support for Ally",
              title = label)
plot.out
}

# plot all three margins
plot.me.ex  <- plot.me(model = me.leader.supp, label = "Exports")
plot.me.ex
# plot imports
plot.me.imp  <- plot.me(model = me.imports.supp, label = "Imports")
plot.me.imp
# changes
plot.ch.ex  <- plot.me(model = me.ch.exports, label = "Change Exports")
plot.ch.ex
# changes w/ FE
plot.chfe.ex  <- plot.me(model = me.chfe.exports, label = "Change Exports & Dyad FE")
plot.chfe.ex
# trade balance
plot.balance  <- plot.me(model = me.balance, label = "Trade Balance")
plot.balance
# trade balance
plot.balance.fe  <- plot.me(model = me.balance.fe, label = "Change Trade Balance & Dyad FE")
plot.balance.fe



# combine 
grid.arrange(plot.balance, plot.balance.fe,
             plot.me.imp, plot.me.ex,
             nrow = 2)
me.plots.mp <- arrangeGrob(plot.balance, plot.balance.fe,
                           plot.me.imp, plot.me.ex,
                           nrow = 2)
ggsave("figures/me-plots-mp.png", me.plots.mp, height = 8, width = 10)


