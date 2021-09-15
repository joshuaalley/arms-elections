# Joshua Alley
# Analysis of exports by all leading states

# load McManus and Nieman signals data
# mp.signals <- read.csv("data/signals-data.csv")

# load elections data
nelda <- read_dta("data/nelda.dta") %>%
          filter(types == "Executive") %>%
          select(
            ccode, year, mmdd, electionid,
            nelda8, # term limit
            nelda21 # incumbent running
          ) %>%
          mutate(
            election = 1,
            incumbent = ifelse(nelda21 == "yes", 1, 0)
          )



# combine latent support with trade data
dyadic.trade.major <- latent.supp %>% 
                       select(year, ccode2, 
                             median, lag_median,
                             change_median) %>% 
                       left_join(dyadic.trade) %>%
                       rename(ccode = ccode1)  %>%
                      rename(
                        major_imports = flow1,
                        major_exports = flow2,
                        ) %>%
                      filter(ccode == 2 |
                              ccode == 200 |
                              ccode == 220)

# add elections data 
dyadic.trade.major <- left_join(dyadic.trade.major,
                                nelda)
# fill in elections w/ zeros
dyadic.trade.major$election[is.na(dyadic.trade.major$election)] <- 0
dyadic.trade.major$incumbent[is.na(dyadic.trade.major$incumbent)] <- 0

# full data clearning
dyadic.mp.ally <- dyadic.trade.major %>%
                        fill(electionid, .direction = "down") %>%
                        group_by(ccode, ccode2, electionid) %>%
                         mutate(
                            lead_election = lead(election),
                            lag_election = lag(election),
                            lag_atop_defense = lag(atop_defense),
                            lag_xm_qudsest2 = lag(xm_qudsest2),
                            ln_exports = log(major_exports + 1),
                            ln_imports = log(major_imports + 1),
                            lag_ln_exports = lag(ln_exports),
                            change_ln_exports = ln_exports - lag_ln_exports,
                            lag_ln_imports = lag(ln_imports),
                            change_ln_exports = ln_exports - lag_ln_exports,
                            trade_balance = major_exports - major_imports,
                            ihs_trade_balance = asinh(trade_balance),
                            lag_ihs_trade_balance = lag(ihs_trade_balance),
                            # running mean support
                            mean_leader_supp = rollapply(median, 2, mean,
                                                align ='right', fill = lag_median)) %>%
                        filter(atop_defense == 1) %>%
                        select(ccode, ccode2, year, electionid, mean_leader_supp,
                          everything()) 

# dyad id
dyadic.mp.ally$dyad.id <- group_indices(dyadic.mp.ally, ccode, ccode2) 


# fit the same model:
# interact median and election 

# ols w/o any dyad corrections: MP exports
mp.exports.all <- lm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 + 
                       wbgdp2011est1 + wbgdp2011est2 +
                       mindist + capdist + gmlmidongoing + dyadigos,
                     data = dyadic.mp.ally)
summary(mp.exports.all)

# correct se
mp.exports.dr <- dyadRobust(mp.exports.all,
                            dat = dyadic.mp.ally,
                            dyadid = "dyad.id",
                            egoid = "ccode",
                            alterid = "ccode2")


# fixed effects
# nickell bias here- long T helps, but T=N at best
mp.exports.fe <- lm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                      election*mean_leader_supp +
                      lag_election + lead_election + incumbent +
                      xm_qudsest2 + 
                      wbgdp2011est1 + wbgdp2011est2 +
                      mindist + capdist + gmlmidongoing + dyadigos 
                    + factor(dyad.id),
  data = dyadic.mp.ally)
summary(mp.exports.fe)


# changes
mp.chexports.all <- lm(change_ln_exports ~ 
                        election*mean_leader_supp +
                        lag_election + lead_election + incumbent +
                        xm_qudsest2 + 
                        wbgdp2011est1 + wbgdp2011est2 +
                        mindist + capdist + gmlmidongoing + dyadigos,
                      data = dyadic.mp.ally)
summary(mp.chexports.all)


# robust se
mp.chexports.dr <- dyadRobust(mp.chexports.all,
                               dat = dyadic.mp.ally,
                               dyadid = "dyad.id",
                               egoid = "ccode",
                               alterid = "ccode2")


# changes w/ FE 
mp.chexports.fe <- lm(change_ln_exports ~ 
                          election*mean_leader_supp +
                          lag_election + lead_election + incumbent +
                          xm_qudsest2 + 
                          wbgdp2011est1 + wbgdp2011est2 +
                          mindist + capdist + gmlmidongoing + dyadigos 
                        + factor(dyad.id),
                        data = dyadic.mp.ally)
summary(mp.chexports.fe)


# robust se
mp.chexports.fe.dr <- dyadRobust(mp.chexports.fe,
                               dat = dyadic.mp.ally,
                               dyadid = "dyad.id",
                               egoid = "ccode",
                               alterid = "ccode2")




# ols w/o any dyad corrections: US imports
mp.imports.all <- lm(ln_imports ~ lag_ln_imports +
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 + 
                       wbgdp2011est1 + wbgdp2011est2 +
                       mindist + capdist + gmlmidongoing + dyadigos,
                     data = dyadic.mp.ally)
summary(mp.imports.all)


# SUR exports and imports
mp.trade.sur <- systemfit(list(
  ln_exports ~ lag_ln_exports +
    election*mean_leader_supp +
    lag_election + lead_election + incumbent +
    xm_qudsest2 + 
    wbgdp2011est1 + wbgdp2011est2 +
    mindist + capdist + gmlmidongoing + dyadigos,
  ln_imports ~ lag_ln_imports +
    election*mean_leader_supp +
    lag_election + lead_election + incumbent +
    xm_qudsest2 + 
    wbgdp2011est1 + wbgdp2011est2 +
    mindist + capdist + gmlmidongoing + dyadigos),
  data = dyadic.mp.ally
)
# residual correlations are weaker than expected
summary(mp.trade.sur)


# trade balance
mp.balance.all <- lm(ihs_trade_balance ~ lag_ihs_trade_balance +
                       election*mean_leader_supp +
                       lag_election + lead_election + incumbent +
                       xm_qudsest2 + 
                       wbgdp2011est1 + wbgdp2011est2 +
                       mindist + capdist + gmlmidongoing + dyadigos,
                     data = dyadic.mp.ally)
summary(mp.balance.all)

# robust se
mp.balance.dr <- dyadRobust(mp.balance.all,
                                 dat = dyadic.mp.ally,
                                 dyadid = "dyad.id",
                                 egoid = "ccode",
                                 alterid = "ccode2")




### present results ###
# create dataframe with model results- all cluster robust SE

# function to tidy dr lists
dr.clean <- function(est){
data.clean <- cbind.data.frame(est$bhat, est$sehat)
colnames(data.clean) <- c("coef", "se")
data.clean$variable <- rownames(data.clean)
data.clean$model <-  deparse(substitute(est))
data.clean
}

# create a dataframe w/ coefficient estimates
mp.est <- bind_rows(
     dr.clean(mp.exports.dr),
     dr.clean(mp.chexports.dr),
     dr.clean(mp.chexports.fe.dr),
     dr.clean(mp.balance.dr)
  ) %>% # cut FE
  filter(str_detect(variable, "dyad.id", negate = T)) %>%
  filter(str_detect(variable, "Intercept", negate = T))

# nice names for plotting
coef.names.map = c("(Intercept)" = "Intercept",
                   "lag_ln_exports" = "Lag Ln(Exports)",
                   "lag_ln_imports" = "Lag Ln(Imports)",
                   "lag_ihs_trade_balance" = "Lag IHS(Balance)",
                   "election" = "Election",
                   "mean_leader_supp" = "Avg Leader Support", 
                   "election:mean_leader_supp" = "Election x Avg Leader Support",
                   "lag_election" = "Lag Election",
                   "lead_election" = "Lead Election", 
                   "incumbent" = "Incumbent",
                   "xm_qudsest2" = "Allied Democracy",
                   "wbgdp2011est1" = "Major Power GDP",
                   "wbgdp2011est2" = "Ally GDP",
                   "mindist" = "Minimum Distance",
                   "capdist" = "Capital Distance",
                   "gmlmidongoing" = "Ongoing MID",
                   "dyadigos" = "Shared IGOs")
mp.est$variable <- coef.names.map[mp.est$variable]

# model names
model.names.map <- c("mp.exports.dr" = "Ln(Exports)",
                     "mp.chexports.dr" = "Change Ln(Exports)",
                     "mp.chexports.fe.dr" ="Change Ln(Exports)\n & Dyad FE",
                     "mp.balance.dr" = "IHS(Trade Balance)")
mp.est$model <- model.names.map[mp.est$model]


# plot results
ggplot(mp.est, aes(y = factor(variable, ordered = T,
                              levels = rev(coef.names.map)),
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
ggsave("figures/mp-model-coefs.png", height = 6, width = 8)

# interaction terms only
ggplot(filter(mp.est, variable == "Election" | 
                variable == "Avg Leader Support" |
                variable == "Election x Avg Leader Support"),
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
me.leader.supp <- marginaleffects(mp.exports.all,
                vcov = mp.exports.dr$Vhat,
                variables = "mean_leader_supp",
                newdata = typical(election = c(0, 1)))
# changes in exports
me.ch.exports <- marginaleffects(mp.chexports.all,
                                 vcov = mp.chexports.dr$Vhat,
                                 variables = "mean_leader_supp",
                                 newdata = typical(election = c(0, 1)))

# changes in exports w/ fixed effects
me.chfe.exports <- marginaleffects(mp.chexports.fe,
                                 vcov = mp.chexports.fe.dr$Vhat,
                                 variables = "mean_leader_supp",
                                 newdata = typical(election = c(0, 1)))

# trade balance
me.balance <- marginaleffects(mp.balance.all,
                                   vcov = mp.balance.dr$Vhat,
                                   variables = "mean_leader_supp",
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
plot.me.ex  <- plot.me(model = me.leader.supp, label = "Ln(Exports)")
plot.me.ex
# changes
plot.ch.ex  <- plot.me(model = me.ch.exports, label = "Change Ln(Exports)")
plot.ch.ex
# changes w/ FE
plot.chfe.ex  <- plot.me(model = me.chfe.exports, label = "Change Ln(Exports) & Dyad FE")
plot.chfe.ex
# trade balance
plot.balance  <- plot.me(model = me.balance, label = "IHS(Trade Balance)")
plot.balance



# combine 
grid.arrange(plot.me.ex, plot.ch.ex, plot.chfe.ex, plot.balance, nrow = 2)
me.plots.mp <- arrangeGrob(plot.me.ex, plot.ch.ex, plot.chfe.ex,  plot.balance,
                           nrow = 2)
ggsave("figures/me-plots-mp.png", me.plots.mp, height = 8, width = 10)



# trade balance: imports and exports
summary(mp.trade.sur)
summary(mp.imports.all)
