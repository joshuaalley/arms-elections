# Joshua Alley
# Models of allies, trade, and electoral cycles


### US Only ###


# check the raw data
ggplot(us.trade.ally, aes(x = total_trade)) + geom_histogram()
ggplot(us.trade.ally, aes(x = change_trade)) + geom_histogram()
ggplot(us.trade.ally, aes(x = trade_balance)) + geom_histogram()
ggplot(us.trade.ally, aes(x = ln_exports)) + geom_histogram()


# big spaghetti
ggplot(us.trade.ally, aes(x = year,
                          y = ln_exports,
                          color = factor(ccode))) + 
  geom_line() +
  theme(legend.position = NULL)


ggplot(us.trade.ally, aes(x = year,
                          y = ln_exports,
                          color = factor(ccode))) + 
  facet_wrap(~ ccode) +
  geom_line()


ggplot(us.trade.ally, aes(x = year,
                          y = ln_exports,
                          color = factor(ccode))) + 
  facet_wrap(~ atop_defense) +
  geom_line() +
  theme(legend.position = NULL)

# changes by ally/not
ggplot(us.trade.ally, aes(x = year,
                          y = change_ln_exports,
                          group = factor(ccode))) + 
  facet_wrap(~ atop_defense) +
  geom_line() +
  theme(legend.position = NULL)

# changes in loess without color
ggplot(us.trade.ally, aes(x = year,
                          y = change_ln_exports,)) + 
  facet_wrap(~ atop_defense) +
  geom_line(alpha = .5) +
  geom_smooth() +
  theme(legend.position = NULL)



# total trade: show cycles
us.trade.total <- us.trade.ally %>%
                   group_by(year) %>%
                   summarize(
                     #total_trade = sum(total_trade, na.rm = TRUE),
                     total_trade_change = sum(change_trade, na.rm = TRUE),
                     #total_exports = sum(ln_exports, na.rm = TRUE),
                     total_exports_change = sum(change_ln_exports, na.rm = TRUE),
                     #total_imports = sum(ln_imports, na.rm = TRUE),
                     total_imports_change = sum(change_ln_imports, na.rm = TRUE),
                     time_to_elec = min(time_to_elec),
                     cold_war = max(cold_war),
                     .groups = "keep"
                   ) %>% # remove pure NA w/ sum = 0
                  filter(year > 1950 & year < 2020) %>% 
                  ungroup() %>%   
                 # election cycle indicator
                 mutate(
                 elec.cycle = c(c(rep(1952, 2)), 
                    rep(
                      seq(from = 1956, to = 2016, by = 4), 
                      each = 4),
                    c(rep(2020, 3)))
                 ) %>%
                  pivot_longer(
                    cols = -c(year, time_to_elec, cold_war, elec.cycle),
                    names_to = "trade",
                    values_to = "value"
                  ) 

# plot by cycle- too much pasta here
ggplot(us.trade.total, aes(x = time_to_elec,
                           color = factor(elec.cycle),
                           y = value)) +
  geom_point() +
  geom_line() +
  scale_x_reverse() +
  facet_wrap(~ trade,
             labeller = labeller(trade = c("total_exports_change" = "Exports",
                                           "total_imports_change" = "Imports",
                                           "total_trade_change" = "Total Trade"))) +
  labs(y = "Annual Trade Change",
       x = "Years to Presidential Election")

# small multiples by year 
ggplot(us.trade.total, aes(x = time_to_elec,
                           color = factor(trade),
                           y = value)) +
  geom_point() +
  geom_line() +
  scale_x_reverse() +
  facet_wrap(~ elec.cycle, scales = "free_y") +
  scale_color_manual(values = wes_palette("GrandBudapest1"),
                     labels = c("total_exports_change" = "Exports",
                                "total_imports_change" = "Imports",
                                "total_trade_change" = "Total Trade")) +
  labs(y = "Annual Trade Change",
       x = "Years to Presidential Election",
       color = "Trade\nFlow",
       title = "Trade Changes by Presidential Election Cycle")




# aggregate cycles in box plot
ggplot(us.trade.total, aes(x = factor(time_to_elec,
                                      ordered = TRUE,
                                    levels = c("3", "2",
                                               "1", "0")), y = value)) +
  facet_wrap(~ trade,
             labeller = labeller(trade = c("total_exports_change" = "Exports",
                                           "total_imports_change" = "Imports",
                                           "total_trade_change" = "Total Trade"))) +
  geom_boxplot(outlier.shape = NA) +
  ylim(-10, 10) +
  labs(y = "Annual Trade Change",
       x = "Years to Presidential Election")
ggsave("figures/us-trade-cycles.png", height = 6, width = 8)

# simple correlation
cor.test(us.trade.ally$time_to_elec, us.trade.ally$change_ln_exports)
cor.test(us.trade.ally$time_to_elec, us.trade.ally$change_ln_imports)
cor.test(us.trade.ally$time_to_elec, us.trade.ally$change_trade)

# cold war split
ggplot(us.trade.total, aes(x = factor(time_to_elec), y = value)) +
  facet_wrap(~ trade + cold_war,
             nrow = 3,
             labeller = labeller(trade = c("total_exports_change" = "Exports",
                                           "total_imports_change" = "Imports",
                                           "total_trade_change" = "Total Trade"))) +
  geom_boxplot(outlier.shape = NA) +
  ylim(-15, 15) +
  labs(y = "Annual Trade Change",
       x = "Years to Presidential Election")


# alliance split 
# total trade: show cycles
us.trade.total.all <- us.trade.ally %>%
  group_by(year, atop_defense) %>%
  summarize(
    #total_trade = sum(total_trade, na.rm = TRUE),
    total_trade_change = sum(change_trade, na.rm = TRUE),
    #total_exports = sum(ln_exports, na.rm = TRUE),
    total_exports_change = sum(change_ln_exports, na.rm = TRUE),
    #total_imports = sum(ln_imports, na.rm = TRUE),
    total_imports_change = sum(change_ln_imports, na.rm = TRUE),
    time_to_elec = min(time_to_elec),
    .groups = "keep"
  ) %>% # remove pure NA w/ sum = 0
  filter(year >= 1950) %>%
  pivot_longer(
    cols = -c(year, time_to_elec, atop_defense),
    names_to = "trade",
    values_to = "value"
  )

ggplot(drop_na(us.trade.total.all, atop_defense), aes(x = factor(time_to_elec,
                                      ordered = TRUE,
                                      levels = c("3", "2",
                                                 "1", "0")), 
                           y = value, 
                           color = factor(atop_defense))) +
  facet_wrap(~ trade,
             labeller = labeller(trade = c("total_exports_change" = "Exports",
                                           "total_imports_change" = "Imports",
                                           "total_trade_change" = "Total Trade"))) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_grey(start = .8, end = .4) +
  ylim(-6, 6) +
  labs(y = "Annual Trade Change",
       x = "Years to Presidential Election",
       color = "Defensive Alliance")
ggsave("figures/us-trade-cycles-all.png", height = 6, width = 8)


# complete cases to use robust lm w
# complete cases of dyad data: changes and rescaled continuous regressors
comp.us.elec <- function(data){
  out <- data %>% select(change_ln_exports, change_ln_imports,
                          change_ihs_balance, change_trade,
                          time_to_elec, atop_defense, rep_pres,
                          lag_election, lead_election, cold_war,
                          xm_qudsest2,  cowmidongoing, dyadigos,
                          change_gdp_o, change_gdp_d, Distw, eu_member, 
                          us_arms, lag_us_arms,
                          Comlang, Contig, Evercol) %>%
                 drop_na()
  out[, 9:ncol(out)] <- apply(out[, 9:ncol(out)], 2, function(x) 
                          arm::rescale(x, 
                              binary.inputs = "0/1"))
  out
}

### exports 
# model w/o changes or transformation 
us.exports.elec <- rlm(exports ~ lag_exports + lag_imports +
                        time_to_elec*atop_defense + rep_pres +
                         cold_war +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw + eu_member +
                        Comlang + Contig + Evercol,
                       maxit = 40,
                      data = us.trade.ally)
summary(us.exports.elec)


# model w/ log
us.lnexports.elec <- rlm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                          time_to_elec*atop_defense + rep_pres +
                           cold_war +
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          GDP_o + GDP_d + Distw + eu_member +
                          Comlang + Contig + Evercol,
                         maxit = 40,
                        data = us.trade.ally)
summary(us.lnexports.elec)


# close to unit root, so changes
# model w/ log
us.chexports.elec <- rlm(change_ln_exports ~ 
                          time_to_elec*atop_defense + rep_pres +
                           cold_war +
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          change_gdp_o + change_gdp_d + Distw + eu_member +
                          Comlang + Contig + Evercol,
                        data = comp.us.elec(us.trade.ally))
summary(us.chexports.elec)



# cold war threat
# model w/ log
us.chexports.elec.cw <- rlm(change_ln_exports ~ 
                           time_to_elec*atop_defense + rep_pres +
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           change_gdp_o + change_gdp_d + Distw + eu_member +
                           Comlang + Contig + Evercol,
                         data = filter(us.trade.ally, cold_war == 1))
summary(us.chexports.elec.cw)


us.chexports.elec.pcw <- rlm(change_ln_exports ~ 
                              time_to_elec*atop_defense + rep_pres +
                              xm_qudsest2 +  cowmidongoing + dyadigos +
                              change_gdp_o + change_gdp_d + Distw + eu_member +
                              Comlang + Contig + Evercol,
                            data = filter(us.trade.ally, cold_war == 0))
summary(us.chexports.elec.pcw)



### imports 
# model w/o changes or transformation 
us.imports.elec <- rlm(imports ~ lag_imports + lag_exports +
                        time_to_elec*atop_defense + rep_pres +
                         cold_war +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw + eu_member +
                        Comlang + Contig + Evercol,
                       maxit = 40,
                      data = us.trade.ally)
summary(us.imports.elec)


# model w/ log
us.lnimports.elec <- rlm(ln_imports ~ lag_ln_imports + lag_ln_exports +
                          time_to_elec*atop_defense + rep_pres +
                         cold_war +
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          change_gdp_o + change_gdp_d + Distw + eu_member +
                          Comlang + Contig + Evercol,
                         maxit = 40,
                        data = us.trade.ally)
summary(us.lnimports.elec)

# close to unit root, so changes
# model w/ log
us.chimports.elec <- rlm(change_ln_imports ~ 
                          time_to_elec*atop_defense + rep_pres +
                           cold_war +
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          change_gdp_o + change_gdp_d + Distw + eu_member +
                          Comlang + Contig + Evercol,
                         data = comp.us.elec(us.trade.ally))
summary(us.chimports.elec)



### total trade and trade balance
# close to unit root, so changes in trade
us.chtrade.elec <- rlm(change_trade ~ 
                        time_to_elec*atop_defense + rep_pres +
                         cold_war +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw + eu_member +
                        Comlang + Contig + Evercol,
                       data = comp.us.elec(us.trade.ally))
summary(us.chtrade.elec)


# changes in trade balance
us.balance.elec <- rlm(change_ihs_balance ~ 
                        time_to_elec*atop_defense + rep_pres +
                         cold_war +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw + eu_member +
                        Comlang + Contig + Evercol,
                       data = comp.us.elec(us.trade.ally))
summary(us.balance.elec)



### present results 



# create a dataframe w/ coefficient estimates
us.coef.est <- bind_rows(
    "Change Trade" = as.data.frame(summary(us.chtrade.elec)[["coefficients"]]),
    "Change Exports" = as.data.frame(summary(us.chexports.elec)[["coefficients"]]),
    "Change Imports" = as.data.frame(summary(us.chimports.elec)[["coefficients"]]),
    "Change Trade Balance" = as.data.frame(summary(us.balance.elec)[["coefficients"]]),
    .id = "model"
    ) %>%
   rownames_to_column("variable") %>%
   mutate(
     variable = gsub("\\...[0-9]*$","", variable)
   ) %>% 
    # cut intercept terms
  filter(str_detect(variable, "Intercept", negate = T))


modelsummary(list("Change Exports" = us.chexports.elec,
                  "Change Imports" = us.chimports.elec,
                  "Change Trade" = us.chtrade.elec,
                  "Change Balance" = us.balance.elec),
             coef_map =  coef.names.map,
             estimate = "{estimate}",
             statistic = "({conf.low}, {conf.high})",
             gof_omit = "^(?!R2|Num)",
             output = "latex")

# nice names for plotting
us.coef.est$variable <- coef.names.map[us.coef.est$variable]
colnames(us.coef.est) <- c("variable", "model", "coef", "se",
                           "t-value")

# plot coefficients
ggplot(us.coef.est, aes(y = factor(variable, ordered = T,
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
ggsave("figures/us-trade-coefs.png", height = 6, width = 9)

# interaction terms only
ggplot(filter(us.coef.est, variable == "Years to Election" | 
                variable == "Defense Pact" |
                variable == "Defense Pact x Years to Election"),
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


# typical observations for US
typical.func.us <- function(x){
  dat <- typical(model = x, time_to_elec = c(0, 1, 2, 3),
                 atop_defense = c(0, 1)) 
  dat$rep_pres <- 0
  dat 
}

# Marginal effects function 
me.us.elec <- function(model, formula, rm.wt, data){
  
  # no dyad robust for US 
  # marginal effects 
  me.est <- marginaleffects(model,
                  variables = "atop_defense",
                  newdata = typical.func.us(model))
  
  me.def.plot <- plot_cme(model, effect = "atop_defense", 
           condition = "time_to_elec")
  
  # predicted outcomes
  pred.out <- bind_cols(as.data.frame(
                          predict(model, 
                          newdata = typical.func.us(model),
                          interval = "confidence")
                          ),
                          typical.func.us(model))
  
  res <- list(me.est, pred.out)
}

# exports 
exports.res <- me.us.elec(model = us.chexports.elec,
                           data = us.trade.ally)

# imports
imports.res <- me.us.elec(model = us.chimports.elec,
                           data = us.trade.ally)

# total trade
trade.res <- me.us.elec(model = us.chtrade.elec,
                           data = us.trade.ally)

# trade balance 
balance.res <- me.us.elec(model = us.balance.elec,
                data = us.trade.ally)


# combine predictions 
us.elec.pred <- bind_rows(
          "Change Exports" = exports.res[[2]],
          "Change Imports" = imports.res[[2]],
          "Change Trade" = trade.res[[2]],
          "Trade Balance" = balance.res[[2]],
                .id = "outcome"
)

# plot
ggplot(us.elec.pred, aes(y = fit, 
                    x = time_to_elec,
                    group = factor(atop_defense),
                    color = factor(atop_defense))) +
  facet_wrap(~ outcome) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = lwr, ymax = upr),
                   position = position_dodge(width = .1)) +
  scale_color_grey("Defense Pact", 
                  start = 0.7,
                  end = 0.1,
                  labels = c(`0` = "No", `1` = "Yes")) +
  labs(y = "Predicted Outcome",
       x = "Years to Presidential Election")
ggsave("figures/us-elec-pred.png", height = 6, width = 8)


# reverse predictions
# plot
ggplot(us.elec.pred, aes(y = fit, 
                         x = factor(atop_defense),
                         group = factor(time_to_elec),
                         color = factor(time_to_elec))) +
  facet_wrap(~ outcome) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = lwr, ymax = upr),
                  position = position_dodge(width = .1)) +
  scale_color_grey("Years to Election", 
                   start = 0.7,
                   end = 0.1) +
  scale_x_discrete(labels = c(`0` = "No", `1` = "Yes")) +
  labs(y = "Predicted Outcome",
       x = "Defense Pact")



# combine marginal effects  
us.elec.me <- bind_rows(
  "Change Exports" = exports.res[[1]],
  "Change Imports" = imports.res[[1]],
  "Change Trade" = trade.res[[1]],
  "Trade Balance" = balance.res[[1]],
  .id = "outcome"
)

# plot
ggplot(us.elec.me, aes(y = dydx, 
                  x = time_to_elec)) +
  facet_wrap(~ outcome) +
  scale_x_reverse() +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(
    ymin = dydx - 1.96*std.error,
    ymax = dydx + 1.96*std.error),
    position = position_dodge(width = .1)) +
  labs(y = "Estimated Marginal Effect of Alliance",
       x = "Years to Presidential Election")
ggsave("appendix/us-defense-me.png", height = 6, width = 8)





# using dyadic.trade.major data: all mp and non-mp dyads 
# outside the US. This is totally a US story

dyadic.major.nous <- filter(dyadic.trade.major, ccode1 != 2)

### exports 

# close to unit root, so changes
# model w/ log
mp.chexports.elec <- rlm(change_ln_exports ~ change_ln_imports +
                           election*atop_defense +
                           lag_election + lead_election + 
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           change_gdp_o + change_gdp_d + Distw + eu_member +
                           Comlang + Contig + Evercol,
                         data = dyadic.major.nous)
summary(mp.chexports.elec)



### imports 

# close to unit root, so changes
# model w/ log
mp.chimports.elec <- rlm(change_ln_imports ~ change_ln_exports +
                           election*atop_defense +
                           lag_election + lead_election + 
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           change_gdp_o + change_gdp_d + Distw + eu_member +
                           Comlang + Contig + Evercol,
                         data = dyadic.major.nous)
summary(mp.chimports.elec)



### total trade and trade balance
# close to unit root, so changes in trade
mp.chtrade.elec <- rlm(change_trade ~ 
                         election*atop_defense +
                         lag_election + lead_election + 
                         xm_qudsest2 +  cowmidongoing + dyadigos +
                         change_gdp_o + change_gdp_d + Distw + eu_member +
                         Comlang + Contig + Evercol,
                       data = dyadic.major.nous)
summary(mp.chtrade.elec)


# IHS transformed trade balance changes
mp.balance.elec <- rlm(change_ihs_balance ~ 
                         election*atop_defense +
                         lag_election + lead_election + 
                         xm_qudsest2 +  cowmidongoing + dyadigos +
                         change_gdp_o + change_gdp_d + Distw + eu_member +
                         Comlang + Contig + Evercol,
                       data = dyadic.major.nous)
summary(mp.balance.elec)


### present results 
# typical observations for US
typical.func.all <- function(x){
  dat <- typical(model = x, election = c(0, 1),
                 atop_defense = c(0, 1))
  dat$lag_election <- 0
  dat$lead_election <- 0
  dat 
}

# Marginal effects function 
me.all.elec <- function(model, formula, rm.wt, data){
  
  form <- as.formula(formula)
  # change enviro to capture weights
  environment(form) <- environment()
  # first, dyad-robust standard errors
  # correct se: add rlm weights to OLS
  est.dr <- dyadRobust(lm(form,
                          weights = rm.wt,
                          data = comp.us.elec(data)
  ),
  dat = comp.us.elec(data),
  dyadid = "dyad.id",
  egoid = "us.code",
  alterid = "ccode")
  
  
  # marginal effects 
  me.est <- marginaleffects(model,
                            vcov = est.dr$Vhat,
                            variables = "time_to_elec",
                            newdata = typical.func.us(model))
  
  # predicted outcomes
  pred.out <- bind_cols(as.data.frame(
    predict(model, 
            newdata = typical.func.us(model),
            interval = "confidence")
  ),
  typical.func.us(model))
  
  res <- list(me.est, pred.out)
}

