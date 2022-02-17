# Joshua Alley
# Models of allies, trade, and electoral cycles


### US Only ### 


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
                     time_to_elec = min(time_to_elec)
                   ) %>% # remove pure NA w/ sum = 0
                  filter(year >= 1950) %>%
                  pivot_longer(
                    cols = -c(year, time_to_elec),
                    names_to = "trade",
                    values_to = "value"
                  )


ggplot(us.trade.total, aes(x = year, y = value,
                           group = trade, color = trade)) +
  geom_line(size = 1, alpha = .75) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") 

ggplot(us.trade.total, aes(x = factor(time_to_elec), y = value)) +
  facet_wrap(~ trade,
             labeller = labeller(trade = c("total_exports_change" = "Exports",
                                   "total_imports_change" = "Imports",
                                   "total_trade_change" = "Total Trade"))) +
  geom_boxplot(outlier.shape = NA) +
  ylim(-15, 15) +
  labs(y = "Annual Trade Change",
       x = "Years to Presidential Election")
ggsave("figures/us-trade-cycles.png", height = 6, width = 8)


# complete cases to use robust lm w
# complete cases of dyad data: changes and rescaled continuous regressors
comp.us.elec <- function(data){
  out <- data %>% select(change_ln_exports, change_ln_imports,
                          change_ihs_balance, change_trade,
                          time_to_elec, atop_defense, rep_pres,
                          lag_election, lead_election, 
                          xm_qudsest2,  cowmidongoing, dyadigos,
                          GDP_o, GDP_d, Distw,
                          Comlang, Contig, Evercol) %>%
                 drop_na()
  out[, 9:20] <- apply(out[, 9:20], 2, function(x) 
                          arm::rescale(x, 
                              binary.inputs = "0/1"))
  out
}

### exports 
# model w/o changes or transformation 
us.exports.elec <- rlm(exports ~ lag_exports + lag_imports +
                        time_to_elec + atop_defense + rep_pres +
                        lag_election + lead_election + incumbent +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw +
                        Comlang + Contig + Evercol,
                       maxit = 40,
                      data = us.trade.ally)
summary(us.exports.elec)


# model w/ log
us.lnexports.elec <- rlm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                          time_to_elec + atop_defense + rep_pres +
                          lag_election + lead_election + incumbent +
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          GDP_o + GDP_d + Distw +
                          Comlang + Contig + Evercol,
                         maxit = 40,
                        data = us.trade.ally)
summary(us.lnexports.elec)


# close to unit root, so changes
# model w/ log
us.chexports.elec <- rlm(change_ln_exports ~ change_ln_imports +
                          time_to_elec*atop_defense + rep_pres +
                          lag_election + lead_election + 
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          GDP_o + GDP_d + Distw +
                          Comlang + Contig + Evercol,
                        data = comp.us.elec(us.trade.ally))
summary(us.chexports.elec)



### imports 
# model w/o changes or transformation 
us.imports.elec <- rlm(imports ~ lag_imports + lag_exports +
                        time_to_elec + atop_defense + rep_pres +
                        lag_election + lead_election + incumbent +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw +
                        Comlang + Contig + Evercol,
                       maxit = 40,
                      data = us.trade.ally)
summary(us.imports.elec)


# model w/ log
us.lnimports.elec <- rlm(ln_imports ~ lag_ln_imports + lag_ln_exports +
                          time_to_elec + atop_defense + rep_pres +
                          lag_election + lead_election + incumbent +
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          GDP_o + GDP_d + Distw +
                          Comlang + Contig + Evercol,
                         maxit = 40,
                        data = us.trade.ally)
summary(us.lnimports.elec)

# close to unit root, so changes
# model w/ log
us.chimports.elec <- rlm(change_ln_imports ~ change_ln_exports +
                          time_to_elec*atop_defense + rep_pres +
                          lag_election + lead_election + 
                          xm_qudsest2 +  cowmidongoing + dyadigos +
                          GDP_o + GDP_d + Distw +
                          Comlang + Contig + Evercol,
                         data = comp.us.elec(us.trade.ally))
summary(us.chimports.elec)



### total trade and trade balance
# close to unit root, so changes in trade
us.chtrade.elec <- rlm(change_trade ~ 
                        time_to_elec*atop_defense + rep_pres +
                        lag_election + lead_election + 
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw +
                        Comlang + Contig + Evercol,
                       data = comp.us.elec(us.trade.ally))
summary(us.chtrade.elec)


# close to unit root, so changes in trade
us.balance.elec <- rlm(change_ihs_balance ~ 
                        time_to_elec*atop_defense + rep_pres +
                        lag_election + lead_election + 
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw +
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
       x = "Years to Election")
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
  labs(y = "Estimated Marginal Effect of Alliance")
ggsave("figures/us-defense-me.png", height = 6, width = 8)





# using dyadic.trade.major data: all mp and non-mp dyads 
# outside the US. This is totally a US story

dyadic.major.nous <- filter(dyadic.trade.major, ccode1 != 2)

### exports 
# model w/o changes or transformation 
mp.exports.elec <- rlm(exports ~ lag_exports + lag_imports +
                         election + atop_defense +
                         lag_election + lead_election + incumbent +
                         xm_qudsest2 +  cowmidongoing + dyadigos +
                         GDP_o + GDP_d + Distw +
                         Comlang + Contig + Evercol,
                       maxit = 40,
                       data = dyadic.major.nous)
summary(mp.exports.elec)


# model w/ log
mp.lnexports.elec <- rlm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                           election + atop_defense +
                           lag_election + lead_election + incumbent +
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           GDP_o + GDP_d + Distw +
                           Comlang + Contig + Evercol,
                         maxit = 40,
                         data = dyadic.major.nous)
summary(mp.lnexports.elec)


# close to unit root, so changes
# model w/ log
mp.chexports.elec <- rlm(change_ln_exports ~ change_ln_imports +
                           election*atop_defense +
                           lag_election + lead_election + 
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           GDP_o + GDP_d + Distw +
                           Comlang + Contig + Evercol,
                         data = dyadic.major.nous)
summary(mp.chexports.elec)



### imports 
# model w/o changes or transformation 
mp.imports.elec <- rlm(imports ~ lag_imports + lag_exports +
                         election + atop_defense +
                         lag_election + lead_election + incumbent +
                         xm_qudsest2 +  cowmidongoing + dyadigos +
                         GDP_o + GDP_d + Distw +
                         Comlang + Contig + Evercol,
                       maxit = 40,
                       data = dyadic.major.nous)
summary(mp.imports.elec)


# model w/ log
mp.lnimports.elec <- rlm(ln_imports ~ lag_ln_imports + lag_ln_exports +
                           election + atop_defense +
                           lag_election + lead_election + incumbent +
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           GDP_o + GDP_d + Distw +
                           Comlang + Contig + Evercol,
                         maxit = 40,
                         data = dyadic.major.nous)
summary(mp.lnimports.elec)

# close to unit root, so changes
# model w/ log
mp.chimports.elec <- rlm(change_ln_imports ~ change_ln_exports +
                           election*atop_defense +
                           lag_election + lead_election + 
                           xm_qudsest2 +  cowmidongoing + dyadigos +
                           GDP_o + GDP_d + Distw +
                           Comlang + Contig + Evercol,
                         data = dyadic.major.nous)
summary(mp.chimports.elec)



### total trade and trade balance
# close to unit root, so changes in trade
mp.chtrade.elec <- rlm(change_trade ~ 
                         election*atop_defense +
                         lag_election + lead_election + 
                         xm_qudsest2 +  cowmidongoing + dyadigos +
                         GDP_o + GDP_d + Distw +
                         Comlang + Contig + Evercol,
                       data = dyadic.major.nous)
summary(mp.chtrade.elec)


# IHS transformed trade balance changes
mp.balance.elec <- rlm(change_ihs_balance ~ 
                         election*atop_defense +
                         lag_election + lead_election + 
                         xm_qudsest2 +  cowmidongoing + dyadigos +
                         GDP_o + GDP_d + Distw +
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

