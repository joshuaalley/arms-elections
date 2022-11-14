# Joshua Alley
# Analyze arms tranfers 


### log normal hurdle model
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
pred.usarms <- ggplot(us.arms.res[[2]], aes(y = fit, 
                         x = time_to_elec,
                         group = factor(atop_defense),
                         color = factor(atop_defense))) +
   scale_x_reverse() + # decreasing time to election
   #geom_hline(yintercept = 0) +
   geom_line() +
   geom_pointrange(aes(ymin = lwr, ymax = upr),
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
me.usarms <- ggplot(us.arms.res[[1]], aes(y = dydx, 
                       x = time_to_elec)) +
   scale_x_reverse() +
   geom_hline(yintercept = 0) +
   geom_line() +
   geom_pointrange(aes(
      ymin = dydx - 1.96*std.error,
      ymax = dydx + 1.96*std.error),
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




