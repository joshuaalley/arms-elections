# Joshua Alley
# Analyze which allies respond to elections 



# arms trade models
us.arms.ex <- rlm(us_arms ~ lag_us_arms +
                     time_to_elec*atop_defense + 
                     rep_pres + cold_war +
                     xm_qudsest2 +  cowmidongoing + dyadigos +
                     GDP_o + GDP_d + Distw + eu_member +
                     Comlang + Contig + Evercol,
                  maxit = 40,
                 data = us.trade.ally)
summary(us.arms.ex)


# changes in arms exports
us.arms.chex <- rlm(change_us_arms ~ 
                     time_to_elec*atop_defense + 
                     rep_pres + cold_war +
                     xm_qudsest2 +  cowmidongoing + dyadigos +
                     GDP_o + GDP_d + Distw + eu_member +
                     Comlang + Contig + Evercol,
                  data = us.trade.ally)
summary(us.arms.chex)

# ME and predicted values
us.arms.res <- me.us.elec(model = us.arms.ex,
                data = us.trade.ally)


# during cold war 
us.arms.ex.cw <- rlm(us_arms ~ lag_us_arms +
                     time_to_elec*atop_defense + 
                     rep_pres +
                     xm_qudsest2 +  cowmidongoing + dyadigos +
                     GDP_o + GDP_d + Distw + 
                     Comlang + Contig + Evercol,
                     maxit = 40,
                  data = filter(us.trade.ally, cold_war == 1))
summary(us.arms.ex.cw)


us.arms.cw.res <- me.us.elec(model = us.arms.ex.cw,
                          data = filter(us.trade.ally, cold_war == 1))

# post cold war
us.arms.ex.pcw <- rlm(us_arms ~ lag_us_arms +
                        time_to_elec*atop_defense + 
                        rep_pres +
                        xm_qudsest2 +  cowmidongoing + dyadigos +
                        GDP_o + GDP_d + Distw +
                         Comlang + Contig + Evercol,
                      maxit = 40,
                     data = filter(us.trade.ally, cold_war == 0))
summary(us.arms.ex.pcw)


us.arms.pcw.res <- me.us.elec(model = us.arms.ex.pcw,
                          data = filter(us.trade.ally, cold_war == 0))


# results
# combine predictions 
us.arms.pred <- bind_rows(
   "All Years" = us.arms.res[[2]],
   "Cold War" = us.arms.cw.res[[2]],
   "Post-Cold War" = us.arms.pcw.res[[2]],
   .id = "time"
)

# plot
ggplot(us.arms.pred, aes(y = fit, 
                         x = time_to_elec,
                         group = factor(atop_defense),
                         color = factor(atop_defense))) +
   facet_wrap(~ time, scales = "free_y") +
   scale_x_reverse() + # decreasing time to election
   #geom_hline(yintercept = 0) +
   geom_line() +
   geom_pointrange(aes(ymin = lwr, ymax = upr),
                   position = position_dodge(width = .1)) +
   scale_color_grey("Defense Pact", 
                    start = 0.7,
                    end = 0.1,
                    labels = c(`0` = "No", `1` = "Yes")) +
   labs(y = "Predicted Arms Exports",
        x = "Years to Election")


# combine marginal effects  
us.arms.me <- bind_rows(
   "All Years" = us.arms.res[[1]],
   "Cold War" = us.arms.cw.res[[1]],
   "Post-Cold War" = us.arms.pcw.res[[1]],
   .id = "time"
)

# plot
ggplot(us.arms.me, aes(y = dydx, 
                       x = time_to_elec)) +
   facet_wrap(~ time, scales = "free_y") +
   scale_x_reverse() +
   geom_hline(yintercept = 0) +
   geom_line() +
   geom_pointrange(aes(
      ymin = dydx - 1.96*std.error,
      ymax = dydx + 1.96*std.error),
      position = position_dodge(width = .1)) +
   labs(y = "Estimated Marginal Effect of Alliance")


# set up analysis with allied data

# us signals
us.signals <- read.csv("data/revise-latent-supp/Individual Signals Dataset.csv", header=TRUE) %>% 
               select(year, starts_with("us_"))

# us support
us.supp <- filter(latent.supp, ccode1 == 2) %>%
            rename(ccode = ccode2) %>%
            select(-year) %>%
            bind_cols(
               filter(us.signals, us_pact == 1)) %>%
            select(-us_arms)


ggplot(us.supp, aes(x = year, y = us_arms)) +
   facet_wrap(~ ccode, scales = "free_y") +
   geom_line(size = 1) +
   geom_vline(xintercept=c(pres.elections), linetype="dotted") 



# filter and clean us ally data
us.all.data <- filter(us.trade.ally, atop_defense == 1) %>%
                left_join(us.supp) %>%
               group_by(ccode, president) %>%
               mutate(
                # running mean support
                 mean_leader_supp = rollapply(median, 2, mean,
                                 align ='right', fill = lag_median),
                 total_leader_words = rollapply(us_words, 2, sum,
                              align ='right', fill = 0),
                 total_leader_vis = rollapply(us_vis, 2, sum,
                                      align ='right', fill = 0)
               ) %>%
              group_by(rep_pres, ccode) %>%
              mutate(
                mean_party_supp = rollapply(median, 2, mean,
                            align ='right', fill = lag_median)
              ) %>%
              ungroup() %>%
              select(
                change_ln_exports, change_ln_imports,
                change_trade, change_ihs_balance,
                v2clstown, mean_leader_supp, 
                mean_party_supp, us_words, us_arms, us_troops,
                us_nukes, us_vis, total_leader_words, total_leader_vis,
                  eu_member, near_elec, 
                  xm_qudsest2, cowmidongoing, dyadigos,
                  GDP_o, GDP_d, Distw,
                  rep_pres,
                  Comlang, Contig, Evercol
              ) %>% 
              drop_na()


# all panes
us.all.data[, 5:ncol(us.all.data)] <- apply(us.all.data[, 5:ncol(us.all.data)],
                              2, function(x)
                              arm::rescale(x, binary.inputs = "0/1"))


# separate models for near elections 
us.all.elec <- rlm(change_ln_exports ~ 
                     eu_member +  
                      us_arms + us_troops + us_nukes +
                      total_leader_vis + total_leader_words + 
                     xm_qudsest2 +  cowmidongoing + dyadigos +
                     GDP_o + GDP_d + Distw +
                     rep_pres +
                     Comlang + Contig + Evercol,
                   data = filter(us.all.data, near_elec == 1))
summary(us.all.elec)


# separate models for near elections: no election 
us.all.noelec <- rlm(change_ln_exports ~ 
                      eu_member +  
                        us_words + us_arms + us_troops +
                        us_nukes + us_vis +
                      xm_qudsest2 +  cowmidongoing + dyadigos +
                      GDP_o + GDP_d + Distw +
                      rep_pres +
                      Comlang + Contig + Evercol,
                   data = filter(us.all.data, near_elec == 0))
summary(us.all.noelec)

# varying slopes model- start with exports 


# compile model code
vars.us.ally <- cmdstan_model("data/vars-us-ally.stan",
                              cpp_options = list(stan_threads = TRUE))

# regressors matrices

# regressors: fixed
us.reg.fixed <- as.matrix(select(us.all.data, 
                                 xm_qudsest2, cowmidongoing, dyadigos,
                                 GDP_o, GDP_d, Distw,
                                 rep_pres,
                                 Comlang, Contig, Evercol))


us.reg.var <- as.matrix(select(us.all.data,
                               v2clstown, mean_leader_supp, 
                               eu_member))



table(us.all.data$near_elec) 
us.all.data$near_elec <- recode(us.all.data$near_elec,
                                `0` = 1, `1` = 2)
table(us.all.data$near_elec)

# data
us.ex.data <- list(
   y = us.all.data$change_ln_exports,
   N = nrow(us.all.data),
   P = ncol(us.reg.fixed),
   X = us.reg.fixed,
   J = length(unique(us.all.data$near_elec)),
   elec = us.all.data$near_elec,
   L = ncol(us.reg.var),
   Z = us.reg.var
   )

# stan model fit
fit.us.ex <- vars.us.ally$sample(
  data = us.ex.data,
  seed = 12,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = 4,
  threads_per_chain = 4,
  refresh = 200
)
fit.us.ex$cmdstan_diagnose()
fit.us.ex$cmdstan_summary()


# brms expression
us.all.priors <- c(set_prior("normal(0, 1)", class = "b"),
                   #set_prior("normal(0, 1)", class = "sd"),
                   set_prior("normal(0, 1)", class = "sigma"))
                   #set_prior("normal(0, 1)", class = "Intercept"))

us.ally.elec <- brm(change_ln_exports ~ 
                      v2clstown + mean_leader_supp + eu_member + near_elec + 
                      xm_qudsest2 +  cowmidongoing + dyadigos +
                      GDP_o + GDP_d + Distw +
                      rep_pres +
                      Comlang + Contig + Evercol,
                    data = us.all.data, 
                    prior = us.all.priors,
                    family = "student",
                    backend = "cmdstanr",
                    iter = 2000,
                    warmup = 1000,
                    cores = 4, 
                    chains = 4,
                    threads = threading(4),
                    control = list(
                      max_treedepth = 20
                    ))
