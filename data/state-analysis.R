# Joshua Alley
# Elections and state cycles in defense contracting


### raw data 
table(state.data$time_to_elec)
table(state.data$swing)
table(state.data$core)


# plot obligations
ggplot(state.data, aes(x = ln_obligations)) + geom_histogram()
ggplot(state.data, aes(x = change_ln_obligations)) + geom_histogram()


ggplot(state.data, 
       aes(y = ln_obligations,
           x = s_comp)) +
  geom_point() 

# over time

# all states 
ggplot(state.data, aes(x = year, y = ln_obligations,
                       group = state)) +
  geom_line()


# small multiples
ggplot(state.data, aes(x = year, y = ln_obligations)) +
  facet_wrap(~ state) + 
  geom_line()


ggplot(state.data, aes(x = year, y = change_ln_obligations)) +
  facet_wrap(~ state) + 
  geom_line()


# specific sectors
ggplot(contracts.state.long %>% 
         filter(name != "other" & 
                  name != "ln_obligations_other" &
                  name != "ln_obligations" &
                  name != "lag_ln_obligations" &
                  name != "change_ln_obligations"),
       aes(x = year, y = value,
           group = name,
           color = name)) +
  facet_wrap(~ state, scales = "free_y") +
  geom_line()


ggplot(contracts.state.long %>% 
         filter(name != "other" & 
                  name != "ln_obligations_other" &
                  name != "ln_obligations" &
                  name != "lag_ln_obligations" &
                  name != "change_ln_obligations"),
       aes(x = name, y = value)) +
  geom_boxplot()


# mean and SD summary
contracts.var.sum <- contracts.state.long %>% 
                      group_by(name) %>%
                      summarize(
                        sd = sd(value, na.rm = TRUE),
                        mean = mean(value, na.rm = TRUE),
                        sd_mean = sd / mean 
                      )


ggplot(contracts.state.long %>% 
         filter(name != "other" & 
                  name != "ln_obligations_other" &
                  name != "ln_obligations" &
                  name != "lag_ln_obligations" &
                  name != "change_ln_obligations"),
       aes(x = factor(year), y = value)) +
  facet_wrap(~ name) +
  geom_boxplot()



# swing vs core- changes by election proximity
# overall levels by year
ggplot(drop_na(state.data, time_to_elec),
       aes(x = factor(year), y = ln_obligations,
           fill = factor(comp.sum))) +
  geom_boxplot()

# changes by election proximity
ggplot(drop_na(state.data, time_to_elec),
       aes(x = factor(time_to_elec), y = ln_obligations,
           fill = factor(comp.sum))) +
  geom_boxplot()


ggplot(drop_na(state.data, time_to_elec),
       aes(x = factor(time_to_elec), y = change_ln_obligations,
           fill = factor(comp.sum))) +
  geom_boxplot()



# elections and contracts numbers: expected negative coef
contracts.ts.reg <- lm(all_contracts ~ lag_all_contracts +
                         time_to_elec,
                       data = contracts.data.clean)
summary(contracts.ts.reg)


ggplot(contracts.data.clean, aes(x = year,
                                 color = factor(elec.cycle),
                                 y = all_contracts)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  labs(y = "Total Prime Contracts",
       x = "Year",
       title = "Aggregate Defense Contracting",
       color = "Election Cycle")
summary(contracts.data.clean$all_contracts)

# summarize by election proximity- look within cycles
contracts.elec.agg <- ggplot(contracts.data.clean, aes(x = time_to_elec,
                                                       color = factor(elec.cycle),
                                                       y = all_contracts)) +
  geom_point() +
  geom_line() +
  scale_x_reverse() +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  labs(y = "Total Prime Contracts",
       x = "Years to Presidential Election",
       title = "Aggregate Defense Contracting",
       color = "Election Cycle")
contracts.elec.agg

# cycles by type of contract
contracts.data.key <- select(contracts.data.clean,
                             year, time_to_elec, missile_space,
                             aircraft, vehicles, arms,
                             electronics, ships, elec.cycle) %>% 
  pivot_longer(
    -c(year, time_to_elec, elec.cycle),
    names_to = "allocation",
    values_to = "value"
  )

# plot over time
ggplot(contracts.data.key, aes(x =  year,
                               y = value,
                               group = allocation,
                               color = allocation)) +
  geom_line(linewidth = 2) +
  labs(y = "Total Defense Contracts",
       x = "Year",
       title = "Defense Contracting Allocations over time")

contracts.elec.sector <- ggplot(contracts.data.key, aes(x = time_to_elec,
                                                        color = factor(elec.cycle),
                                                        y = value)) +
  geom_point() +
  geom_line() +
  scale_x_reverse() +
  facet_wrap(~ allocation, scales = "free_y",
             labeller = labeller(allocation = 
                                   c("aircraft" = "Aircraft",
                                     "missile_space" = "Missiles & Space",
                                     "electronics" = "Electronics",
                                     "ships" = "Ships",
                                     "vehicles" = "Vehicles",
                                     "arms"= "Weapons & Ammo"))
  ) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  labs(y = "Total Prime Contracts",
       x = "Years to Presidential Election",
       title = "Sectoral Defense Contracting",
       color = "Election Cycle")
contracts.elec.sector

# combine sectoral and aggregate plots
grid.arrange(contracts.elec.agg, contracts.elec.sector,
             nrow = 1,
             layout_matrix = rbind(c(1, 1, 2, 2, 2),
                                   c(1, 1, 2, 2, 2),
                                   c(1, 1, 2, 2, 2)))
contract.cycles <- arrangeGrob(contracts.elec.agg, contracts.elec.sector,
                               nrow = 1,
                               layout_matrix = rbind(c(1, 1, 2, 2, 2),
                                                     c(1, 1, 2, 2, 2),
                                                     c(1, 1, 2, 2, 2)))
ggsave("appendix/contract-cycles.png", contract.cycles, height = 6, width = 8)




# overall electoral competition 

# missing data is: 
visdat::vis_miss(select(state.data,
                        ln_obligations, s_comp, incumbent,
                          diff_vote_share, time_to_elec,
                          swing, core, rep_pres,
                          pivot_prox,
                          poptotal, ln_ngdp, iraq_war))

# simple model: OLS
elec.lm <- lm(ln_obligations ~ lag_ln_obligations +
                swing + 
                core +
                rep_pres +
                poptotal + ln_ngdp + iraq_war,
              data = state.data) 
summary(elec.lm)


# OLS w/ state FE
elec.lm.st <- lm(ln_obligations ~ lag_ln_obligations +
                swing + 
                core +
                rep_pres +
                poptotal + ln_ngdp + iraq_war + 
                factor(state),
              data = state.data) 
summary(elec.lm.st)

# robust
elec.rlm <- rlm(ln_obligations ~ lag_ln_obligations +
                  swing + core +
                  rep_pres +
                  poptotal + ln_ngdp + iraq_war,
              data = state.data) 
summary(elec.rlm)


# plot/tabulate results
comp.res.state <- list(elec.lm, elec.rlm)
names(comp.res.state) <- c("OLS", "Robust")
modelplot(comp.res.state, coef_map = coef.names.map.state)
modelsummary(comp.res.state,
             coef_map = coef.names.map.state,
             # statistic = c("conf.int",
             #               "s.e. = {std.error}", 
             #               "t = {statistic}"),
             gof_omit = "IC|R2|Log.Lik.|F",
             note = "Standard Error in Parentheses",
             title = "Electoral Competition and Defense Contracting: 2001-2020")

mfx.lm.state <- avg_slopes(elec.lm)
mfx.rlm.state <- avg_slopes(elec.rlm)

comp.mfx.state <- list(mfx.lm.state, mfx.rlm.state)
names(comp.mfx.state) <- c("OLS", "Robust")

modelsummary(comp.mfx.state,
             shape = term + model ~ statistic,
             statistic = c("{std.error}",
                           "{statistic}"),
             coef_map = coef.names.map.state,
             gof_map = NA,
             title = "\\label{tab:state-res} Marginal Effects of Electoral Competition on Defense Contracting Awards: 2001-2020")

# calculate LRMs 
# summary first
cont.res.lm <- summary(elec.lm)
cont.res.rlm <- summary(elec.rlm)

lrm.calc <- function(mod, est){
  
  coef <- as.data.frame(est[["coefficients"]][, 1:3])
  colnames(coef) <- c("est", "se", "t")
  coef$lrm <- NA
  
  for(i in 1:nrow(coef)){
  
  coef$lrm[i] <- (coef$est[i]) /(1 -  coef$est[2])
  
  # formula for deltamethod 
  formula <- as.formula(paste0("~ (x", i, ") / (1 - x2)"))
  
  coef$lrm.se[i] <- msm::deltamethod(g = formula,
                                  mean = coef(mod),
                                  cov = vcov(mod))
  
}
  # output
  coef$var <- rownames(coef)
  # nice names
  coef$var <- coef.names.map.state[coef$var]
  # drop LDV
  coef <- coef[3:nrow(coef), ]
  coef # final output
}

est.lm <- lrm.calc(mod = elec.lm, est = cont.res.lm)
est.lm
est.rlm <- lrm.calc(mod = elec.rlm, est = cont.res.rlm)
est.rlm

# lrm estimates
lrm.all <- bind_rows(OLS = est.lm,
                     Robust = est.rlm,
                     .id = "Model")
ggplot(lrm.all, aes(y = var, x = lrm,
                    color = Model)) +
  geom_pointrange(aes(xmin = lrm - 1.96*lrm.se,
                      xmax = lrm + 1.96*lrm.se),
                  position = position_dodge(width = .5))

# Presidential vote 

# plot obligations: loess 
ggplot(drop_na(state.data, time_to_elec), 
       aes(group = factor(time_to_elec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           color = factor(time_to_elec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = ln_obligations,
           x = diff_vote_share)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom")


# plot obligations: linear
ggplot(drop_na(state.data, time_to_elec), 
       aes(group = factor(time_to_elec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           color = factor(time_to_elec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = ln_obligations,
           x = diff_vote_share)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "bottom")

# changes 
ggplot(drop_na(state.data, time_to_elec), 
       aes(group = factor(time_to_elec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           color = factor(time_to_elec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = change_ln_obligations,
           x = diff_vote_share)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom")



# allow impact of electoral competition variables to shift over time

comp.time <- brm(bf(ln_obligations ~ #ar(time = year,
                                            #gr = state,
                                            #p = 1) +
                   (1 | year) +    
                   (1 + lag_ln_obligations | state) +
                  (swing + core | iraq_war) +
                   swing + core +
                   rep_pres  +
                   poptotal + ln_ngdp,
                  sigma ~ swing + core),
                 family = student(),
                 prior = c(
                   set_prior("normal(0, 2)", class = "b"),
                   set_prior("normal(0, 2)", class = "sd")
                   #set_prior("normal(0, .3)", class = "ar")
                   ),
                 data = state.data,
                 cores = 4,
                 backend = "cmdstanr",
                 control = list(
                   adapt_delta = .99,
                   max_treedepth = 20)
                 ) 
summary(comp.time)
coefs.comp <- coef(comp.time) 
coefs.comp[["iraq_war"]]

coefs.var.state <- bind_rows(Swing = as.data.frame(coefs.comp$iraq_war[, , 2]),
                             Core = as.data.frame(coefs.comp$iraq_war[, , 3]),
                             .id = "Variable")
coefs.var.state$war <- factor(c("No", "Yes"),
                              ordered = TRUE,
                              levels = c("Yes", "No"))

ggplot(coefs.var.state, aes(x = war, y = Estimate)) +
  facet_wrap(~ Variable) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = Q2.5,
                      ymax = Q97.5)) +
  labs(
    x = "At War?"
  )





### state data with arms deals
state.data.deals <- left_join(state.data, 
                              select(arms.deals.year,
                                     year, deals))
  


# add deals to model
# contracting from time to presidential elections and pivot proximity
deals.state <- lm(ln_obligations ~ lag_ln_obligations +
                    deals +
                    swing + core + 
                    rep_pres +
                    poptotal + ln_ngdp + iraq_war,
                   data = state.data.deals) 
summary(deals.state)



### examine state contracts by sector

# this puts orders before contracts
sector.list <- c("aircraft", "arms", "electronics", "missile_space",
                 "ships", "vehicles")
formula.sector <- vector(mode = "list", length = length(sector.list))
  
for(i in 1:length(sector.list)){
  formula.sector[[i]] <- as.formula(
    paste(
      paste0(sector.list[i], "~"), 
      paste0(sector.list[i], "_lag"), 
      #paste0(sector.list[i], "_change ", "~"), 
      #paste0(" + ", "deals_", sector.list[i]),
      paste0(" + ", "swing + core"),
      paste0("+ iraq_war + rep_pres + ln_ngdp + poptotal")
    ))
  }

sector.state.sys <- systemfit(formula.sector, data = state.data.ord)
summary(sector.state.sys)
