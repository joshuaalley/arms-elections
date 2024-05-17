# Joshua Alley
# Analyze arms deals 


### model arms deals 
pois.deals.cycle <- brm(bf(deals ~ 
                             time_to_elec_0 +
                             time_to_elec_1 +
                             time_to_elec_2 +
                             ally + v2x_polyarchy +
                            cold_war + gwot +
                            rep_pres + 
                            ln_petrol_rev + 
                            ln_rgdp + cowmidongoing +
                            ln_pop + ln_distw + 
                            Comlang,
                          hu ~ ally + v2x_polyarchy + cowmidongoing + ln_rgdp,
                          center = FALSE),
                       family = hurdle_poisson(),
                       backend = "cmdstanr",
                       prior = c(prior(normal(0, .5), class = "b")),
                       cores = 4,
                       refresh = 500,
                       data = us.deals.comp)
summary(pois.deals.cycle)

pois.cycle.pred <- year.dum.pred(pois.deals.cycle) %>%
                     filter(v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[[3]])
ggplot(pois.cycle.pred, aes(y = estimate, 
                           x = time_to_elec)) +
  scale_x_reverse() + # decreasing time to election
  geom_line(linewidth = 1) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  size = 1, linewidth = 2) +
  labs(title = "Elections and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election") 
ggsave("appendix/elec-pred-deals.png", height = 6, width = 8)


# hurdle poisson model of deals: democracy and time to election
# no control vars
pois.deals.nocont <- brm(bf(deals ~ 
                              time_to_elec_0*v2x_polyarchy +
                              time_to_elec_1*v2x_polyarchy +
                              time_to_elec_2*v2x_polyarchy,
                           center = FALSE),
                        family = poisson(),
                        backend = "cmdstanr",
                        prior = c(prior(normal(0, .5), class = "b")),
                        cores = 4,
                        refresh = 500,
                        data = us.deals.comp)
summary(pois.deals.nocont)


pois.nocont.pred <- year.dum.pred(pois.deals.nocont)
  
  predictions(pois.deals.nocont, conf_level = .9,
                               newdata = datagrid(model = pois.deals.nocont,
                                                  time_to_elec = c(0, 1, 2, 3),
                                                  v2x_polyarchy = fivenum))

ggplot(pois.nocont.pred, aes(y = estimate, 
                            x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs,
             ncol = 5) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  labs(title = "Elections, Democracy, and Arms Deals",
       subtitle = "Poisson without Controls",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("appendix/nocont-deals-pred.png", height = 6, width = 8)


# hurdle poisson model of deals: democracy and time to election
# main estimates in MS
pois.deals.democ <- brm(bf(deals ~ 
                             time_to_elec_0*v2x_polyarchy +
                             time_to_elec_1*v2x_polyarchy +
                             time_to_elec_2*v2x_polyarchy +
                            cold_war + gwot +
                            rep_pres + 
                             ln_petrol_rev + 
                            ln_rgdp + cowmidongoing +
                            ln_pop + ln_distw + 
                            Comlang,
                          hu ~ ally + v2x_polyarchy + cowmidongoing + ln_rgdp,
                          center = FALSE),
                       family = hurdle_poisson(),
                       backend = "cmdstanr",
                       prior = c(prior(normal(0, .5), class = "b")),
                       cores = 4,
                       refresh = 500,
                       data = us.deals.comp)
summary(pois.deals.democ)

pois.democ.pred <- year.dum.pred(pois.deals.democ)

ggplot(pois.democ.pred, aes(y = estimate, 
                           x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs,
             ncol = 5) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  size = 1,
                  linewidth = 2) +
  labs(title = "Elections, Democracy, and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
 ggsave("figures/fg1-democ-deals-pred.png", height = 6, width = 10,
        dpi = 800)

 # posterior predictive check
 pp_check(pois.deals.democ, type = "rootogram", 
          style = "hanging") +
   labs(title = "Hurdle Poisson Posterior Predictive Check: Arms Deals")
 ggsave("appendix/pp-check-deals.png", height = 6, width = 8)

 
# full fitted draws
fit.democ <- posterior_epred(pois.deals.democ, 
                           newdata = datagrid(model = pois.deals.democ,
                                              ally = 1,
                                              time_to_elec_0 = c(0, 1),
                                              time_to_elec_1 = c(0, 1),
                                              time_to_elec_2 = c(0, 1),
                                              v2x_polyarchy = fivenum)) 

pois.comp.dmin <- pois.democ.pred  %>%
             filter(
      v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[1]) 

key.pois.draws <- as.data.frame(fit.democ[, pois.comp.dmin$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("b > c", "d > c"), alpha = .1)



# hurdle poisson model of deals:
# add an interaction of allies
pois.deals.ally <- brm(bf(deals ~ 
                      time_to_elec_0*v2x_polyarchy*ally +
                      time_to_elec_1*v2x_polyarchy*ally +
                      time_to_elec_2*v2x_polyarchy*ally +
                    cold_war + gwot +
                    rep_pres + 
                      ln_petrol_rev + 
                    ln_rgdp + cowmidongoing +
                    ln_pop + ln_distw + 
                    Comlang,
                    hu ~ v2x_polyarchy + ally + cowmidongoing + ln_rgdp,
                    center = FALSE),
                  family = hurdle_poisson(),
                  backend = "cmdstanr",
                  prior = c(prior(normal(0, .5), class = "b")),
                  cores = 4,
                  refresh = 500,
                  data = us.deals.comp)
summary(pois.deals.ally)


# poisson model predictions 
pois.deals.est <- me.us.elec.all(pois.deals.ally, data = us.deals.comp)  


pred.us.deals <- ggplot(pois.deals.est[[2]], aes(y = estimate, 
                                x = time_to_elec,
                                group = factor(ally),
                                color = factor(ally))) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs,
             ncol = 5) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  size = .75,
                  linewidth = 1.5) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections, Democracy, Alliances and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election") 
pred.us.deals
ggsave("figures/fg4-us-arms-plots.png", height = 4, width = 8,
       dpi = 800)



# test difference 
# democracy at minimum 
pois.comp.dmin <- pois.deals.est[[2]] %>%
                     filter(
                       ally == 1 & 
                      v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[1]) 

key.pois.draws <- as.data.frame(pois.deals.est[[4]][, pois.comp.dmin$rowid])
colnames(key.pois.draws) <- c("a", "b", "c", "d")
hypothesis(key.pois.draws, c("b > a", "b > c", "d > c"), alpha = .1)


# poisson 
pc.deals <- brm(deals ~  
                  time_to_elec_0*v2x_polyarchy +
                  time_to_elec_1*v2x_polyarchy +
                  time_to_elec_2*v2x_polyarchy +
                  ally +
                   cold_war + 
                   rep_pres + 
                  ln_petrol_rev + 
                   ln_rgdp + 
                   ln_pop + ln_distw + 
                   Comlang,
                 family = poisson(),
                 cores = 4,
                 prior = c(prior(normal(0, .5), class = "b")),
                 backend = "cmdstanr",
                 refresh = 500,
                 data = us.deals.comp)
summary(pc.deals)

# check estimates: predictions 
pc.deals.est <- me.us.elec(pc.deals, data = us.deals.comp)  

ggplot(pc.deals.est[[2]], aes(y = estimate, 
                               x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs,
             ncol = 5) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  size = 1,
                  linewidth = 2) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals",
       subtitle = "Poisson",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("appendix/deals-pred-pois.png", height = 6, width = 8)




# zero-inflated poisson 
zip.deals <- brm(deals ~  
                   time_to_elec_0*v2x_polyarchy +
                   time_to_elec_1*v2x_polyarchy +
                   time_to_elec_2*v2x_polyarchy +
                   ally +
                   cold_war + 
                   rep_pres + 
                   ln_petrol_rev + 
                   ln_rgdp + 
                   ln_pop + ln_distw + 
                   Comlang,
                 family = zero_inflated_poisson(),
                 cores = 4,
                 refresh = 500,
                 prior = c(prior(normal(0, .5), class = "b")),
                 backend = "cmdstanr",
                 data = us.deals.comp)


# check estimates: predictions 
zip.deals.est <- me.us.elec(zip.deals, data = us.deals.comp)  


ggplot(zip.deals.est[[2]], aes(y = estimate, 
                               x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy, labeller = democ.all.labs,
             ncol = 5) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  size = 1,
                  linewidth = 2) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals",
       subtitle = "Zero-Inflated Poisson",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("appendix/deals-pred-zip.png", height = 6, width = 8)





### Show predictions with standard poisson and negative binomial
# negative binomial 
nb.deals <- brm(deals ~  
                  time_to_elec_0*v2x_polyarchy +
                  time_to_elec_1*v2x_polyarchy +
                  time_to_elec_2*v2x_polyarchy +
                   ally +
                   cold_war + 
                   eu_member +
                  ln_petrol_rev + 
                   ln_rgdp + 
                   ln_pop + ln_distw + 
                   Comlang,
                 family = negbinomial(),
                 cores = 4,
                 prior = c(prior(normal(0, .5), class = "b")),
                 backend = "cmdstanr",
                 data = us.deals.comp)


# posterior predictive check
pp_check(pc.deals, type = "rootogram", 
         style = "hanging") +
  labs(title = "Poisson Posterior Predictive Check: Arms Deals")
ggsave("appendix/pois-pp-check.png", height = 6, width = 8)


pp_check(nb.deals, type = "rootogram", 
         style = "hanging") +
  labs(title = "Negative Binomial Posterior Predictive Check: Arms Deals")
ggsave("appendix/nb-pp-check.png", height = 6, width = 8)



# table with model coefficients for appendix
# nice names
coef.names.deals.brm = c("b_time_to_elec" = "Years to Election",
                         "b_time_to_elec_0" = "Presidential Election",
                         "b_time_to_elec_1" = "1 Year to Election",
                         "b_time_to_elec_2" = "2 Years to Election",
                         "time_to_elec_1" = "1 Year to Election",
                         "time_to_elec_2" = "2 Years to Election",
                   "b_v2x_polyarchy" = "Polyarchy",
                   "b_ally" = "US Ally",
                   "v2x_polyarchy" = "Polyarchy",
                   "ally" = "US Ally",
                   "v2x_polyarchy × b_time_to_elec" = "Polyarchy x Years to Election",
                   "b_time_to_elec × v2x_polyarchy" = "Polyarchy x Years to Election",
                   "b_time_to_elec_0 × v2x_polyarchy" = "Polyarchy x Presidential Election",
                   "b_time_to_elec_1 × v2x_polyarchy" = "Polyarchy x 1 Year to Election",
                   "b_time_to_elec_2 × v2x_polyarchy" = "Polyarchy x 2 Years to Election",
                   "b_time_to_elec × ally" = "Ally x Years to Election",
                   "b_ally × v2x_polyarchy" = "Ally x Polyarchy",
                   "b_time_to_elec × ally × v2x_polyarchy" = "Ally x Years to Election\nx Polyarchy",
                   "b_cowmidongoing" = "Ongoing MID",
                   "b_ln_pop" = "Log Population",
                   "b_ln_petrol_rev" = "Log Petrol Revenue",
                   "b_ln_rgdp" = "Log GDP",
                   "b_ln_distw" = "Log Distance",
                   "b_eu_member" = "EU Member",
                   "b_gwot" = "Global War on Terror",
                   "b_cold_war" = "Cold War",
                   "b_rep_pres" = "Republican President",
                   "b_Comlang" = "Common Language",
                   "b_Intercept" = "Intercept",
                   "b_hu_v2x_polyarchy" = "Hurdle: Polyarchy",
                   "b_hu_ally" = "Hurdle: US Ally",
                   "b_hu_ln_rgdp" = "Hurdle: Log GDP",
                   "b_hu_cowmidongoing" = "Hurdle: Ongoing MID",
                   "b_hu_Intercept" = "Hurdle: Intercept")

pois.models <- list(pois.deals.cycle, pois.deals.nocont, pois.deals.democ, pois.deals.ally)
names(pois.models) <- c("Generic Cycle", "Regime Cycle (No Controls)", "Regime Cycle", "Regime and Ally Cycle")
pois.mod.tab <- modelsummary(pois.models,
             output = "latex",
             gof_map = "none",
             conf.level = .9,
             longtable = FALSE,
             fmt = fmt_significant(1),
             coef_rename = coef.names.deals.brm,
             statistic = "({conf.low}, {conf.high})",
             #notes = list('90\\% Credible Intervals in parentheses.'),
             title = "\\label{tab:pois-regs}: Coefficient estimates from Poisson models of US arms deals.") %>%
  kable_styling(font_size = 8, 
                latex_options = c("HOLD_position", "scale_down")) %>%
  footnote(general = "90% Credible Intervals in parentheses.")
pois.mod.tab
save_kable(pois.mod.tab, "appendix/deals-reg-tabs.tex")



# model of change in regime type and deal timing within states 
us.deals.comp.vardem <- filter(us.deals.comp, var_democ >= .14)
reg.change.deals <- fepois(deals ~  
                             time_to_elec_0*v2x_polyarchy +
                             time_to_elec_1*v2x_polyarchy +
                             time_to_elec_2*v2x_polyarchy +
                           cold_war + 
                           rep_pres + 
                           ln_petrol_rev + 
                           ln_rgdp + 
                           ln_pop | country,
                         data = us.deals.comp.vardem)
summary(reg.change.deals)


reg.change.deals <- lm(deals ~  
                         time_to_elec_0*v2x_polyarchy +
                         time_to_elec_1*v2x_polyarchy +
                         time_to_elec_2*v2x_polyarchy +
                             cold_war + 
                             rep_pres + 
                             ln_petrol_rev + 
                             ln_rgdp + 
                             ln_pop + factor(country),
                           data = us.deals.comp.vardem)
summary(reg.change.deals)


slopes(reg.change.deals, variables = c("time_to_elec_0"), conf_level = .95,
       newdata = datagrid(model = reg.change.deals, 
                          v2x_polyarchy = fivenum))

plot_slopes(reg.change.deals, variables = c("time_to_elec_0"), 
            by = "v2x_polyarchy") +
  geom_hline(yintercept = 0)

pred.reg.change.deals <- year.dum.pred(reg.change.deals)

ggplot(pred.reg.change.deals, aes(y = estimate, 
                              x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy,
             ncol = 5) + 
  scale_x_reverse() + # decreasing time to election
  #geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  labs(title = "Elections and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
