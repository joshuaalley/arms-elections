# Joshua Alley
# robustness checks of arms deals 
# including dropping autocracies, linear time to election,
# incumbent/not, alternative autocracy codings, and 
# alternative specifications (post-treatment)

# list of autocracies to drop 
# KSA, Iran, Egypt, UAE, Argentina, Brazil
ccode.autoc.drop <- c(670, 630, 651, 696, 140, 160)
# for of models
models.autoc.drop <- vector(mode = "list",
                            length = length(ccode.autoc.drop))

# use this deals model
# hurdle poisson model of deals: democracy and time to election
# For loop over data
for(i in 1:length(ccode.autoc.drop)){
  
  models.autoc.drop[[i]] <- brm(bf(deals ~ 
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
                                data = filter(us.deals.comp,
                                              ccode != ccode.autoc.drop[i]))
}


# function to clean it up 
autoc.drop.func <- function(model.drop) {
  pred <- year.dum.pred(model.drop)
  pred 
}

# get all predictions
pred.autoc.drop <- lapply(models.autoc.drop, autoc.drop.func)
names(pred.autoc.drop) <- countrycode(ccode.autoc.drop, 
                                      origin = "cown",
                                      destination = "country.name")

autoc.drop.res <- bind_rows(pred.autoc.drop,
                            .id = "dropped") %>%
  filter(
    v2x_polyarchy <= 0.02 |
      v2x_polyarchy >= 0.92  
  )
autoc.drop.res$v2x_polyarchy[autoc.drop.res$v2x_polyarchy == 0.013] <- 0.012

ggplot(autoc.drop.res, aes(y = estimate, 
                           x = time_to_elec)) +
  facet_grid(v2x_polyarchy ~ dropped,
             labeller = democ.all.labs) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  size = 1,
                  linewidth = 2) +
  labs(title = "Elections, Democracy, and Arms Deals",
       subtitle = "Cut Potential Outliers",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("appendix/democ-deals-pred-drop.png", height = 8, width = 12)




### results with dummies for years to election instead of linear term
pois.deals.democ.linear <- brm(bf(deals ~ 
                                    time_to_elec*v2x_polyarchy +
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
summary(pois.deals.democ.linear)


pois.lin.pred <- predictions(pois.deals.democ.linear, conf_level = .9,
                             newdata = datagrid(model = pois.deals.democ.linear,
                                                ally = 1,
                                                time_to_elec = c(0, 1, 2, 3),
                                                v2x_polyarchy = fivenum))              

ggplot(pois.lin.pred, aes(y = estimate, 
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
       subtitle = "Linear Time to Election",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("appendix/democ-deals-pred-lin.png", height = 6, width = 8)



### alternative autocracy measures



### run model with incumbency as additional modifier
pois.deals.incum <- brm(bf(deals ~ 
                             incumbent*time_to_elec_0*v2x_polyarchy +
                             incumbent*time_to_elec_1*v2x_polyarchy +
                             incumbent*time_to_elec_2*v2x_polyarchy +
                             # (1 + time_to_elec_0*v2x_polyarchy +
                             # time_to_elec_1*v2x_polyarchy +
                             # time_to_elec_2*v2x_polyarchy |
                             #  incumbent) +
                             cold_war + gwot + ally +
                             rep_pres + 
                             ln_petrol_rev + 
                             ln_rgdp + cowmidongoing +
                             ln_pop + ln_distw + 
                             Comlang,
                           #hu ~ ally + v2x_polyarchy + cowmidongoing + ln_rgdp,
                           center = FALSE),
                        family = poisson(),
                        backend = "cmdstanr",
                        prior = c(prior(normal(0, .5), class = "b")
                                  #prior(normal(0, 1), class = "sd")
                                  ),
                        cores = 4,
                        refresh = 500,
                        data = us.deals.comp)
summary(pois.deals.incum)


# clean up the results 
pred.incum <- predictions(pois.deals.incum, conf_level = .9,
            newdata = datagrid(model = pois.deals.incum,
                               ally = 1,
                               incumbent = c(0, 1),
                               time_to_elec_0 = c(0, 1),
                               time_to_elec_1 = c(0, 1),
                               time_to_elec_2 = c(0, 1),
                               v2x_polyarchy = fivenum)) %>%
  rowwise() %>%
  mutate(
    dum_sum = sum(time_to_elec_0, time_to_elec_1, time_to_elec_2)
  ) %>%
  filter(dum_sum <= 1) %>%
  mutate(
    incumbent = case_when(
      incumbent == 1 ~ "Incumbent",
      incumbent == 0 ~ "Lame Duck"
    ),
    time_to_elec = case_when(
      time_to_elec_0 == 1 ~ 0,
      time_to_elec_1 == 1 ~ 1,
      time_to_elec_2 == 1 ~ 2,
      (time_to_elec_0 == 0 &
         time_to_elec_1 == 0 &
         time_to_elec_2 == 0) ~ 3
    )
  )


ggplot(pred.incum, aes(y = estimate, 
                       color = incumbent,
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
       x = "Years to Presidential Election",
       color = "President")
ggsave("appendix/incum-deals-pred.png", height = 6, width = 8)