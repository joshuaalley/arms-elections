# Joshua Alley
# robustness check: drop autocracies one by one 


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
