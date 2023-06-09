# Joshua Alley
# look more carefully at sectors


### examine state contracts by sector

# log every contract value
state.data.ord.log <- state.data.ord %>%
                       mutate(across(aircraft:vehicles, function(x) log(x + 1))) %>%
                       mutate(across(aircraft_lag:vehicles_lag, function(x) log(x + 1)))

# long data for plotting to look at raw
sector.ob.long <- state.data.ord %>%
                    ungroup() %>%
                    select(aircraft:vehicles) %>%
                    select(-other) %>%
                    pivot_longer(cols = everything())
ggplot(sector.ob.long, aes(x = value)) +
  facet_wrap(~ name) +
  geom_histogram()

# logged
sector.lnob.long <- state.data.ord.log %>%
  ungroup() %>%
  select(aircraft:vehicles) %>%
  select(-other) %>%
  pivot_longer(cols = everything())
ggplot(sector.lnob.long, aes(x = value)) +
  facet_wrap(~ name) +
  geom_histogram() +
  labs(
    x = "Log Contracts (Millions $)",
    y = "Count",
    title = "Distribution of Contracts by Sector"
  )

# use logged outcome with hurdle gamma 

# create formulae for each weapon type
sector.list <- c("aircraft", "arms", "electronics", "missile_space",
                 "ships", "vehicles")
formula.sector <- vector(mode = "list", length = length(sector.list))

for(i in 1:length(sector.list)){
  formula.sector[[i]] <- bf(
    paste(
      paste0(sector.list[i], "~"), 
      #" ar(time = year, gr = state, p = 1)", 
      paste0(" + ", "deals_", sector.list[i], "*swing"),
      paste0(" + ", "gwot + rep_pres + ln_ngdp + poptotal")),
     center = FALSE)
}

# define specific families
family.list <- c("student", "hurdle_logno", "gaussian",
                 rep("frechet", 3))

# fit separate models
sector.models <-  vector(mode = "list", length = length(sector.list))
for(i in 1:length(sector.models)){
  sector.models[[i]] <- brm(formula = formula.sector[[i]],
                            family = skew_normal(), # not happy with this choice
                           data = state.data.ord.log,
                           prior = c(
                             set_prior("normal(0, 2)", class = "b")
                           ),
                           backend = "cmdstanr",
                           control = list(adapt_delta = 0.99),
                           cores = 4)
}
names(sector.models) <- sector.list

# pp_check 
pp.cont.sector <- vector(mode = "list", length = length(sector.models))
pp.cont.sector <- lapply(sector.models,
                          function(x)
                            pp_check(x, type = "hist"))  
pp.cont.sector

lapply(sector.models, function(x) summary(x))

# examine the deals cycles by sector 
# total deals- summarize at country-year level and add covariates
us.deals.sector <- us.arms.cat %>%
  group_by(ccode, year, weapon.type) %>%
  select(
    country, ccode, year, deals, weapon.type,
  ) %>%
  summarize(
    deals = sum(deals, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  right_join(select(us.trade.ally,
                    ccode, year,
                    atop_defense, ally, ally_democ,
                    cold_war, democ_bin,
                    v2x_polyarchy, 
                    rep_pres, time_to_elec, 
                    eu_member, ln_rgdp,
                    ln_pop, ln_distw,
                    Comlang,
                    Contig, Evercol)) %>%
  filter(year >= 1950) %>%
  mutate(
    nz_deals = ifelse(deals > 0, 1, 0)
  ) %>% # pakistan/east pak duplicate gives warning- drop
  distinct()  
# NA from right join- move to zero
us.deals.sector$deals[is.na(us.deals.sector$deals)] <- 0
us.deals.sector$nz_deals[is.na(us.deals.sector$nz_deals)] <- 0



# now set up formula
deals.sector <- vector(mode = "list", length = length(sector.list))
for(i in 1:length(sector.list)){
  
  deals.sector[[i]] <- brm(deals ~ 
                      time_to_elec*ally*v2x_polyarchy +
                      cold_war + 
                      eu_member +
                      rep_pres + 
                      ln_rgdp + 
                      ln_pop + ln_distw + 
                      Comlang,
                    family = zero_inflated_poisson(link = "log"),
                    backend = "cmdstanr",
                    prior = c(prior(normal(0, .5), class = "b")),
                    cores = 4,
                    data = filter(us.deals.sector,
                                  weapon.type == sector.list[[i]])
  )
}

# pp_check 
pp.deals.sector <- vector(mode = "list", length = length(deals.sector))
pp.deals.sector <- lapply(deals.sector,
       function(x)
         pp_check(x, type = "rootogram", 
                  style = "hanging"))  
pp.deals.sector

# look at interactions
deals.sector.est <- lapply(deals.sector,
                           function(x)
                           me.us.elec(x, data = x$data))  

# take predictions
pred.inter.sector <- bind_rows(sapply(deals.sector.est, function(x) x[2]))
pred.inter.sector$weapon <- toupper(rep(sector.list, each = 40))
# max and min only for interpretation
pred.inter.sector <- pred.inter.sector %>% 
                     group_by(weapon) %>%
                     filter(v2x_polyarchy == max(v2x_polyarchy) |
                              v2x_polyarchy == min(v2x_polyarchy)) %>%
                     mutate(
                       dem.labs = case_when(
                         v2x_polyarchy == max(v2x_polyarchy) ~ "Maximum Democ",
                         v2x_polyarchy == min(v2x_polyarchy) ~ "Minimum Democ",
                       )
                     )

# plot
ggplot(pred.inter.sector, aes(y = estimate, 
                                x = time_to_elec,
                                group = factor(ally),
                                color = factor(ally))) +
  facet_grid(weapon ~ fct_rev(dem.labs)) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")

# switch axes in grid
ggplot(pred.inter.sector, aes(y = estimate, 
                              x = time_to_elec,
                              group = factor(ally),
                              color = factor(ally))) +
  facet_grid(fct_rev(dem.labs) ~ weapon) + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("US Ally", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Elections and Arms Deals: Specific Sectors",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
ggsave("figures/deals-sector.png", height = 7, width = 10)

summary(deals.sector[[1]])
summary(deals.sector[[2]])
