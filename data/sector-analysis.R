# Joshua Alley
# look more carefully at sectors


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
      paste0(" + ", "deals_", sector.list[i], "*gwot"),
      paste0(" + ", "swing + core + rep_pres + ln_ngdp + poptotal")
    ))
}

sector.state.sys <- systemfit(formula.sector, data = state.data.ord)
summary(sector.state.sys)

# grab covariance matrix of residuals- system of eq isn't worth it. 
arms.cor <- cbind.data.frame(
  sector.list,
  summary(sector.state.sys)$residCor)
colnames(arms.cor) <- c("variable", sector.list)
#arms.cor[upper.tri(arms.cor)] <- NA
arms.cor



# fit separate models
sector.models <-  vector(mode = "list", length = length(sector.list))
for(i in 1:length(sector.models)){
  sector.models[[i]] <- lm(formula = formula.sector[[i]],
                           data = state.data.ord)
}
names(sector.models) <- sector.list
modelplot(sector.models)



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
                    v2x_polyarchy2, 
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
                      time_to_elec*ally*v2x_polyarchy2 +
                      cold_war + 
                      eu_member +
                      rep_pres + 
                      ln_rgdp + 
                      ln_pop + ln_distw + 
                      Comlang,
                    family = poisson(link = "log"),
                    backend = "cmdstanr",
                    prior = c(prior(normal(0, .5), class = "b")),
                    cores = 4,
                    data = filter(us.deals.sector,
                                  weapon.type == sector.list[[i]])
  )
}

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
                     filter(v2x_polyarchy2 == max(v2x_polyarchy2) |
                              v2x_polyarchy2 == min(v2x_polyarchy2)) %>%
                     mutate(
                       dem.labs = case_when(
                         v2x_polyarchy2 == max(v2x_polyarchy2) ~ "Maximum Democ",
                         v2x_polyarchy2 == min(v2x_polyarchy2) ~ "Minimum Democ",
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
  labs(title = "Elections and Arms Deals",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election")
