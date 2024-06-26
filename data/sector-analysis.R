# Joshua Alley
# look more carefully at sectors


### examine state contracts by sector

# log every contract value
state.data.ord.rs <- state.data.ord %>%
  group_by(year) %>%
  mutate(   
  across(aircraft:vehicles, function(x) 
      x / sum(x, na.rm = TRUE)),
  across(aircraft_lag:vehicles_lag, function(x) 
    x / sum(x, na.rm = TRUE))) %>%
  ungroup()
# long data for plotting to look at raw
sector.ob.long <- state.data.ord %>%
                    ungroup() %>%
                    select(aircraft:vehicles) %>%
                    select(-other) %>%
                    pivot_longer(cols = everything())
ggplot(sector.ob.long, aes(x = value)) +
  facet_wrap(~ name) +
  geom_histogram()

# rescaled
sector.ob.rs <- state.data.ord.rs %>%
  ungroup() %>%
  select(aircraft:vehicles) %>%
  select(-other) %>%
  pivot_longer(cols = everything())
ggplot(sector.ob.rs, aes(x = value)) +
  facet_wrap(~ name) +
  geom_histogram() +
  labs(
    x = "Rescaled Contracts",
    y = "Count",
    title = "Distribution of Contracts by Sector"
  )

# create formulae for each weapon type
sector.list <- c("aircraft", "arms", "electronics", "missile_space",
                 "ships", "vehicles")
formula.sector <- vector(mode = "list", length = length(sector.list))

for(i in 1:length(sector.list)){
  formula.sector[[i]] <- bf(
    paste(
      paste0(sector.list[i], "~"), 
      paste0("(", sector.list[i], "_lag", " || state) +"),
      paste0("deals_", sector.list[i], "*swing"),
      " + core + gwot + rep_pres + ln_ngdp + poptotal + time_to_elec"),
     center = FALSE)
}
formula.sector

# fit separate models
sector.models <-  vector(mode = "list", length = length(sector.list))
for(i in 1:length(sector.models)){
  sector.models[[i]] <- ordbetareg(formula = formula.sector[[i]],
                           data = state.data.ord.rs,
                           backend = "cmdstanr",
                           cores = 4,
                           refresh = 500)
}
names(sector.models) <- sector.list

# pp_check 
pp.cont.sector <- vector(mode = "list", length = length(sector.models))
pp.cont.sector <- lapply(sector.models,
                          function(x)
                            pp_check(x))  
pp.cont.sector

lapply(sector.models, function(x) summary(x))



# summarize output 

# get scale factors 
total.cont.sector <- state.data.ord %>%
  group_by(year) %>%
  summarize(   
    across(aircraft:vehicles, function(x) 
      sum(x, na.rm = TRUE))) %>%
   select(-c(other, year))
total.cont.sector

scale.sector <- apply(total.cont.sector, 2, function(x)
                      median(x))
scale.sector


# make a function to apply to every model 
present.sector.cont <- function(model, sector, scale.factor){
  
  deals.data <- select(ungroup(state.data.ord), paste0("deals_", sector))
  min.deals <- round(min(deals.data, na.rm = TRUE), digits = 2)
  max.deals <- round(max(deals.data, na.rm = TRUE), digits = 2)
  
  sector.lab <- paste0("deals_", sector)
  sector.nice <- str_to_title(gsub("_", " & ",
                                   sector))
  
  # hypothetical data 
  hyp.data <- datagrid(model = model, 
                       swing = c(0, 1),
                       deals = seq(from = min.deals,
                                   to = max.deals, by = 1),
                       gwot = 0,
                       rep_pres = 0,
                       core = 0,
                       time_to_elec = 1,
                       poptotal = median(state.data.deals$poptotal),
                       ln_ngdp = median(state.data.deals$ln_ngdp),
                       state = "Wisconsin") %>%
                 select(-c(2)) %>%
                 rename_with(
                   ~ sector.lab, deals
                 )
  
  # marginal effect of deals
  deals.est <- slopes(model,
                           newdata = hyp.data,
                           by = "swing",
                           variables = sector.lab,
                           conf_level = .90,
  ) %>%
    mutate_at(
      c("estimate", "conf.low", "conf.high"),
      function(x) x * scale.factor
    )
  deals.est
  
  
  # all draws 
  deals.draws <- prepare_predictions(model)
  deals.inter <- as.data.frame(deals.draws$dpars$mu$fe$b)
  
  # use separate plots 
  deals.post <- as.data.frame(deals.inter[, 2] * scale.factor)
  colnames(deals.post) <- c("Deals")
  deals.pos <- round(sum(deals.post > 0) / 4000, digits = 2)
  
  deals.swing.post <- as.data.frame(deals.inter[, 10] * scale.factor)
  colnames(deals.swing.post) <- c("Deals:Swing")
  deals.swing.pos <- round(sum(deals.swing.post > 0) / 4000, digits = 2)
  
  deals.post.all <- bind_cols(deals.post, deals.swing.post)
  
  xmin <- min(deals.post.all)
  xmax <- max(deals.post.all)
  
  deals.dens <- ggplot(deals.post.all, aes(x = Deals)) +
    geom_density()
  deals.dens
  dens.data <- ggplot_build(deals.dens)$data[[1]]
  dens.med <- quantile(dens.data$y)[4]
  
  deals.dens <- deals.dens + geom_area(data = subset(dens.data, x > 0),
                                       aes(x=x, y=y), fill="darkgrey") +
    xlim(xmin, xmax) +
    labs(x = "", y = "Density",
         title = "Deals") +
    annotate("text", x = xmin, y = dens.med, label = as.character(deals.pos), 
             size = 6, parse = TRUE) +
    theme_bw(base_size = 12)
  deals.dens
  
  
  deals.swing.dens <- ggplot(deals.post.all, aes(x = `Deals:Swing`)) +
    geom_density()
  deals.swing.dens
  dens.data <- ggplot_build(deals.swing.dens)$data[[1]]
  dens.med <- quantile(dens.data$y)[4]
  
  deals.swing.dens <- deals.swing.dens + geom_area(data = subset(dens.data, x > 0),
                                                   aes(x=x, y=y), fill="darkgrey") +
    xlim(xmin, xmax) +
    labs(x = "", y = "Density",
         title = "Deals: Swing") +
    annotate("text", x = xmin, y = dens.med, label = as.character(deals.swing.pos), 
             size = 6, parse = TRUE) +
    theme_bw(base_size = 12)
  deals.swing.dens
  
  grid.arrange(deals.swing.dens, deals.dens, 
                            top = grid::textGrob(sector.nice,
                                  gp = grid::gpar(col = "black", fontsize = 18)))
  deals.inter.plot <- arrangeGrob(deals.swing.dens, deals.dens, 
                                  top = grid::textGrob(sector.nice, 
                                  gp = grid::gpar(col = "black", fontsize = 18)))
  

  # marginal effect of swing
  swing.est <- slopes(model,
                           newdata = hyp.data,
                           variables = "swing",
                           by = sector.lab,
                           conf_level = .90) %>%
    mutate_at(
      c("estimate", "conf.low", "conf.high"),
      function(x) x * scale.factor
    )
  
  pred.cont <- predictions(model, conf.level = .9,
                           newdata = hyp.data
  ) %>%
    mutate_at(
      c("estimate", "conf.low", "conf.high"),
      function(x) x * scale.factor
    )

  list(hyp.data, deals.est, swing.est, pred.cont, deals.inter.plot)
  
}

# apply function to models, data, and sectors, each with unique scale factor
res.sector.cont <- mapply(present.sector.cont, 
                          model = sector.models, 
                          sector = sector.list,
                          scale.factor = scale.sector,
                          SIMPLIFY = FALSE, USE.NAMES = TRUE) 

# deals ME 
margins.deals.sector <- bind_rows(lapply(res.sector.cont, "[[", 2))
margins.deals.sector$term <- gsub("deals_", "", margins.deals.sector$term)
margins.deals.sector$term <- str_to_title(gsub("_", " & ",
                                               margins.deals.sector$term))
glimpse(margins.deals.sector)

ggplot(margins.deals.sector, aes(x = factor(swing), y = estimate)) +
  facet_wrap(~ term, scales = "free_y") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1, linewidth = 2) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(`0` = "No", `1` = "Yes")) +
  labs(
    title = "Marginal Impact of Arms Deals by Weapon Type",
    x = "Swing State",
    y = "Impact of Arms Deals\n(Millions $)"
  )


# combine the different sector plots
grid.arrange(res.sector.cont[["aircraft"]][[5]], res.sector.cont[["arms"]][[5]],
             res.sector.cont[["electronics"]][[5]], res.sector.cont[["missile_space"]][[5]],
             res.sector.cont[["ships"]][[5]], res.sector.cont[["vehicles"]][[5]],
             ncol = 3)
me.deals.sector <- arrangeGrob(res.sector.cont[["aircraft"]][[5]], res.sector.cont[["arms"]][[5]],
                               res.sector.cont[["electronics"]][[5]], res.sector.cont[["missile_space"]][[5]],
                               res.sector.cont[["ships"]][[5]], res.sector.cont[["vehicles"]][[5]],
                               ncol = 3)
ggsave("figures/fg6-me-deals-sector.png", me.deals.sector, 
       height = 7, width = 10,
       dpi = 800)


# swing state ME
margins.swing.sector <- bind_rows(lapply(res.sector.cont, "[[", 3)) %>%
                         pivot_longer(cols = c(starts_with("deals_"))) %>%
                         rename(deals = value) %>%
                         drop_na()
glimpse(margins.swing.sector)

margins.swing.sector$name <- gsub("deals_", "", margins.swing.sector$name)
margins.swing.sector$name <- str_to_title(gsub("_", " & ",
                                               margins.swing.sector$name))


ggplot(margins.swing.sector, aes(x = deals, y = estimate)) +
  facet_wrap(~ name, scales = "free") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .5) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Marginal Impact of Swing States",
    x = "Arms Deals",
    y = "Impact of Swing Status"
  )


pred.out.sector <- bind_rows(lapply(res.sector.cont, "[[", 4)) %>%
  pivot_longer(cols = c(starts_with("deals_"))) %>%
  rename(deals = value) %>%
  group_by(name) %>%
  drop_na(deals)
glimpse(pred.out.sector)

ggplot(pred.out.sector, aes(x = deals, y = estimate,
                      fill = factor(swing))) +
  facet_wrap(~ name, scales = "free") +
  geom_line(linewidth = 2) +
  scale_fill_grey(labels = c(`0` = "Not Swing", `1` = "Swing")) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
               alpha = .5) +
  labs(x = "Arms Deals",
       fill = "Electoral\nCompetition",
       y = "Predicted Defense Contracts",
       title = "Predicted Contracts by Weapon Type")





### examine the deals cycles by sector ###
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
  pivot_wider(id_cols = c("ccode", "year"),
              names_from = "weapon.type",
              values_from = "deals")
# zero deals with NA
us.deals.sector[, 3:8][is.na(us.deals.sector[, 3:8])] <- 0


# separate datasets
weapon.type <- colnames(us.deals.sector)[3:8]

sector.data.func <- function(sector){
  data <- select(us.deals.sector,
           ccode, year, {{sector}}) %>%
    right_join(select(us.trade.ally,
                      ccode, year,
                      atop_defense, ally,
                      cold_war, democ_bin,
                      v2x_polyarchy, cowmidongoing,
                      rep_pres, time_to_elec, 
                      ln_petrol_rev, ln_rgdp,
                      ln_pop, ln_distw,
                      Comlang,
                      Contig, Evercol)) %>%
    rename(deals = {{sector}}) %>%
    filter(year >= 1950) %>%
    mutate(
      nz_deals = ifelse(deals > 0, 1, 0)
    ) %>% # pakistan/east pak duplicate gives warning- drop
    distinct()  
  # make sure zeros for no deals 
  data$deals[is.na(data$deals)] <- 0
  data$nz_deals[is.na(data$nz_deals)] <- 0
  
  # time to election dummies
  data <- data %>%
    mutate(
    time_to_elec_0 = ifelse(time_to_elec == 0, 1, 0),
    time_to_elec_1 = ifelse(time_to_elec == 1, 1, 0),
    time_to_elec_2 = ifelse(time_to_elec == 2, 1, 0)
  )
  
  # output
  data
}

us.deals.sector.list <- lapply(weapon.type, sector.data.func)


# deals by weapon type
sector.deals.total <- us.arms.cat %>%
  group_by(weapon.type) %>%
  summarize(
    deals = sum(deals, na.rm = TRUE),
    .groups = "keep"
  ) 
sector.deals.total
5732 / sum(sector.deals.total$deals) # aircraft
1567 / sum(sector.deals.total$deals) # vehicles
1040 / sum(sector.deals.total$deals) # ships
5732 / sum(sector.deals.total$deals) + # aircraft
  1567 / sum(sector.deals.total$deals) + # vehicles
    1040 / sum(sector.deals.total$deals) # ships

# now set up formula
deals.sector <- vector(mode = "list", length = length(sector.list))
for(i in 1:length(sector.list)){
  
  deals.sector[[i]] <- brm(bf(deals ~ 
                                time_to_elec_0*v2x_polyarchy +
                                time_to_elec_1*v2x_polyarchy +
                                time_to_elec_2*v2x_polyarchy +
                      cold_war + 
                      rep_pres + 
                      ln_rgdp + cowmidongoing + ln_petrol_rev +
                      ln_pop + ln_distw + 
                      Comlang,
                      hu ~ ally + cowmidongoing + ln_rgdp + v2x_polyarchy,
                      center = FALSE),
                    family = hurdle_poisson(link = "log"),
                    backend = "cmdstanr",
                    prior = c(prior(normal(0, .5), class = "b")),
                    cores = 4,
                    refresh = 500,
                    data = us.deals.sector.list[[i]] # zero deals 
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
pred.inter.sector$weapon <- rep(sector.list, each = 20)
pred.inter.sector$weapon <- str_to_title(gsub("_", " & ", pred.inter.sector$weapon))
# max and min only for interpretation
pred.inter.sector <- pred.inter.sector %>% 
                     group_by(weapon) %>%
                     filter(v2x_polyarchy == max(v2x_polyarchy) |
                              v2x_polyarchy == min(v2x_polyarchy)) %>%
                     mutate(
                       dem.labs = case_when(
                         v2x_polyarchy == max(v2x_polyarchy) ~ "Maximum Democracy",
                         v2x_polyarchy == min(v2x_polyarchy) ~ "Minimum Democracy",
                       )
                     ) %>%
                     select(dem.labs, time_to_elec,
                            weapon, 
                            estimate, conf.low, conf.high)


# plot
ggplot(pred.inter.sector, aes(y = estimate, 
                              x = time_to_elec,
                              group = dem.labs,
                              color = dem.labs
                              )) +
  facet_wrap( ~ weapon, ncol = 7,
              scales = "free_y") + 
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .25),
                  size = .75,
                  linewidth = 1.5) +
  scale_color_grey("Regime",
                   start = 0.7,
                   end = 0.1) +
  labs(title = "Elections and Arms Deals: Specific Sectors",
       y = "Predicted Arms Deals",
       x = "Years to Presidential Election") +
  theme(legend.position = "bottom")
ggsave("figures/fg5-deals-sector.png", height = 5, width = 10,
       dpi = 800)



# get substantive comparisons: minimum democracy
for(i in 1:length(deals.sector)){
  
  sector.dmin <- deals.sector.est[[i]][[2]] %>%
    filter(
      ally == 1 & 
        v2x_polyarchy == fivenum(us.deals.comp$v2x_polyarchy)[1]) 
  
  key.sector.draws <- as.data.frame(deals.sector.est[[i]][[3]][, sector.dmin$rowid])
  
  colnames(key.sector.draws) <- c("a", "b", "c", "d")
  
  print(sector.list[i]) 
  print(hypothesis(key.sector.draws, c("d > a"), alpha = .1))
  
}


# total from summing across 
pred.sector.total <- pred.inter.sector %>%
  group_by(dem.labs, time_to_elec) %>%
  summarize(
    estimate = sum(estimate),
    conf.low = sum(conf.low),
    conf.high = sum(conf.high),
    .groups = "keep"
  ) %>%
  mutate(
    weapon = "Total"
  )
pred.sector.total


# coefficient tables for appendix: deals
names(deals.sector) <- c("Aircraft", 
                         "Arms",
                         "Electronics",
                         "Missile and Space",
                         "Ships", "Vehicles")
deals.sector.tab <- modelsummary(deals.sector,
             output = "latex",
             gof_map = "none",
             conf.level = .9,
             longtable = FALSE,
             fmt = fmt_significant(2),
             coef_rename = coef.names.deals.brm,
             statistic = "({conf.low}, {conf.high})",
             #notes = list('90\\% Credible Intervals in parentheses.'),
             title = "\\label{tab:pois-regs-sector}: Coefficient estimates from hurdle Poisson models of U.S. arms deals by sector.") %>%
  kable_styling(font_size = 8, 
                latex_options = c("HOLD_position")) %>%
  footnote(general = "90% Credible Intervals in parentheses.")
deals.sector.tab
save_kable(deals.sector.tab, "appendix/deals-reg-sector.tex")

# table for appendix: contracts
names(sector.models) <- c("Aircraft", 
                          "Arms",
                          "Electronics",
                          "Missile and Space",
                          "Ships", "Vehicles")
sector.mod.tab <- modelsummary(sector.models,
             output = "latex", 
             gof_map = "none",
             conf.level = .9,
             coef_omit = c("sd"),
             longtable = FALSE,
             escape = FALSE,
             fmt = fmt_significant(1),
             coef_rename = coef.names.cont.brm,
             statistic = "({conf.low}, {conf.high})",
             title = "\\label{tab:cont-regs-sector}: Coefficient estimates from models of defense contract awards by sector.") %>%
  kable_styling(font_size = 8, 
                latex_options = c("HOLD_position", "scale_down")) %>%
  footnote(general = "90% Credible Intervals in parentheses.")
sector.mod.tab
save_kable(sector.mod.tab, "appendix/cont-reg-sector.tex")

