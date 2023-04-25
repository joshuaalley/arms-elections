# Joshua Alley
# Elections and state cycles in defense contracting


### raw data 
table(state.data$incumbent)
table(state.data$time_to_selec)
table(state.data$time_to_pelec)


# plot obligations
ggplot(state.data, aes(x = ln_obligations)) + geom_histogram()

ggplot(drop_na(state.data, incumbent), 
              aes(x = factor(time_to_selec,
                             ordered = TRUE,
                             levels = c("3", "2",
                             "1", "0")),
                  y = ln_obligations,
                  color = factor(incumbent))) +
  geom_boxplot()


ggplot(state.data, 
       aes(color = factor(time_to_selec,
                      ordered = TRUE,
                      levels = c("3", "2",
                                 "1", "0")),
           y = ln_obligations,
           x = s_comp)) +
  geom_point() 


# elections and contracts: expected negative coef
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
ggsave("figures/contract-cycles.png", contract.cycles, height = 6, width = 8)



# state data 
ggplot(contracts.data.state %>% 
         left_join(elections.data) %>%
         filter(usml_cont != "other"),
       aes(x = year, y = ln_obligations,
           group = usml_cont,
           color = usml_cont)) +
  facet_wrap(~ state, scales = "free_y") +
  geom_line()


# overall electoral competition 
# simple model: OLS
elec.lm <- lm(ln_obligations ~ s_comp + incumbent +
                time_to_selec +
                diff_vote_share + time_to_pelec +
                pivot_prox +
                poptotal + ln_ngdp + iraq_war,
              data = state.data) 
summary(elec.lm)

# robust
elec.rlm <- rlm(ln_obligations ~ s_comp + incumbent +
                time_to_selec +
                diff_vote_share + time_to_pelec +
                pivot_prox +
                poptotal + ln_ngdp + iraq_war,
              data = state.data) 
summary(elec.rlm)



sen.vote.log <- glm(incumb.win ~ ln_obligations +
                   poptotal + ln_ngdp + pres_election + iraq_war,
                 data = state.data,
                 family = binomial(link = "logit"))
summary(sen.vote.log)



# Presidential vote 

# plot obligations: loess 
ggplot(state.data, 
       aes(group = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           color = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = ln_obligations,
           x = diff_vote_share)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom")


# plot obligations: linear
ggplot(state.data, 
       aes(group = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           color = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = ln_obligations,
           x = diff_vote_share)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "bottom")




# contracting from time to presidential elections and pivot proximity
pres.ob.prox <- lm(ln_obligations ~ time_to_pelec*pivot_prox +
                     iraq_war +
                      poptotal + ln_ngdp,
                   data = state.data) 
summary(pres.ob.prox)


### state data with arms deals
state.data.deals <- left_join(state.data, 
                              select(arms.deals.year,
                                     year, deals))

ggplot(state.data.deals, aes(x = factor(time_to_pelec),
                             y = deals)) +
  geom_boxplot()
  

# add deals to model
# contracting from time to presidential elections and pivot proximity
deals.state <- lm(ln_obligations ~ deals +
                    s_comp + 
                    diff_vote_share +
                    time_to_pelec +
                     iraq_war +
                     poptotal + ln_ngdp,
                   data = state.data.deals) 
summary(deals.state)



# link orders with state contracts by sector
# this puts orders before contracts
sector.list <- c("aircraft", "arms", "electronics", "missile_space",
                 "ships", "vehicles")
formula.sector <- vector(mode = "list", length = length(sector.list))
  
for(i in 1:length(sector.list)){
  formula.sector[[i]] <- as.formula(
    paste(
      paste0(sector.list[i], "_change ", "~"), 
      # paste0(sector.list[i]), "~",
      # paste0(sector.list[i], "_lag"),
      paste0(" + ", "deals_", sector.list[i]),
      paste0(" + ", "s_comp"),
      paste0(" + ", "diff_vote_share + time_to_pelec"),
      paste0("+ iraq_war")
    ))
  }

sector.state.sys <- systemfit(formula.sector, data = state.data.ord)
summary(sector.state.sys)

# interact with vote share 
for(i in 1:length(sector.list)){
  formula.sector[[i]] <- as.formula(
    paste(
      paste0(sector.list[i], "_change ", "~"), 
      # paste0(sector.list[i]), "~",
      # paste0(sector.list[i], "_lag"),
      paste0(" + ", "deals_", sector.list[i], "*time_to_pelec"),
      paste0(" + ", "time_to_pelec + s_comp + iraq_war")
    ))
}

sector.state.lvote <- systemfit(formula.sector, data = state.data.ord)
summary(sector.state.lvote)
