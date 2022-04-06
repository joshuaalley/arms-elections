# Joshua Alley
# plot cycles in key cases 


# exports are also not stationary in almost any panel
ggplot(dyadic.mp.ally, aes(x = year, y = exports,
                           color = factor(ccode1),
                           group = factor(ccode1))) +
  facet_wrap(~ ccode2) +
  geom_line()
# in logs too 
ggplot(dyadic.mp.ally, aes(x = year, y = ln_exports,
                           color = factor(ccode1),
                           group = factor(ccode1))) +
  facet_wrap(~ ccode2) +
  geom_line()



# Key cases US
us.trade.key <- us.trade.ally %>%
              mutate(
                pc_exports = change_exports / lag_exports
              ) %>%
              filter(ccode == 20 |
                       ccode == 200 |
                       ccode == 220 |
                       ccode == 732 |
                       ccode == 740)

ggplot(us.trade.key, aes(x = year, y = pc_exports)) +
  facet_wrap(~ ccode) +
  geom_line(size = 2) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  scale_color_viridis_c()


ggplot(us.trade.key, aes(x = year, y = ln_exports)) +
  facet_wrap(~ ccode) +
  geom_point(aes(shape = factor(incumbent)), size = 2) +
  geom_line(size = 1) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  scale_color_viridis_c()

# S. Korea 
ggplot(filter(us.trade.key, ccode == 732),
       aes(x = year, y = ln_exports)) +
  geom_point(aes(shape = factor(incumbent)), size = 2) +
  geom_line(size = 1) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  ggtitle("South Korea")


# Japan
# US exports are increasing before almost every election 
ggplot(filter(us.trade.key, ccode == 740),
       aes(x = year, y = change_exports)) +
  geom_point(aes(shape = factor(incumbent)), size = 2) +
  geom_line(size = 1) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  ggtitle("Japan")

# test it
t.test(us.trade.key$change_ln_exports ~ us.trade.key$election)
t.test(us.trade.key$pc_exports ~ us.trade.key$election)
cor.test(us.trade.key$change_ln_exports, us.trade.key$time_to_elec)



# us arms exports 
us.arms.sum <- us.trade.ally %>%
  group_by(year, atop_defense) %>%
  summarize(
    us_arms = sum(us_arms, na.rm = TRUE),
    cold_war = max(cold_war, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  left_join(elections.data) %>% 
  drop_na(atop_defense)

# plot over time
ggplot(us.arms.sum, aes(x = year, y = us_arms,
                        group = factor(atop_defense),
                        color = factor(atop_defense))) +
  geom_line(size = 1) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") 

# electoral differences: ally or not 
ggplot(us.arms.sum, aes(x = factor(time_to_elec,
                                   ordered = TRUE,
                                   levels = c("3", "2",
                                              "1", "0")),
                        y = us_arms,
                        color = factor(atop_defense))) +
  geom_boxplot()


# electoral differences: ally or not: Cold War
ggplot(us.arms.sum, aes(x = factor(time_to_elec,
                                   ordered = TRUE,
                                   levels = c("3", "2",
                                              "1", "0")), 
                        y = us_arms,
                        color = factor(atop_defense))) +
     facet_wrap(~ cold_war) +
     geom_boxplot(outlier.shape = NA)



### Select cases for intro paragraphs

# key variables for matching
# match within election years
casem.elec <- vector(mode = "list", length = length(pres.elections))


for(i in 1:length(casem.elec)){
us.trade.nearel <- as.data.frame(select(us.trade.ally,
                       ccode, year, destination,
                       change_ln_exports, change_ln_imports,
                       change_ihs_balance, change_trade,
                       time_to_elec, atop_defense, # rep_pres,
                       xm_qudsest2, dyadigos, ln_gdp_d,
                      Distw,# eu_member, cowmidongoing,
                       us_arms) %>%
                      filter(ccode >= 100 & ccode <= 920) %>%
                      filter(year == pres.elections[i]))

match.vars <- c("time_to_elec", "rep_pres",
                "cold_war", "xm_qudsest2",  
                "cowmidongoing", "dyadigos",
                 "change_gdp_d", "Distw", "eu_member")

drop.vars <- c("change_ln_exports", "change_ln_imports",
               "change_ihs_balance", "change_trade",
               "us_arms", "ccode", "destination", "atop_defense",
               "time_to_elec", "year")

casem <- case.match(data = us.trade.nearel, id.var="destination", 
                     leaveout.vars = drop.vars,
                     distance = "mahalanobis", case.N = 2, 
                     number.of.matches.to.return = 10,
                     treatment.var = "atop_defense",
                    max.variance = TRUE)
casem.elec[[i]] <- casem
}

names(casem.elec) <- pres.elections

# combine all matched observations
casem.elec.all <- bind_rows(casem.elec,
                            .id = "year")


# look at cases:
# used this data from intro cases 
key.cases <- filter(us.trade.ally, ccode == 390 | ccode == 380) %>%
                select(ccode, year, atop_defense, 
                       time_to_elec,
                       change_ln_exports, change_ln_imports,
                       change_ihs_balance, change_trade, us_arms,
                       change_us_arms) 

t.test(key.cases$change_ln_exports ~ key.cases$ccode)
t.test(key.cases$us_arms ~ key.cases$ccode)

key.cases.long <- key.cases %>%
             pivot_longer(
             cols = -c(ccode, year, time_to_elec, atop_defense),
              names_to = "trade",
            values_to = "value"
            )

ggplot(key.cases.long, aes(x = factor(time_to_elec,
                        ordered = TRUE, 
                        levels = c("3", "2", "1", "0")), 
                        y = value,
                        color = factor(ccode))) +
  facet_wrap(~ trade, scales = "free_y",
             labeller = labeller(trade = c("change_ln_exports" = "Exports",
                                           "change_ln_imports" = "Imports",
                                           "change_ihs_balance" = "Trade Balance",
                                           "us_arms" = "Arms Transfers",
                                           "change_us_arms" = "Change Arms Transfers",
                                           "change_trade" = "Total Trade"))) +
  geom_boxplot(outlier.shape = NA) 

# all years 
ggplot(key.cases.long, aes(x = year, 
                                              y = value,
                                              group = factor(ccode),
                                              color = factor(ccode))) +
  facet_wrap(~ trade, scales = "free_y",
             labeller = labeller(trade = c("change_ln_exports" = "Exports",
                                           "change_ln_imports" = "Imports",
                                           "change_ihs_balance" = "Trade Balance",
                                           "us_arms" = "Arms Transfers",
                                           "change_us_arms" = "Change Arms Transfers",
                                           "change_trade" = "Total Trade"))) +
  geom_line() +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") 


# look at 1980s
# all years 
ggplot(filter(key.cases.long, trade == "change_ln_exports" | trade == 
                     "change_us_arms" & (year >= 1988 & year <= 2002)), aes(x = year, 
                           y = value,
                           group = factor(ccode),
                           color = factor(ccode))) +
  facet_wrap(~ trade, scales = "free_y",
             labeller = labeller(trade = c("change_ln_exports" = "Exports",
                                           "change_ln_imports" = "Imports",
                                           "change_ihs_balance" = "Trade Balance",
                                           "us_arms" = "Arms Transfers",
                                           "change_us_arms" = "Change Arms Transfers",
                                           "change_trade" = "Total Trade"))) +
  geom_line() +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  xlim(1988, 2002)
