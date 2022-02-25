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
ggplot(us.arms.sum, aes(x = factor(time_to_elec), y = us_arms,
                        color = factor(atop_defense))) +
  geom_boxplot()


# electoral differences: ally or not: Cold War
ggplot(us.arms.sum, aes(x = factor(time_to_elec), y = us_arms,
                        color = factor(atop_defense))) +
     facet_wrap(~ cold_war) +
     geom_boxplot()
