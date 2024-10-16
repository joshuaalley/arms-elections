# Joshua Alley
# presentation figures

library(ggcarly)

# first cycles figure
ggplot(filter(pois.democ.pred,
              v2x_polyarchy == min(pois.democ.pred$v2x_polyarchy) |
                v2x_polyarchy == median(pois.democ.pred$v2x_polyarchy) |
                v2x_polyarchy == max(pois.democ.pred$v2x_polyarchy) ),
       aes(y = estimate, 
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
       y = "Arms Deals",
       x = "Years to Presidential Election") +
  theme_carly_presents()
ggsave("presentation/democ-deals-pred.png", height = 6, width = 8)


# Drive by allies
ggplot(filter(pois.deals.est[[2]],
              v2x_polyarchy == min(pois.deals.est[[2]]$v2x_polyarchy) |
                v2x_polyarchy == median(pois.deals.est[[2]]$v2x_polyarchy) |
                v2x_polyarchy == max(pois.deals.est[[2]]$v2x_polyarchy)), 
                 aes(y = estimate, 
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
  labs(y = "Arms Deals",
       x = "Years to Presidential Election") +
  theme_carly_presents() +
  theme(legend.position = "bottom")
ggsave("presentation/democ-deals-ally.png", height = 6, width = 8,
       dpi = 800)


# look at regime change
us.deals.democ.key <- filter(us.deals.democ.change,
                             str_detect(country, 
                                        "Greece|Portugal"
                             ))
ggplot(us.deals.democ.key, aes(x = time_to_elec,
                               y = deals.year,
                               group = democ_bin,
                               fill = democ_bin)) +
  facet_wrap(~ country) +
  scale_x_reverse() +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_grey(start = .5, end = .2) +
  labs(x = "Years to Election",
       y = "Deals\nper Year",
       fill = "Regime",
       title = "Regime Changes and Arms Deal Timing") +
  theme_carly_presents() +
  theme(legend.position = "bottom") 
ggsave("presentation/deals-regime-change.png", height = 6, width = 8,
       dpi = 800)


### contracts
# total defense contracts
ggplot(contracts.data.clean, aes(x =  factor(time_to_elec,
                                             ordered = TRUE,
                                             levels = c("3", "2",
                                                        "1", "0")),
                                 y = all_contracts)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = "Total\n Prime\n Contracts",
       x = "Years to Presidential Election",
       title = "Aggregate Defense Contracting") +
  theme_carly_presents()
ggsave("presentation/contract-cycles.png", height = 8, width = 10)


# Arms deals ME
ggplot(deals.est, aes(x = factor(swing), y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1, linewidth = 2) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(`0` = "No", `1` = "Yes")) +
  labs(
    title = "Marginal Impact of Arms Deals",
    x = "Swing State",
    y = "Impact of\nArms Deals"
  ) +
  theme_carly_presents()
ggsave("presentation/deals-me.png", height = 6, width = 8)

# plot intervals 
ggplot(pred.08.key, aes(y = reorder(state, diff.median), 
                                            x = diff.median)) +
  facet_wrap(~ comp, scales = "free_y",
             ncol = 1) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = diff.lower, xmax = diff.upper)) +
  labs(
    x = "Difference in Contracts",
  ) +
  theme_carly_presents() +
  theme(
    axis.title.y = element_blank(),
  ) 
ggsave("presentation/state-08-est.png",
       height = 12, width = 10)

base.map <- ggplot() +
  geom_polygon(data = states, aes(x = long, y=lat,
                                  group = group),
               color = "black",
               linewidth = .5,
               fill = NA
  ) 

ggplot(data = states.08.data, aes(x = long, y=lat,
                                  fill = diff.median,
                                  group = group)) +
  facet_wrap(~ comp, ncol = 2) +
  geom_polygon(aes(group = group
  ),
  color = "grey",
  linewidth = 1
  ) +
  scale_fill_viridis_c() +
  labs(title = "2007-2008: 32 Additional Arms Deals",
       fill = "Posterior\nMedian\nDifference") +
  theme_carly_presents() +
  theme(
    legend.position = c(0.75, 0.3),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
ggsave("presentation/state-08-map.png",
       height = 6, width = 8)

