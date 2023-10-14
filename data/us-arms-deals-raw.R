# Joshua Alley
# raw arms deals

# runs before us-arms-deals 

### raw by country
# total deals- summarize at country-year level and add covariates
us.deals <- us.arms.cat %>%
  group_by(ccode, year) %>%
  select(
    country, ccode, year, deals
  ) %>%
  summarize(
    deals = sum(deals, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  right_join(select(us.trade.ally,
                    ccode, year,
                    atop_defense, ally, ally_democ,
                    cold_war, gwot, democ_bin,
                    v2x_polyarchy, cowmidongoing,
                    rep_pres, time_to_elec, 
                    eu_member, ln_rgdp,
                    ln_pop, ln_distw,
                    Comlang,
                    Contig, Evercol)) %>%
  filter(year >= 1949) %>%
  group_by(ccode) %>%
  mutate(
    var_democ = sd(v2x_polyarchy, na.rm = TRUE),
    nz_deals = ifelse(deals > 0, 1, 0)
  ) %>% # pakistan/east pak duplicate gives warning- drop
  distinct() 
# NA from right join- move to zero
us.deals$deals[is.na(us.deals$deals)] <- 0
us.deals$nz_deals[is.na(us.deals$nz_deals)] <- 0

us.deals$country <- countrycode(sourcevar = us.deals$ccode,
                                         origin = "cown",
                                         destination = "country.name")
# complete cases
us.deals.comp <- drop_na(us.deals) %>%
  group_by(ccode) %>%
  mutate(
    lag_deals = lag(deals),
    change_deals = deals - lag(deals),
  )

# check for labels
fivenum(us.deals.comp$v2x_polyarchy)

# nice labeller
democ.all.labs <- labeller(democ_bin = c(`1` = "Democracy", `0` = "Nondemocracy"),
                           v2x_polyarchy = c(`0.012` = "Minimum\nDemocracy",
                                             `0.176` = "1st Quartile\nDemocracy",
                                             `0.363` = "Median\nDemocracy",
                                             `0.731` = "3rd Quartile\nDemocracy",
                                             `0.926` = "Maximum\nDemocracy"),
                           ally = c(`1` = "US Ally", `0` = "Not Ally"))


# time-series of deals
ggplot(us.deals, 
       aes(x = year,
           y = deals,
           group = ccode)) +
  geom_line()

ggplot(filter(us.deals, ally == 1 & !is.na(democ_bin)), 
       aes(x = factor(time_to_elec,
                      ordered = TRUE,
                      levels = c("3", "2",
                                 "1", "0")),
           y = deals,
           color = factor(democ_bin))) +
  facet_wrap(~ ccode) +
  geom_boxplot(outlier.shape = NA) 


# line plots in latin america
# time-series of deals
ggplot(filter(us.deals, ally == 1 & !is.na(democ_bin) &
                ccode < 200), 
       aes(x = year,
           y = deals,
           color = factor(democ_bin))) +
  facet_wrap(~ ccode) +
  geom_point(aes(shape = factor(time_to_elec))) 

# line plots in Asia/ME
# time-series of deals
ggplot(filter(us.deals, ally == 1 & !is.na(democ_bin) &
                ccode > 600), 
       aes(x = year,
           y = deals,
           color = factor(democ_bin))) +
  facet_wrap(~ ccode) +
  geom_point(aes(shape = factor(time_to_elec))) 


# aggregate summary 
poly.sum <- fivenum(us.deals$v2x_polyarchy)
poly.sum
us.deals.sum <- us.deals %>%
  ungroup() %>% # otherwise case_when is super slow 
  mutate(
    democ_grp = case_when(
      v2x_polyarchy <= poly.sum[2] ~ "1st Quartile",
      v2x_polyarchy > poly.sum[2] &
        v2x_polyarchy <= poly.sum[3] ~ "2nd Quartile",
      v2x_polyarchy > poly.sum[3] &
        v2x_polyarchy <= poly.sum[4] ~ "3rd Quartile",
      v2x_polyarchy > poly.sum[4] ~ "4th Quartile"
    )
  ) %>% 
  group_by(
    democ_grp, time_to_elec, atop_defense
  ) %>%
  summarize(
    deals = sum(deals, na.rm = TRUE),
    n = n(),
    deals.state = deals / n,
    .groups = "keep"
  ) %>%
  drop_na()
table(us.deals.sum$democ_grp)


ggplot(us.deals.sum, aes(x = time_to_elec,
                         y = deals.state,
                         color = factor(atop_defense))) +
  facet_wrap(~ democ_grp) +
  scale_x_reverse() +
  scale_color_grey(
    start = 0.7,
    end = 0.1,
    labels = c(`0` = "No", `1` = "Yes")) +
  geom_point() +
  geom_line() +
  labs(x = "Time to Election",
       y = "Deals per Country in Group",
       color = "Alliance")
ggsave("appendix/deals-democ-raw.png", height = 6, width = 8)


# summary by country 
deals.country.sum <- us.deals %>%
  group_by(
    ccode, time_to_elec,
  ) %>%
  summarize(
    v2x_polyarchy = median(v2x_polyarchy),
    democ_bin = median(democ_bin),
    deals = sum(deals, na.rm = TRUE),
    n = n(),
    deals.year = deals / n,
    .groups = "keep"
  ) %>%
  ungroup() %>% # otherwise case_when is super slow 
  mutate(
    democ_grp = case_when(
      v2x_polyarchy <= poly.sum[2] ~ "1st Quartile",
      v2x_polyarchy > poly.sum[2] &
        v2x_polyarchy <= poly.sum[3] ~ "2nd Quartile",
      v2x_polyarchy > poly.sum[3] &
        v2x_polyarchy <= poly.sum[4] ~ "3rd Quartile",
      v2x_polyarchy > poly.sum[4] ~ "4th Quartile",
    )
  ) %>%
  filter(deals > 15) %>% 
  drop_na()
deals.country.sum$country <- countrycode(sourcevar = deals.country.sum$ccode,
                                         origin = "cown",
                                         destination = "country.name")

# plot it
ggplot(deals.country.sum, aes(x = time_to_elec,
                              y = deals.year,
                              fill = factor(democ_bin))) +
  facet_wrap(~ country) +
  scale_x_reverse() +
  geom_bar(stat = "identity") +
  labs(x = "Time to Election",
       y = "Average Deals per Year",
       color = "Alliance")


# look at changes in regime and deals behavior

# line plot for all high variance states
ggplot(filter(us.deals,
              var_democ >= .1917 & ally == 1), 
       aes(x = year,
           y = deals, 
           group = country,
           color = factor(democ_bin))) +
  facet_wrap(~ country) +
  geom_line()

# now group it 
us.deals.democ.change <- filter(us.deals,
                                var_democ >= .1917) %>% 
               group_by(country, democ_bin, time_to_elec) %>%
               summarize(
                 ally = median(ally),
                 n = n(),
                 deals = sum(deals),
                 deals.year = deals / n,
                 .groups = "keep"
               ) %>%
              filter(ally == 1)
# compare deals in autocratic and democratic
ggplot(us.deals.democ.change, aes(x = time_to_elec,
                                  y = deals.year,
                          group = factor(democ_bin),
                          fill = factor(democ_bin))) +
         facet_wrap(~ country) +
         scale_x_reverse() +
         geom_bar(stat = "identity",
                  position = "dodge") 
  