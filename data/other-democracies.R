# Joshua Alley
# analysis of other democracies arms sales and timing
# nelda, and arms deal data from SIPRI


# load three datasets and modify in the same way 
de.deals <- read.csv("data/de-trade-register.csv")
fr.deals <- read.csv("data/fr-trade-register.csv")
uk.deals <- read.csv("data/uk-trade-register.csv")


# clean all of them in the same way
clean.dem.data <- function(data){
  
  colnames(data) <- c("country2", "country1",
                      "year", "number",
                      "weapon", "weapon.des", 
                      "delivered", "delivery.years",
                      "status", "comments",
                      "tiv.per.unit", "total.tiv",
                      "tiv.delivered")
  
  data$deal <- 1
  
  data.sum <- data %>%
  mutate(
    ccode1 = countrycode(country1, origin = "country.name",
                         destination = "cown"), 
    ccode2 = countrycode(country2, origin = "country.name",
                         destination = "cown")
  ) %>% 
  group_by(year, ccode1, ccode2) %>%
    summarize(
      deals = n(),
      .groups = "keep"
    )
  # output
  data.sum 
}

# output
dem.data.list <- list(uk.deals, fr.deals, de.deals)

dem.data.clean <- lapply(dem.data.list, clean.dem.data)
names(dem.data.clean) <- c("United Kingdom", "France", "Germany")

deals.dem <- bind_rows(dem.data.clean,
                            .id = "seller")
# check German ccodes- FRG
deals.dem$ccode1[deals.dem$seller == "Germany" &
                        deals.dem$year < 1990] <- 260


# grab dyadic controls from relevant data 
dyadic.cont.dem <- dyadic.cont %>%
                    filter(
                      ccode1 == 200 | ccode1 == 220 | 
                        ccode1 == 260 | ccode1 == 255
                    ) %>%
                    select(
                      ccode1, ccode2, year,
                      atop_defense, dyadigos, cowmidongoing
                    )

cepii.data.dem <- read_dta("data/TRADHIST_v4.dta") %>%
  filter(year > 1948) %>%
  select(
    iso_o, iso_d, year,
    Dist_d, Comlang, Evercol
  ) %>%
  mutate(
    ccode1 = countrycode(iso_o, origin = "iso3c",
                         destination = "cown"), 
    ccode2 = countrycode(iso_d, origin = "iso3c",
                         destination = "cown")
  ) %>% # dem major powers
  filter(ccode1 %in% dyadic.cont.dem$ccode1) %>%
  group_by(ccode1, ccode2) %>% 
  mutate(
    # key controls
    ln_distw = log(Dist_d)
  )

# controls dem
dyad.dem.data <- left_join(dyadic.cont.dem, cepii.data.dem)


# add in deals data
dem.deals.data <- left_join(dyad.dem.data, deals.dem)
# NA deals are 0
dem.deals.data$deals[is.na(dem.deals.data$deals)] <- 0


# grab other controls from US sample
dem.deals.data <- dem.deals.data %>%
                   rename(ccode = ccode2) %>%
                   left_join(
                     distinct(select(us.trade.ally,
                            ccode, year,
                            v2x_polyarchy,
                            ln_petrol_rev, cold_war,
                            eu_member, ln_rgdp,
                            ln_pop))
                   )

# use nelda for elections
nelda.relab <- read_dta("data/nelda.dta") %>%
  mutate(
    election = case_when(
      ccode == 200 & types == "Legislative/Parliamentary" ~ 1,
      (ccode == 255 | ccode == 260) &
        types == "Legislative/Parliamentary" ~ 1,
      ccode == 220 & types == "Excutive" ~ 1,
      .default = 0
    )
  ) %>%
  filter(election == 1) %>%
  select(
    ccode, year, 
    election, 
  ) %>%
  rename(
    ccode1 = ccode
  ) %>%
  distinct() 

dem.deals.data <- dem.deals.data %>%
                    left_join(nelda.relab) %>%
                    group_by(ccode1) %>%
                    mutate(
                      lag_election = lead(election)
                    )

# fill elections variables 
dem.deals.data$election[is.na(dem.deals.data$election)] <- 0
dem.deals.data$lag_election[is.na(dem.deals.data$lag_election)] <- 0



### analyze deals
# all other democraies 
dem.deals.model <- brm(bf(deals ~ 
                             election*v2x_polyarchy +
                             lag_election*v2x_polyarchy +
                             cold_war + cowmidongoing +
                             ln_petrol_rev + 
                             ln_rgdp + 
                             ln_pop + ln_distw + 
                             Comlang + Evercol,
                           hu ~ atop_defense + cowmidongoing +
                            v2x_polyarchy + ln_rgdp,
                           center = FALSE),
                        family = hurdle_poisson(),
                        backend = "cmdstanr",
                        prior = c(prior(normal(0, .5), class = "b")),
                        cores = 4,
                        refresh = 500,
                        data = dem.deals.data)
summary(dem.deals.model)

# predicted outcomes
# use a function for this and country components
dem.deals.pres <- function(model, title){
pred.dem.deals <- predictions(model, conf_level = .9,
            newdata = datagrid(model = model,
                               atop_defense = 1,
                               election = c(0, 1),
                               lag_election = c(0, 1),
                               v2x_polyarchy = fivenum(dem.deals.data$v2x_polyarchy,
                                                       na.rm = TRUE))) %>%
  mutate(
    time_to_elec = factor(case_when(
      election == 1 ~ "Election\nYear",
      lag_election == 1 ~ "Year\nBefore",
      .default = "None"
  ),
  ordered = TRUE,
  levels = c("None", "Year\nBefore",
             "Election\nYear"))
  ) %>%
  filter(
    !(election == 1 & lag_election == 1)
  )

plot.dem <- ggplot(pred.dem.deals, aes(y = estimate, 
                            x = time_to_elec)) +
  facet_wrap(~ v2x_polyarchy,
             ncol = 5) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  size = 1,
                  linewidth = 2) +
  labs(title = title,
       y = "Predicted Arms Deals",
       x = "Election Proximity")
plot.dem
# output
out <- list(pred.dem.deals, plot.dem)
}

# output pooled results
dem.deals.res <- dem.deals.pres(model = dem.deals.model,
                                title = "Democratic Major Powers")
dem.deals.res[[2]]

# fit models for each country
# UK first
uk.deals.model <- brm(bf(deals ~ 
                            election*v2x_polyarchy +
                            lag_election*v2x_polyarchy +
                            cold_war + cowmidongoing +
                            ln_petrol_rev + 
                            ln_rgdp + 
                            ln_pop + ln_distw + 
                            Comlang + Evercol,
                          hu ~ atop_defense + cowmidongoing +
                            v2x_polyarchy + ln_rgdp,
                          center = FALSE),
                       family = hurdle_poisson(),
                       backend = "cmdstanr",
                       prior = c(prior(normal(0, .5), class = "b")),
                       cores = 4,
                       refresh = 500,
                       data = filter(dem.deals.data,
                                     ccode1 == 200))
summary(uk.deals.model)

# results
uk.deals.res <- dem.deals.pres(uk.deals.model,
                               title = "United Kingdom")
uk.deals.res[[2]]

# france next
fr.deals.model <- brm(bf(deals ~ 
                           election*v2x_polyarchy +
                           lag_election*v2x_polyarchy +
                           cold_war + cowmidongoing +
                           ln_petrol_rev + 
                           ln_rgdp + 
                           ln_pop + ln_distw + 
                           Comlang + Evercol,
                         hu ~ atop_defense + cowmidongoing +
                           v2x_polyarchy + ln_rgdp,
                         center = FALSE),
                      family = hurdle_poisson(),
                      backend = "cmdstanr",
                      prior = c(prior(normal(0, .5), class = "b")),
                      cores = 4,
                      refresh = 500,
                      data = filter(dem.deals.data,
                                    ccode1 == 220))
summary(fr.deals.model)

# results
fr.deals.res <- dem.deals.pres(fr.deals.model,
                               title = "France")
fr.deals.res[[2]]

# Germany last
de.deals.model <- brm(bf(deals ~ 
                           election*v2x_polyarchy +
                           lag_election*v2x_polyarchy +
                           cold_war + 
                           ln_petrol_rev + 
                           ln_rgdp + 
                           ln_pop + ln_distw + 
                           Comlang + Evercol,
                         hu ~ atop_defense + 
                           v2x_polyarchy + ln_rgdp,
                         center = FALSE),
                      family = hurdle_poisson(),
                      backend = "cmdstanr",
                      prior = c(prior(normal(0, .5), class = "b")),
                      cores = 4,
                      refresh = 500,
                      data = filter(dem.deals.data,
                                    ccode1 == 255 | ccode1 == 260))
summary(de.deals.model)

# results
de.deals.res <- dem.deals.pres(de.deals.model,
                               title = "Germany")
de.deals.res[[2]]


# combine all these figures
grid.arrange(dem.deals.res[[2]], uk.deals.res[[2]],
             fr.deals.res[[2]], de.deals.res[[2]])
other.dem.res <- arrangeGrob(dem.deals.res[[2]], uk.deals.res[[2]],
            fr.deals.res[[2]], de.deals.res[[2]])
ggsave("appendix/other-dem-res.png", other.dem.res,
       height = 12, width = 16)
