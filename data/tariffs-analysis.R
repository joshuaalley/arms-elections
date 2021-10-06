# Joshua Alley
# Analysis of Allied Tariffs

# load data from WITS
tariffs <- read.csv("data/allied-tariffs.csv") %>%
            rename(
              country = Reporter.Name,
              year = Tariff.Year,
              avg_tariff = Simple.Average,
              wavg_tariff = Weighted.Average,
              min_tariff = Minimum.Rate,
              max_tariff = Maximum.Rate
            ) %>%
            mutate(
              log_max_tariff = log(max_tariff + 1)
            )
sum(tariffs$year == tariffs$Trade.Year)
tariffs$ccode <- countrycode(sourcevar = tariffs$country,
                             origin = "country.name",
                             destination = "cown")

# EU support
eu.tariffs <- filter(tariffs, country == "European Union")
eu.support <- filter(us.trade.ally, 
                     ((ccode >= 200 & ccode <= 325) | ccode == 350) & 
                       year >= 1988
                         ) %>%
              group_by(year) %>%
                summarize(
                  mean_leader_supp = mean(mean_leader_supp, na.rm = TRUE),
                  incumbent = max(incumbent),
                  change_gdp_d = sum(change_gdp_d, na.rm = TRUE),
                  change_gdp_o = max(change_gdp_o, na.rm = TRUE),
                  lag_latency_pilot = max(lag_latency_pilot),
                  lag_rivalry_thompson = max(lag_rivalry_thompson),
                  adv_signal_last3 = max(adv_signal_last3),
                  xm_qudsest2 = mean(xm_qudsest2, na.rm = TRUE),
                  gmlmidongoing = max(gmlmidongoing, na.rm = TRUE),
                  dyadigos = mean(dyadigos, na.rm = TRUE),
                  Distw = mean(Distw, na.rm = TRUE),
                  Comlang = min(Comlang, na.rm = TRUE),
                  Contig = min(Contig, na.rm = TRUE),
                  Evercol = min(Evercol, na.rm = TRUE),
                  time_to_elec = max(time_to_elec, na.rm = TRUE)
                ) %>%
              mutate(
                country = "European Union"
              ) %>%
             filter(year <= 2010) %>%
             left_join(select(eu.tariffs,
                              year, ccode, 
                              avg_tariff, wavg_tariff,
                              min_tariff, log_max_tariff)) %>%
             select(-country)
  
  


# need to think about the EU

# join w/ support data 
tariffs.all <- left_join(select(ungroup(us.trade.ally),
                                ccode, year,
                                time_to_elec, mean_leader_supp,
                                  incumbent,
                                  lag_latency_pilot, lag_rivalry_thompson,
                                  adv_signal_last3, xm_qudsest2, 
                                  gmlmidongoing, dyadigos,
                                  change_gdp_o, change_gdp_d, Distw,
                                  Comlang, Contig, Evercol),
                         select(tariffs,
                                year, ccode, 
                                avg_tariff, wavg_tariff,
                                min_tariff, log_max_tariff)) %>%
                  mutate(
                    lag_avg_tariff = lag(avg_tariff),
                    lag_wavg_tariff = lag(wavg_tariff),
                    lag_log_max_tariff = lag(log_max_tariff)
                  ) %>%
                  drop_na(avg_tariff) %>% # lots of missing over time 
                  bind_rows(eu.support)
summary(tariffs.all$year)


# avg tariffs
# rlm w/o any dyad corrections: avg allied tariff
us.tariff.all <- rlm(avg_tariff ~ 
                        lag_avg_tariff +
                        time_to_elec*mean_leader_supp +
                        incumbent +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + xm_qudsest2 + 
                        gmlmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw +
                        Comlang + Contig + Evercol,
                      data = tariffs.all)
summary(us.tariff.all)

plot_cme(us.tariff.all,
         effect = "mean_leader_supp",
         condition = "time_to_elec") +
  geom_hline(yintercept = 0) +
  labs(
    x = "Time to Election",
    y = "Marginal Effect of Avg. Leader Support",
    title = "Average Tariffs"
  )



# rlm w/o any dyad corrections: weighted avg tariff
# Weighted average tariff: Average tariffs, weighted by value of imports
us.wtariff.all <- rlm(wavg_tariff ~ 
                       #lag_wavg_tariff +
                       time_to_elec*mean_leader_supp +
                       incumbent +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       gmlmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + Distw +
                       Comlang + Contig + Evercol,
                     data = tariffs.all)
summary(us.wtariff.all)

all.wtariff <- plot_cme(us.wtariff.all,
                   effect = "mean_leader_supp",
                   condition = "time_to_elec") +
               geom_hline(yintercept = 0) +
               labs(
                 x = "Time to Election",
                 y = "Marginal Effect of Avg. Leader Support",
                title = "Weighted Average Tariffs")
all.wtariff

# switch interaction in ME plot
plot_cme(us.wtariff.all,
         condition = "mean_leader_supp",
         effect = "time_to_elec") +
  geom_hline(yintercept = 0) +
  labs(
    x = "Avg. Leader Support",
    y = "Marginal Effect of Time to Election",
    title = "Weighted Average Tariffs")


# too little variation in minimum tariffs
table(tariffs.all$min_tariff) 
summary(tariffs.all$min_tariff)
# rlm w/o dyad corrections: max tariffs
us.maxtariff.all <- rlm(log_max_tariff ~ 
                        #lag_log_max_tariff +
                        time_to_elec*mean_leader_supp +
                        incumbent +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + xm_qudsest2 + 
                        gmlmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + Distw +
                        Comlang + Contig + Evercol,
                      data = tariffs.all)
summary(us.maxtariff.all)

all.maxtariff <- plot_cme(us.maxtariff.all,
                        effect = "mean_leader_supp",
                        condition = "time_to_elec") +
  geom_hline(yintercept = 0) +
  labs(
    x = "Time to Election",
    y = "Marginal Effect of Avg. Leader Support",
    title = "Maximum Tariff Rate")
all.maxtariff


# present tariff results
tariff.model.list <- list(us.wtariff.all, us.maxtariff.all)
names(tariff.model.list) <- c("Allied Tariffs",
                          "Maximum Tariffs")

modelsummary(tariff.model.list,
             "figures/tariff-model-coefs.tex",
             statistic = c("({conf.low}, {conf.high})"),
             coef_map = coef.names.map.us,
             coef_omit = c("ccode"),
             gof_map = list(
               list("raw" = "nobs", "clean" = "N", "fmt" = 0)))


# plot interaction terms
grid.arrange(all.wtariff, all.maxtariff, nrow = 1)
tariff.me <- arrangeGrob(all.wtariff, all.maxtariff, nrow = 1)
ggsave("figures/tariff-me.png", tariff.me, height = 6, width = 8)
