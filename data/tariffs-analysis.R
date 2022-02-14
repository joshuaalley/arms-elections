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
                  xm_qudsest2 = mean(xm_qudsest2, na.rm = TRUE),
                  cowmidongoing = max(cowmidongoing, na.rm = TRUE),
                  dyadigos = mean(dyadigos, na.rm = TRUE),
                  Distw = mean(Distw, na.rm = TRUE),
                  Comlang = min(Comlang, na.rm = TRUE),
                  Contig = min(Contig, na.rm = TRUE),
                  Evercol = min(Evercol, na.rm = TRUE),
                  election = max(election, na.rm = TRUE)
                ) %>%
              mutate(
                country = "European Union",
                ccode1 = 2,
                ccode2 = 1000, # set arbitrary ccode
              ) %>%
             filter(year <= 2010) %>%
             left_join(select(eu.tariffs,
                              year, ccode, 
                              avg_tariff, wavg_tariff,
                              min_tariff, log_max_tariff)) %>%
             select(-country)
  
# load data from WITS
tariffs.mp <- read.csv("data/mp-all-tariffs.csv") %>%
  rename(
    year = Tariff.Year,
    avg_tariff = Simple.Average,
    wavg_tariff = Weighted.Average,
    min_tariff = Minimum.Rate,
    max_tariff = Maximum.Rate
  ) %>%
  mutate(
    log_max_tariff = log(max_tariff + 1)
  )
sum(tariffs.mp$year == tariffs.mp$Trade.Year)
tariffs.mp$ccode1 <- countrycode(sourcevar = tariffs.mp$Partner.Name,
                             origin = "country.name",
                             destination = "cown")
tariffs.mp$ccode2 <- countrycode(sourcevar = tariffs.mp$Reporter.Name,
                                 origin = "country.name",
                                 destination = "cown")

# join w/ support data 
tariffs.all <- left_join(select(ungroup(dyadic.mp.ally),
                                ccode1, ccode2, year,
                                election, mean_leader_supp,
                                lag_election, lead_election,
                                v2clstown,
                                  incumbent, xm_qudsest2, 
                                  cowmidongoing, dyadigos,
                                  change_gdp_o, change_gdp_d, Distw,
                                  Comlang, Contig, Evercol),
                         select(tariffs.mp,
                                year, ccode1, ccode2, 
                                avg_tariff, wavg_tariff,
                                min_tariff, log_max_tariff)) %>%
                  group_by(ccode1, ccode2) %>% 
                  mutate(
                    lag_avg_tariff = lag(avg_tariff),
                    change_avg_tariff = avg_tariff - lag_avg_tariff,
                    lag_wavg_tariff = lag(wavg_tariff),
                    change_wavg_tariff = wavg_tariff - lag_wavg_tariff,
                    lag_log_max_tariff = lag(log_max_tariff),
                    change_log_max_tariff = log_max_tariff - lag_log_max_tariff
                  ) %>%
                  bind_rows(eu.support) %>%
                  # avoid perfect dyadid collinearity 
                  drop_na(change_wavg_tariff,
                              election, mean_leader_supp,
                              v2clstown,
                              lag_election, lead_election,
                              incumbent, xm_qudsest2,
                              cowmidongoing, dyadigos,
                              change_gdp_o, change_gdp_d)
summary(tariffs.all$year)

# sample size drops b/c dyads in tariffs.mp are not all allies and 
# European states enter the EU, leaving no tariffs of in those dyads. 
# dyad id
tariffs.all$dyad.id <- group_indices(tariffs.all, ccode1, ccode2)
tariffs.all$election <- factor(tariffs.all$election)

# remove dyads w/ one obs- creates fit issues
filter(as.data.frame(table(tariffs.all$dyad.id)), Freq == 1)
tariffs.all <- filter(tariffs.all, dyad.id != 2 & dyad.id != 72)


# avg tariffs
# rlm w/o any dyad corrections: avg allied tariff
mp.tariff.all <- rlm(asinh(change_avg_tariff) ~ 
                         election*mean_leader_supp +
                       lag_election + lead_election +
                       v2clstown +
                         incumbent + xm_qudsest2 +
                         cowmidongoing + dyadigos +
                         change_gdp_o + change_gdp_d +
                       factor(dyad.id),
                     maxit = 40,
                      data = tariffs.all)
summary(mp.tariff.all)
plot(mp.tariff.all$residuals, mp.tariff.all$w)

all.tariff <- plot_cme(mp.tariff.all,
         effect = "mean_leader_supp",
         condition = "election") +
           geom_hline(yintercept = 0) +
         scale_x_discrete("Election", labels = c(`0` = "No", `1` = "Yes")) + 
         labs(
           y = "Marginal Effect of Avg. Leader Support",
           title = "Average Tariffs"
          )
all.tariff



# rlm w/o any dyad corrections: weighted avg tariff
# Weighted average tariff: Average tariffs, weighted by value of imports
mp.wtariff.all <- rlm(asinh(change_wavg_tariff) ~ 
                       election*mean_leader_supp +
                       lag_election + lead_election +
                        v2clstown +
                       incumbent + xm_qudsest2 + 
                       cowmidongoing + dyadigos +
                       change_gdp_o + change_gdp_d + 
                        factor(dyad.id),
                     data = tariffs.all)
summary(mp.wtariff.all)
plot(mp.wtariff.all$residuals, mp.wtariff.all$w)

all.wtariff <- plot_cme(mp.wtariff.all,
                   effect = "mean_leader_supp",
                   condition = "election") +
               geom_hline(yintercept = 0) +
               scale_x_discrete("Election", labels = c(`0` = "No", `1` = "Yes")) + 
               labs(
                 y = "Marginal Effect of Avg. Leader Support",
                title = "Weighted Average Tariffs")
all.wtariff

# switch interaction in ME plot
plot_cme(mp.wtariff.all,
         condition = "mean_leader_supp",
         effect = "election") +
  geom_hline(yintercept = 0) +
  labs(
    x = "Avg. Leader Support",
    y = "Marginal Effect of Election",
    title = "Weighted Average Tariffs")


# too little variation in minimum tariffs
table(tariffs.all$min_tariff) 
summary(tariffs.all$min_tariff)
# rlm w/o dyad corrections: max tariffs
mp.maxtariff.all <- rlm(change_log_max_tariff ~ 
                        election*mean_leader_supp +
                        lag_election + lead_election +
                          v2clstown +
                        incumbent + xm_qudsest2 + 
                        cowmidongoing + dyadigos +
                        change_gdp_o + change_gdp_d + 
                          factor(dyad.id),
                      data = tariffs.all)
summary(mp.maxtariff.all)

all.maxtariff <- plot_cme(mp.maxtariff.all,
                        effect = "mean_leader_supp",
                        condition = "election") +
  geom_hline(yintercept = 0) +
  scale_x_discrete("Election", labels = c(`0` = "No", `1` = "Yes")) + 
  labs(
    x = "Election",
    y = "Marginal Effect of Avg. Leader Support",
    title = "Maximum Tariff Rate")
all.maxtariff


# present tariff results
tariff.model.list <- list(mp.tariff.all, mp.maxtariff.all)
names(tariff.model.list) <- c("Average Tariffs",
                          "Maximum Tariffs")

modelsummary(tariff.model.list,
             "figures/tariff-model-coefs.tex",
             statistic = c("({conf.low}, {conf.high})"),
             coef_map = coef.names.map,
             coef_omit = c("ccode"),
             gof_map = list(
               list("raw" = "nobs", "clean" = "N", "fmt" = 0)))


# plot interaction terms
grid.arrange(all.tariff, all.maxtariff, nrow = 1)
tariff.me <- arrangeGrob(all.tariff, all.maxtariff, nrow = 1)
ggsave("figures/tariff-me.png", tariff.me, height = 6, width = 8)
