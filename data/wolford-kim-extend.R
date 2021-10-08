# Joshua Alley
# Wolford and Kim 2017 Replication for US


# load data
wk.data <- read_dta("data/WolfordKimReplication.dta") %>%
             mutate(
               denied_count = round(totpet * prdeny)
             )
promises.data <- read_dta("data/ReplicationData_ISQ_Promises.dta") %>%
                  select(ccode, year, statements_americas,
                         us_intervene_amer, 
                         lag_latency_pilot, lag_rivalry_thompson,
                         adv_signal_last3, log_distance,
                         sample_cow,
                         sample_atop)


### US data 
# generate US data and add Blankenship statements
wk.data.us <- filter(wk.data, ccode1 == 2) %>%
  rename(ccode = ccode2) %>%
  left_join(promises.data) %>%
  mutate(
    lag_statements = lag(statements_americas),
    nonzero_pet = ifelse(totpet > 0, 1, 0)
  ) %>%
  left_join(select(latent.supp, year, ccode2, 
                   median, lag_median,
                   change_median) %>%
              rename(ccode = ccode2))


# plot statements over time

# elections
pres.elections <- seq(from = 1952, to = 2020, by = 4)

# create promises
promises.annual <- promises.data %>%
                    group_by(year) %>%
                    summarize(
                      total_statements = sum(statements_americas, na.rm = TRUE),
                      .groups = "keep"
                    ) %>%
                    ungroup() %>%
                    mutate(
                      lag_statements = lag(total_statements),
                      election = ifelse(year %in% pres.elections, 1, 0),
                      lag_election = lag(election),
                      lead_election = lead(election),
                      # presidential partisanship
                      rep_pres = ifelse((year >= 1921 & year <= 1932) | # Harding, Coolidge, Hoover
                                          (year >= 1953 & year <= 1960) | # Ike
                                          (year >= 1969 & year <= 1976) | # Nixon/Ford
                                          (year >= 1981 & year <= 1992) | # Reagan/Bush
                                          (year >= 2001 & year <= 2008) | # HW Bush
                                          (year >= 2017), # Trump
                                        1, 0),
                      change_pres = ifelse(rep_pres != lag(rep_pres), 1, 0)
                    )

# Create a vector of presidential administrations
promises.annual$president <- pres.full(promises.annual)
  
# time to election
promises.annual$time_to_elec <- rep(seq(from = 3, to = 0, by = -1),
                                    length.out = nrow(promises.annual))


# add to country data
wk.data.us <- left_join(wk.data.us,
                              select(promises.annual,
                                     year, election,
                                     lag_election, lead_election,
                                     rep_pres, change_pres,
                                     president))

wk.data.us.ally <- filter(wk.data.us, sample_atop == 1)


# plot promises by country
ggplot(wk.data.us.ally, aes(x = year, y = statements_americas,
                            color = factor(change_pres),
                            group = 1)) +
  facet_wrap(~ partneriso3,
             scales = "free_y") +
  geom_point() +
  geom_line() +
  theme_bw()


# plot statements over time
ggplot(promises.annual, aes(x = year, y = total_statements,
                            shape = factor(change_pres),
                            color = president,
                            group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw()


# model statements as a function of elections
# poisson
statements.elect <- glm(total_statements ~ lag_statements +
                          lag_election + election +
                          lead_election +
                          rep_pres + change_pres,
                        data = promises.annual,
                        family = "poisson")
summary(statements.elect)

# negative binomial
statements.elect.nb <- MASS::glm.nb(total_statements ~ lag_statements +
                                      lag_election + election +
                                      lead_election +
                                      rep_pres + change_pres,
                        data = promises.annual)
summary(statements.elect.nb)


## look at AD petitions and denials over time now
petition.annual <- wk.data.us.ally %>%
                   group_by(year) %>%
                   summarize(
                     total_pet = sum(totpet, na.rm = TRUE),
                     avg_deny = mean(prdeny, na.rm = TRUE),
                     total_denied = sum(denied_count, na.rm = TRUE)
                   ) %>%
                 left_join(promises.annual) %>%
                 ungroup() %>%
                filter(!is.nan(avg_deny)) %>%
                mutate(
                  lag_petitions = lag(total_pet),
                  lag_denied = lag(total_denied),
                  lag_avg_deny = lag(avg_deny)
                )

# plot 
ggplot(petition.annual, aes(x = year, y = avg_deny,
                            color=factor(president), 
                            group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw()


# plot total petitions
ggplot(petition.annual, aes(x = year, y = total_pet,
                            color=factor(president), 
                            group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw()


# model total petitions
# ols
petitions.elect.ols <- lm(total_pet ~ lag_petitions +
                         lag_election + election +
                         lead_election +
                         rep_pres + change_pres,
                       data = petition.annual)
summary(petitions.elect.ols)


# poisson
petitions.elect <- glm(total_pet ~ lag_petitions +
                          lag_election + election +
                          lead_election +
                          rep_pres + change_pres,
                        data = petition.annual,
                        family = "poisson")
summary(petitions.elect)


# negative binomial
petitions.elect.nb <- MASS::glm.nb(total_pet ~ lag_petitions +
                                      lag_election +
                                      lead_election +
                                      rep_pres + change_pres,
                                    data = petition.annual)
summary(petitions.elect.nb)



# model total denied
# plot total denied
ggplot(petition.annual, aes(x = year, y = total_denied,
                            color=factor(president), 
                            group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw()

# poisson
denied.elect <- glm(total_denied ~ total_pet +
                         lag_denied +
                         lag_election + election +
                         lead_election +
                          lag_statements +
                         rep_pres + change_pres,
                       data = petition.annual,
                       family = "poisson")
summary(denied.elect)


# avg denied: binomial
# poisson
denied.elect.avg <- glm(avg_deny ~ total_pet +
                      lag_avg_deny +
                      lag_election + election +
                      lead_election +
                      lag_statements +
                      rep_pres + change_pres,
                    data = petition.annual,
                    family = binomial(link = "logit"))
summary(denied.elect.avg)


# replicate wolford and Kim's results
# everything but the SE
wk.def.deny <- glm(prdeny ~ Llngdprat + Lcow_def2 +
                    Llngdprat:Lcow_def2 +
                  Llngdppc1 + Llngdppc2 + 
                  Lpolity1 + Lpolity2 + Ladcap2 
                  + Lbothmem + Llnimpdep1 + Llnexpdep1 +
                Lcow_imr,
    data = wk.data,
   family = binomial(link = "logit"))
summary(wk.def.deny)


# same thing, but w/o the ratio
# total petitions vs percentage denied
ggplot(wk.data, aes(x = totpet, y = prdeny)) + geom_jitter(alpha = .5)
# Poisson model 
wk.def.deny.nr <- glm(denied_count ~ Llngdprat + Lcow_def2 +
                     Llngdprat:Lcow_def2 +
                      totpet +
                     Llngdppc1 + Llngdppc2 + 
                     Lpolity1 + Lpolity2 + Ladcap2 + 
                     Lbothmem + Llnimpdep1 + Llnexpdep1 +
                     Lcow_imr,
                    family = "poisson",
                   data = wk.data)
summary(wk.def.deny.nr)

# delete all no petition observations- adjust for IMR of non-zero(approx heckman)
# Ideal is a two-stage model w/ selection
# problem w/ two-stage is that one needs an instrument




### US analysis

# number of petitions
us.pet <- zeroinfl(totpet ~ Llngdprat + 
                     lead_election + election + lag_election +
                     change_pres + rep_pres +
                         lag_statements +
                         lag_latency_pilot + lag_rivalry_thompson +
                         adv_signal_last3 + log_distance +
                         Llngdprat:lag_statements +
                         Llngdppc1 + Llngdppc2 + 
                         Lpolity2 + Ladcap2 +
                         Llnimpdep1 + Llnexpdep1,
                       dist = "negbin",
                       data = wk.data.us)
summary(us.pet)

# number of petitions
us.all.pet <- zeroinfl(totpet ~ Llngdprat + 
                         lead_election + election + lag_election +
                         #change_pres + #rep_pres +
                         lag_statements +
                         lag_latency_pilot + lag_rivalry_thompson +
                         adv_signal_last3 + log_distance +
                         Llngdprat:lag_statements +
                         Llngdppc1 + Llngdppc2 + 
                         Lpolity2 + Ladcap2 +
                         Llnimpdep1 + Llnexpdep1,
                      dist = "negbin",
                      data = wk.data.us.ally)
summary(us.all.pet)
# us.pet.marg <- cplot(us.all.pet, 
#       x = "Llngdprat", dx = "lag_statements", 
#       what = "effect",
#       rug = TRUE,
#       draw = TRUE)

# model zero petitions 
wk.data.us.comp <- wk.data.us.ally %>%
                     select(
                       nonzero_pet, totpet, prdeny, denied_count,
                       Llngdprat, lag_statements,
                       lag_election, election, lead_election,
                       change_pres, rep_pres,
                       lag_latency_pilot, lag_rivalry_thompson,
                         adv_signal_last3, log_distance,
                         Llngdppc1, Llngdppc2, 
                         Lpolity2, Ladcap2, Lbothmem,
                         Llnimpdep1, Llnexpdep1
                     ) %>%
                  drop_na(!c(prdeny, denied_count))

us.nz.pet <- glm(nonzero_pet ~  Llngdprat + 
                   lag_election + election + lead_election +
                     lag_statements + change_pres + rep_pres +
                     Llngdprat:lag_statements +
                   lag_latency_pilot + lag_rivalry_thompson +
                   adv_signal_last3 + log_distance +
      Llngdppc1 + Llngdppc2 + 
      Lpolity2 + Ladcap2 +
    Llnimpdep1 + Llnexpdep1,
    data = wk.data.us.comp,
    family = binomial(link = "probit"))
summary(us.nz.pet)

wk.data.us.comp$imr_nzpet <- sampleSelection::invMillsRatio(us.nz.pet, all = FALSE)$IMR1


# US model of petition denial
us.all.deny <- glm(prdeny ~ Llngdprat + 
                     lag_statements +
                     Llngdprat:lag_statements +
                     lag_election + election + lead_election + 
                     change_pres + rep_pres +
                     lag_latency_pilot + lag_rivalry_thompson +
                     adv_signal_last3 + log_distance +
                     Llngdppc1 + Llngdppc2 + 
                      Lpolity2 + Ladcap2 +
                     Llnimpdep1 + Llnexpdep1 +
                     imr_nzpet,
                   data = wk.data.us.comp,
                   family = binomial(link = "logit"))
summary(us.all.deny)

# margins
us.deny.marg <- cplot(us.all.deny, 
                      x = "Llngdprat", dx = "lag_statements", 
                      what = "effect",
                      rug = TRUE,
                      draw = TRUE)
abline(h = 0)
us.deny.marg

# US model of petition denial: no ratio
wk.data.us.comp.count <- filter(wk.data.us.comp, !is.na(denied_count))
table(wk.data.us.comp.count$denied_count)

# try negative binomial 
us.all.deny.nb <- MASS::glm.nb(denied_count ~ Llngdprat + 
                                 lag_statements +
                                 Llngdprat:lag_statements +
                                 lag_election + election + lead_election + 
                                 change_pres + rep_pres +
                                 lag_latency_pilot + lag_rivalry_thompson +
                                 adv_signal_last3 + log_distance +
                                 Llngdppc1 + Llngdppc2 + 
                                 Lpolity2 + Ladcap2 +
                                 Llnimpdep1 + Llnexpdep1 +
                                 imr_nzpet,
                        init.theta = 50,
                        control=glm.control(maxit=2500, trace = FALSE),
                      data = wk.data.us.comp.count)
summary(us.all.deny.nb)

# had to really massage it to get any results, looks under dispersed enough
# for Poisson
us.all.deny.nr <- glm(denied_count ~Llngdprat + 
                        lag_statements +
                        Llngdprat:lag_statements +
                        lag_election + election + lead_election + 
                        change_pres + rep_pres +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + log_distance +
                        Llngdppc1 + Llngdppc2 + 
                        Lpolity2 + Ladcap2 +
                        Llnimpdep1 + Llnexpdep1 +
                        imr_nzpet,
                     family = "poisson",
                   data = wk.data.us.comp.count)
summary(us.all.deny.nr)


# margins
cplot(us.all.deny.nr, 
        x = "Llngdprat", dx = "lag_statements", 
        what = "effect",
        rug = TRUE,
       draw = TRUE)
abline(h = 0)



### Latent Support
wk.data.supp <- wk.data %>%
                  filter(ccode1 != 1) %>%
                 left_join(latent.supp) 

# replicate models:

# peitions and median support: nothing doing
wk.supp.pet <- zeroinfl(totpet ~ Llngdprat + lag_median +
                      Llngdprat:lag_median +
                      Llngdppc1 + Llngdppc2 + 
                      Lpolity2 + Ladcap2 
                    + Lbothmem + Llnimpdep1 + Llnexpdep1,
                    dist = "negbin",
                    data = wk.data.supp)
summary(wk.supp.pet)
wk.pet.marg <- cplot(wk.supp.pet, 
      x = "Llngdprat", dx = "median", 
      what = "effect",
      rug = TRUE,
      draw = TRUE)
abline(h = 0)


# denial and median support: nothing doing
wk.supp.deny <- glm(prdeny ~ Llngdprat + lag_median +
                     Llngdprat:lag_median +
                     Llngdppc1 + Llngdppc2 + 
                     Lpolity2 + Ladcap2 
                   + Lbothmem + Llnimpdep1 + Llnexpdep1 +
                     Lcow_imr,
                   data = wk.data.supp,
                   family = binomial(link = "logit"))
summary(wk.supp.deny)
