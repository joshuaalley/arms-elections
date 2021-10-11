# Joshua Alley
# Wolford and Kim 2017 Replication for US


# load data
wk.data <- read_dta("data/WolfordKimReplication.dta") %>%
             mutate(
               denied_count = round(totpet * prdeny)
             )


### US data 
# generate US data and add Blankenship statements
wk.data.us <- filter(wk.data, ccode1 == 2) %>%
  rename(ccode = ccode2) %>%
  left_join(promises.data) %>%
  mutate(
    lag_statements = lag(statements_americas),
    nonzero_pet = ifelse(totpet > 0, 1, 0)
  )


# add to country data
wk.data.us <- left_join(wk.data.us,
                              select(us.trade.ally,
                                     change_leader_supp,
                                     ln_total_statements,
                                     ccode, year, election, time_to_elec,
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
                                      lag_election + election +
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
                          lag_statements*time_to_elec +
                         rep_pres + change_pres,
                       data = petition.annual,
                       family = "poisson")
summary(denied.elect)


# avg denied: binomial
# poisson
denied.elect.avg <- glm(avg_deny ~ total_pet +
                      lag_avg_deny +
                      lag_statements*time_to_elec +
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
                       Llngdprat, change_leader_supp, time_to_elec, 
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
                   #lag_election + election + lead_election +
                    change_leader_supp*time_to_elec + change_pres + rep_pres +
                    # Llngdprat:lag_statements +
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
                     change_leader_supp*time_to_elec +
                     Llngdprat +
                     #lag_election + election + lead_election + 
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
us.deny.marg <- plot_cme(us.all.deny,
                         effect = "change_leader_supp",
                         condition = "time_to_elec") +
  geom_hline(yintercept = 0) 
us.deny.marg

# US model of petition denial: no ratio
wk.data.us.comp.count <- filter(wk.data.us.comp, !is.na(denied_count))
table(wk.data.us.comp.count$denied_count)

# try negative binomial 
us.all.deny.nb <- MASS::glm.nb(denied_count ~ Llngdprat + 
                                 change_leader_supp*time_to_elec +
                                 #lag_election + election + lead_election + 
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
us.all.deny.nr <- glm(denied_count ~ Llngdprat + 
                        change_leader_supp*time_to_elec +
                        #lag_election + election + lead_election + 
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
plot_cme(us.all.deny.nr,
         effect = "change_leader_supp",
         condition = "time_to_elec") +
  geom_hline(yintercept = 0)

