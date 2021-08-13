# Joshua Alley
# Wolford and Kim 2017 Replication for US


# load packages
library(haven)
library(tidyverse)
library(margins)


# load data
wk.data <- read_dta("data/WolfordKimReplication.dta")
promises.data <- read_dta("data/ReplicationData_ISQ_Promises.dta") %>%
                  select(ccode, year, statements_americas,
                         us_intervene_amer, sample_cow,
                         sample_atop)
latent.supp <- read.csv("data/Major Protege Dataset v1.1.csv")



### US data 
# generate US data and add Blankenship statements
wk.data.us <- filter(wk.data, ccode1 == 2) %>%
               rename(ccode = ccode2) %>%
               left_join(promises.data) %>%
                mutate(
                  lag_statements = lag(statements_americas)
                  )

# US Allies only
wk.data.us.ally <- filter(wk.data, ccode1 == 2) %>%
  rename(ccode = ccode2) %>%
  left_join(promises.data) %>%
  mutate(
    lag_statements = lag(statements_americas)
  ) %>%
  filter(sample_atop == 1)


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
# delete all no petition observations- really a two-stage model w/ selection
# problem w/ two-stage is that one needs an instrument




### US analysis

# number of petitions
us.all.pet <- MASS::glm.nb(totpet ~ Llngdprat + 
                             lag_statements +
                             Llngdprat:lag_statements +
                             Llngdppc1 + Llngdppc2 + 
                             Lpolity2 + Ladcap2 +
                             Llnimpdep1 + Llnexpdep1 +
                             Lcow_imr,
                           data = wk.data.us.ally)
summary(us.all.pet)


# US model of petition denial
us.all.deny <- glm(prdeny ~ Llngdprat + 
                     lag_statements +
                     Llngdprat:lag_statements +
                     Llngdppc1 + Llngdppc2 + 
                      Lpolity2 + Ladcap2 +
                     Llnimpdep1 + Llnexpdep1 +
                     Lcow_imr,
                   data = wk.data.us.ally,
                   family = binomial(link = "logit"))
summary(us.all.deny)

# margins
us.deny.marg <- cplot(us.all.deny, 
                      x = "Llngdprat", dx = "lag_statements", 
                      what = "effect",
                      rug = TRUE,
                      draw = TRUE)
us.deny.marg




### Latent Support
wk.data.supp <- wk.data %>%
                  filter(ccode1 != 1) %>%
                 left_join(latent.supp) %>%
              filter(Lcow_def2 == 1)

# replicate models: hold alliance in support

# denial and median support: nothing doing
wk.supp.deny <- glm(prdeny ~ Llngdprat + median +
                     Llngdprat:median +
                     Llngdppc1 + Llngdppc2 + 
                     Lpolity2 + Ladcap2 
                   + Lbothmem + Llnimpdep1 + Llnexpdep1 +
                     Lcow_imr,
                   data = wk.data.supp,
                   family = binomial(link = "logit"))
summary(wk.supp.deny)
