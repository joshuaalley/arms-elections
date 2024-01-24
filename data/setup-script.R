# Joshua Alley
# Set up script for arms deals and US elections


# load packages
library(conflicted)
library(tidyverse)
library(haven)
library(marginaleffects)
library(pscl)
library(peacesciencer)
library(cmdstanr)
library(bayesplot)
library(MASS)
library(brms)
library(systemfit)
library(cspp)
library(countrycode)
library(wesanderson)
library(modelsummary)
library(gridExtra)
library(lubridate)
library(fastDummies)
library(vdemdata)
library(ordbetareg)
library(ggridges)
library(kableExtra)
library(interflex)
library(fixest)

# set seed
set.seed(12)
# set ggplot theme
theme_set(theme_bw(base_size = 14))
# set modelsummary TeX output
options(modelsummary_format_numeric_latex = "plain")


# manage conflicts
conflict_scout()
# set preferences 
conflict_prefer("lag", "dplyr")
conflict_prefer("lead", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("expand", "tidyr")
conflict_prefer("pack", "tidyr")
conflict_prefer("unpack", "tidyr")
conflict_prefer("chol2inv", "Matrix")
conflict_prefer("Position", "ggplot2")
conflict_prefer("rcond", "Matrix")
conflict_prefer("ar", "brms")
conflict_prefer("marginal_effects", "brms")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("between", "dplyr")
conflict_prefer("combine", "dplyr")
conflict_prefer("order_by", "doBy")
conflict_prefer("some", "purrr")
conflict_prefer("ar", "brms")
conflict_prefer("first", "dplyr")
conflict_prefer("rhat", "bayesplot")


# function to create presidential indicator
pres.full <- function(data){
data$president <- rep(NA, times = nrow(data))
data$president[data$year >= 1945 & data$year < 1953] <- "Truman"
data$president[data$year >= 1953 & data$year < 1961] <- "Eisenhower"
data$president[data$year >= 1961 & data$year < 1964] <- "Kennedy"
data$president[data$year >= 1964 & data$year < 1969] <- "Johnson"
data$president[data$year >= 1969 & data$year <= 1974] <- "Nixon"
data$president[data$year > 1974 & data$year < 1977] <- "Ford"
data$president[data$year >= 1977 & data$year < 1981] <- "Carter"
data$president[data$year >= 1981 & data$year < 1989] <- "Reagan"
data$president[data$year >= 1989 & data$year < 1993] <- "HW Bush"
data$president[data$year >= 1993 & data$year < 2001] <- "Clinton"
data$president[data$year >= 2001 & data$year < 2009] <- "W Bush"
data$president[data$year >= 2009 & data$year < 2017] <- "Obama"
data$president[data$year >= 2017] <- "Trump"
data$president
}

# function to tidy dyad robust lists
dr.clean <- function(est){
  data.clean <- cbind.data.frame(est$bhat, est$sehat)
  colnames(data.clean) <- c("coef", "se")
  data.clean$variable <- rownames(data.clean)
  data.clean$model <-  deparse(substitute(est))
  data.clean
}


coef.names.map = c("deals" = "Total Arms Deals",
                   "election" = "Election",
                   "atop_defense" = "Defense Pact",
                   "ally" = "US Ally",
                   "time_to_elec" = "Years to Election",
                   "v2x_polyarchy" = "Partner Polyarchy",
                   "cowmidongoing" = "Ongoing MID",
                   "gwot" = "Global War on Terror",
                   "cold_war" = "Cold War",
                   "rep_pres" = "Republican President",
                   "ln_petrol_rev" = "Log Petrol Revenue",
                   "ln_rgdp" = "Log Partner GDP",
                   "ln_pop" = "Log Partner Population",
                   "ln_distw" = "Log Pop. Weighted Distance)",
                   "Contig" = "Contiguous",
                   "Comlang" = "Common Language",
                   "Evercol" = "Former Colony")

coef.names.map.state = c(
                   "obligations" = "Contracts",
                   "lag_obligations" = "Lag Contracts",
                   "deals" = "Arms Deals",
                   "swing" = "Swing State",
                   "core" = "Core State",
                   "s_comp" = "Senate Vote Difference",
                   "time_to_elec" = "Time to Election",
                   "incumbent" = "Senate Incumbent",
                   "gwot" = "Global War\non Terror",
                   "poptotal" = "Population (Rescaled)",
                   "ln_ngdp" = "Log GDP (Rescaled)",
                   "rep_pres" = "Republican President")



# typical observations for US
typical.func.us <- function(x){
  dat <- datagrid(model = x, 
                  time_to_elec_0 = c(0, 1),
                  time_to_elec_1 = c(0, 1),
                  time_to_elec_2 = c(0, 1),
                  ally = 1,
                  v2x_polyarchy = fivenum)
  dat$rep_pres <- 0
  dat 
}

typical.func.us.all <- function(x){
  dat <- datagrid(model = x, 
                  time_to_elec_0 = c(0, 1),
                  time_to_elec_1 = c(0, 1),
                  time_to_elec_2 = c(0, 1),
                  ally = c(0, 1),
                  v2x_polyarchy = fivenum)
  dat$rep_pres <- 0
  dat 
}

# Marginal effects function 
me.us.elec <- function(model, formula, rm.wt, data){
  
  # no dyad robust for US 
  # marginal effects 
  me.est <- slopes(model, variables = c("time_to_elec_0"), conf_level = .9,
                            newdata = datagrid(model = model, 
                                               ally = 1,
                                               v2x_polyarchy = fivenum))
  
   # me.def.plot <- plot_slopes(model, conf_level = .9,
   #                        variables = c("v2x_polyarchy"),
   #                       condition = "time_to_elec", draw = FALSE)
  
  # predicted outcomes
  pred.out <- predictions(model, conf_level = .9,
                          newdata = typical.func.us(model))  %>%
    rowwise() %>%
    mutate(
      dum_sum = sum(time_to_elec_0, time_to_elec_1, time_to_elec_2)
    ) %>%
    filter(dum_sum <= 1) %>%
    mutate(
      time_to_elec = case_when(
        time_to_elec_0 == 1 ~ 0,
        time_to_elec_1 == 1 ~ 1,
        time_to_elec_2 == 1 ~ 2,
        (time_to_elec_0 == 0 &
           time_to_elec_1 == 0 &
           time_to_elec_2 == 0) ~ 3
      )
    )
  
  # full fitted draws
  fit.out <- posterior_epred(model, 
                    newdata = typical.func.us(model))
  
  res <- list(me.est, pred.out, fit.out)
}


# Marginal effects function 
me.us.elec.all <- function(model, formula, rm.wt, data){
  
  # no dyad robust for US 
  # marginal effects 
  me.est <- slopes(model, variables = c("ally"), conf_level = .9,
                   newdata = datagrid(model = model, 
                                      time_to_elec_0 = c(0, 1),
                                      time_to_elec_1 = c(0, 1),
                                      time_to_elec_2 = c(0, 1),
                                      v2x_polyarchy = fivenum))
  
  me.def.plot <- plot_slopes(model, conf_level = .9,
                             variables = c("ally", "v2x_polyarchy"),
                             condition = "time_to_elec_0", draw = FALSE)
  
  # predicted outcomes
  pred.out <- predictions(model, conf_level = .9,
                          newdata = typical.func.us.all(model))  %>%
    rowwise() %>%
    mutate(
      dum_sum = sum(time_to_elec_0, time_to_elec_1, time_to_elec_2)
    ) %>%
    filter(dum_sum <= 1) %>%
    mutate(
      time_to_elec = case_when(
        time_to_elec_0 == 1 ~ 0,
        time_to_elec_1 == 1 ~ 1,
        time_to_elec_2 == 1 ~ 2,
        (time_to_elec_0 == 0 &
           time_to_elec_1 == 0 &
           time_to_elec_2 == 0) ~ 3
      )
    )
  
  # full fitted draws
  fit.out <- posterior_epred(model, 
                             newdata = typical.func.us.all(model))
  
  res <- list(me.est, pred.out, me.def.plot, fit.out)
}


# function to transfer dummy estimates to year count 
# after making model predictions
year.dum.pred <- function(estimates){
  dum.pred <- predictions(estimates, conf_level = .9,
                             newdata = datagrid(model = estimates,
                                                ally = 1,
                                                time_to_elec_0 = c(0, 1),
                                                time_to_elec_1 = c(0, 1),
                                                time_to_elec_2 = c(0, 1),
                                                v2x_polyarchy = fivenum))

# transfer predictions back
dum.pred <- dum.pred %>%
  rowwise() %>%
  mutate(
    dum_sum = sum(time_to_elec_0, time_to_elec_1, time_to_elec_2)
  ) %>%
  filter(dum_sum <= 1) %>%
  mutate(
    time_to_elec = case_when(
      time_to_elec_0 == 1 ~ 0,
      time_to_elec_1 == 1 ~ 1,
      time_to_elec_2 == 1 ~ 2,
      (time_to_elec_0 == 0 &
         time_to_elec_1 == 0 &
         time_to_elec_2 == 0) ~ 3
    )
  )

dum.pred

}
