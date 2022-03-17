# Joshua Alley
# Set up analysis for economic bargaining in alliances


# load packages
library(conflicted)
library(tidyverse)
library(haven)
library(marginaleffects)
library(pscl)
library(peacesciencer)
library(cmdstanr)
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
library(interflex)
library(vdemdata)
library(caseMatch)


# set seed
set.seed(12)
# set ggplot theme
theme_set(theme_bw())
# set modelsummary TeX output
options(modelsummary_format_numeric_latex = "plain")

# manage conflicts
conflict_scout()
# set preferences 
conflict_prefer("lag", "dplyr")
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


coef.names.map = c("lag_exports" = "Lag Exports",
                   "lag_imports" = "Lag Imports",
                   "lag_trade_balance" = "Lag Trade Balance",
                   "election" = "Election",
                   "atop_defense" = "Defense Pact",
                   "time_to_elec" = "Years to Election",
                   "time_to_elec:atop_defense" = "Defense Pact x Years to Election",
                   "mean_leader_supp" = "Change Leader Support", 
                   "election:mean_leader_supp" = "Election x Change Leader Support",
                   "time_to_elec:mean_leader_supp" = "Years to Election x Change Leader Support",
                   "lag_election" = "Lag Election",
                   "lead_election" = "Lead Election", 
                   "incumbent" = "Incumbent",
                   "xm_qudsest2" = "Partner Democracy",
                   "v2clstown" = "Partner Economic Control",
                   "GDP_o" = "US GDP",
                   "change_gdp_o" = "Change US GDP",
                   "GDP_d" = "Partner GDP",
                   "change_gdp_d" = "Change Partner GDP",
                   "Distw" = "Pop. Weighted Distance)",
                   "Contig" = "Contiguous",
                   "Comlang" = "Common Language",
                   "Evercol" = "Former Colony",
                   "cowmidongoing" = "Ongoing MID",
                   "dyadigos" = "Shared IGOs",
                   "lag_latency_pilot" = "Lag Partner Latency",
                   "lag_rivalry_thompson" = "Lag Rivalry",
                   "adv_signal_last3" = "Prior Adversary Signal",
                   "eu_member" = "EU Member",
                   "cold_war" = "Cold War",
                   "rep_pres" = "Republican President",
                   "change_ln_imports" = "Change Ln(Imports)",
                   "change_ln_exports" = "Change Ln(Exports",
                   "lag_us_arms" = "Lag Ln(Arms Transfers)",
                   "pred_nz_arms" = "Pred. Prob. of Arms Transfer")
