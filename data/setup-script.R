# Joshua Alley
# Set up script for economic bargaining in alliances


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
                   "ally" = "US Ally",
                   "time_to_elec" = "Years to Election",
                   "democ_bin" = "Democracy",
                   "time_to_elec:ally" = "Ally x Years to Election",
                   "time_to_elec:democ_bin" = "Democracy x Years to Election",
                   "ally:democ_bin" = "Ally x Democracy",
                   "time_to_elec:ally:democ_bin" = "Ally x Years to Election\nx Democracy",
                   "time_to_elec:atop_defense" = "Defense Pact x Years to Election",
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

coef.names.map.state = c(
                   "lag_ln_obligations" = "Lag Contracts",
                   "pred_deals" = "Pred. Arms Deals",
                   "pred_deals:gwot" = "Pred. Arms Deals: GWOT",
                   "gwot" = "Global War\non Terror",
                   "swing" = "Swing State",
                   "swing:gwot" = "Swing: GWOT",
                   "core" = "Core State",
                   "s_comp" = "Senate Vote Difference",
                   "time_to_elec" = "Time to Election",
                   "incumbent" = "Senate Incumbent",
                   "poptotal" = "Population (Rescaled)",
                   "ln_ngdp" = "Log GDP (Rescaled)",
                   "rep_pres" = "Republican President")



# typical observations for US
typical.func.us <- function(x){
  dat <- datagrid(model = x, time_to_elec = c(0, 1, 2, 3),
                  ally = c(0, 1),
                  v2x_polyarchy = fivenum)
  dat$rep_pres <- 0
  dat 
}

# Marginal effects function 
me.us.elec <- function(model, formula, rm.wt, data){
  
  # no dyad robust for US 
  # marginal effects 
  me.est <- slopes(model, variables = c("ally"),
                            newdata = datagrid(model = model, 
                                               time_to_elec = c(0, 1, 2, 3),
                                               v2x_polyarchy = fivenum))
  
   me.def.plot <- plot_cme(model, variables = c("ally", "v2x_polyarchy"),
                         condition = "time_to_elec", draw = FALSE)
  
  # predicted outcomes
  pred.out <- predictions(model, 
                          newdata = typical.func.us(model))
  
  # full fitted draws
  fit.out <- posterior_epred(model, 
                    newdata = typical.func.us(model))
  
  res <- list(me.est, pred.out, me.def.plot, fit.out)
}


