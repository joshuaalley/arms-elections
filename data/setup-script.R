# Joshua Alley
# Set up analysis for economic bargaining in alliances


# load packages
library(conflicted)
library(tidyverse)
library(haven)
library(marginaleffects)
library(pscl)
library(peacesciencer)
library(brms)
library(systemfit)
library(cspp)
library(countrycode)
library(wesanderson)
library(dyadRobust)
library(modelsummary)
library(gridExtra)
library(lubridate)
library(MCMCpack)
library(doBy)


# set seed
set.seed(12)
# set ggplot theme
theme_set(theme_bw())

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
