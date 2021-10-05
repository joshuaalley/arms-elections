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
library(fastDummies)


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

# function to tidy dr lists
dr.clean <- function(est){
  data.clean <- cbind.data.frame(est$bhat, est$sehat)
  colnames(data.clean) <- c("coef", "se")
  data.clean$variable <- rownames(data.clean)
  data.clean$model <-  deparse(substitute(est))
  data.clean
}
