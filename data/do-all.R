# Joshua Alley
# execute all code in arms-elections paper


# source calls 
# set up- functions and packages
source("data/setup-script.R", echo = TRUE)
# data cleaning
source("data/data-cleaning.R", echo = TRUE)
source("data/clean-state-data.R", echo = TRUE)
# analysis of arms deals 
source("data/us-arms-deals-raw.R", echo = TRUE)
source("data/us-arms-deals.R", echo = TRUE)
source("data/deals-robustness-check.R", echo = TRUE)
# analysis of contracting
source("data/joint-est-raw.R", echo = TRUE)
source("data/joint-est.R", echo = TRUE)

# analysis by sector and other checks
source("data/sector-analysis.R", echo = TRUE)
source("data/other-democracies.R", echo = TRUE)
source("data/interactions-check.R", echo = TRUE)
