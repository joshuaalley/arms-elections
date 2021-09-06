# Joshua Alley
# Analysis of exports from US to allies around elections 

# use peacesciener package
library(tidyverse)
library(peacesciencer)
library(brms)
library(systemfit)
library(cspp)
library(countrycode)
library(wesanderson)


# generate dyad-year data
dyadic.trade <- create_dyadyears(system = "cow",
                                 mry = TRUE, 
                                 directed = FALSE) %>%
                 add_minimum_distance() %>%
                 add_capital_distance() %>%
                 add_cow_trade() %>%
                 add_sdp_gdp() %>%
                 add_gml_mids() %>%
                 add_democracy() %>%
                 add_atop_alliance() %>%
                 add_igos()
# flow1 is imports to ccode1 from ccode2
# flow2 is vice-versa- imports by ccode2 from ccode1


# get us exports
us.trade <- filter(dyadic.trade, 
                   ccode1 == 2 & year >= 1946) %>%
            rename(
              us_imports = flow1,
              us_exports = flow2,
              ccode = ccode2
            ) %>%
            left_join(promises.data) %>%
            left_join(promises.annual) %>%
            mutate(
              lag_statements = lag(statements_americas),
              ln_exports = log(us_exports + 1),
              ln_imports = log(us_imports + 1),
              lag_ln_exports = lag(ln_exports),
              lag_ln_imports = lag(ln_imports),
              change_ln_exports = ln_exports - lag_ln_exports,
              trade_balance = us_exports - us_imports,
              ihs_trade_balance = asinh(trade_balance),
              lag_ihs_trade_balance = lag(ihs_trade_balance)
            ) %>%
            group_by(president, ccode) %>%
            drop_na(lag_statements) %>% # messes w/ cumulative sum
            mutate(total_pres_statements =
                     cumsum(statements_americas),
                   ln_total_statements = log(total_pres_statements + 1)) %>%
            ungroup()

# allies only
us.trade.ally <- filter(us.trade, atop_defense == 1)

# look at the raw data
ggplot(us.trade, aes(x = us_exports)) + geom_histogram()
ggplot(us.trade, aes(x = ln_exports)) + geom_histogram()
# allied states
ggplot(us.trade.ally, aes(x = us_exports)) + geom_histogram()
ggplot(us.trade.ally, aes(x = ln_exports)) + geom_histogram()

# statements
ggplot(us.trade.ally, aes(x = total_pres_statements)) + geom_histogram()
ggplot(us.trade.ally, aes(x = ln_total_statements)) + geom_histogram()

ggplot(us.trade, aes(x = us.imports)) + geom_histogram()
ggplot(us.trade, aes(x = ln_imports)) + geom_histogram()

# look at raw by election
ggplot(us.trade.ally, aes(x = factor(time_to_elec), y = ln_exports,
                          color = ln_total_statements)) +
    geom_jitter() +
  scale_color_gradientn(
    colours = wes_palette("Zissou1", 100, type = "continuous")) 

# look at trade balance by election
ggplot(us.trade.ally, aes(x = factor(time_to_elec), y = trade_balance,
                          color = ln_total_statements)) +
  geom_jitter()  +
  scale_color_gradientn(
    colours = wes_palette("Zissou1", 100, type = "continuous")) 

ggplot(us.trade.ally, aes(x = factor(time_to_elec), y = ihs_trade_balance,
                          color = ln_total_statements)) +
  geom_jitter() +
  scale_color_gradientn(
    colours = wes_palette("Zissou1", 100, type = "continuous")) 


# ols w/o any dyad corrections: US exports
us.exports.all <- lm(ln_exports ~ lag_ln_exports + lag_ln_imports +
                       time_to_elec + time_to_elec:ln_total_statements +
                        ln_total_statements + change_pres +
                        lag_latency_pilot + lag_rivalry_thompson +
                        adv_signal_last3 + xm_qudsest2 + 
                         wbgdp2011est1 + wbgdp2011est2 +
                       mindist + capdist + gmlmidongoing + dyadigos,
                       data = us.trade.ally)
summary(us.exports.all)


# ols w/o any dyad corrections: US imports
us.imports.all <- lm(ln_imports ~ lag_ln_exports + lag_ln_imports +
                       time_to_elec + time_to_elec:ln_total_statements +
                       ln_total_statements + change_pres +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       wbgdp2011est1 + wbgdp2011est2 +
                       mindist + capdist + gmlmidongoing + dyadigos,
                     data = us.trade.ally)
summary(us.imports.all)


# SUR exports and imports
trade.sur <- systemfit(list(
  ln_exports ~ lag_ln_exports +
    time_to_elec + time_to_elec:ln_total_statements +
    ln_total_statements + change_pres +
    lag_latency_pilot + lag_rivalry_thompson +
    adv_signal_last3 + xm_qudsest2 + 
    wbgdp2011est1 + wbgdp2011est2 +
    mindist + capdist + gmlmidongoing + dyadigos,
  ln_imports ~ lag_ln_imports +
    time_to_elec + time_to_elec:ln_total_statements +
    ln_total_statements + change_pres +
    lag_latency_pilot + lag_rivalry_thompson +
    adv_signal_last3 + xm_qudsest2 + 
    wbgdp2011est1 + wbgdp2011est2 +
    mindist + capdist + gmlmidongoing + dyadigos),
  data = us.trade.ally
)
# residual correlations are weaker than expected
summary(trade.sur)


# trade balance
us.balance.all <- lm(ihs_trade_balance ~ lag_ihs_trade_balance +
                       time_to_elec + time_to_elec:ln_total_statements +
                       ln_total_statements + change_pres +
                       lag_latency_pilot + lag_rivalry_thompson +
                       adv_signal_last3 + xm_qudsest2 + 
                       wbgdp2011est1 + wbgdp2011est2 +
                       mindist + capdist + gmlmidongoing + dyadigos,
                     data = us.trade.ally)
summary(us.balance.all)


# quick brms model
bf.exports.all <- brmsformula(ln_exports ~ lag_ln_exports + lag_ln_imports +
                                time_to_elec + time_to_elec:ln_total_statements +
                                ln_total_statements + change_pres +
                                lag_latency_pilot + lag_rivalry_thompson +
                                adv_signal_last3 + xm_qudsest2 + 
                                wbgdp2011est1 + wbgdp2011est2 +
                                mindist + gmlmidongoing + dyadigos +
                                (1 | ccode) + (1 | year),
                              center = TRUE) +
                                student()
exports.priors <- c(
                   set_prior("normal(0, .5)", class = "b"),
                   set_prior("normal(0, 1)", class = "Intercept"),
                   set_prior("normal(0, 1)", class = "sigma"),
                   set_prior("normal(0, 1)", class = "sd")
) 

# fit the model
brm.ally.exports <- brm(bf.exports.all, 
                     data = us.trade.ally,
                     prior = exports.priors,
                     iter = 2000, warmup = 1000,
                     chains = 4, cores = 4,
                     backend = "cmdstanr",
                      control = list(max_treedepth = 20))
summary(brm.ally.exports, prob = .9)
mcmc_plot(brm.ally.exports, "hist_by_chain", pars = "sigma")
mcmc_plot(brm.ally.exports, "intervals", pars = "b",
          prob = .9)
mcmc_plot(brm.ally.exports, "intervals", 
          pars = c("b_ln_total_statements",
                   "b_time_to_elec",
                   "b_time_to_elec:ln_total_statements"),
          prob = .9)


### exports by state 

# load exports data by state
state.exports <- read.csv("data/state-exports-country.csv")
# remove commas and measure in billions
state.exports$value <- as.numeric(gsub(",","", state.exports$value)) / 1000000

# remove unknown state
state.exports <- filter(state.exports, state != "Unknown") 

# state exports ccode
state.exports$ccode <- countrycode(origin = "country.name",
                                   sourcevar = state.exports$destination,
                                   destination = "cown")

# add state level info
state.exports.dyad <-  left_join(state.exports,
                        (create_dyadyears(system = "cow",
                                          mry = TRUE, 
                                          directed = FALSE) %>%
                          add_cow_mids() %>%
                          add_atop_alliance() %>%
                          add_igos() %>%
                          rename(
                            ccode = ccode2
                          ) %>%
                        filter(ccode1 == 2 & year >= 1946) %>%   
                        select(ccode, year,
                               cowmidongoing, xm_qudsest2,
                               atop_defense, dyadigos))
                      )
# bring in PWT data


# state data from CSPP 
cspp.data <- get_cspp_data(vars =
            c("minwage", "foreign_born",
              "poptotal", "ogdp", # total industry GDP
              "labfree", "econfree", "regfree", # labor, econ, regulation freedoms
              "atotspt", # total state and local spending
              "fairtrade", "rep_unified", "s_diffs", "h_diffs"
              ),
  years = seq(2002, 2019))

# add to dataframe
state.exports.ally <- left_join(state.exports, cspp.data) %>%
                       filter(atop_defense == 1)
state.exports$dyadid <- group_indices(state.exports, state, destination) 



