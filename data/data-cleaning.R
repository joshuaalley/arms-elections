# Joshua Alley
# data cleaning 



# elections
pres.elections <- seq(from = 1952, to = 2020, by = 4)

# load Blankenship promises data
promises.data <- read_dta("data/ReplicationData_ISQ_Promises.dta") %>%
  select(ccode, year, statements_americas,
         us_intervene_amer, 
         lag_latency_pilot, lag_rivalry_thompson,
         adv_signal_last3, log_distance,
         sample_cow,
         sample_atop)

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
    lead_election = lead(election)
  )

# Create a vector of presidential administrations
promises.annual$president <- pres.full(promises.annual)

# time to election
promises.annual$time_to_elec <- rep(seq(from = 3, to = 0, by = -1),
                                    length.out = nrow(promises.annual))



# combine latent support with trade data
# first create it with modified scripts
source("data/revise-latent-supp/Generate US Support.R")
source("data/revise-latent-supp/Generate UK Support.R")
source("data/revise-latent-supp/Generate France Support.R")

latent.supp <- bind_rows(phi.us,
                         phi.fr,
                         phi.uk) %>%
  drop_na(year) %>%
  mutate(
    lag_median = lag(median),
    change_median = median - lag_median,
    ccode2 = countrycode(country,
                         origin = "country.name",
                         destination = "cown"))
# w/ original
# latent.supp <- read.csv("data/Major Protege Dataset v1.1.csv")%>%
#                  drop_na(year) %>%
#                  mutate(
#                    lag_median = lag(median),
#                    change_median = median - lag_median)
# latent.supp$ccode2[latent.supp$year < 1991 & latent.supp$ccode2 == 255] <- 260


# use peacesciencer to 
# generate dyad-year data on other dimensions
dyadic.trade <- create_dyadyears(system = "cow",
                                 mry = TRUE, 
                                 directed = FALSE) %>%
  add_gml_mids() %>%
  add_democracy() %>%
  add_atop_alliance() %>%
  add_igos()
# flow1 is imports to ccode1 from ccode2
# flow2 is vice-versa- imports by ccode2 from ccode1




# pull better trade data
cepii.data <- read_dta("data/TRADHIST_v4.dta") %>%
  filter(year > 1948) %>%
  select(
    iso_o, iso_d, year,
    FLOW, FLOW_0, 
    IPTOT_o, IPTOT_d, 
    XPTOT_o, XPTOT_d,
    TARIFF_d, 
    GDP_o, GDP_d,
    POP_o, POP_d,
    Distw, Comlang, Contig,
    Evercol
  ) %>%
  mutate(
    ccode1 = countrycode(iso_o, origin = "iso3c",
                         destination = "cown"), 
    ccode2 = countrycode(iso_d, origin = "iso3c",
                         destination = "cown")
  ) %>% # dem major powers
  filter(ccode1 == 2 | ccode1 == 200 |
           ccode1 == 220) %>%
  mutate(
    # express trade in billions
    XPTOT_o = XPTOT_o / 1000000000,
    IPTOT_o = IPTOT_o / 1000000000,
    lag_exports = lag(XPTOT_o),
    lag_imports = lag(IPTOT_o),
    
    change_gdp_o = GDP_o - lag(GDP_o),
    change_gdp_d = GDP_d - lag(GDP_d),
    
    ln_gdp_o = log(GDP_o),
    ln_gdp_d = log(GDP_d),
    ln_pop_o = log(POP_o),
    ln_pop_d = log(POP_d),
    ln_exports = log(XPTOT_o + 1), 
    ln_imports = log(IPTOT_o + 1), 
    ln_distw = log(Distw),
    change_exports = XPTOT_o - lag_exports,
    ihs_change_exports = asinh(change_exports),
    change_imports = IPTOT_o - lag_imports,
    ihs_change_imports = asinh(change_imports),
    lag_ln_exports = lag(ln_exports),
    change_ln_exports = ln_exports - lag_ln_exports,
    lag_ln_imports = lag(ln_imports),
    change_ln_exports = ln_exports - lag_ln_exports,
    trade_balance = ln_exports - ln_imports,
    lag_trade_balance = lag(trade_balance),
    change_trade_balance = trade_balance - lag_trade_balance
  )



# load archigos data
archigos <- read.delim("data/archigos.txt") %>%
  filter(ccode == 2 |
           ccode == 200 |
           ccode == 220) %>%
  select(
    ccode, leader,
    startdate, enddate
  ) %>%
  mutate(
    year = year(startdate),
    start_month = month(startdate),
    end_month = month(enddate),
    months = end_month - start_month,
    maj_year = ifelse(months >= 6, 
                      1, 0)
  ) %>% # leader for more than half of year
  filter(maj_year == 1)

# load elections data
nelda <- read_dta("data/nelda.dta") %>%
  filter(types == "Executive") %>%
  select(
    ccode, year, mmdd, electionid,
    nelda8, # term limit
    nelda21 # incumbent running
  ) %>%
  mutate(
    election = 1,
    incumbent = ifelse(nelda21 == "yes", 1, 0)
  )


# make the merger
dyadic.trade.major <- latent.supp %>% 
  select(year, ccode1, ccode2, 
         median, lag_median,
         change_median) %>% 
  left_join(cepii.data) %>%
  left_join(dyadic.trade) %>%
  rename(ccode = ccode1) 
# add elections data 
dyadic.trade.major <- left_join(dyadic.trade.major,
                                nelda) %>%
  left_join(archigos)
# fill in elections w/ zeros
dyadic.trade.major$election[is.na(dyadic.trade.major$election)] <- 0
dyadic.trade.major$election[is.na(dyadic.trade.major$lag_election)] <- 0
dyadic.trade.major$election[is.na(dyadic.trade.major$lead_election)] <- 0
dyadic.trade.major$incumbent[is.na(dyadic.trade.major$incumbent)] <- 0


# full data cleaning
dyadic.mp.ally <- dyadic.trade.major %>%
  group_by(ccode, ccode2) %>%
  mutate(
    lead_election = lead(election),
    lag_election = lag(election),
    lag_atop_defense = lag(atop_defense),
    lag_xm_qudsest2 = lag(xm_qudsest2)
  ) %>% 
  ungroup() %>%
  mutate(
    # prior support and changes
    prior_leader_supp = ifelse(leader != lag(leader),
                               lag_median, NA)
  ) %>%
  fill(prior_leader_supp, 
       .direction = "down") %>% 
  mutate(
    change_leader_supp = median - prior_leader_supp
  ) %>%
  group_by(ccode, ccode2, leader) %>%
  mutate(
    # running mean support
    mean_leader_supp = rollapply(median, 2, mean,
                                 align ='right', fill = lag_median),
    # running sum of changes
    total_change_supp = rollapply(change_leader_supp, 
                                  2, sum,
                                  align ='right', 
                                  fill = lag(change_leader_supp))
  ) %>% 
  select(ccode, ccode2, year, electionid, leader, mean_leader_supp,
         lag_median, prior_leader_supp, change_leader_supp, 
         total_change_supp,
         everything()) 
# w/ original measure, add: 
# filter(atop_defense == 1 &
#    (ccode == 2 | ccode == 200 | ccode == 220))


# dyad id
dyadic.mp.ally$dyad.id <- group_indices(dyadic.mp.ally, ccode, ccode2) 





# get us exports
us.trade.ally <- filter(dyadic.mp.ally, 
                        ccode == 2) %>%
  ungroup() %>%
  rename(
    us_imports = IPTOT_o,
    us_exports = XPTOT_o,
    us.code = ccode,
    ccode = ccode2
  ) %>%
  left_join(promises.data) %>%
  mutate(
    lag_us_exports = lag(us_exports),
    lag_us_imports = lag(us_imports),
    change_us_exports = us_exports - lag_us_exports,
    change_us_imports = us_imports - lag_us_imports,
    growth_us_exports = change_us_exports / lag_us_exports,
    growth_us_imports = change_us_imports / lag_us_imports,
    lag_us_trade = lag(FLOW) / 1000000000, 
    ln_us_trade = log((FLOW / 1000000000) + 1),
    lag_ln_trade = lag(ln_us_trade),
    change_us_trade = (FLOW - lag(FLOW)) / 1000000000,
    change_lnus_trade = ln_us_trade - lag_ln_trade,
    ihs_change_trade = asinh(change_us_trade),
    growth_us_trade = change_us_trade / lag_us_trade, 
    ln_us_exports = log(us_exports), 
    ln_us_imports = log(us_imports), 
    lag_ln_exports = lag(ln_exports),
    lag_ln_imports = lag(ln_imports),
    change_lnus_exports = ln_us_exports - lag_ln_exports,
    change_lnus_imports = ln_us_imports - lag_ln_imports,
    # presidential partisanship
    rep_pres = ifelse((year >= 1921 & year <= 1932) | # Harding, Coolidge, Hoover
                        (year >= 1953 & year <= 1960) | # Ike
                        (year >= 1969 & year <= 1976) | # Nixon/Ford
                        (year >= 1981 & year <= 1992) | # Reagan/Bush
                        (year >= 2001 & year <= 2008) | # HW Bush
                        (year >= 2017), # Trump
                      1, 0),
    change_pres = ifelse(rep_pres != lag(rep_pres), 1, 0)
  ) %>%
  left_join(select(promises.annual, year, 
                   president, time_to_elec)) %>%
  group_by(president, ccode) %>%
  mutate(
    # running mean stat
    total_statements = rollapply(statements_americas, 2, sum,
                                 align ='right', fill = 0),
    ln_total_statements = log(total_statements + 1)
  )
us.trade.ally$dyad.id <- group_indices(us.trade.ally, us.code, ccode) 

# fix INF values- small states w/ no trade in lagged year
us.trade.ally$growth_us_trade[us.trade.ally$growth_us_trade == Inf] <- 1
us.trade.ally$growth_us_exports[us.trade.ally$growth_us_exports == Inf] <- 1
us.trade.ally$growth_us_imports[us.trade.ally$growth_us_imports == Inf] <- 1
