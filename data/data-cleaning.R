# Joshua Alley
# data cleaning 



# combine latent support with trade data
# first create it with modified scripts
 source("data/revise-latent-supp/Generate US Support.R")
 source("data/revise-latent-supp/Generate UK Support.R")
 source("data/revise-latent-supp/Generate France Support.R")
# combine results
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


# bring in state control of economy
vdem <- vdem %>%
         select(
           country_name, country_id, year, 
           v2x_polyarchy, v2clstown
         ) %>% 
         filter(year >= 1950) 
# cow country codes
vdem$ccode2 <- countrycode(vdem$country_id,
                          origin = "vdem",
                          destination = "cown")
# greater values mean less state control
ggplot(vdem, aes(x = v2clstown)) + geom_histogram()


# add to latent support
latent.supp <- left_join(latent.supp,
                         select(vdem, -c(country_name,
                                         country_id))) %>%
                mutate( # reverse state control: positive = greater control 
                  v2clstown = -1 * v2clstown
                )

# greater values mean less state control
ggplot(latent.supp, aes(x = v2clstown)) + geom_histogram()


# elections
pres.elections <- seq(from = 1952, to = 2020, by = 4)

# elections data
elections.data <- as.data.frame(x = seq(from = 1945, to = 2020, by = 1))
colnames(elections.data) <- "year"

# add elections
elections.data <- mutate(elections.data,
                   election = ifelse(year %in% pres.elections, 1, 0),
                   lag_election = lag(election),
                   lead_election = lead(election))

# Create a vector of presidential administrations
elections.data$president <- pres.full(elections.data)

# time to election
elections.data$time_to_elec <- rep(seq(from = 3, to = 0, by = -1),
                                    length.out = nrow(elections.data))



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
    lag_statements = lag(total_statements)
    ) %>%
  left_join(elections.data)


# use peacesciencer to 
# generate dyad-year data on other dimensions
dyadic.trade <- create_dyadyears(system = "cow",
                                 mry = TRUE, 
                                 directed = FALSE) %>%
  add_cow_mids() %>%
  add_democracy() %>%
  add_atop_alliance() %>%
  add_igos() %>%
  filter(year >= 1949)
# flow1 is imports to ccode1 from ccode2
# flow2 is vice-versa- imports by ccode2 from ccode1
dyadic.trade$atop_defense[dyadic.trade$year == 2019] <- NA
dyadic.trade$atop_defense[dyadic.trade$year == 2020] <- NA




# pull better trade data
cepii.data <- read_dta("data/TRADHIST_v4.dta") %>%
  filter(year > 1948) %>%
  select(
    iso_o, iso_d, year,
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
  group_by(ccode1, ccode2) %>% 
  mutate(
    # key controls
    change_gdp_o = GDP_o - lag(GDP_o),
    change_gdp_d = GDP_d - lag(GDP_d),
    ln_distw = log(Distw),
    
    ln_gdp_o = log(GDP_o),
    ln_gdp_d = log(GDP_d),
    ln_pop_o = log(POP_o),
    ln_pop_d = log(POP_d))
    
    
# save this chunk for IMF data 
imf.dot <- read.csv("data/imf-dot.csv") %>%
            filter(origin == "France" | 
                     origin == "United Kingdom" |
                     origin == "United States") %>%
           pivot_longer(-c(origin, flow_type, destination,
                           origin_imf_code, 
                           destination_imf_code)) %>%
           rename(year = name) %>%
           pivot_wider(id_cols = c(origin, destination, year,
                                   origin_imf_code,
                                   destination_imf_code),
                       names_from = flow_type,
                       values_from = value) %>% 
           mutate(year = as.numeric(substr(year, 2, 5)))

# rebase to 2018 us dollars
cpi.data <- read_dta("data/cpi_avg.dta")

imf.dot <- left_join(imf.dot, cpi.data) %>%
    mutate(
      ccode1 = countrycode(origin_imf_code,
                      origin = "imf", destination = "cown"),
      ccode2 = countrycode(destination_imf_code,
                      origin = "imf", destination = "cown"),
      # measure flows in billions
      imports = imports / 1000000000,
      exports = exports / 1000000000,
      trade_balance = trade_balance / 1000000000,
      # rebase
      imports = (imports * cpi_avg_2018) / cpi_avg,
      exports = (exports * cpi_avg_2018) / cpi_avg,
      total_trade = log(imports + exports + 1),
      change_trade = total_trade - lag(total_trade),
      trade_balance = (trade_balance * cpi_avg_2018) / cpi_avg,
    ln_exports = log(exports + 1), 
    ln_imports = log(imports + 1), 
    lag_exports = lag(exports),
    lag_imports = lag(imports),
    change_exports = exports - lag_exports,
    pc_exports = change_exports / lag_exports,
    change_imports = imports - lag_imports,
    lag_ln_exports = lag(ln_exports),
    change_ln_exports = ln_exports - lag_ln_exports,
    lag_ln_imports = lag(ln_imports),
    change_ln_imports = ln_imports - lag_ln_imports,
    change_ln_exports = ln_exports - lag_ln_exports,
    lag_trade_balance = lag(trade_balance),
    change_trade_balance = trade_balance - lag_trade_balance,
    ihs_trade_balance = asinh(trade_balance),
    lag_ihs_balance = lag(ihs_trade_balance),
    change_ihs_balance = ihs_trade_balance - lag_ihs_balance,
    ihs_change_balance = asinh(change_trade_balance),
  ) 
# fix a couple small ccode issues
# north korea
imf.dot$ccode2[imf.dot$destination == "Korea, Dem. People's Rep. of"] <- 731
# east germany
imf.dot$ccode2[imf.dot$destination == "Eastern Germany"] <- 265

# missing ccodes are now aggregates
imf.dot <- drop_na(imf.dot, ccode2)

# join with cepii data
trade.full <- left_join(imf.dot, cepii.data)



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
dyadic.trade.major <- trade.full %>%
  left_join(dyadic.trade) %>%
  rename(ccode = ccode1)


# add elections data 
dyadic.trade.major <- left_join(dyadic.trade.major,
                                nelda) %>%
  left_join(archigos)
# fill in elections w/ zeros
dyadic.trade.major$election[is.na(dyadic.trade.major$election)] <- 0
dyadic.trade.major$incumbent[is.na(dyadic.trade.major$incumbent)] <- 0

# dyadic.trade.major %>%
dyadic.trade.major <- dyadic.trade.major %>% 
  rename(ccode1 = ccode) %>%
  group_by(ccode1, ccode2) %>%
  mutate(
    lag_atop_defense = lag(atop_defense),
    lag_xm_qudsest2 = lag(xm_qudsest2),
    lag_election = lag(election),
    lead_election = lead(election),
    eu_member = ifelse(((ccode2 >= 200 & ccode2 <= 325) | ccode2 == 350) & 
      year >= 1988, 1, 0)
  ) 

# dyad id
dyadic.trade.major$dyad.id <- group_indices(dyadic.trade.major, ccode1, ccode2) 

# full data cleaning: cut down to MP allies
dyadic.mp.ally <- latent.supp %>%
  left_join(dyadic.trade.major) %>%
  ungroup() %>%
  mutate(
    # prior support and changes
    prior_leader_supp = ifelse(leader != lag(leader),
                               lag_median, NA)
  ) %>%
  fill(c(prior_leader_supp, atop_defense),
       .direction = "down") %>%
  # filter for alliances only
  filter(atop_defense == 1) %>%
  mutate(
    change_leader_supp = median - prior_leader_supp
  ) %>%
  group_by(ccode1, ccode2, leader) %>%
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
  select(ccode1, ccode2, year, electionid, leader, atop_defense, mean_leader_supp,
         lag_median, prior_leader_supp, change_leader_supp,
         total_change_supp,
         everything())
  
# w/ original measure, add: 
# %>% filter(atop_defense == 1 &
#    (ccode == 2 | ccode == 200 | ccode == 220))


# dyad id
dyadic.mp.ally$dyad.id <- group_indices(dyadic.mp.ally, ccode1, ccode2) 





# get us exports
us.trade.ally <- filter(dyadic.trade.major, 
                        ccode1 == 2) %>%
  ungroup() %>%
  rename(
    us.code = ccode1,
    ccode = ccode2
  ) %>%
  right_join(select(elections.data, year, 
                   president, time_to_elec)) %>%
  left_join(promises.data) %>%
  group_by(ccode) %>% 
  mutate(
    # presidential partisanship
    rep_pres = ifelse((year >= 1921 & year <= 1932) | # Harding, Coolidge, Hoover
                        (year >= 1953 & year <= 1960) | # Ike
                        (year >= 1969 & year <= 1976) | # Nixon/Ford
                        (year >= 1981 & year <= 1992) | # Reagan/Bush
                        (year >= 2001 & year <= 2008) | # HW Bush
                        (year >= 2017), # Trump
                      1, 0),
    change_pres = ifelse(rep_pres != lag(rep_pres), 1, 0),
    near_elec = ifelse(time_to_elec == 0 | time_to_elec == 1, 1, 0),
    cold_war = ifelse(year >= 1989, 1, 0)
  ) %>%
  group_by(president, ccode) %>%
  mutate(
    # running mean stat
    total_statements = rollapply(statements_americas, 2, sum,
                                 align ='right', fill = 0),
    ln_total_statements = log(total_statements + 1)
  )
us.trade.ally$dyad.id <- group_indices(us.trade.ally, us.code, ccode) 


# pull in US arms trade data 
us.arms.sipri <- read.csv("data/us-arms-exports.csv")%>%
                  pivot_longer(-recipient, names_to = "year",
                               values_to = "us_arms") 
us.arms.sipri$year <- as.numeric(str_remove(us.arms.sipri$year, "X"))
us.arms.sipri$us_arms[us.arms.sipri$us_arms == 0] <- 0.25 # small values are 0, not NA
us.arms.sipri$us_arms[is.na(us.arms.sipri$us_arms)] <- 0 # replace NA w/ 0 

# create countrycodes 
us.arms.sipri$ccode <- countrycode(us.arms.sipri$recipient,
                                   origin = "country.name",
                                   destination = "cown")
us.arms.sipri$ccode[us.arms.sipri$recipient == "Serbia"] <- 345
us.arms.sipri$ccode[us.arms.sipri$recipient == "Micronesia"] <- 987

# merge 
us.trade.ally <- left_join(us.trade.ally, us.arms.sipri)


# export mp trade to iso3c codes
unique(countrycode(dyadic.mp.ally$ccode2, origin = "cown", destination = "iso3c"))

# add to text file 
# define text file
mp.iso3c <- file("data/mp-ally-iso3c.txt")
# write blocks to text
writeLines(unlist(unique(countrycode(dyadic.mp.ally$ccode2, origin = "cown", destination = "iso3c"))),
           con = mp.iso3c,
           sep = ";")
close(mp.iso3c)
