# Joshua Alley
# data cleaning 


# start with vdem data
vdem <- vdem %>%
         select(
           country_name, country_id, year, 
           v2x_polyarchy
         ) %>% 
         filter(year >= 1950) 
# cow country codes
vdem$ccode <- countrycode(vdem$country_id,
                          origin = "vdem",
                          destination = "cown")

# add oil data
petrol.data <- read.csv("data/petrol-revenue.csv") %>%
                rename(petrol_rev = oil_gas_value_2014_PTO) %>%
                mutate(ln_petrol_rev = log(petrol_rev + 1))
# merge with vdem
vdem <- left_join(vdem, petrol.data)
summary(vdem$petrol_rev)

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


# use peacesciencer to 
# generate dyad-year data on other dimensions
dyadic.cont <- create_dyadyears(system = "cow",
                                 mry = TRUE, 
                                 directed = FALSE) %>%
  add_cow_mids() %>%
  add_democracy() %>%
  add_atop_alliance() %>%
  add_igos() %>%
  filter(year >= 1949)

dyadic.cont$atop_defense[dyadic.cont$year == 2019] <- NA
dyadic.cont$atop_defense[dyadic.cont$year == 2020] <- NA






# pull trade data with some useful controls
cepii.data <- read_dta("data/TRADHIST_v4.dta") %>%
  filter(year > 1948) %>%
  select(
    iso_o, iso_d, year,
    Dist_d, Comlang, Contig,
    Evercol
  ) %>%
  mutate(
    ccode1 = countrycode(iso_o, origin = "iso3c",
                         destination = "cown"), 
    ccode2 = countrycode(iso_d, origin = "iso3c",
                         destination = "cown")
  ) %>% # dem major powers
  filter(ccode1 == 2) %>%
  group_by(ccode1, ccode2) %>% 
  mutate(
    # key controls
    ln_distw = log(Dist_d)
  )
    
    
# save this chunk for IMF data 
imf.dot <- read.csv("data/imf-dot.csv") %>%
            filter(origin == "United States") %>%
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

# join with cepii data and full cepii controls
trade.full <- left_join(imf.dot, cepii.data) %>%
               group_by(ccode1, ccode2) %>%
               fill(34:38)



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
  left_join(dyadic.cont) %>%
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



# get us exports
us.trade.ally <- filter(dyadic.trade.major, 
                        ccode1 == 2) %>%
  ungroup() %>%
  rename(
    us.code = ccode1,
    ccode = ccode2
  ) %>%
  left_join(vdem) %>%
  right_join(select(elections.data, year, 
                   president, time_to_elec)) %>%
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
    cold_war = ifelse(year <= 1989, 1, 0),
    gwot = ifelse(year >= 2001 & year <= 2011, 
                  1, 0)
  ) %>%
  group_by(ccode) %>%
  fill(atop_defense, .direction = "down")
us.trade.ally$dyad.id <- group_indices(us.trade.ally, us.code, ccode) 

# alliances- add Taiwan, Israel, KSA
us.trade.ally$ally <- us.trade.ally$atop_defense
us.trade.ally$ally[us.trade.ally$ccode == 666] <- 1 # israel
us.trade.ally$ally[us.trade.ally$ccode == 670] <- 1 # saudi arabia
us.trade.ally$ally[us.trade.ally$ccode == 713] <- 1 # taiwan


# democratic allies
# above average unified democ score
us.trade.ally$democ_bin <- ifelse(us.trade.ally$v2x_polyarchy > 
                                    mean(us.trade.ally$v2x_polyarchy, na.rm = T),
                                  1, 0)


# add GDP data from wb
# bring in wb data
wb.key <- read.csv("data/wb-pop-gdp.csv") %>%
  select(ccode, year, 
         rgdpe, pop) %>%
  mutate(
    ln_rgdp = log(rgdpe),
    ln_pop = log(pop)
  )

# join wb
us.trade.ally <- left_join(us.trade.ally, wb.key)



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
us.trade.ally <- left_join(us.trade.ally, us.arms.sipri) %>%
                   group_by(ccode) %>%
                   mutate(
                     us_arms = log(us_arms + 1),
                     lag_us_arms = lag(us_arms),
                     change_us_arms = us_arms - lag_us_arms,
                     nz_us_arms = ifelse(us_arms > 0, 1, 0)
                   )


ggplot(us.trade.ally, aes(x = us_arms)) + geom_histogram()




### Contracting data

# load exports and time-series it 
us.arms.year <- us.arms.sipri %>% 
  group_by(year) %>%
  summarize(
    us_arms = sum(us_arms, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    lag_us_arms = lag(us_arms)
  )



### load contracts data ### 
contracts.data <- read.csv("data/contracts-data.csv") %>%
  filter(year < 2021) # some 2020 obs from 2019- cut

# negative values are deobligations- usually due to changes in project scope
# https://datalab.usaspending.gov/analyst-guide/ 
# move these to zero
contracts.data$fed.obligation[contracts.data$fed.obligation < 0] <- 0

# share of subcontracted
sum(contracts.data$sub.contract) / sum(contracts.data$n.cont)

# annual by program
contracts.data.yr <- contracts.data %>%
  group_by(year) %>% 
  summarize(
    obligations = sum(fed.obligation, na.rm = TRUE)
  ) %>% 
  mutate( # obligations in billions
    obligations = obligations / 1000000000
  )

# plot
ggplot(contracts.data.yr, aes(x = year, y = obligations)) +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  xlim(2000, 2020) +
  geom_line()

# annual by program
contracts.data.pyr <- contracts.data %>%
  group_by(year, program) %>% 
  summarize(
    obligations = sum(fed.obligation, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  mutate( # obligations in billions
    obligations = obligations / 1000000000
  )

# plot
ggplot(contracts.data.pyr, aes(x = year, y = obligations)) +
  facet_wrap(~ program, scales = "free_y") +
  geom_vline(xintercept=c(pres.elections), linetype="dotted") +
  xlim(2000, 2020) +
  geom_line()

# pivot wider
contracts.data.pyr$program[contracts.data.pyr$program == ""] <- "Unknown"
contracts.data.wide <- contracts.data.pyr %>%
  pivot_wider(id_cols = "year",
              names_from = "program",
              values_from = "obligations")
colnames(contracts.data.wide) <- c("year", "unknown", "air_engines", "airframes",
                                   "all_others", "ammunition", "building",
                                   "combat_vehicles", "construct", "construct_equip",
                                   "electronics", "materials_equip", "medical",
                                   "missile_space", "noncom_vehicles", "other_air",
                                   "other_fuel", "petrol", "photo", "production",
                                   "containers_handling", "services", "ships",
                                   "subsistence", "textiles", "railway", "weapons")

# create summary categories and add other info
contracts.data.clean <- contracts.data.wide %>%
  rowwise() %>%
  mutate(
    all_contracts = sum(c_across(unknown:weapons))
  ) %>%
  ungroup() %>%
  left_join(us.arms.year) %>%
  left_join(elections.data) %>%
  left_join(us.trade.year) %>%
  mutate(
    aircraft = air_engines + airframes + other_air,
    lag_aircraft = lag(aircraft),
    vehicles = combat_vehicles + noncom_vehicles,
    lag_vehicles = lag(vehicles),
    arms = ammunition + weapons,
    lag_arms = lag(arms),
    transport = railway + other_fuel + petrol,
    lag_transport = lag(transport),
    arms_cont = aircraft + ships + vehicles + arms,
    lag_arms_cont = lag(arms_cont),
    non_arms = all_contracts - aircraft - ships - vehicles - arms,
    lag_non_arms = lag(non_arms),
    lag_ships = lag(ships),
    lag_missile_space = lag(missile_space),
    lag_electronics = lag(electronics),
    lag_all_contracts = lag(all_contracts),
    change_all_contracts = all_contracts - lag_all_contracts
  ) 

contracts.data.clean$elec.cycle <- c(rep(2004, 4), rep(2008, 4), 
                                     rep(2012, 4), rep(2016, 4),
                                     rep(2020, 4))



### clean specific orders data ###
# SIPRI with a ton of manual cleaning before import here 
us.trade.regis <- read.csv("data/us-trade-register.csv")
colnames(us.trade.regis)[1] <- "country"

# trim whitespace ends throughout
us.trade.regis <- as.data.frame(apply(us.trade.regis, 2, function(x) str_trim(x)))

# Tabs to NA
us.trade.regis$country[us.trade.regis$country == ""] <- NA
us.trade.regis <- fill(us.trade.regis, country, .direction = "down")

# trim white space from years and orders- then to numeric
us.trade.regis$ordered <- as.numeric(us.trade.regis$ordered)
us.trade.regis$year <- as.numeric(us.trade.regis$year)
us.trade.regis$delivered <- as.numeric(us.trade.regis$delivered)

# start and end years of deliveries
us.trade.regis$deliv.start <- as.numeric(substr(us.trade.regis$years.delivered, 1, 4))
us.trade.regis$deliv.end <- as.numeric(substr(us.trade.regis$years.delivered, 6, 9))
us.trade.regis$deliv.end[is.na(us.trade.regis$deliv.end)] <- us.trade.regis$deliv.start[is.na(us.trade.regis$deliv.end)]

# comments to lower case
us.trade.regis$comments <- str_to_lower(us.trade.regis$comments)

# remove blank rows and add some new variables
us.trade.regis <- drop_na(us.trade.regis, year) %>%
                    mutate(
                      deliv.dur = deliv.end - deliv.start,
                      deliv.lag = deliv.start - year,
                      second.hand = ifelse(str_detect(comments, "second-hand"),
                                           1, 0),
                      aid = ifelse(str_detect(comments, "aid"),
                                           1, 0),
                      offset = ifelse(str_detect(comments, "offset"),
                                   1, 0),
                      cost.m = str_extract(comments, "\\$\\d+[:space:][m]"),
                      cost.mr = str_extract(comments, "\\$\\d+\\-\\d+[:space:][m]"),
                      cost.b = str_extract(comments, "\\$\\d+[:space:][b]")
                    )

# combine the costs and clean up
us.trade.regis <- unite(us.trade.regis,
                                  cost.m, 
                                   cost.mr, 
                                    cost.b,
                             na.rm = TRUE) %>%
                mutate(
                 cost.m = str_remove(cost.m, "\\$"),
                 cost.size = str_extract(cost.m, "[a-z]"),
                 cost.low = as.numeric(str_extract(cost.m, "\\d+")),
                 cost.high = as.numeric(str_remove(str_extract(cost.m, "\\-\\d+"), "\\-"))
                ) %>%
                rowwise() %>%
                mutate(
                 cost = mean(c(cost.low, cost.high), na.rm = TRUE),
                 cost.est = ifelse(cost > 0, 1, 0),
                ) %>%
                ungroup()
us.trade.regis$cost[is.nan(us.trade.regis$cost)] <- NA
us.trade.regis$cost.est[is.na(us.trade.regis$cost.est)] <- 0

us.trade.regis$cost[us.trade.regis$cost.size == "b" & !is.na(us.trade.regis$cost.size)] <- us.trade.regis$cost[
                        us.trade.regis$cost.size == "b" &
                      !is.na(us.trade.regis$cost.size)]*1000


# summary to match contracts categories
table(contracts.data.pyr$program)
us.trade.regis$weapon.des <- str_to_lower(us.trade.regis$weapon.des)
table(us.trade.regis$weapon.des)

# summary here
us.trade.regis$weapon.type <- NA
# aircraft and helicopters 
us.trade.regis$weapon.type[
  str_detect(us.trade.regis$weapon.des, "aircraft") |
  str_detect(us.trade.regis$weapon.des, "helicopter") |
  str_detect(us.trade.regis$weapon.des, "uav") |
  str_detect(us.trade.regis$weapon.des, "turbo") |
  str_detect(us.trade.regis$weapon.des, "ground attack ac") |
  str_detect(us.trade.regis$weapon.des, "light transport ac") |
  str_detect(us.trade.regis$weapon.des, "transport ac") |
  str_detect(us.trade.regis$weapon.des, "reconnaissance ac") |
  str_detect(us.trade.regis$weapon.des, "recce/sigint ac") |
  str_detect(us.trade.regis$weapon.des, "air refuel system") |
  str_detect(us.trade.regis$weapon.des, "radial engine")] <- "aircraft"

# ships 
us.trade.regis$weapon.type[
  str_detect(us.trade.regis$weapon.des, "aircraft carrier") |
  str_detect(us.trade.regis$weapon.des, "submarine") |
  str_detect(us.trade.regis$weapon.des, "ship") |  
  str_detect(us.trade.regis$weapon.des, "mine") |
  str_detect(us.trade.regis$weapon.des, "landing") |
  str_detect(us.trade.regis$weapon.des, "frigate") |
  str_detect(us.trade.regis$weapon.des, "cruiser") |
  str_detect(us.trade.regis$weapon.des, "destroyer") |
  str_detect(us.trade.regis$weapon.des, "corvette") |
  str_detect(us.trade.regis$weapon.des, "tug") |
  str_detect(us.trade.regis$weapon.des, "naval gun") |
  str_detect(us.trade.regis$weapon.des, "gunboat") |
  str_detect(us.trade.regis$weapon.des, "fac") |
  str_detect(us.trade.regis$weapon.des, "abl") |
  str_detect(us.trade.regis$weapon.des, "patrol craft")] <- "ships"


us.trade.regis$weapon.type[
  str_detect(us.trade.regis$weapon.des, "sam") |
  str_detect(us.trade.regis$weapon.des, "missile") |
  str_detect(us.trade.regis$weapon.des, "rocket") |
  str_detect(us.trade.regis$weapon.des, "slbm") |
  str_detect(us.trade.regis$weapon.des, "torpedo") |
  str_detect(us.trade.regis$weapon.des, "abm") |
  str_detect(us.trade.regis$weapon.des, "ssm") |
  str_detect(us.trade.regis$weapon.des, "asw mrl") |
  str_detect(us.trade.regis$weapon.des, "asm") |
  str_detect(us.trade.regis$weapon.des, "sraam") |
  str_detect(us.trade.regis$weapon.des, "bvraam") |
  str_detect(us.trade.regis$weapon.des, "coastal defence system") |
  us.trade.regis$weapon.des == "arm"
  ] <- "missile_space"

us.trade.regis$weapon.type[
  str_detect(us.trade.regis$weapon.des, "radar") |
    str_detect(us.trade.regis$weapon.des, "sonar") |
    str_detect(us.trade.regis$weapon.des, "sigint system") |
    str_detect(us.trade.regis$weapon.des, "aals") 
] <- "electronics"


us.trade.regis$weapon.type[
  str_detect(us.trade.regis$weapon.des, "ifv/afsv") |
    str_detect(us.trade.regis$weapon.des, "ifv") |
    str_detect(us.trade.regis$weapon.des, "tank") |
    str_detect(us.trade.regis$weapon.des, "apc") |
    str_detect(us.trade.regis$weapon.des, "aev") |
    str_detect(us.trade.regis$weapon.des, "apv") |
    str_detect(us.trade.regis$weapon.des, "spaag") |
    str_detect(us.trade.regis$weapon.des, "self-propelled gun") |
    str_detect(us.trade.regis$weapon.des, "self-propelled mrl") |
    str_detect(us.trade.regis$weapon.des, "reconnaissance av") |
    str_detect(us.trade.regis$weapon.des, "arv") |
    str_detect(us.trade.regis$weapon.des, "alv") |
    str_detect(us.trade.regis$weapon.des, "afsv") |
    str_detect(us.trade.regis$weapon.des, "opv") |
    str_detect(us.trade.regis$weapon.des, "armoured car")] <- "vehicles"


us.trade.regis$weapon.type[
  str_detect(us.trade.regis$weapon.des, "mortar") |
    str_detect(us.trade.regis$weapon.des, "anti-tank missile") |
    str_detect(us.trade.regis$weapon.des, "bomb") |
    str_detect(us.trade.regis$weapon.des, "shell") |
    str_detect(us.trade.regis$weapon.des, "aa gun") |
    str_detect(us.trade.regis$weapon.des, "ciws") |
    str_detect(us.trade.regis$weapon.des, "towed gun")] <- "arms"


us.trade.regis$weapon.type[
  str_detect(us.trade.regis$weapon.des, "bomber aircraft")] <- "aircraft"

# Results check
table(us.trade.regis$weapon.type)
sum(is.na(us.trade.regis$weapon.type))
# missing is all engines and turbines, some air, some naval, some ground
table(us.trade.regis$weapon.type, us.trade.regis$offset)
table(us.trade.regis$weapon.type, us.trade.regis$aid)

# summarize by category 
us.trade.regis.sum <- us.trade.regis %>%
                       group_by(weapon.type) %>%
                       summarize(
                         n = n(),
                         mean.deliv.lag = mean(deliv.lag, na.rm = TRUE),
                         mean.deliv.dur = mean(deliv.dur, na.rm = TRUE),
                         second.hand = sum(second.hand, na.rm = TRUE),
                         aid = sum(aid, na.rm = TRUE),
                         prop.second.hand = mean(second.hand, na.rm = TRUE),
                         prop.aid = sum(aid, na.rm = TRUE) / n,
                         med.ordered = median(ordered, na.rm = TRUE),
                         .groups = "keep"
                       )
us.trade.regis.sum


# deals by year
# summarize by category and year
arms.deals.year <- us.trade.regis %>%
  group_by(year) %>%
  filter(year >= 1950 & year <= 2020) %>%
  summarize(
    deals = n(),
    second.hand = sum(second.hand, na.rm = TRUE),
    aid = sum(aid, na.rm = TRUE),
    .groups = "keep"
  ) 

arms.deals.year.elec <- drop_na(arms.deals.year) %>%
  left_join(elections.data)

# deals
ggplot(arms.deals.year.elec,
       aes(x = factor(time_to_elec),
           y = deals)) +
  scale_x_discrete(limits  = rev) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Time to Presidential Election",
       y = "Arms Deals")
t.test(deals ~ election, data = arms.deals.year.elec)


# long to look at aid and second-hand
arms.deals.year.long <- arms.deals.year %>%
  left_join(elections.data) %>%
  pivot_longer(
    cols = c(deals, aid, second.hand)
  )

ggplot(drop_na(arms.deals.year.long,
               time_to_elec),
       aes(x = factor(time_to_elec),
           y = value)) +
  facet_wrap(~ name, scales = "free_y") +
  scale_x_discrete(limits  = rev) +
  geom_boxplot(outlier.shape = NA)


# summarize by category and year
us.arms.sum.year <- us.trade.regis %>%
  group_by(year, weapon.type) %>%
  summarize(
    deals = n(),
    mean.deliv.lag = mean(deliv.lag, na.rm = TRUE),
    mean.deliv.dur = mean(deliv.dur, na.rm = TRUE),
    second.hand = sum(second.hand, na.rm = TRUE),
    aid = sum(aid, na.rm = TRUE),
    prop.aid = aid / deals,
    prop.second.hand =  second.hand / deals,
    ordered = median(ordered, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  left_join(elections.data)

# plot raw cycles- deals
ggplot(drop_na(us.arms.sum.year,
               time_to_elec),
       aes(x = factor(time_to_elec),
           color = factor(time_to_elec),
           y = aid)) +
  scale_x_discrete(limits  = rev) +
  facet_wrap(~ weapon.type, scales = "free_y") +
  geom_boxplot(outlier.shape = NA)

# plot raw cycles- aid
ggplot(drop_na(us.arms.sum.year,
               time_to_elec),
       aes(x = factor(time_to_elec),
           color = factor(time_to_elec),
           y = aid)) +
  scale_x_discrete(limits  = rev) +
  facet_wrap(~ weapon.type, scales = "free_y") +
  geom_boxplot(outlier.shape = NA)

# second-hand
ggplot(drop_na(us.arms.sum.year,
               time_to_elec),
       aes(x = factor(time_to_elec),
           y = aid)) +
  scale_x_discrete(limits  = rev) +
  facet_wrap(~ weapon.type, scales = "free_y") +
  geom_boxplot(outlier.shape = NA)


# summarize by category and recipient
us.arms.cat <- us.trade.regis %>%
  group_by(country, year, weapon.type) %>%
  drop_na(weapon.type) %>% # drop engines
  summarize(
    deals = n(),
    mean.deliv.lag = mean(deliv.lag, na.rm = TRUE),
    mean.deliv.dur = mean(deliv.dur, na.rm = TRUE),
    second.hand = sum(second.hand, na.rm = TRUE),
    aid = sum(aid, na.rm = TRUE),
    prop.aid = sum(aid, na.rm = TRUE) / deals,
    ordered = median(ordered, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  mutate(
    aid.second = sum(aid, na.rm = TRUE) +
      sum(second.hand, na.rm = TRUE),
  )
us.arms.cat$ccode <- countrycode(us.arms.cat$country, 
                                 origin = "country.name",
                                 destination = "cown")
# wider for use elsewhere
us.arms.cat.wide <- pivot_wider(us.arms.cat, 
                      id_cols = c(ccode, country, year),
                      names_from = weapon.type,
                     values_from = deals,
                    values_fill = NA) %>%
           rename( # facilitate merging 
            vehicles_dl = vehicles,
            aircraft_dl = aircraft,
            arms_dl = arms,
            electronics_dl = electronics,
           ships_dl = ships,
           missile_space_dl = missile_space)

# add other variables
us.arms.cat <- left_join(us.arms.cat, select(us.trade.ally,
                                  ccode, year,
                               ally, cold_war,
                               rep_pres, time_to_elec, 
                               eu_member, ln_rgdp,
                               ln_pop, ln_distw,
                               Comlang,
                               Contig, Evercol,
                               nz_us_arms)) %>%
                 filter(year >= 1950)
ggplot(us.arms.cat, aes(x = deals)) + geom_histogram()
ggplot(us.arms.cat, aes(x = aid)) + geom_histogram()
ggplot(us.arms.cat, aes(x = second.hand)) + geom_histogram()


# wide formatted data 
arms.cat.all <- pivot_wider(us.arms.cat.all,
                           id_cols = "year",
                           names_from = c("ally", "weapon.type"),
                           values_from = c("deals"))
arms.cat.all[is.na(arms.cat.all)] <- 0
colnames(arms.cat.all) <- str_replace(colnames(arms.cat.all), "0", "nall")
colnames(arms.cat.all) <- str_replace(colnames(arms.cat.all), "1", "all")

arms.cat.all <- arms.cat.all %>%
  ungroup() %>%
  mutate_at(c("nall_aircraft", "nall_arms", "nall_electronics", "nall_missile_space",
              "nall_ships", "nall_vehicles"), 
            .funs = list(lag = lag,
                         change = function(x) x - lag(x))) %>%
  mutate_at(c("all_aircraft", "all_arms", "all_electronics", 
              "all_missile_space",
              "all_ships", "all_vehicles"), 
            .funs = list(lag = lag,
                         change = function(x) x - lag(x))) %>%
  mutate(
    deals_vehicles = nall_vehicles + all_vehicles,
    deals_ships = nall_ships + all_ships,
    deals_missile_space = nall_missile_space + all_missile_space,
    deals_electronics = nall_electronics + all_electronics,
    deals_arms = nall_arms + all_arms,
    deals_aircraft = nall_aircraft + all_aircraft
  )

