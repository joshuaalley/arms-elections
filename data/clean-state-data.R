# Joshua Alley 
# clean US state-level data


# load exports data by state
state.exports <- read.csv("data/state-exports-country.csv")
# remove commas and measure in thousands
state.exports$value <- as.numeric(gsub(",","", state.exports$value)) / 1000

# remove unknown state
state.exports <- filter(state.exports, state != "Unknown") 

# state exports ccode
state.exports$ccode <- countrycode(origin = "country.name",
                                   sourcevar = state.exports$destination,
                                   destination = "cown")

# add destination level info
state.exports.dyad <-  left_join(state.exports,
                                 (create_dyadyears(system = "cow",
                                                   mry = TRUE, 
                                                   directed = FALSE) %>%
                                    add_cow_mids() %>%
                                    add_atop_alliance() %>%
                                    add_igos() %>%
                                    add_democracy() %>% 
                                    rename(
                                      ccode = ccode2
                                    ) %>%
                                    filter(ccode1 == 2 & year >= 1946) %>%   
                                    select(ccode, year,
                                           cowmidongoing, xm_qudsest2,
                                           atop_defense, dyadigos) %>%
                                    mutate(
                                      lag_atop_defense = lag(atop_defense),
                                      lag_xm_qudsest2 = lag(xm_qudsest2)
                                    ))) %>%
  left_join(select(ungroup(us.trade.ally),
                   ccode, year,
                   time_to_elec,
                   ln_total_statements))
# fix ATOP alliance 0s 
state.exports.dyad$atop_defense[state.exports.dyad$year == 2019] <- NA
state.exports.dyad$atop_defense[state.exports.dyad$year == 2020] <- NA
state.exports.dyad <- fill(state.exports.dyad, atop_defense, .direction = "down")



# bring in PWT data
pwt.key <- read.csv("data/pwt-100.csv") %>%
  select(country, year, 
         rgdpe, pop, xr, csh_g)
pwt.key$ccode <- countrycode(origin = "country.name",
                             sourcevar = pwt.key$country,
                             destination = "cown")

state.exports.dyad <- left_join(state.exports.dyad,
                                pwt.key) %>%
  drop_na(ccode)  %>% # otherwise matches all ccode NAs
  mutate(
    lag_ln_rgdpe = lag(log(rgdpe)),
    lag_pop = lag(pop),
    lag_xr = lag(xr),
    lag_csh_g = lag(csh_g)
  ) 

# state GDP data from FRED
state.gdp <- read.csv("data/state-gdp.csv") %>%
  pivot_longer(-observation_date)
state.gdp$name[state.gdp$name == "AKNGSP_20080605"] <- "AKNGSP"
state.gdp$measure <- substr(state.gdp$name, 3, 6)
state.gdp$st.abb <- substr(state.gdp$name, 1, 2)
state.gdp$year <- as.numeric(substr(state.gdp$observation_date, 1, 4))

# real gdp
state.gdp <- state.gdp %>%
  filter(measure == "NGSP") %>%
  mutate(
    ln_ngdp = log(value)
  ) %>%
  select(
    st.abb, year, ln_ngdp
  )


# state data from CSPP 
cspp.data <- get_cspp_data(vars =
                             c("minwage", "foreign_born",
                               "poptotal", 
                               "labfree", "econfree", "regfree", # labor, econ, regulation freedoms
                               "atotspt", # total state and local spending
                               "fairtrade", "rep_unified", "s_diffs", "h_diffs"
                             ),
                           years = seq(2002, 2019)) %>%
  left_join(state.gdp)

# Add cspp data
state.exports.dyad <- left_join(state.exports.dyad, cspp.data) 

# check duplicates
state.exports.dyad$duplicates <- duplicated(state.exports.dyad)
state.exports.dyad <- filter(state.exports.dyad, duplicates == FALSE)

state.exports.dyad <- state.exports.dyad %>%
  mutate(
    lag_poptotal = lag(poptotal),
    lag_ln_ngdp = lag(ln_ngdp)
  )


# clean up w/ new variables
state.exports.dyad <- state.exports.dyad %>%
  mutate(
    ln_state_exports = log(value),
    lag_ln_exports = lag(ln_state_exports),
    ihs_state_exports = asinh(value),
    lag_ihs_exports = lag(ihs_state_exports),
    
    election = ifelse(year == 2000 | 
                        year == 2004 |
                        year == 2008 |
                        year == 2012 |
                        year == 2016 |
                        year == 2020, 
                      1, 0),
    lead_election = lead(election),
    lag_election = lag(election),
  )

# add electoral votes
electoral.votes <- read.csv("data/electoral-votes.csv")
votes.00s <- select(electoral.votes,
                    state, EV00) %>% 
  rename(elec_votes = EV00) %>%
  # two reps of each obs
  slice(rep(1:n(), each = 2))
votes.00s$year <- rep(c(2004, 2008))

votes.10s <- select(electoral.votes,
                    state, EV10) %>%
  rename(elec_votes = EV10) %>%
  # two reps of each obs
  slice(rep(1:n(), each = 3))
votes.10s$year <- rep(c(2012, 2016, 2020))
evotes.state <- bind_rows(votes.00s, votes.10s)

# join with elections data below

# elections data
election.res <- read.csv("data/pres-election-res.csv") %>%
  select(year, state, office,
         party_simplified, 
         candidatevotes, totalvotes) %>%
  filter(office == "US PRESIDENT" & 
           (party_simplified == "DEMOCRAT" | 
              party_simplified == "REPUBLICAN")) %>%
  pivot_wider(id_cols = c("state", "year", "totalvotes"),
              names_from = "party_simplified",
              values_from = c("candidatevotes"),
              values_fn = sum) %>%
  group_by(state) %>%
  mutate(
    state = stringr::str_to_title(state),
    dem_vote_share = DEMOCRAT / totalvotes,
    rep_vote_share = REPUBLICAN / totalvotes,
    diff_vote_share = abs(dem_vote_share - rep_vote_share),
    # following MA and McLaren NBER
    close_state = ifelse(abs(diff_vote_share) < .04, 1, 0),
    lag_close_state = lag(close_state),
    state_winner = ifelse(rep_vote_share > dem_vote_share,
                          "Rep", "Dem")
  ) %>%
  filter(year >= 2002 & state != "District Of Columbia") %>% # match exports sample
  left_join(evotes.state) %>%
  ungroup()
glimpse(election.res)

# create pivotal rank
election.res$natl_winner <- NA
election.res$natl_winner[election.res$year == 2004] <- "Rep"
election.res$natl_winner[election.res$year == 2008] <- "Dem"
election.res$natl_winner[election.res$year == 2012] <- "Dem"
election.res$natl_winner[election.res$year == 2016] <- "Rep"
election.res$natl_winner[election.res$year == 2020] <- "Dem"

# id 2004 pivot (generalize to function for each election)
pivot.state <- function(data, year){
  
  pivot.list <- vector(mode = "list", length = length(year))
  
  for(i in 1:length(year)){
    
    data <- election.res %>%
      select(
        state, year,
        dem_vote_share, rep_vote_share,
        elec_votes, natl_winner, state_winner,
      ) 
    data <- data[data$year ==  year[i], ]
    # break up ifelse for clarity
    data <- if(data$natl_winner == "Rep"){
      arrange(data, dem_vote_share) %>%
        mutate(votes_won = cumsum(elec_votes))
    } else{
      arrange(data, rep_vote_share) %>%
        mutate(votes_won = cumsum(elec_votes))
    }
    # rank pivotal
    data <- data %>%
      mutate(
        pivotal = ifelse((votes_won > 270 & lag(votes_won < 270)),
                         1, 0),
        pivotal_row = ifelse((votes_won > 270 & lag(votes_won < 270)),
                             row_number(), 0),
        pivot_prox = ifelse(row_number() <= unique(pivotal_row)[2],
                            unique(pivotal_row)[2] - row_number(),
                            unique(pivotal_row)[2] - row_number()),
        # absolute value per Wright 09
        pivot_prox = abs(pivot_prox)
      )
    data 
    pivot.list[[i]] <- data
    pivot.list[[i]]
  } # end ifelse
  pivot.list
} # end function

# rank pivotal states by election
pivot.list <- pivot.state(election.res, unique(election.res$year))

# merge with election res 
election.res <- left_join(election.res, bind_rows(pivot.list))

# add to dyad data
state.exports.dyad <- left_join(state.exports.dyad, election.res)
glimpse(state.exports.dyad) 
# fill in election data for intervening years
state.exports.dyad <- state.exports.dyad %>%
  fill(diff_vote_share, lag_close_state,
       pivot_prox,
       .direction = "down") %>%
  group_by(state) %>%
  mutate(
    lag_diff_vote = lag(diff_vote_share))


# check duplicates
state.exports.dyad$duplicates <- duplicated(state.exports.dyad)

# Cut out duplicates from merging
state.exports.dyad <- filter(state.exports.dyad, duplicates == FALSE)



### state and election covariates
# load senate data here:
senate.data <- read.csv("data/senate-data.csv")
glimpse(senate.data)

# annual by state, year, and program 
contracts.data.state <- contracts.data %>%
  group_by(year, state, program) %>% 
  summarize(
    obligations = sum(fed.obligation, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  mutate( # obligations in millions
    obligations = obligations / 1000000,
    ln_obligations = log(obligations + 1)
  ) %>%
  filter(state %in% senate.data$state) %>%
  arrange(state, year) %>%
  ungroup()


# consolidate to match contract sectors
contracts.data.state$usml_cont <- NA
contracts.data.state$usml_cont[contracts.data.state$program == "SHIPS"] <- "ships"

contracts.data.state$usml_cont[str_detect(contracts.data.state$program, 
                                          "AIR")] <- "aircraft"

contracts.data.state$usml_cont[contracts.data.state$program == "MISSILE AND SPACE SYSTEMS"] <- "missile_space"

contracts.data.state$usml_cont[contracts.data.state$program == "COMBAT VEHICLES" |
                                 contracts.data.state$program == "NON-COMBAT VEHICLES" ] <- "vehicles"

contracts.data.state$usml_cont[contracts.data.state$program == "WEAPONS" |
                                 contracts.data.state$program == "AMMUNITION"] <- "arms"


contracts.data.state$usml_cont[contracts.data.state$program == 
                                 "ELECTRONICS AND COMMUNICATION EQUIPMENT"] <- "electronics"

contracts.data.state$usml_cont[is.na(contracts.data.state$usml_cont)] <- "other"


# wide contract data by sector 
contracts.state.wide <- drop_na(contracts.data.state, usml_cont) %>%
  group_by(state, year, usml_cont) %>%
  summarize(
    obligations = sum(obligations, na.rm = TRUE),
    ln_obligations = log(obligations + 1),
    .groups = "keep"
  ) %>%
  pivot_wider(id_cols = c("state", "year"),
              names_from = "usml_cont",
              values_from = "ln_obligations") %>%
  mutate(
    ln_obligations = aircraft + arms + electronics + missile_space +
      other + ships + vehicles
  )

# nothing leads to NA 
contracts.state.wide[is.na(contracts.state.wide)] <- 0


# senate
state.sen.data <- left_join(contracts.state.wide, senate.data) %>%
  group_by(state) %>%
  mutate(
    incumbent = ifelse(str_detect(incumb, "incumb"),
                       1, 0),
    election.year = ifelse(election == 1, year, NA),
    incumb.win = ifelse((incumb == "GOP incumb" & 
                           party_simplified == "REPUBLICAN") |
                          (incumb == "Dem incumb" & 
                             party_simplified == "DEMOCRAT") |
                          (incumb == "other incumb" & 
                             party_simplified == "OTHER"),
                        1, 0)
  ) %>%
  fill(election.year, incumbent, .direction = "up") %>%
  mutate(
    time_to_elec = election.year - year
  )
# elections only in merge, so replace NA with 0
state.sen.data$election[is.na(state.sen.data$election)] <- 0

# merge with state data 
state.sen.data$state <- str_to_title(state.sen.data$state)
state.data <- left_join(state.sen.data, cspp.data) %>%
  filter(year <= 2019) %>%
  mutate(
    poptotal = arm::rescale(poptotal), 
    ln_ngdp = arm::rescale(ln_ngdp)
  )


# add presidential elections data
state.data <- left_join(state.data, election.res) %>% 
  left_join(elections.data, by = "year") %>%
  rename(
    sen_election = election.x,
    pres_election = election.y,
    time_to_selec = time_to_elec.x,
    time_to_pelec = time_to_elec.y
  )

# fill in election data for intervening years
state.data <- state.data %>%
  fill(diff_vote_share, lag_close_state,
       pivot_prox,
       .direction = "down") %>%
  group_by(state) %>%
  mutate(
    lag_diff_vote = lag(diff_vote_share)) %>%
  ungroup()


# State data w/ contracts by type
state.data.ord <- left_join(state.data, arms.cat.all) %>%
  group_by(state) %>%
  mutate_at(c("aircraft", "arms", "electronics", "missile_space",
              "other", "ships", "vehicles"), 
            .funs = list(lag = lag,
                         change = function(x) x - lag(x))) 




# load 1976 to 2003 data: market level
# contracts.data.76 <- read_dta("data/carril-duggan-market_V1.dta") 
# 
# contracts.76.yr <- contracts.data.76 %>%
#   group_by(actfy) %>%
#   summarize(
#     obligations = sum(mktdollars, na.rm = TRUE)
#   ) %>% 
#   mutate( # obligations in billions
#     obligations = obligations / 1000000000
#   ) %>%
#   rename(
#     year = actfy
#   )
# 
# 
# # plot
# ggplot(contracts.76.yr, aes(x = year, y = obligations)) +
#   geom_vline(xintercept=c(pres.elections), linetype="dotted") +
#   xlim(1976, 2004) +
#   geom_line()   