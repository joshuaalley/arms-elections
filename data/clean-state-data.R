# Joshua Alley 
# clean US state-level data



# state GDP data from FRED
state.gdp <- read.csv("data/state-gdp.csv") %>%
  pivot_longer(-GeoName) %>%
  mutate(
    year = as.numeric(substr(name, 2, 5)),
    ln_ngdp = log(value)
    ) %>%
  rename(state = GeoName) %>%
  select(
    state, year, ln_ngdp
  )


# state data from CSPP 
cspp.data <- get_cspp_data(vars = c("foreign_born",
                               "poptotal", 
                               "labfree", "econfree", "regfree", # labor, econ, regulation freedoms
                               "atotspt", # total state and local spending
                               "fairtrade", "rep_unified", "s_diffs", "h_diffs"
                             ),
                           years = seq(2000, 2020)) %>%
  left_join(state.gdp) %>%
  mutate(
    election = ifelse(year == 2000 | 
                        year == 2004 |
                        year == 2008 |
                        year == 2012 |
                        year == 2016 |
                        year == 2020, 
                      1, 0),
    lead_election = lead(election),
    lag_election = lag(election)
  )

# add electoral votes
electoral.votes <- read.csv("data/electoral-votes.csv")

votes.90s <- select(electoral.votes,
                    state, EV90) %>% 
  rename(elec_votes = EV90)
votes.90s$year <- 2000

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
evotes.state <- bind_rows(votes.90s, votes.00s, votes.10s)

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
    close_state = ifelse(abs(diff_vote_share) < .05, 1, 0),
    lag_close_state = lag(close_state),
    state_winner = ifelse(rep_vote_share > dem_vote_share,
                          "Rep", "Dem"),
    rep_pres = ifelse((year >= 2001 & year <= 2008) | # HW Bush
                        (year >= 2017), # Trump
                      1, 0)
  )

# code swing states using Kriner and Reeves (2015) criteria
# swing if three elections where loser gets < .45
election.res <- election.res %>%
  group_by(state) %>%
  mutate(
    loser_share = ifelse(state_winner == "Rep",
                         dem_vote_share, rep_vote_share),
    # three cycle mean of the loser's vote share
    loser_avg = zoo::rollmean(loser_share, k = 3, fill = NA),
    # lagged for past three 
    lag_loser_avg = lag(loser_avg),
    swing = ifelse(lag_loser_avg >= .45, 1, 0),
    rep_avg = lag(zoo::rollmean(rep_vote_share, k = 3, fill = NA)),
    dem_avg = lag(zoo::rollmean(dem_vote_share, k = 3, fill = NA)),
    core = ifelse((rep_pres == 1 & rep_avg >= .55) |
                    (rep_pres == 0 & dem_avg >= .55), 
                  1, 0)
  ) %>%
  filter(year >= 1999 & state != "District Of Columbia") %>% # match exports sample
  left_join(evotes.state) %>%
  ungroup()
glimpse(election.res)

# create pivotal rank
election.res$natl_winner <- NA
election.res$natl_winner[election.res$year == 2000] <- "Rep"
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
    winner <- unique(data$natl_winner)
    # break up ifelse for clarity
    data <- if(winner == "Rep"){
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
pivot.data <- bind_rows(pivot.list) %>%
               select(
                 state, year, pivot_prox
               )

# merge with election res 
election.res <- left_join(election.res, pivot.data)




# add to state data
state.data.raw <- left_join(cspp.data, election.res)
glimpse(state.data.raw) 
# fill in election data for intervening years
state.data.raw <- state.data.raw %>%
  fill(diff_vote_share, lag_close_state,
       pivot_prox,
       .direction = "down") %>%
  group_by(state) %>%
  mutate(
    lag_diff_vote = lag(diff_vote_share))



### state and election covariates

# annual by state, year, and program 
contracts.data.state <- contracts.data %>%
  group_by(year, state, program) %>% 
  summarize(
    obligations = sum(fed.obligation, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  group_by(state) %>% 
  mutate( # obligations in millions
    obligations = obligations / 1000000,
    ln_obligations = log(obligations + 1),
    lag_ln_obligations = lag(obligations),
    state = str_to_title(state)
  ) %>%
  filter(state %in% state.data.raw$state) %>%
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
  group_by(state) %>% 
  mutate(
    ln_obligations = aircraft + arms + electronics + missile_space +
      ships + vehicles,
    lag_ln_obligations = lag(ln_obligations),
    change_ln_obligations = ln_obligations - lag_ln_obligations,
    ln_obligations_other = aircraft + arms + electronics + missile_space +
      ships + vehicles + other
  )

# nothing leads to NA 
contracts.state.wide[is.na(contracts.state.wide)] <- 0

# move it back long
contracts.state.long <- 
  pivot_longer(contracts.state.wide,
               cols = -c(state, year)) %>%
  filter(name != "lag_ln_obligations")

# add contracts to state data
state.data.raw <- left_join(state.data.raw, 
                                contracts.state.wide %>%
                                  mutate( # ensures proper merge 
                                    state = str_to_title(state)
                                  ))

# senate data
# load senate data here:
senate.data <- read.csv("data/senate-data.csv")
glimpse(senate.data)
senate.data$state <- str_to_title(senate.data$state)

# clean up as there are multiple elections in some years- creates duplicate obs
senate.data <- senate.data %>%
   group_by(state, year) %>% 
   mutate( 
     election = 1,
     incumbent = ifelse(str_detect(incumb, "incumb"),
                                 1, 0),
     s_comp = abs(vote.share - .5),
     incumb_win = ifelse((incumb == "GOP incumb" & 
                            party_simplified == "REPUBLICAN") |
                           (incumb == "Dem incumb" & 
                              party_simplified == "DEMOCRAT") |
                           (incumb == "other incumb" & 
                              party_simplified == "OTHER"),
                         1, 0)
     ) %>%
   summarize(
     election = max(election),
     s_comp = mean(s_comp, na.rm = TRUE),
     incumbent = max(incumbent, na.rm = TRUE),
     incumb_win = mean(incumb_win, na.rm = TRUE),
     .groups = "keep"
   )
glimpse(senate.data)

state.sen.data <- left_join(select(state.data.raw, state, year),
                            senate.data) %>%
  group_by(state) %>%
  mutate(
    election.year = ifelse(election == 1, year, NA),
  ) %>%
  fill(election.year, incumbent, .direction = "up") %>%
  mutate(
    time_to_selec = election.year - year
  )
# elections only in merge, so replace NA with 0
state.sen.data$election[is.na(state.sen.data$election)] <- 0

# merge senate and all other state data 
state.data <- left_join(state.sen.data, state.data.raw,
                        by = c("state", "year")) %>%
  filter(year <= 2020) %>%
  mutate(
    poptotal = arm::rescale(poptotal), 
    ln_ngdp = arm::rescale(ln_ngdp),
    gwot = ifelse(year >= 2001 & year <= 2011, 
                      1, 0),
    rep_pres = ifelse((year >= 2001 & year <= 2008) | # HW Bush
                        (year >= 2017), # Trump
                      1, 0)
  ) %>%
  fill(core, swing, .direction = "up") %>%
  rename(
    sen_election = election.x,
    pres_election = election.y
  )




# fill in election data for intervening years
# back fill- next election is what matters
state.data <- state.data %>%
  fill(diff_vote_share,
       pivot_prox, s_comp,
       .direction = "up") %>%
  fill(poptotal,
       .direction = "down") %>%
  group_by(state) %>%
  mutate(
    lag_ln_ngdp = lag(ln_ngdp),
    change_ln_ngdp = ln_ngdp - lag_ln_ngdp,
    lag_poptotal = lag(poptotal),
    change_poptotal = poptotal - lag(poptotal)
  ) %>%
  ungroup() %>%
  mutate(
    election.cycle =ifelse(year <= 2000, 2000,
                      ifelse(year <= 2004, 2004,
                             ifelse(year <= 2008, 2008,
                                    ifelse(year <= 2012, 2012,
                                           ifelse(year <= 2016, 2016,
                                           2020))))),
    time_to_elec = election.cycle - year
  )


# State data w/ contracts by type
state.data.ord <- left_join(state.data, arms.cat.all) %>%
  group_by(state) %>%
  mutate_at(c("aircraft", "arms", "electronics", "missile_space",
              "other", "ships", "vehicles"), 
            .funs = list(lag = lag,
                         change = function(x) x - lag(x))) 



# Variation in pivot_prox and vote share by state
state.elec.sum <- state.data %>% 
                   group_by(state) %>%
                   summarize(
                     mean.prox = mean(pivot_prox, na.rm = TRUE), 
                     mean.diff = mean(diff_vote_share, na.rm = TRUE),
                     sd.prox = sd(pivot_prox, na.rm = TRUE),
                     sd.diff = sd(diff_vote_share, na.rm = TRUE)
                   )
ggplot(state.elec.sum, aes(x = mean.prox, y = sd.prox)) +
  geom_point()

ggplot(state.elec.sum, aes(x = mean.diff, y = sd.diff)) +
  geom_point()


# swing and pivotal
table(state.data$swing, state.data$pivot_prox)
table(state.data$swing, state.data$elec_votes)
summary(state.data$elec_votes)
state.data <- state.data %>%
               mutate(
                 pivotal = ifelse(pivot_prox <= 5, 1, 0),
                 swing_pivot = swing*pivotal,
                 swing_war = swing*gwot
               )

# swing and core summary
state.data$comp.sum <- ifelse(state.data$swing == 1, "Swing",
                              ifelse(state.data$core == 1, "Core",
                                     "Neither"))

state.data$swing.sum <- ifelse(state.data$swing == 1, "Swing State", 
                               "Not Swing")

