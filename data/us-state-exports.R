# Joshua Alley
# state exports in election years


### exports by state ### 

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
                                               mean_leader_supp, time_to_elec,
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

# merge with electio.res 
election.res <- left_join(election.res, bind_rows(pivot.list))

# add to dyad data
state.exports.dyad <- left_join(state.exports.dyad, election.res)
state.exports.dyad 
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
# examine if desired
#duplicates <- filter(state.exports.dyad, duplicates == TRUE) %>%
#               arrange(state, destination, year)


# Cut out duplicates from merging
state.exports.dyad <- filter(state.exports.dyad, duplicates == FALSE)




# model state exports
summary(state.exports.dyad$value)

# plot exports
ggplot(state.exports.dyad, aes(x = value)) + geom_histogram()
#ggplot(state.exports.dyad, aes(x = lamw_state_exports)) + geom_histogram()
ggplot(state.exports.dyad, aes(x = ln_state_exports)) + geom_histogram()
ggplot(state.exports.dyad, aes(x = ihs_state_exports)) + geom_histogram()
# long right tail- outliers and skewed
# no 0s- so no hurdle model here
sum(state.exports.dyad$value == 0)
# work in logs
quantile(state.exports.dyad$ihs_state_exports)
quantile(state.exports.dyad$ln_state_exports)




# complete data on key dimensions 
state.exports.dyad.comp <- select(state.exports.dyad,
                                  state, ccode, year, 
                                  value, lag_close_state, lag_diff_vote,
                                  #time_to_elec,
                                  pivot_prox, election,
                                  lag_election, lead_election,
                                  lag_poptotal, election, lead_election,
                                  atop_defense, lag_ln_ngdp,
                                  lag_ln_rgdpe, lag_pop, lag_xr, lag_csh_g) %>%
  filter(election == 1) %>%
  mutate(
    fin_crisis = ifelse(year == 2007 | year == 2008 |
                                year == 2009,
                              1, 0),
    reelection_bush = ifelse(year == 2004 |
                               year == 2003, 1, 0),
    reelection_obama = ifelse(year == 2012 |
                                year == 2011, 1, 0),
    reelection_trump = ifelse(year == 2020 |
                                year == 2019, 1, 0),
    dem_incumbent = ifelse(year == 2012 | year == 2016, 
                           1, 0),
    ln_state_exports = log(value),
    lag_ln_exports = lag(ln_state_exports),
    change_ln_exports = ln_state_exports - lag_ln_exports,
  ) %>%
  drop_na()

# create a presidential variable
state.exports.dyad.comp$president <- pres.full(state.exports.dyad.comp)



# rescale IVs by 2sd 
state.exports.dyad.comp[, 9:21] <- lapply(state.exports.dyad.comp[, 9:21],
                                          function(x)
                                            arm::rescale(x, binary.inputs = "0/1"))
# year dummies
state.exports.dyad.comp <- dummy_cols(state.exports.dyad.comp, select_columns = "year")


state.exports.dyad.comp$dyad.id <- group_indices(state.exports.dyad.comp, state, ccode) 
state.exports.dyad.comp$cntry.id <- group_indices(state.exports.dyad.comp, ccode) 
state.exports.dyad.comp$state.id <- group_indices(state.exports.dyad.comp, state) 



# outcome distribution
summary(state.exports.dyad.comp$value)
ggplot(state.exports.dyad.comp, aes(x = ln_state_exports)) + geom_histogram()


# simple weighted model:
lm.state.exports <- rlm(ln_state_exports ~ lag_ln_exports +
                         atop_defense*lag_diff_vote +
                       reelection_bush + reelection_obama + reelection_trump +
                         lag_poptotal + lag_ln_ngdp + fin_crisis +
                         lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                       data = state.exports.dyad.comp)
summary(lm.state.exports)



# pivot proximity instead: 
lm.state.exports.prox <- rlm(ln_state_exports ~ lag_ln_exports +
                              atop_defense*pivot_prox +
                              lag_poptotal + lag_ln_ngdp + fin_crisis +
                              reelection_bush + reelection_obama + reelection_trump +
                              lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                            data = state.exports.dyad.comp)
summary(lm.state.exports.prox)


# dyad robust se 
state.exports.dr <- dyadRobust(lm(ln_state_exports ~ lag_ln_exports +
                                    atop_defense*lag_diff_vote +
                                    reelection_bush + reelection_obama + reelection_trump +
                                    lag_poptotal + lag_ln_ngdp + fin_crisis +
                                    lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                                  weights = lm.state.exports$w,
                                  data = state.exports.dyad.comp),
                               dat = state.exports.dyad.comp,
                               dyadid = "dyad.id",
                               egoid = "state",
                               alterid = "ccode")

# dyad robust se
state.exports.prox.dr <- dyadRobust(lm(ln_state_exports ~ lag_ln_exports +
                                         atop_defense*pivot_prox +
                                         reelection_bush + reelection_obama + reelection_trump +
                                         lag_poptotal + lag_ln_ngdp + fin_crisis +
                                         lag_ln_rgdpe + lag_pop + lag_xr + lag_csh_g,
                                       weights = lm.state.exports.prox$w,
                                       data = state.exports.dyad.comp),
                                    dat = state.exports.dyad.comp,
                                    dyadid = "dyad.id",
                                    egoid = "state",
                                    alterid = "ccode")

# clean up dr results
state.est <- bind_rows(
  dr.clean(state.exports.dr),
  dr.clean(state.exports.prox.dr)
) %>% # cut FE and intercept terms
  filter(str_detect(variable, "dyad.id", negate = T)) %>%
  filter(str_detect(variable, "Intercept", negate = T))

# nice names for plotting
coef.names.state = c("(Intercept)" = "Intercept",
                     "lag_ln_exports" = "Lag Ln(Exports)",
                     "atop_defense" = "Alliance",
                     "lag_diff_vote" = "Prior Election Vote Difference",
                     "pivot_prox" = "Pivot State Proximity",
                     "atop_defense:lag_diff_vote" = "Alliance x Vote Difference",
                     "atop_defense:pivot_prox" = "Alliance x Pivot Proximity",
                     "reelection_bush" = "Bush Reelection",
                     "reelection_obama" = "Obama Reelection",
                     "reelection_trump" = "Trump Reelection",
                     "lag_poptotal" = "Lag State Population", 
                     "lag_ln_ngdp" = "Lag Ln(State GDP)", 
                     "fin_crisis" = "Great Recession",
                     "lag_ln_rgdpe" = "Lag LN(Destination GDP)",
                     "lag_pop" = "Destination Population",
                     "lag_xr" = "Destination Exchange Rate",
                     "lag_csh_g" = "Destination Government Spending")
state.est$variable <- coef.names.state[state.est$variable]

# model names
model.names.state <- c("state.exports.dr" = "Vote Difference",
                       "state.exports.prox.dr" = "Pivot Proximity")
state.est$model <- model.names.state[state.est$model]



# plot results
ggplot(state.est, aes(y = factor(variable, ordered = T,
                                 levels = rev(coef.names.state)),
                      x = coef,
                      group = model,
                      color = model)) +
  geom_vline(xintercept = 0) +
  #xlim(-0.3, 0.3) +
  geom_pointrange(aes(
    xmin = coef - 1.96*se,
    xmax = coef + 1.96*se),
    position = position_dodge(width = 1)
  ) +
  scale_color_grey() +
  labs(x = "Estimate",
       y = "Term",
       color = "Model")
ggsave("figures/state-model-coefs.png", height = 6, width = 8)


# plot ME 
# level of exports 
state.ex.vote <- marginaleffects(lm.state.exports,
                                 vcov = state.exports.dr$Vhat,
                                 variables = "atop_defense",
                                 newdata = typical(lag_diff_vote = 
                                                     c(seq(from = 0.0, to = .48, by = .01))))
# level of exports 
state.ex.prox <- marginaleffects(lm.state.exports.prox,
                                 vcov = state.exports.prox.dr$Vhat,
                                 variables = "atop_defense",
                                 newdata = typical(pivot_prox = 
                                                     c(seq(from = 0, to = 30, by = 1))))

# plot all three margins
plot.vdiff.ex  <- ggplot(state.ex.vote, 
                         aes(y = dydx, x = lag_diff_vote,
                             ymin = dydx - 1.96*std.error,
                             ymax = dydx + 1.96*std.error)) +       
  geom_line(size = 1) +
  geom_ribbon(alpha = .1) +
  labs(y = "Estimated Marginal Effect of Alliance",
       x = "Prior Election Vote Difference",
       title = "Vote Difference")
plot.vdiff.ex
# plot imports
plot.prox.ex  <- ggplot(state.ex.prox, 
                        aes(y = dydx, x = pivot_prox,
                            ymin = dydx - 1.96*std.error,
                            ymax = dydx + 1.96*std.error)) +
  #geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_ribbon(alpha = .1) +
  labs(y = "Estimated Marginal Effect of Alliance",
       x = "Proximity to Electoral College Pivot",
       title = "Pivot Proximity")
plot.prox.ex


# plot marginal effects
grid.arrange(plot.vdiff.ex, plot.prox.ex, nrow = 1)
me.all.state <- arrangeGrob(plot.vdiff.ex, plot.prox.ex, nrow = 1)
ggsave("figures/me-all-state.png", me.all.state,
       height = 6, width = 8)




# brms model
bf.exports.state <- brmsformula(ln_state_exports ~ lag_ln_exports +
                                  lag_atop_defense*lag_diff_vote + 
                                  reelection_bush + reelection_obama +
                                  reelection_trump + 
                                  lag_poptotal + 
                                  lag_rgdpe + lag_pop + lag_xr + lag_csh_g +
                                  (1 | state) + (1 | ccode),
                                center = TRUE) +
  student()
exports.priors.state <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "sigma"),
  set_prior("normal(0, 1)", class = "sd")
) 

# fit the model
brm.state.exports <- brm(bf.exports.state, 
                         data = state.exports.dyad.comp,
                         prior = exports.priors.state,
                         iter = 2000, warmup = 1000,
                         chains = 4, cores = 4,
                         backend = "cmdstanr",
                         control = list(max_treedepth = 20))
summary(brm.state.exports, prob = .9)





