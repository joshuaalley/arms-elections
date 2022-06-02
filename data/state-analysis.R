# Joshua Alley
# Senate elections and state cycles


# load senate data here:
senate.data <- read.csv("data/senate-data.csv")
glimpse(senate.data)

# annual by state, year, and program 
contracts.data.state <- contracts.data %>%
  group_by(year, state) %>% 
  summarize(
    obligations = sum(fed.obligation, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  mutate( # obligations in billions
    obligations = obligations / 1000000000
  ) %>%
  filter(state %in% senate.data$state) %>%
  arrange(state, year) %>%
  ungroup()

# senate
state.sen.data <- left_join(contracts.data.state, senate.data) %>%
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

# merge with state contracts data
state.sen.data$state <- str_to_title(state.sen.data$state)
state.data <- left_join(state.sen.data, cspp.data) %>%
               filter(year <= 2019)


### raw data 
table(state.data$incumbent)
table(state.data$time_to_elec)


# plot obligations
ggplot(drop_na(state.data, incumbent), 
              aes(x = factor(time_to_elec,
                             ordered = TRUE,
                             levels = c("3", "2",
                             "1", "0")),
                  y = obligations,
                  color = factor(incumbent))) +
  geom_boxplot()


# simple model: OLS
sen.ob.ols <- lm(obligations ~ time_to_elec*incumbent,
                  data = state.data) 
summary(sen.ob.ols)

# robust
sen.ob.rlm <- rlm(obligations ~ time_to_elec*incumbent,
                 data = state.data) 
summary(sen.ob.rlm)


sen.vote.log <- glm(incumb.win ~ obligations +
                   poptotal + ln_ngdp,
                 data = filter(state.data, incumbent == 1),
                 family = binomial(link = "logit"))
summary(sen.vote.log)
