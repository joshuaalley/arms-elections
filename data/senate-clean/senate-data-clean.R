# Joshua Alley
# Clean senate elections data


# load senate data
senate.data <- read.csv("data/1976-2020-senate.csv") %>%
  mutate(
    election = 1,
    vote.share = candidatevotes / totalvotes
  ) %>% # remove uncompetitive candidates
  filter(vote.share >= .1)

glimpse(senate.data)

# key outcomes
senate.data.key <- select(senate.data, state, year, special,
                          election, party_simplified,
                          vote.share) %>% 
  group_by(state, year, special) %>%
  filter(vote.share == max(vote.share, na.rm = TRUE)) %>%
  group_by(state, year) %>%
  mutate(
    election_id = 1:n()
  ) %>%
  ungroup()

# incumbency,
senate.incumb <- read.csv("data/senate_incumbency.csv") %>%
  select(-c(state.lower, stpost)) %>%
  group_by(state, year) %>%
  mutate(
    election_id = 1:n()
  ) %>%
  ungroup()

senate.data.key <- left_join(senate.data.key, senate.incumb) %>%
  filter(year >= 1980)

# output here- edited manually b/c of challenges with jungle primaries
# and uncontested runs
write.csv(senate.data.key, "data/senate-data.csv", row.names = FALSE)