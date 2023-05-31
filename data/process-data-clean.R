# Joshua Alley
# clean data for process model 

# state component
state.data.ml <- select(state.data, state, year,
                        ln_obligations, lag_ln_obligations,
                        swing, core, time_to_elec,
                        rep_pres, poptotal,
                        ln_ngdp, gwot) %>% 
  distinct() %>% 
  group_by(state) %>%
  mutate(
    state.year.txt = paste0(state, ".", year)
  ) %>% # remove missing for STAN
  drop_na() 
state.data.ml$state <- state.data.ml %>%
  group_by(state) %>%
  group_indices()
state.data.ml$year.id <- state.data.ml %>%
  group_by(year) %>%
  group_indices()

# plot obligations
ggplot(state.data.ml, aes(x = ln_obligations)) + geom_histogram()

# clean up ordering
state.data.ml <- state.data.ml %>%
  select(state, year, state.year.txt, state.year, year.id,
         ln_obligations,
         everything()) %>%
  filter(year <= 2019) %>% # match missing in COW 
  group_by(year, state.year.txt) %>%
  mutate(n = n(), .groups = "drop") %>%
  filter(n == 1) 
class(state.data.ml) <- "data.frame"


# state data for analysis 
state.yr.final <- state.data.ml %>%
  select(swing, core, gwot, time_to_elec,
         rep_pres, poptotal, 
         ln_ngdp) 
# rescale population and GDP by 2sd 
state.yr.final$ln_ngdp <- arm::rescale(state.yr.final$ln_ngdp,
                                binary.inputs = "0/1")
state.yr.final$poptotal <- arm::rescale(state.yr.final$poptotal,
                                       binary.inputs = "0/1")


# matrix for stan
state.yr.mean <- as.matrix(state.yr.final)




# orders data  
us.arms.deals <- us.arms.cat %>%
                  group_by(ccode, year) %>%
                  summarize(
                    deals = sum(deals, na.rm = TRUE),
                    .groups = "keep"
                  ) %>% 
                  right_join(select(us.trade.ally,
                          ccode, year, time_to_elec,
                          ally, v2x_polyarchy2,
                          Comlang, ln_rgdp, 
                          ln_distw, eu_member)) %>%
                distinct()
# rescale for model input
us.arms.deals$ln_rgdp <- arm::rescale(us.arms.deals$ln_rgdp, binary.inputs = "0/1")
us.arms.deals$ln_distw <- arm::rescale(us.arms.deals$ln_distw, binary.inputs = "0/1")
# no deals are NA, make zero
us.arms.deals$deals[is.na(us.arms.deals$deals)] <- 0
# drop missing- cow and other key controls only through 2014
us.arms.deals <- drop_na(us.arms.deals)
class(us.arms.deals) <- "data.frame"


# group indices 
us.arms.deals$cntry.index <- us.arms.deals %>% group_by(ccode) %>%
  group_indices()


ggplot(us.arms.deals, aes(x = deals)) + geom_histogram()


# create a matrix to index when state-year obs apply 
# (double-indexing failed)
state.yr.idmat <- left_join(
  select(us.arms.deals, ccode, year),
  select(state.data.ml, year, state.year.txt)
) %>%
  distinct() %>%
  group_by(ccode, year, state.year.txt) %>%
  summarise(n = dplyr::n(), .groups = "drop") %>%
  filter(n == 1L) %>% 
  mutate(
    present = 1, # to fill dummies
  ) %>%
  pivot_wider( # wider 
    id_cols = c(ccode, year),
    names_from = "state.year.txt",
    values_from = "present"
  ) %>%
  select(-c(ccode, year))
state.yr.idmat[is.na(state.yr.idmat)] <- 0
state.yr.idmat <- state.yr.idmat[, 2:ncol(state.yr.idmat)]



# separate data- select variables
us.arms.deals.iv <- us.arms.deals %>% 
  select(
    time_to_elec, ally, v2x_polyarchy2, 
    Comlang, ln_rgdp, 
    ln_distw, eu_member
  ) %>%
  mutate(
    time_to_elec_ally = ally * time_to_elec,
    time_to_elec_v2x_polyarchy2 = time_to_elec * v2x_polyarchy2,
    ally_v2x_polyarchy2 = ally * v2x_polyarchy2,
    elec_ally_democ = time_to_elec * ally * v2x_polyarchy2
  )

# time indices
year.id.state <- state.data.ml %>% group_by(year) %>%
  group_indices()

us.arms.deals$year.id <- us.arms.deals %>% group_by(year) %>%
  group_indices()

# data 
process.data <- list(
  N = nrow(us.arms.deals),
  y_arms = us.arms.deals$deals,
  
  X = us.arms.deals.iv,
  K = ncol(us.arms.deals.iv),

  S = nrow(state.data.ml),
  y_ob = state.data.ml$ln_obligations,
  
  Z = t(as.matrix(state.yr.idmat)),
  year_ob = year.id.state,
  
  st = max(state.data.ml$state),
  state = state.data.ml$state,
  lag_y_ob = state.data.ml$lag_ln_obligations,
  G = state.yr.mean,
  L = ncol(state.yr.mean),
  gwot = state.data.ml$gwot
)

