# Joshua Alley
# clean data for process model 

# state component
state.data.ml <- select(state.data, state, year,
                        change_ln_obligations, 
                        swing, core, 
                        time_to_elec,
                        rep_pres, change_poptotal,
                        change_ln_ngdp, iraq_war) %>% 
  distinct() %>% 
  group_by(state) %>%
  mutate(
    state.year.txt = paste0(state, ".", year)
  ) %>% # remove missing for STAN
  drop_na() 
state.data.ml$state.year <- state.data.ml %>%
  group_by(state, year) %>%
  group_indices()
state.data.ml$year.id <- state.data.ml %>%
  group_by(year) %>%
  group_indices()

# plot obligations
ggplot(state.data.ml, aes(x = change_ln_obligations)) + geom_histogram()

# clean up ordering
state.data.ml <- state.data.ml %>%
  select(state, year, state.year.txt, state.year, year.id,
         change_ln_obligations,
         everything()) %>%
  filter(year <= 2018) %>% # match missing in COW 
  group_by(year, state.year.txt) %>%
  mutate(n = n(), .groups = "drop") %>%
  filter(n == 1) 
class(state.data.ml) <- "data.frame"


# state data for analysis 
state.yr.final <- state.data.ml %>%
  mutate(
    intercept = 1
  ) %>%
  select(intercept,
         swing, core, 
         time_to_elec,
         rep_pres, change_poptotal,
         change_ln_ngdp, iraq_war) 
# rescale obligations and GDP by 2sd 
state.yr.final[, 2:3] <- apply(state.yr.final[, 2:3], 2,
       function(x) arm::rescale(x,
                                binary.inputs = "0/1"))


# matrix for stan
state.yr.cont <- as.matrix(select(state.yr.final,
                                   intercept,
                                  rep_pres, change_poptotal,
                                  change_ln_ngdp, iraq_war))

state.yr.comp <- as.matrix(select(state.yr.final,
                                  swing, core, time_to_elec))




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
                 filter(year %in% state.data.ml$year) %>% # pakistan/east pak duplicate gives warning- drop
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
us.arms.deals$ally.id <- us.arms.deals %>% group_by(ally) %>%
  group_indices()


ggplot(us.arms.deals, aes(x = deals)) + geom_histogram()


# create a matrix to index when state-year obs apply 
# (double-indexing failed)
yr.idmat <- left_join(
    select(us.arms.deals, ccode, year),
    select(state.data.ml, year),
    multiple = "all"
   ) %>%
  distinct() %>%
  group_by(ccode, year) %>%
  summarise(present = dplyr::n(), 
            .groups = "drop") %>%
  mutate(
    obs = paste(ccode, year, sep = "-")
  ) %>%
  pivot_wider( # wider 
    id_cols = c("obs"),
    names_from = c("year"),
    values_from = "present"
  ) %>%
  select(-c(obs))
yr.idmat[is.na(yr.idmat)] <- 0



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
  y_ob = state.data.ml$change_ln_obligations,
  
  Z = as.matrix(yr.idmat),
  T = ncol(yr.idmat),
  year_ob = year.id.state,
  
  G = state.yr.cont,
  L = ncol(state.yr.cont),
  H = state.yr.comp,
  M = ncol(state.yr.comp)
)

