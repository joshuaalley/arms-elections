# Joshua Alley
# Elections and state cycles in defense contracting


### raw data 
table(state.data$incumbent)
table(state.data$time_to_selec)
table(state.data$time_to_pelec)


# plot obligations
ggplot(state.data, aes(x = ln_obligations)) + geom_histogram()

ggplot(drop_na(state.data, incumbent), 
              aes(x = factor(time_to_selec,
                             ordered = TRUE,
                             levels = c("3", "2",
                             "1", "0")),
                  y = ln_obligations,
                  color = factor(incumbent))) +
  geom_boxplot()


ggplot(state.data, 
       aes(color = factor(time_to_selec,
                      ordered = TRUE,
                      levels = c("3", "2",
                                 "1", "0")),
           y = ln_obligations,
           x = s_comp)) +
  geom_point() 


# Senate incumbent models
# simple model: OLS
# robust
sen.ob.rlm <- rlm(ln_obligations ~ s_comp + incumbent +
                    time_to_selec +
                    diff_vote_share + time_to_pelec +
                    poptotal + ln_ngdp + iraq_war,
                 data = state.data) 
summary(sen.ob.rlm)



sen.vote.log <- glm(incumb.win ~ ln_obligations +
                   poptotal + ln_ngdp + pres_election + iraq_war,
                 data = state.data,
                 family = binomial(link = "logit"))
summary(sen.vote.log)



# Presidential vote 

# plot obligations: loess 
ggplot(state.data, 
       aes(group = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           color = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = ln_obligations,
           x = diff_vote_share)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom")


# plot obligations: linear
ggplot(state.data, 
       aes(group = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           color = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = ln_obligations,
           x = diff_vote_share)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "bottom")


# by election timing 
# plot obligations
ggplot(state.data, 
       aes(y = ln_obligations,
           x = diff_vote_share)) +
  facet_wrap(~ time_to_pelec) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom")


# prior vote share and contracting 
pres.ob.vote <- rlm(ln_obligations ~ s_comp + 
                      diff_vote_share +
                      time_to_pelec +
                      iraq_war +
                      poptotal + ln_ngdp, #+ factor(state),
                  data = state.data) 
summary(pres.ob.vote)


# contracting from time to presidential elections and pivot proximity
pres.ob.prox <- lm(ln_obligations ~ time_to_pelec + pivot_prox +
                     iraq_war +
                      poptotal + ln_ngdp,
                   data = state.data) 
summary(pres.ob.prox)




# link orders with state contracts by sector
# this puts orders before contracts
sector.list <- c("aircraft", "arms", "electronics", "missile_space",
                 "ships", "vehicles")
formula.sector <- vector(mode = "list", length = length(sector.list))
  
for(i in 1:length(sector.list)){
  formula.sector[[i]] <- as.formula(
    paste(
      paste0(sector.list[i], "_change ", "~"), 
      # paste0(sector.list[i]), "~",
      # paste0(sector.list[i], "_lag"),
      paste0(" + ", "s_comp"),
      paste0(" + ", "diff_vote_share + time_to_pelec"),
      # paste0(" + ", "all_", sector.list[i], "_lag"),
      # paste0(" + ", "nall_", sector.list[i], "_lag"),
      paste0("+ iraq_war")
      #paste0(" + ", "factor(election.cycle)")
      #paste0(" + ", "factor(state)")
    ))
  }

sector.state.sys <- systemfit(formula.sector, data = state.data.ord)
summary(sector.state.sys)

# interact with pivot proximity  
for(i in 1:length(sector.list)){
  formula.sector[[i]] <- as.formula-(
    paste(
      paste0(sector.list[i]), "_change", "~", 
      #paste0(sector.list[i], "_lag"),
      paste0(" + ", "all_", sector.list[i], "_lag", "*pivot_prox"),
      paste0(" + ", "nall_", sector.list[i], "_lag", "*pivot_prox"),
      paste0(" + ", "iraq_war")
    ))
}

sector.state.lvote <- systemfit(formula.sector, data = state.data.ord)
summary(sector.state.lvote)
