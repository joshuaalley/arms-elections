# Joshua Alley
# Senate elections and state cycles


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



# Senate incumbent models
# simple model: OLS
# robust
sen.ob.rlm <- lm(ln_obligations ~ time_to_selec*incumbent +
                    poptotal + ln_ngdp + pres_election,
                 data = state.data) 
summary(sen.ob.rlm)


sen.ob.brm <- brm(ln_obligations ~ sen_election*incumbent +
                    poptotal + ln_ngdp + pres_election +
                    (1 | state),
                  family = student(),
                  backend = "cmdstanr",
                  control = list(
                    max_treedepth = 20
                  ),
                  data = state.data,
                  cores = 4,
                  threads = 4,
                  prior = c(
                    prior(normal(0, .5), class = b),
                    prior(normal(0, 1), class = sd),
                    prior(normal(0, 1), class = sigma)
                  ))
summary(sen.ob.brm)





sen.vote.log <- glm(incumb.win ~ ln_obligations +
                   poptotal + ln_ngdp + pres_election,
                 data = filter(state.data, incumbent == 1),
                 family = binomial(link = "logit"))
summary(sen.vote.log)



# Presidential vote 


# plot obligations
ggplot(state.data, 
       aes(color = factor(time_to_pelec,
                          ordered = TRUE,
                          levels = c("3", "2",
                                     "1", "0")),
           y = ln_obligations,
           x = pivot_prox)) +
  geom_point() +
  theme(legend.position = "bottom")


pres.ob.vote <- lm(ln_obligations ~ time_to_pelec*lag_diff_vote +
                      poptotal + ln_ngdp + factor(state),
                  data = state.data) 
summary(pres.ob.vote)


# Presidential vote 
pres.ob.prox <- lm(ln_obligations ~ time_to_pelec*pivot_prox +
                      poptotal + ln_ngdp + factor(state),
                   data = state.data) 
summary(pres.ob.prox)




pres.ob.brm <- brm(ln_obligations ~ time_to_pelec*lag_diff_vote +
                    poptotal + ln_ngdp + 
                    (1 | state),
                  family = student(),
                  backend = "cmdstanr",
                  control = list(
                    max_treedepth = 20
                  ),
                  data = state.data,
                  cores = 4,
                  threads = 4,
                  prior = c(
                    prior(normal(0, .5), class = b),
                    prior(normal(0, 1), class = sd),
                    prior(normal(0, 1), class = sigma)
                  ))
summary(pres.ob.brm)





# link orders with state contracts by sector
sector.list <- c("air", "arms", "electronics", "missile_space",
                 "ships", "vehicles")
formula.sector <- vector(mode = "list", length = length(sector.list))
  
for(i in 1:length(sector.list)){
  formula.sector[[i]] <- as.formula(
    paste(
      sector.list[i], "~", 
      paste0(sector.list[i], "_lag"),
      paste0(" + ", sector.list[i], "_or"),
      paste0(" + ", sector.list[i], "_or_lag"),
      paste0(" + ", "factor(state)")
    ))
  }

sector.state.sys <- systemfit(formula.sector, data = state.cont.yr)
summary(sector.state.sys)

# changes: very close to unit roots in all series 
for(i in 1:length(sector.list)){
  formula.sector[[i]] <- as.formula(
    paste(
      paste0(sector.list[i], "_change"), "~", 
      paste0(" + ", sector.list[i], "_or_change"),
      paste0(" + ", "factor(state)")
    ))
}

sector.state.sys <- systemfit(formula.sector, data = state.cont.yr)
summary(sector.state.sys)
