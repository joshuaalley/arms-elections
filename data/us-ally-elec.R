# Joshua Alley
# Analyze which allies respond to elections 




### log normal hurdle model
us.arms.comp <- select(us.trade.ally,
                       ccode, year,
   us_arms, lag_us_arms, nz_us_arms,
   change_us_arms,
   time_to_elec, atop_defense, 
   rep_pres, cold_war,
   xm_qudsest2,  cowmidongoing, dyadigos,
   GDP_o, GDP_d, Distw, eu_member,
   Comlang, Contig, Evercol) %>%
   mutate(
      election_defense = time_to_elec*atop_defense
   ) %>%
   drop_na() %>%
   ungroup()

# non-zero arms
us.arms.nz <- glm(nz_us_arms ~ 
                     atop_defense + 
                     rep_pres + cold_war +
                     xm_qudsest2 +  cowmidongoing + dyadigos +
                     GDP_o + GDP_d + Distw + eu_member +
                     Comlang + Contig + Evercol,
                  data = us.arms.comp,
                  family = binomial(link = "logit"))
summary(us.arms.nz)




# predicted prob of non-zero arms
us.arms.comp$pred_nz_arms <- predict(us.arms.nz, type = "response")
ggplot(us.arms.comp, aes(x = pred_nz_arms)) + geom_histogram()
ggplot(us.arms.comp, aes(x = pred_nz_arms,
                         group = nz_us_arms,
                         fill = nz_us_arms)) + geom_histogram()

# arms trade models
us.arms.ex <- rlm(us_arms ~ lag_us_arms +
                     time_to_elec*atop_defense + 
                     rep_pres + cold_war +
                     xm_qudsest2 +  cowmidongoing + dyadigos +
                     GDP_o + GDP_d + Distw + eu_member +
                     Comlang + Contig + Evercol + pred_nz_arms,
                  data = filter(us.arms.comp, nz_us_arms == 1))
summary(us.arms.ex)

# changes in arms exports: same as trade levels
us.arms.chex <- rlm(change_us_arms ~ 
                      time_to_elec*atop_defense + 
                      rep_pres + cold_war +
                      xm_qudsest2 +  cowmidongoing + dyadigos +
                      GDP_o + GDP_d + Distw + eu_member +
                      Comlang + Contig + Evercol +
                      pred_nz_arms,
                   data = filter(us.arms.comp, nz_us_arms == 1))
summary(us.arms.chex)

# ME and predicted values
us.arms.res <- me.us.elec(model = us.arms.chex,
                          data = filter(us.arms.comp, nz_us_arms == 1))


# results
# combine predictions 
us.arms.pred <- bind_rows(
   "All Years" = us.arms.res[[2]],
   .id = "time"
)

# plot
ggplot(us.arms.pred, aes(y = fit, 
                         x = time_to_elec,
                         group = factor(atop_defense),
                         color = factor(atop_defense))) +
   facet_wrap(~ time) +
   scale_x_reverse() + # decreasing time to election
   #geom_hline(yintercept = 0) +
   geom_line() +
   geom_pointrange(aes(ymin = lwr, ymax = upr),
                   position = position_dodge(width = .1)) +
   scale_color_grey("Defense Pact", 
                    start = 0.7,
                    end = 0.1,
                    labels = c(`0` = "No", `1` = "Yes")) +
   labs(y = "Predicted Arms Exports",
        x = "Years to Election")


# combine marginal effects  
us.arms.me <- bind_rows(
   "All Years" = us.arms.res[[1]],
   .id = "time"
)

# plot
ggplot(us.arms.me, aes(y = dydx, 
                       x = time_to_elec)) +
   facet_wrap(~ time, scales = "free_y") +
   scale_x_reverse() +
   geom_hline(yintercept = 0) +
   geom_line() +
   geom_pointrange(aes(
      ymin = dydx - 1.96*std.error,
      ymax = dydx + 1.96*std.error),
      position = position_dodge(width = .1)) +
   labs(y = "Estimated Marginal Effect of Alliance",
        x = "Years to Election")


### hurdle models instead 
# regressor matrices

# regressors: fixed
hurdle.arms.nz <- as.matrix(select(us.arms.comp, 
                                   lag_us_arms,
                                   time_to_elec, atop_defense, 
                                   election_defense,
                                   rep_pres, cold_war,
                                   xm_qudsest2,  cowmidongoing, dyadigos,
                                   GDP_o, GDP_d, Distw, eu_member,
                                   Comlang, Contig, Evercol))


hurdle.arms.z <- as.matrix(select(us.arms.comp,
                                  atop_defense, 
                                  rep_pres, cold_war,
                                  xm_qudsest2,  cowmidongoing,
                                  Comlang, Contig, Evercol))


# data
us.arms.data <- list(
   Y = us.arms.comp$us_arms,
   N = nrow(us.arms.comp),
   K = ncol(hurdle.arms.nz),
   X = hurdle.arms.nz,
   K_hu = ncol(hurdle.arms.z),
   X_hu = hurdle.arms.z
)

# compile model code
arms.hurdle <- cmdstan_model("data/stan-hurdle-logn.stan",
                             cpp_options = list(stan_threads = TRUE))

# stan model fit
fit.us.arms <- arms.hurdle$sample(
   data = us.arms.data,
   seed = 12,
   iter_warmup = 1000,
   iter_sampling = 1000,
   chains = 4,
   parallel_chains = 4,
   threads_per_chain = 4,
   refresh = 200,
   max_treedepth = 20,
   adapt_delta = .90
)
fit.us.ex$cmdstan_diagnose()
fit.us.ex$cmdstan_summary()



### Arms sales cycles

# load arms sales data
arms.sales <- read.csv("data/us-arms-sales.csv")
glimpse(arms.sales)
# nothing after year- notifications
# .1 after year- authorizations
# .2 after year- deliveries

# reshape 
arms.sales <- pivot_longer(arms.sales,
                           cols = -c(country),
                           names_to = "year",
                           values_to = "sale")
# label type of flow 
arms.sales$year <- as.numeric(str_remove(arms.sales$year, "X"))
arms.sales$flow <- round(arms.sales$year%%1 * 10)
arms.sales$flow <- recode(arms.sales$flow, `0` = "notif",
                          `1` = "author",
                          `2` = "deliv")
# clean up years and flow
arms.sales$year <- round(arms.sales$year)
arms.sales$sale <- as.numeric(gsub("[[:punct:]]", "", arms.sales$sale))
# sales in billions
arms.sales$sale <- arms.sales$sale / 1000000
summary(arms.sales$sale)
# Set NA to zero
arms.sales$sale[is.na(arms.sales$sale)] <- 0

# log transform
arms.sales$sale <- log(arms.sales$sale + 1)

# add countrycode 
arms.sales$ccode <- countrycode(arms.sales$country,
                                origin = "country.name",
                                destination = "cown")
arms.sales$ccode[arms.sales$country == "Serbia"] <- 345

# remove non-countries 
arms.sales <- drop_na(arms.sales, ccode)

# plot sales overall
ggplot(arms.sales, aes(x = sale,
                       group = flow,
                       fill = flow)) +
   geom_histogram(position = position_dodge())

# plot flows over time 
arms.sales.yr <- arms.sales %>%
                  group_by(year, flow) %>%
                  summarize(
                     sales = sum(sale, na.rm = TRUE),
                     .groups = "keep"
                  )

ggplot(arms.sales.yr, aes(x = year, y = sales,
                          group = flow,
                          color = flow)) +
   geom_line() +
   geom_vline(xintercept=c(pres.elections), linetype="dotted") +
   xlim(2000, 2020)

# pivot wider and add covariates 
us.arms.sales <- pivot_wider(arms.sales,
                             id_cols = c("country", "ccode", "year"),
                             names_from = "flow",
                             values_from = "sale") %>%
                 left_join(us.trade.ally) %>%
                 group_by(ccode, year) %>%
                 mutate(
                    lag_author = lag(author),
                    lag_deliv = lag(deliv)
                 )

# correlation between sales and arms: stronger in year than with lags
cor.test(us.arms.sales$author, us.arms.sales$us_arms)
cor.test(us.arms.sales$deliv, us.arms.sales$us_arms)




### Signals to allies and exports 

# us signals
us.signals <- read.csv("data/revise-latent-supp/Individual Signals Dataset.csv", header=TRUE) %>% 
               select(year, starts_with("us_"))

# us support
us.supp <- filter(latent.supp, ccode1 == 2) %>%
            rename(ccode = ccode2) %>%
            select(-year) %>%
            bind_cols(
               filter(us.signals, us_pact == 1)) %>%
            select(-us_arms)


# filter and clean us ally data
us.all.data <- filter(us.trade.ally, atop_defense == 1) %>%
                left_join(us.supp) %>%
               group_by(ccode, president) %>%
               mutate(
                # running mean support
                 mean_leader_supp = rollapply(median, 2, mean,
                                 align ='right', fill = lag_median),
                 total_leader_words = rollapply(us_words, 2, sum,
                              align ='right', fill = 0),
                 total_leader_vis = rollapply(us_vis, 2, sum,
                                      align ='right', fill = 0)
               ) %>%
              group_by(rep_pres, ccode) %>%
              mutate(
                mean_party_supp = rollapply(median, 2, mean,
                            align ='right', fill = lag_median)
              ) %>%
              ungroup() %>%
              select(
                change_ln_exports, change_ln_imports,
                change_trade, change_ihs_balance,
                v2clstown, mean_leader_supp, 
                mean_party_supp, us_words, us_arms, change_us_arms, us_troops,
                us_nukes, us_vis, total_leader_words, total_leader_vis,
                  eu_member, near_elec, 
                  xm_qudsest2, cowmidongoing, dyadigos,
                  GDP_o, GDP_d, Distw,
                  rep_pres,
                  Comlang, Contig, Evercol
              ) %>% 
              drop_na()


# all panes
us.all.data[, 5:ncol(us.all.data)] <- apply(us.all.data[, 5:ncol(us.all.data)],
                              2, function(x)
                              arm::rescale(x, binary.inputs = "0/1"))


# separate models for near elections 
us.all.supp <- rlm(change_ln_exports ~ 
                      eu_member +  
                      change_us_arms + us_troops + us_nukes +
                      total_leader_vis + total_leader_words + 
                      xm_qudsest2 +  cowmidongoing + dyadigos +
                      GDP_o + GDP_d + Distw +
                      rep_pres +
                      Comlang + Contig + Evercol,
                   data = us.all.data)
summary(us.all.supp)


# separate models for near elections 
us.all.elec <- rlm(change_ln_exports ~ 
                     eu_member +  
                      change_us_arms + us_troops + us_nukes +
                      total_leader_vis + total_leader_words + 
                     xm_qudsest2 +  cowmidongoing + dyadigos +
                     GDP_o + GDP_d + Distw +
                     rep_pres +
                     Comlang + Contig + Evercol,
                   data = filter(us.all.data, near_elec == 1))
summary(us.all.elec)


# separate models for near elections: no election 
us.all.noelec <- rlm(change_ln_exports ~ 
                      eu_member +  
                        change_us_arms + us_troops + us_nukes +
                        total_leader_vis + total_leader_words + 
                      xm_qudsest2 +  cowmidongoing + dyadigos +
                      GDP_o + GDP_d + Distw +
                      rep_pres +
                      Comlang + Contig + Evercol,
                   data = filter(us.all.data, near_elec == 0))
summary(us.all.noelec)


