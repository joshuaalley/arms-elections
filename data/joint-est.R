# Joshua Alley
# joint model with arms deals and contracts


# ordbeta reg for transformed outcomes
formula.state <- bf(obligations_rs ~
                      (lag_obligations_rs || state) +
                      deals*swing + core +
                      gwot + time_to_elec + 
                      rep_pres  +
                      poptotal + ln_ngdp,
                    center = FALSE)
deals.state <- ordbetareg(formula.state,
                  true_bounds = c(0, 1),
                   data = state.data.deals,
                   cores = 4,
                   backend = "cmdstanr",
                  refresh = 200
) 
summary(deals.state)
pp_check(deals.state)
fixef(deals.state)


# back to outcome scale: use median yearly contract sum
scale.factor <- median(state.data.deals$sum_ob, na.rm = T)

# negative deals coef implies that more deals reduce the share of 
# contracts non-swing states receive. 

# fake data
hyp.data <- datagrid(model = deals.state,
         swing = c(0, 1),
         deals = seq(from = min(state.data.deals$deals),
                      to = max(state.data.deals$deals), by = 1),
         gwot = 0,
         rep_pres = 0,
         core = 0,
         time_to_elec = 1,
         poptotal = median(state.data.deals$poptotal),
         ln_ngdp = median(state.data.deals$ln_ngdp),
         state = "Wisconsin") %>%
         arrange(swing) 

# smaller hypothetical- 1st to 3rd quartile deals
hyp.data.iqr <- datagrid(model = deals.state,
                     swing = c(0, 1),
                     deals = c(quantile(state.data.deals$deals)[2],
                               quantile(state.data.deals$deals)[4]),
                     gwot = 0,
                     rep_pres = 0,
                     core = 0,
                     time_to_elec = 1,
                     poptotal = median(state.data.deals$poptotal),
                     ln_ngdp = median(state.data.deals$ln_ngdp))

ev.hyp <- fitted(deals.state, 
                 newdata = hyp.data.iqr,
                 scale = "response",
                 summary = TRUE)
ev.hyp <- apply(ev.hyp, 2, function(x)
  quantile(x * scale.factor,
           probs = c(.05, .5, .9)))
ev.hyp

# data and estimate
hyp.data.est <- bind_cols(hyp.data.iqr, t(ev.hyp))

# plot 
ggplot(hyp.data.est, aes(x = factor(swing), y = `50%`,
                         color = factor(deals))) +
  geom_pointrange(aes(ymin = `5%`, ymax = `90%`),
                  position = position_dodge(width = .5))

# marginal effect of deals
deals.est <- slopes(deals.state,
                         newdata = hyp.data,
                         by = "swing",
                         variables = "deals",
                         conf_level = .90) %>%
  mutate_at(
    c("estimate", "conf.low", "conf.high"),
    function(x) x * scale.factor 
  )
deals.est

# draws:
deals.state.draws <- prepare_predictions(deals.state)
deals.inter <- as.data.frame(deals.state.draws$dpars$mu$fe$b)
hypothesis(deals.inter, c("b_deals:swing > b_deals"))
hypothesis(deals.inter, c("b_deals:swing > 0"))
hypothesis(deals.inter, c("b_deals > 0"))


deals.inter <- function(model, scale.factor){
  
  # all draws 
  deals.draws <- prepare_predictions(model)
  deals.inter <- as.data.frame(deals.draws$dpars$mu$fe$b)
  
  # use separate plots 
  deals.post <- as.data.frame(deals.inter[, 2] * scale.factor)
  colnames(deals.post) <- c("Deals")
  deals.pos <- round(sum(deals.post > 0) / 4000, digits = 2)
  deals.pos <- sub("^0+", "", deals.pos)
  
  deals.swing.post <- as.data.frame(deals.inter[, 10] * scale.factor)
  colnames(deals.swing.post) <- c("Deals:Swing")
  deals.swing.pos <- round(sum(deals.swing.post > 0) / 4000, digits = 2)
  deals.swing.pos <- sub("^0+", "", deals.swing.pos)
  
  deals.post.all <- bind_cols(deals.post, deals.swing.post)
  
  xmin <- min(deals.post.all)
  xmax <- max(deals.post.all)
  
  deals.dens <- ggplot(deals.post.all, aes(x = Deals)) +
    geom_density()
  deals.dens
  dens.data <- ggplot_build(deals.dens)$data[[1]]
  dens.med <- quantile(dens.data$y)[4]
  
  deals.dens <- deals.dens + geom_area(data = subset(dens.data, x > 0),
                                       aes(x=x, y=y), fill="darkgrey") +
    xlim(xmin, xmax) +
    labs(x = "", y = "Density",
         title = "Deals") +
    annotate("text", x = xmin, y = dens.med, label = as.character(deals.pos), 
             size = 6, parse = TRUE) +
    theme_bw(base_size = 12)
  deals.dens
  
  
  deals.swing.dens <- ggplot(deals.post.all, aes(x = `Deals:Swing`)) +
    geom_density()
  deals.swing.dens
  dens.data <- ggplot_build(deals.swing.dens)$data[[1]]
  dens.med <- quantile(dens.data$y)[4]
  
  deals.swing.dens <- deals.swing.dens + geom_area(data = subset(dens.data, x > 0),
                                                   aes(x=x, y=y), fill="darkgrey") +
    xlim(xmin, xmax) +
    labs(x = "", y = "Density",
         title = "Deals: Swing") +
    annotate("text", x = xmin, y = dens.med, label = as.character(deals.swing.pos), 
             size = 6, parse = TRUE) +
    theme_bw(base_size = 12)
  deals.swing.dens
  
  grid.arrange(deals.dens, deals.swing.dens)
  deals.inter.plot <- arrangeGrob(deals.dens, deals.swing.dens)
  deals.inter.plot 
  
}

deals.inter.plot <- deals.inter(deals.state, scale.factor)

# with hypothetical data 
pred.out.hyp <- prepare_predictions(deals.state, newdata = hyp.data)

# marginal effect of swing
swing.est <- marginaleffects(deals.state,
                         newdata = hyp.data,
                         variables = "swing",
                         by = "deals",
                         conf_level = .90) %>%
  mutate_at(
    c("estimate", "conf.low", "conf.high"),
    function(x) x * scale.factor 
  )



# plot everything
slope.deals <- ggplot(deals.est, aes(x = factor(swing), y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1, linewidth = 2) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c(`0` = "No", `1` = "Yes")) +
  labs(
    title = "Marginal Impact of Arms Deals",
    x = "Swing State",
    y = "Impact of Increasing Arms Deals"
  )
slope.deals

slope.swing <-  ggplot(swing.est, aes(x = deals, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .5) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Marginal Impact of Swing States",
    x = "Total Arms Deals",
    y = "Impact of Swing Status"
  )
slope.swing

pred.cont <- predictions(deals.state, conf.level = .9, 
                        newdata = hyp.data
                        ) %>%
  mutate_at(
    c("estimate", "conf.low", "conf.high"),
    function(x) x * scale.factor 
  )
# plot it
pred.cont.plot <- ggplot(pred.cont, aes(x = deals, y = estimate,
                      fill = factor(swing))) +
  geom_line(linewidth = 2) +
  scale_fill_grey(labels = c(`0` = "Not Swing", `1` = "Swing")) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .5) +
  labs(x = "Arms Deals",
       fill = "Electoral\nCompetition",
       y = "Predicted Defense Contracts",
       title = "Predicted Contracts")
pred.cont.plot

# combine it all 
grid.arrange(deals.inter.plot, slope.swing,
             pred.cont.plot,
             layout_matrix = rbind(c(1, 2),
                                   c(3, 3)))
plot.state.inter <- arrangeGrob(deals.inter.plot, slope.swing,
                                 pred.cont.plot,
                                 layout_matrix = rbind(c(1, 2),
                                                       c(3, 3)))
ggsave("figures/deals-swing-me.png", plot.state.inter,
       height = 10, width = 12)

# state varying coefs
coefs.joint <- coef(deals.state) 
coefs.joint[["state"]]



# coefficients
coef.joint <- get_estimates(deals.state)


# nice term labels
coef.joint$term <- str_remove(coef.joint$term, "b_")
coef.joint$var <- coef.names.map.state[coef.joint$term]
coef.joint$var <- factor(coef.joint$var, ordered = T,
                         levels = coef.names.map.state)

# plot it 
ggplot(drop_na(coef.joint, var), aes(y = fct_rev(var), x = estimate)) +
  #facet_wrap(~ group, scales = "free") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low,
                      xmax = conf.high),
                  linewidth = 1.5,
                  size = .75) +
    labs(
    x = "Estimate and 95% Credible Interval",
    y = "Variable",
    title = "Arms Deals and Defense Contracting",
    subtitle = "2001-2020"
  ) +
  theme(
    axis.text=element_text(size=11),
    axis.title=element_text(size=13),
    title = element_text(size = 15),
    strip.text = element_text(size = 9)
  )


# state ldv estimates
ldv.state <- avg_slopes(deals.state, 
                        variables = "lag_obligations_rs", 
                        type = "response",
                        by = "state") %>%
                select(
                  estimate, conf.low, conf.high, state,
                ) %>%
                rename(
                  state = state,
                  Estimate = estimate,
                  Q2.5 = conf.low,
                  Q97.5 = conf.high
                )
state.intercepts <- as.data.frame(coefs.joint$state[, , 1])
state.intercepts$state <- gsub("\\..*","", row.names(state.intercepts))
  
glimpse(state.intercepts)


# order for plotting 
state.intercepts <- bind_rows("Intercept" = state.intercepts, 
                              "Lagged DV" = ldv.state,
                              .id = "variable") %>%
  arrange(Estimate, .by_group = TRUE) 
state.intercepts$state <- factor(state.intercepts$state, ordered = TRUE,
                                levels = state.intercepts$state[1:50])

ggplot(state.intercepts, aes(y = state, x = Estimate)) +
  facet_wrap(~ variable, scales = "free_x") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5)) +
  labs(
    y = "State",
    x = "Estimate (Untransformed)",
    title = "State Parameters"
  )
ggsave("appendix/state-pars.png", height = 6, width = 8)



### rough calculation of how arms deals feed defense contracts ### 

# aggregate total deals by year, all else equal
# different kinds of alliances- from prediction 

pred.data.hyp <- pois.deals.est[[2]]
glimpse(pred.data.hyp)

pred.deals.elec <- pred.data.hyp %>%
                    group_by(time_to_elec) %>% 
                    summarize(
                      total_deals = sum(estimate),
                      total_low = sum(conf.low),
                      total_high = sum(conf.high)
                    )
# implies one more deal across four hypothetical dyads- 
# driven by allies with median polyarchy or less 
pred.deals.elec

# scale it for all states- 2001 on 
us.deals.comp.2000 <- us.deals.comp %>% 
                      filter(year >= 2001) %>%
                      ungroup() %>%
                        mutate(
                          cycle = case_when(year <= 2000 ~ 2000,
                                                     year <= 2004 ~ 2004,
                                                     year <= 2008 ~ 2008,
                                                     year <= 2012 ~ 2012,
                                                     year <= 2016 ~ 2016,
                                                     year > 2016 ~ 2020))
glimpse(us.deals.comp.2000)

# predictions from observed data
pred.deals.all <- predictions(pois.deals,
                              newdata = us.deals.comp.2000) %>%
  group_by(time_to_elec, cycle) %>% 
  summarize(
    year = mean(year), 
    deals = sum(estimate),
    deals_low = sum(conf.low),
    deals_high = sum(conf.high),
    .groups = "keep"
  )
pred.deals.all 

ggplot(pred.deals.all, aes(x = time_to_elec, y = deals,
                           color = factor(cycle))) +
  geom_line(linewidth = 1) +
  scale_x_reverse()

# get data for states 
swing.data.pdeals <- state.data.deals %>%
                      filter(year >= 2005) %>%
                      select(-deals) %>%
                      left_join(pred.deals.all)

# calculate predictions for observed swing states 
pred.state <- predictions(deals.state,
                              newdata = swing.data.pdeals) %>%
                           mutate_at(
                                c("estimate", "conf.low", "conf.high"),
                                function(x) x * scale.factor 
                              )



ggplot(pred.state, aes(x = time_to_elec, y = estimate,
                          group = state,
                          color = state)) +
  facet_wrap(~ cycle, scales = "free_y") +
  geom_line() +
  scale_x_reverse()

ggplot(pred.state.sw, aes(x = time_to_elec, y = estimate,
                          color = factor(cycle))) +
  facet_wrap(~ state, scales = "free_y") +
  geom_pointrange(aes(ymin = conf.low,
                      ymax = conf.high)) +
  scale_x_reverse()

# look at specific states
example.pred <- filter(pred.state,
                          state == "Florida" |
                          state == "North Carolina" |
                         state == "Pennsylvania")
ggplot(example.pred, aes(x = year, y = estimate,
                          color = factor(cycle),
                          shape = factor(swing))) +
   facet_wrap(~ state, scales = "free_y") +
  geom_point() +
  geom_line()
  





### Robustness Checks ### 


# robustness check- hurdle model

# use a lognormal hurdle instead 
deals.state.hurdle <- brm(bf(obligations ~ 
                               (lag_obligations || state) +
                               deals*swing + core +
                               gwot + time_to_elec + 
                               rep_pres  +
                               poptotal + ln_ngdp,
                             hu ~ ln_ngdp,
                             center = FALSE),
                          family = hurdle_lognormal(),
                          prior = c(
                            set_prior("normal(0, 2)", class = "b")
                          ),
                          data = state.data.deals,
                          cores = 4,
                          backend = "cmdstanr",
                          control = list(
                            adapt_delta = .99,
                            max_treedepth = 20)
) 
summary(deals.state.hurdle)
pp_check(deals.state.hurdle)


# use robust regression 
deals.state.reg <- brm(bf(change_obligations ~ 
                               (1 | state) +
                               deals*swing + core +
                               gwot + time_to_elec + 
                               rep_pres  +
                               poptotal + ln_ngdp,
                             center = FALSE),
                          family = student(),
                          prior = c(
                            set_prior("normal(0, 2)", class = "b")
                          ),
                          data = state.data.deals,
                          cores = 4,
                          backend = "cmdstanr",
                          control = list(
                            adapt_delta = .99,
                            max_treedepth = 20)
) 
summary(deals.state.reg)
pp_check(deals.state.reg)


deals.inter.probs <- function(model, scale.factor, model.name){
  
  # all draws 
  deals.draws <- prepare_predictions(model)
  deals.inter <- as.data.frame(deals.draws$dpars$mu$fe$b)
  
  # use separate plots 
  deals.post <- as.data.frame(deals.inter[, 2] * scale.factor)
  colnames(deals.post) <- c("Deals")
  deals.pos <- round(sum(deals.post > 0) / 4000, digits = 2)
  
  deals.swing.post <- as.data.frame(deals.inter[, 10] * scale.factor)
  colnames(deals.swing.post) <- c("Deals:Swing")
  deals.swing.pos <- round(sum(deals.swing.post > 0) / 4000, digits = 2)
  
  deals.post.all <- bind_cols(deals.post, deals.swing.post)
  
  xmin <- min(deals.post.all)
  xmax <- max(deals.post.all)
  
  deals.dens <- ggplot(deals.post.all, aes(x = Deals)) +
    geom_density()
  deals.dens
  dens.data <- ggplot_build(deals.dens)$data[[1]]
  dens.med <- quantile(dens.data$y)[4]
  
  deals.dens <- deals.dens + geom_area(data = subset(dens.data, x > 0),
                                       aes(x=x, y=y), fill="darkgrey") +
    xlim(xmin, xmax) +
    labs(x = "", y = "Density",
         title = "Deals") +
    annotate("text", x = xmin, y = dens.med, label = as.character(deals.pos), 
             size = 6, parse = TRUE) +
    theme_bw(base_size = 12)
  deals.dens
  
  
  deals.swing.dens <- ggplot(deals.post.all, aes(x = `Deals:Swing`)) +
    geom_density()
  deals.swing.dens
  dens.data <- ggplot_build(deals.swing.dens)$data[[1]]
  dens.med <- quantile(dens.data$y)[4]
  
  deals.swing.dens <- deals.swing.dens + geom_area(data = subset(dens.data, x > 0),
                                                   aes(x=x, y=y), fill="darkgrey") +
    xlim(xmin, xmax) +
    labs(x = "", y = "Density",
         title = "Deals: Swing") +
    annotate("text", x = xmin, y = dens.med, label = as.character(deals.swing.pos), 
             size = 6, parse = TRUE) +
    theme_bw(base_size = 12)
  deals.swing.dens
  
  grid.arrange(deals.dens, deals.swing.dens,
               top = grid::textGrob(model.name,
            gp = grid::gpar(col = "black", fontsize = 20)))
  
  deals.inter.plot <- arrangeGrob(deals.dens, deals.swing.dens,
                                top = grid::textGrob(model.name,
                      gp = grid::gpar(col = "black", fontsize = 20)))
  
  deals.inter.plot 

}

models.check.deals <- list(deals.state.hurdle, deals.state.reg)
models.check <- c("Log-Normal Hurdle", "Student-T: Contract Changes")

res.deals.check <- mapply(deals.inter.probs, 
                          model = models.check.deals, 
                          model.name = models.check,
                          scale.factor = scale.factor,
                          SIMPLIFY = FALSE, USE.NAMES = TRUE)
res.deals.check[[1]]


me.deals.check <- grid.arrange(res.deals.check[[1]], res.deals.check[[2]])
me.deals.check
me.deals.check <- arrangeGrob(res.deals.check[[1]], res.deals.check[[2]])
ggsave("appendix/me-deals-check.png", me.deals.check, 
       height = 6, width = 8)


# posterior prob difference
# draws:
state.reg.draws <- prepare_predictions(deals.state.reg)
reg.inter <- as.data.frame(state.reg.draws$dpars$mu$fe$b)
hypothesis(reg.inter, c("b_deals:swing > b_deals"))
hypothesis(reg.inter, c("b_deals:swing > 0"))
hypothesis(reg.inter, c("b_deals > 0"))




# tabulate results for appendix 
coef.names.cont.brm = c("b_deals" = "Arms Deals",
                        "b_swing" = "Swing State",
                        "b_deals_aircraft" = "Aircraft Deals",
                        "b_deals_arms" = "Arms Deals",
                        "b_deals_electronics" = "Electronics Deals",
                        "b_deals_missile_space" = "Missile \\& Space Deals",
                        "b_deals_ships" = "Ships Deals",
                        "b_deals_vehicles" = "Vehicles Deals",
                        "swing" = "Swing State",
                        "b_core" = "Core State",
                        "b_time_to_elec" = "Time to Election",
                         "b_poptotal" = "Population (Rescaled)",
                         "b_ln_ngdp" = "Log GDP",
                        "b_hu_ln_ngdp" = "Hurdle: Log GDP",
                         "b_gwot" = "Global War on Terror",
                         "b_rep_pres" = "Republican President",
                         "phi" = "$\\phi$",
                         "sigma" = "$\\sigma$",
                         "b_Intercept" = "Intercept",
                         "b_hu_Intercept" = "Hurdle: Intercept")

deals.state.models <- list(deals.state, deals.state.hurdle, deals.state.reg)
names(deals.state.models) <- c("Rescaled Ordered Beta", "Log-Normal Hurdle", "Student-T: Contract Changes")
deals.state.tab <- modelsummary(deals.state.models,
             output = "latex", 
             gof_map = "none",
             conf.level = .9,
             coef_omit = c("sd"),
             longtable = TRUE,
             escape = FALSE,
             fmt = fmt_significant(2),
             coef_rename = coef.names.cont.brm,
             statistic = "({conf.low}, {conf.high})",
             title = "\\label{tab:cont-regs}: Coefficient estimates from models of defense contract awards.") %>%
  kable_styling(font_size = 8, full_width = FALSE,
                latex_options = c("HOLD_position", "scale_down")) %>%
  footnote(general = "90% Credible Intervals in parentheses.")
deals.state.tab
save_kable(deals.state.tab, "appendix/cont-reg-tabs.tex")



# additional check- does the association between deals and contracts in swing states rise with electoral proximity?
# ordbeta reg for transformed outcomes
formula.state.prox <- bf(obligations_rs ~
                      (lag_obligations_rs || state) +
                      deals*swing*time_to_elec + core +
                      gwot +
                      rep_pres  +
                      poptotal + ln_ngdp,
                    center = FALSE)
deals.state.prox <- ordbetareg(formula.state.prox,
                          true_bounds = c(0, 1),
                          data = state.data.deals,
                          cores = 4,
                          backend = "cmdstanr",
                          refresh = 200
) 
summary(deals.state.prox)
fixef(deals.state.prox)

# results: me of deals 
deals.me <- slopes(deals.state.prox, variables = c("deals"),
                   conf_level = .9,
                   newdata = 
                     datagrid(model = deals.state.prox,
                              time_to_elec = c(0, 1, 2, 3),
                              swing = c(0, 1)))
deals.me <- mutate(deals.me,
                          estimate = estimate * scale.factor,
                          conf.low = conf.low * scale.factor,
                          conf.high = conf.high * scale.factor)


ggplot(deals.me, aes(y = estimate, 
                      x = time_to_elec,
                      group = factor(swing),
                      color = factor(swing))) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1)) +
  scale_color_grey("Swing\nState", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Marginal Impact of Arms Deals by Swing State and Election Proximity",
       y = "Estimated Marginal Effect",
       x = "Years to Presidential Election")
ggsave("appendix/deals-me-prox.png", height = 6, width = 8)

