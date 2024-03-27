# Joshua Alley
# joint model with arms deals and contracts


# generate a list of swing states
swing.list <- select(state.data, state, swing, year) %>%
               filter(swing == 1) %>%
               group_by(state) %>%
               summarize(
                 Start = min(year),
                 End = max(year)
               ) %>%
               rename(State = state)
swing.tab <- datasummary_df(swing.list, fmt = 0,
               output = "latex",
             caption = "\\label{tab:swing-list}: List of swing states.") %>%
  kable_styling(font_size = 10,
                latex_options = "hold_position") 
swing.tab
save_kable(swing.tab, "appendix/swing-list.tex")



### state data with arms deals
state.data.deals <- left_join(state.data, 
                              select(arms.deals.year,
                                     year, deals)) %>%
  group_by(state) %>% 
  filter(state != "District of Columbia")  %>% 
  mutate(
    lag_deals = lag(deals),
    change_deals = deals - lag_deals,
    deals_rs = arm::rescale(deals),
    lag_deals_rs = arm::rescale(lag_deals)
  ) %>%
  group_by(year) %>%
  mutate(   
    sum_ob = sum(obligations, na.rm = TRUE),
    obligations_rs = obligations / sum_ob
  ) %>%
  group_by(state) %>%
  mutate(
    lag_obligations_rs = lag(obligations_rs)
             )

summary(state.data.deals$obligations_rs)

# look at state-level contracts
ggplot(state.data.deals, aes(x = year, y = obligations,
                             group = state)) +
  geom_line()

ggplot(state.data.deals, aes(x = year, y = obligations)) +
  facet_wrap(~ state, nrow = 10, scales = "free_y") +
  geom_line() +
  labs(
    x = "Year",
    y = "Defense Contracts",
    title = "State Defense Contracts: 2001-2020"
  )
ggsave("appendix/state-dynamics.png", height = 10, width = 12)


# outcome plot
ggplot(state.data.deals, aes(x = ln_obligations)) + geom_histogram()
ggplot(state.data.deals, aes(x = obligations)) + geom_histogram()
ggplot(state.data.deals, aes(x = obligations_rs)) + geom_histogram()
ggplot(state.data.deals, aes(x = change_obligations)) + geom_histogram()
ggplot(state.data.deals, aes(x = change_ln_obligations)) + geom_histogram()


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

sum((deals.inter[, 10] * scale.factor) > 
      (deals.inter[, 2] * scale.factor)) / nrow(deals.inter)

# function to plot positive posterior mass
deals.inter.func <- function(model, scale.factor){
  
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

deals.inter.plot <- deals.inter.func(deals.state, scale.factor)

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
                                   c(3, 3)),
             top = grid::textGrob("Arms Deals, Swing States, and Defense Contracts",
                        gp = grid::gpar(fontsize = 20)))
plot.state.inter <- arrangeGrob(deals.inter.plot, slope.swing,
                                 pred.cont.plot,
                                 layout_matrix = rbind(c(1, 2),
                                                       c(3, 3)),
                                top = grid::textGrob("Arms Deals, Swing States, and Defense Contracts",
                                                     gp = grid::gpar(fontsize = 20)))
ggsave("appendix/deals-swing-me.png", plot.state.inter,
       height = 6, width = 8)

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



### Predictions by state- take 2007 to 2008

# state data from 2007/2008
state.data.08 <- state.data.deals %>%
                  filter(year == 2007 | year == 2008)
scale.08 <- state.data.deals %>% 
             filter(year == 2008) %>%
             ungroup() %>%
             distinct(sum_ob)

scale.07 <- state.data.deals %>% 
  filter(year == 2007) %>%
  ungroup() %>%
  distinct(sum_ob)


# predictions of contracts
pred.state.08 <- predictions(deals.state, newdata = state.data.08)

# posterior draws for difference 
# gives list.cols, so 


list.diff <- function(l08, l07){
  l08 = l08 * as.numeric(scale.08)
  l07 = l07 * as.numeric(scale.07)
            return(mapply(`-`, l08, l07))
   }
pred.08.draws <- posteriordraws(pred.state.08) %>%
                   select(state, year, draw, swing, core) %>%
                   pivot_wider(id_cols = c("state", "swing", "core"),
                              names_from = c("year"),
                               values_from = c("draw")
                               ) %>%
                    rowwise() %>%
                    mutate(
                      diff = list(list.diff(l08 = `2008`, 
                                            l07 = `2007`)),
                      diff.median = median(diff),
                      diff.upper = quantile(diff, probs = .95),
                      diff.lower = quantile(diff, probs = .05),
                    )

pred.08.key <- pred.08.draws %>%
                select(state, swing, core, diff.median,
                       diff.upper, diff.lower) %>%
                mutate(
                  comp = factor(case_when(
                    swing == 1 ~ "Swing",
                    core == 1 ~ "Core: Republican",
                    .default = "Neither: Democrat"
                  ),
                  ordered = TRUE,
                  levels = c("Swing", "Core: Republican", "Neither: Democrat")),
                  linesize = case_when(
                    swing == 1 ~ .9,
                    core == 1 ~ .8,
                    .default = .7
                  ),
                )



ggplot(pred.08.key, aes(y = reorder(state, diff.median), x = diff.median)) +
  facet_wrap(~ comp, scales = "free_y",
             ncol = 1) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = diff.lower, xmax = diff.upper)) +
  labs(
    x = "Difference in Contracts",
    y = "State",
    title = "Defense Contracting by State",
    subtitle = "2007-2008"
  )
#ggsave("appendix/est-08-cycle-facet.png", height = 8, width = 8)

# map 
# Map of respondents
states <- map_data("state") %>%
  rename(state = region) %>%
  mutate(state = str_to_title(state))
states.08.data <- left_join(states, pred.08.key) %>%
                   filter(state != "District Of Columbia")


ggplot(data = states.08.data, aes(x = long, y=lat,
                                  fill = diff.median,
                                  group = group)) +
  facet_wrap(~ comp, ncol = 2) +
  geom_polygon(aes(group = group,
                  # color = comp
                   ),
               color = "grey",
               linewidth = 1
                ) +
  scale_fill_distiller(type = "seq",
                       direction = 1,
                       palette = "Greys") +
 # scale_color_brewer(palette = "Dark2") +
  labs(title = "Geogrpahy of Defense Contracting Changes",
       subtitle = "2007-2008: 32 Additional Arms Deals",
       #color = "Electoral\nCompetition",
       fill = "Posterior\nMedian\nDifference") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = c(0.75, 0.3),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank())
#ggsave("appendix/est-08-cycle.png", height = 6, width = 8)

# deals rise by 32
table(state.data.08$year, state.data.08$deals)

pred.state.08 <- left_join(pred.state.08, states)

ggplot(pred.state.08, aes(y = state, x = estimate,
                          color = factor(year))) +
  facet_wrap(~ swing, scales = "free") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = .5))

# map the years not helpful
ggplot(data = pred.state.08, aes(x = long, y=lat)) + 
  facet_wrap(~ year) +
  geom_polygon(aes(group = group,
                   fill = estimate),
               color="grey", linewidth = 0.4) +
  scale_fill_distiller(type = "seq",
                       direction = 1,
                       palette = "Greys") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Posterior\nMedian\nPrediction") +
  theme_minimal()


# combine uncertainty and map
interval.08.plot <- ggplot(pred.08.key, aes(y = reorder(state, diff.median), 
                                            x = diff.median)) +
  facet_wrap(~ comp, scales = "free_y",
             ncol = 3) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = diff.lower, xmax = diff.upper)) +
  labs(
    x = "Difference in Contracts",
  ) +
  theme(
    axis.title.y = element_blank(),
  )


base.map <- ggplot() +
  geom_polygon(data = states, aes(x = long, y=lat,
                                         group = group),
  color = "black",
  linewidth = .5,
  fill = NA
  ) 

map.08.plot <- base.map + 
  facet_wrap(~ comp, ncol = 3) +
  geom_polygon(data = states.08.data, aes(x = long, y=lat,
                                          fill = diff.median,
                                          group = group),
  color = "black",
  linewidth = .5
  ) +
  scale_fill_gradient(low = "grey", high = "black") +
  labs(title = "Geogrpahy of Defense Contracting Changes",
       subtitle = "2007-2008: 32 Additional Arms Deals",
       #color = "Electoral\nCompetition"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
map.08.plot

grid.arrange(map.08.plot, interval.08.plot)
map.08.full <- arrangeGrob(map.08.plot, interval.08.plot)
ggsave("figures/map-08-full.png", map.08.full,
       height = 6, width = 10)


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
# create dummies for years to election
state.data.deals <- state.data.deals %>%
  mutate(
    time_to_elec_0 = ifelse(time_to_elec == 0, 1, 0),
    time_to_elec_1 = ifelse(time_to_elec == 1, 1, 0),
    time_to_elec_2 = ifelse(time_to_elec == 2, 1, 0)
  )


formula.state.prox <- bf(obligations_rs ~
                      (lag_obligations_rs || state) +
                      deals*swing*time_to_elec_0 +
                        deals*swing*time_to_elec_1 +
                        deals*swing*time_to_elec_2 +
                        core +
                      gwot +
                      rep_pres  +
                      poptotal + ln_ngdp,
                    center = FALSE)
deals.state.prox <- ordbetareg(formula.state.prox,
                          true_bounds = c(0, 1),
                          data = state.data.deals,
                          cores = 4,
                          backend = "cmdstanr",
                          refresh = 500
) 
summary(deals.state.prox)
fixef(deals.state.prox)

# results: me of deals 
deals.me <- slopes(deals.state.prox, variables = c("deals"),
                   conf_level = .9,
                   newdata = 
                     datagrid(model = deals.state.prox,
                              time_to_elec_0 = c(0, 1),
                              time_to_elec_1 = c(0, 1),
                              time_to_elec_2 = c(0, 1),
                              swing = c(0, 1)))
deals.me <- mutate(deals.me,
                          estimate = estimate * scale.factor,
                          conf.low = conf.low * scale.factor,
                          conf.high = conf.high * scale.factor) %>%
  rowwise() %>%
  mutate(
    dum_sum = sum(time_to_elec_0, time_to_elec_1, time_to_elec_2)
  ) %>%
  filter(dum_sum <= 1) %>%
  mutate(
    time_to_elec = case_when(
      time_to_elec_0 == 1 ~ 0,
      time_to_elec_1 == 1 ~ 1,
      time_to_elec_2 == 1 ~ 2,
      (time_to_elec_0 == 0 &
         time_to_elec_1 == 0 &
         time_to_elec_2 == 0) ~ 3
    )
  )


ggplot(deals.me, aes(y = estimate, 
                      x = time_to_elec,
                      group = factor(swing),
                      color = factor(swing))) +
  scale_x_reverse() + # decreasing time to election
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  linewidth = 2, size = 1) +
  scale_color_grey("Swing\nState", 
                   start = 0.7,
                   end = 0.1,
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Marginal Impact of Arms Deals by Swing State and Election Proximity",
       y = "Estimated Marginal Effect",
       x = "Years to Presidential Election")
ggsave("appendix/deals-me-prox.png", height = 6, width = 8)



# remove wealth and population controls- why not?

# ordbeta reg for transformed outcomes
formula.state.sparse <- bf(obligations_rs ~
                      (lag_obligations_rs || state) +
                      deals*swing + core +
                      gwot + time_to_elec + 
                      rep_pres,
                    center = FALSE)
deals.state.pt <- ordbetareg(formula.state.sparse,
                          true_bounds = c(0, 1),
                          data = state.data.deals,
                          cores = 4,
                          backend = "cmdstanr",
                          refresh = 200
) 
summary(deals.state.pt)


# results
deals.me.pt <- slopes(deals.state.pt, variables = c("deals"),
       conf_level = .9,
       newdata = 
         datagrid(model = deals.state.pt,
                  swing = c(0, 1))) %>%
       mutate(
         estimate = estimate*scale.factor,
         conf.low = conf.low*scale.factor,
         conf.high = conf.high*scale.factor,
       )

ggplot(deals.me.pt, aes(y = estimate, 
                     x = factor(swing))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = .1),
                  linewidth = 2, size = 1) +
  scale_x_discrete(name = "Swing State", 
                   labels = c(`0` = "No", `1` = "Yes")) +
  labs(title = "Marginal Impact of Arms Deals by Swing State",
       subtitle = "Removing Population and Wealth Controls",
       y = "Estimated Marginal Effect")
ggsave("appendix/me-deals-pt-check.png", height = 6, width = 8)
