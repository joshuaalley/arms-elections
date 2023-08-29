# Joshua Alley
# Check interactions with interflex
# for appendix 


# headline interaction: deals and autocracies
us.deals.comp.df <- us.deals.comp
class(us.deals.comp.df) <- "data.frame"

bin.deals <- interflex(estimator = "binning",
                       data = us.deals.comp.df,
                       method = "poisson",
                       Y = "deals",
                       D = "time_to_elec",
                       X = "v2x_polyarchy",
                       cores = 4,
                       base = 3,
                       ylab = "Marginal Effect of Election Proximity",
                       xlab = "Polyarchy",
                       main = "Election Proximity and Arms Deals")
bin.deals
ggsave("appendix/prox-deals-bin.png", height = 6, width = 8)

kernel.deals <- interflex(estimator = "kernel",
          data = us.deals.comp.df,
          method = "poisson",
          Y = "deals",
          X = "time_to_elec",
          D = "v2x_polyarchy",
          base = 3,
          cores = 4,
          ylab = "Marginal Effect of Election Proximity",
          xlab = "Polyarchy",
          main = "Kernel Estimate of Election Proximity and Arms Deals")
kernel.deals



# same process for deals and swing interaction 
state.data.deals.df <- state.data.deals
class(state.data.deals.df) <- "data.frame"

bin.contracts <- interflex(estimator = "binning",
                       data = state.data.deals.df,
                       Y = "obligations",
                       D = "swing",
                       X = "deals",
                       cores = 4,
                       na.rm = TRUE,
                       base = 0,
                       ylab = "Marginal Effect of Swing State",
                       xlab = "Total Arms Deals",
                       main = "Swing State and Contracts")
bin.contracts
ggsave("appendix/swing-cont-bin.png", height = 6, width = 8)

kernel.contracts <- interflex(estimator = "kernel",
                          data = state.data.deals.df,
                          Y = "obligations_rs",
                          D = "swing",
                          X = "deals",
                          cores = 4,
                          na.rm = TRUE,
                          base = 0,
                          ylab = "Marginal Effect of Swing State",
                          xlab = "Total Arms Deals",
                          main = "Kernel Estimate of Swing State and Contracts")
kernel.contracts

gam.contracts <- interflex(estimator = "gam",
                              data = state.data.deals.df,
                              Y = "obligations_rs",
                              X = "swing",
                              D = "deals",
                              cores = 4,
                              na.rm = TRUE)
gam.contracts
