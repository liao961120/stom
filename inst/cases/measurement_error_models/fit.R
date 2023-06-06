library(stom)
source("simulation.R")
set.seed(1977)
d = sim_data( alpha=0,
              delta=-1.8,
              sigma_ET = .5,
              sigma_subj = .5 )
m = stan( "m1.stan", data=d$dat,
          chains=3, parallel_chains=3,
          # seed = 2038786619,
          save_warmup = TRUE,
          init = NULL,        # Initial values for parameters
          adapt_delta = NULL, # default: .8 (larger results to smaller step sizes)
          step_size = NULL    # initial step size (default:1)
)
save_model(m)
