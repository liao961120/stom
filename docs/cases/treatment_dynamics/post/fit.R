source("simulation.R")
set.seed(1977)
d = sim_data()

# Compile stan file
m = cmdstanr::cmdstan_model("m1.stan")

# Fit model
fit = m$sample(
    data=d$dat,
    chains=3, 
    parallel_chains=3,
    refresh = 250
)

# Save fitted model for later use
fit$save_object("m1.RDS")
