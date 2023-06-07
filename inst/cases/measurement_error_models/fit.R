library(stom)
source("simulation.R")
set.seed(1977)
d = sim_data()

# mz = mean(d$dat$z)
# sz = sd(d$dat$z)
# mx = mean(d$dat$x_obs)
# sx = sd(d$dat$x_obs)
# my = mean(d$dat$y)
# sy = sd(d$dat$y)
# for ( x in c("x_obs", "y", "z") )
#     d$dat[[x]] = stom::standardize(d$dat[[x]])
m = stan( "m2.stan", data=d$dat,
          chains=3, parallel_chains=3,
          # seed = 2038786619,
          save_warmup = TRUE,
          iter_warmup  = 1000,
          iter_sampling = 1000,
          init = NULL,        # Initial values for parameters
          #adapt_delta = .99  , # default: .8 (larger results to smaller step sizes)
          step_size = NULL    # initial step size (default:1)
)
# save_model(m)
x = m$metadata()
m$diagnostic_summary()
x$step_size_adaptation
# m = readRDS("m1.RDS")



###### Check which params caused Low E-BFMI ########
# See <https://mc-stan.org/misc/warnings.html#bfmi-low>
dg = m$sampler_diagnostics(format = "df")
s2 = stom::extract(m)

x = sapply(colnames(s2), function(c_) {
    cor( s2[[c_]], dg$energy__ )
})
hist(x)
x[which( abs(x) > .7 )]

############ Trace ############
library(bayesplot)
color_scheme_set("viridis")
pars = stom:::parse_pars("mu_z, sigma_z, sigma_y, sigma_x, tau, b_yz, b_yx, b_xz, a_yx, a_xz")
mcmc_trace(m$draws(pars), facet_args=list(ncol=2))

p = stom::extract(m, pars)
p = p[, pars]
# pairs(p)


plot(m$summary(pars)$mean, d$params[pars]);abline(0,1)
plot(m$summary("x_true")$mean, d$params$x_true); abline(0,1)


