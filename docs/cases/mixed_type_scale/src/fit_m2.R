"Fit hierarchical model.

Usage:
    fit STAN SIM UTILS (-o OUTPUT| --output=OUTPUT)

Options:
    -h --help   Show this screen.
    -o --output Specify output file path.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC)
} else {
    a = docopt::docopt(DOC, args=c(
        "src/m2.stan",
        "cache/sim.RDS",
        "src/utils.R",
        "--output=cache/m2.RDS")
    )
}
print(a)


###### Main ########
source(a$UTILS)
sim = readRDS(a$SIM)

###### Fit MCMC #########
m = cmdstanr::cmdstan_model(a$STAN, dir=".")
fit = m$sample(data=sim,
               chains=N_CORES, parallel_chains=N_CORES,
               iter_sampling=150, iter_warmup=300,
               refresh = 50,
               seed = 1234,
               save_warmup = TRUE,
               init = write_init_files_partialpool(sim, seed=40, dir = "init_m2")
)
stom::save_model(fit, a$output)
########################


# Check parameter recovery
library(stom)
library(dplyr)
library(bayesplot)
params = as_vec("A,S,E,kappa,muA,muS,muE,sigmaA,sigmaS,sigmaE")

# Posterior summary data frame
post = precis(fit, depth=5, pars=params)

##### MCMC diagnostics #####
plot(post$rhat)
post[post$rhat > 1.015,]
# Trace plot
param_inspec = c( post[post$rhat > 1.015,]$variable,
                  as_vec("muA,muS,muE,sigmaA,sigmaS,sigmaE") )
color_scheme_set("viridis")
mcmc_trace(fit$draws(), pars = param_inspec,
           facet_args = list(ncol=4))
############################

##### Visualize recovery #####
params = as_vec("A,S,E,kappa")
recv_m = get_post_summ(post, params, "mean")
recv_q5 = get_post_summ(post, params, "q5")
recv_q95 = get_post_summ(post, params, "q95")

# Plot recovery
n_param0 = c(sim$N, sim$N, sim$Ni, sim$Np)
n_param = sum(n_param0)
plot(1, type="n", xlim = c(0,n_param)+.5, ylim=c(-8,8))
abline(v = cumsum(n_param0)+.5, lty="dashed", col="grey")
for (i in 1:n_param)
    lines( c(i,i), c(unlist(recv_q5)[i], unlist(recv_q95)[i]), col=col.alpha(2), lwd=3 )
points( with(recv_m, c(A,S,E,kappa)), col=2 )
points( with(sim, c(A,S,E,kappa)), pch=19, col=2 )
##############################
