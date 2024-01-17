"Fit simple model.

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
        "src/one_subj.stan",
        "data/one_subj.RDS",
        "src/utils.R",
        "--output=data/one_subj.model.RDS")
    )
}
print(a)

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
               init = init_files_one_subj(seed=40)
)
stom::save_model(fit, a$output)



##### MCMC diagnostics #####
params = as_vec("alpha,dr1,dr2,tau,beta")
post = precis(fit, depth=5, pars=params)
post

# Trace plot
library(bayesplot)
color_scheme_set("viridis")
mcmc_trace(fit$draws(), pars = params,
           facet_args = list(ncol=2))
############################





