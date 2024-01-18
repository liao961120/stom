"Simulate multiple subjects.

Usage:
    simulate STAN (-o OUTPUT| --output=OUTPUT)

Options:
    -h --help   Show this screen.
    -o --output Specify output file path.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC)
} else {
    a = docopt::docopt(DOC, args=c("src/multi_subj.prior.stan", 
                                   "-o", "data/multi_subj.RDS"))
}
print(a)

library(stom)
library(RWiener)

set.seed(2024)
# Sim Params
Ns = 10                      # Number of subjects
N1 = 200
N2 = 200

d = list(
    Ns = Ns,               
    N1 = N1,
    N2 = N2,
    # Hyper-params
    mu_alpha = rnorm(1),
    mu_beta  = rnorm(1),
    mu_tau   = rnorm(1),
    mu_dr1   = rnorm(1),
    mu_dr2   = rnorm(1),
    
    s_alpha  = rtnorm(1),
    s_beta   = rtnorm(1),
    s_tau    = rtnorm(1),
    s_dr1    = rtnorm(1),
    s_dr2    = rtnorm(1),
    
    z_alpha  = rnorm(Ns),
    z_beta   = rnorm(Ns),
    z_tau    = rnorm(Ns),
    z_dr1    = rnorm(Ns),
    z_dr2    = rnorm(Ns)
)

m = cmdstanr::cmdstan_model("src/multi_subj.prior.stan", dir = ".")
sim = m$sample(data=d, seed=123, fixed_param=T, chains=1, iter_sampling=1)

sim_vars = as_vec("RT,cond,resp,Sid,alpha,beta,tau,delta")
s = lapply(sim_vars, \(p) sim$draws(variables=p) |> as.vector())
names(s) = sim_vars

