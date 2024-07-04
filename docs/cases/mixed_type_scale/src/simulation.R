"Simulate data for model fitting.

Usage:
    simulate STAN (-o OUTPUT| --output=OUTPUT)

Options:
    -h --help   Show this screen.
    -o --output Specify output file path.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC)
} else {
    a = docopt::docopt(DOC, args=c("src/sim.stan", "--output=cache/sim.RDS"))
}
print(a)


#### Main #####
library(stom)
m = cmdstanr::cmdstan_model(a$STAN, dir = ".")

d = list(
    N  = 100,  # num of test takers
    Ns = 10,   # num of subjective items
    No = 10,   # num of objective items
    Np = 5     # num of rating-scale points
)
d$Ni = d$Ns + d$No  # total number of items

# Simulate with stan (sim.stan)
sim = m$sample(data=d, seed=123, fixed_param=T, chains=1, iter_sampling=1)
sim_vars = as_vec("A,S,E,kappa,I,R")
s = lapply(sim_vars, \(p) sim$draws(variables=p) |> as.vector())
names(s) = sim_vars
s$R = matrix(s$R, nrow=d$N, ncol=d$Ni)
s = c(s, d)
saveRDS(s, a$output)
# str(s)
