"Simulate multiple subjects.

Usage:
    simulate (-o OUTPUT| --output=OUTPUT)

Options:
    -h --help   Show this screen.
    -o --output Specify output file path.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC)
} else {
    a = docopt::docopt(DOC, args=c("-o", "data/multi_subj.RDS"))
}
print(a)

library(stom)
library(RWiener)

set.seed(2024)
# Params
Ns = 10                      # Number of subjects
alpha = runif(Ns, 1.6, 1.9)  # Boundary separation        (0, Inf)
beta  = runif(Ns, .4, .6)    # Bias (0.5: non-biased)     (0, 1)
dr1 = runif(Ns, .9, 1.2)     # drift rate for stimulus 1  [0, Inf)
dr2 = runif(Ns, -1.2, -.9)   # drift rate for stimulus 2  (-Inf, 0]
tau = runif(Ns, 0.15, .25)   # non-decision time (sec.)   [0, Inf)

N1 = 200     # num. of trials presenting stimulus 1
N2 = 200     # num. of trials presenting stimulus 2

d_all = NULL
for (s in 1:Ns) {
    d1 = cbind( rwiener(N1, alpha[s], tau[s], beta[s], dr1[s]), cond = 1 )
    d2 = cbind( rwiener(N2, alpha[s], tau[s], beta[s], dr2[s]), cond = 2 )
    d = rbind( d1, d2 )
    d = cbind( Sid=s, d )
    d$resp = ifelse(d$resp == "upper", 1, 2)
    d$hit = as.integer(d$resp == d$cond)
    colnames(d)[2] = "RT"
    
    if (s == 1) {
        d_all = d
        next
    }
    d_all = rbind(d_all, d)
}

# str(d)
# table(d1$resp) / sum(table(d1$resp))
# table(d2$resp) / sum(table(d2$resp))

saveRDS( c(as.list(d_all), list(
    alpha = alpha,
    beta = beta,
    dr1 = dr1,
    dr2 = dr2,
    delta = c(dr1, dr2),
    tau = tau,
    N1 = N1,
    N2 = N2,
    Ns = Ns,
    N = nrow(d_all)
)), a$output )
