"Simulate single subject.

Usage:
    simulate (-o OUTPUT| --output=OUTPUT)

Options:
    -h --help   Show this screen.
    -o --output Specify output file path.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC)
} else {
    a = docopt::docopt(DOC, args=c("-o", "data/one_subj.RDS"))
}
print(a)


library(RWiener)

# Params
alpha = 1.8  # Boundary separation        (0, Inf)
beta = 0.5   # Bias (0.5: non-biased)     (0, 1)
dr1 = 1      # drift rate for stimulus 1  [0, Inf)
dr2 = -1     # drift rate for stimulus 2  (-Inf, 0]
tau = 0.2    # non-decision time (sec.)   [0, Inf)

N1 = 200     # num. of trials presenting stimulus 1
N2 = 200     # num. of trials presenting stimulus 2


set.seed(2024)
d1 = cbind( rwiener(N1, alpha, tau, beta, dr1), cond = 1 )
d2 = cbind( rwiener(N2, alpha, tau, beta, dr2), cond = 2 )
d = rbind( d1, d2 )
d = cbind( Sid=1, d )
d$resp = ifelse(d$resp == "upper", 1, 2)
d$hit = as.integer(d$resp == d$cond)
colnames(d)[2] = "RT"
# str(d)
# table(d1$resp) / sum(table(d1$resp))
# table(d2$resp) / sum(table(d2$resp))

saveRDS( c(as.list(d), list(
    alpha = alpha,
    beta = beta,
    dr1 = dr1,
    dr2 = dr2,
    delta = c(dr1, dr2),
    tau = tau,
    N1 = N1,
    N2 = N2,
    N = nrow(d)
)), a$output )
