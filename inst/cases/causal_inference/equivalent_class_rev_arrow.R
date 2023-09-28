#' Demonstration of observation equivalence when reversing an arrow 
#' in a mediation model. Based on the model in Rohrer et al. (2022) Fig. 1 & the
#' text in sect. "Comparing alternative models".
#' 
#' @references 
#' Rohrer, J. M., Hünermund, P., Arslan, R. C., & Elson, M. (2022). 
#'    That’s a Lot to Process! Pitfalls of Popular Path Models. 
#'    Advances in Methods and Practices in Psychological Science, 5(2). 
#'    https://doi.org/10.1177/25152459221095827


library(rethinking)

# Simulation of true DGP
# 
#   P
#  ^ ^
# /   \ 
# S -> E
set.seed(10)
N = 400
S = rnorm(N, .5)
E = rnorm(N, .7 * S, .5)
P = rnorm(N, .3 * S + .7 * E, .5)
Mat0 = cbind( S, E, P )
S0 = S
E0 = E

dat = list(
    N = N,
    S = S,
    E = E,
    P = P
)

# True DGP
m1 = ulam( 
    alist(
        P ~ normal(muP, sP),
        muP <- bSP * S + bEP * E,
        E ~ normal(muE, sE),
        muE <- bES * S,
        S ~ normal(muS,sS),
        bSP ~ normal(0,1),
        bEP ~ normal(0,1),
        bES ~ normal(0,1),
        sE ~ exponential(1.5),
        sS ~ exponential(1.5),
        sP ~ exponential(1.5),
        muS ~ normal(0, 2)
    ), data=dat, chains=4, cores=4, sample=T, cmdstan=T, log_lik = T ) 

# Alternative DGP
m2 = ulam( 
    alist(
        P ~ normal(muP, sP),
        muP <- bSP * S + bEP * E,
        S ~ normal(muS,sS),
        muS <- bES * E,
        E ~ normal(muE,sE),
        bSP ~ normal(0,1),
        bEP ~ normal(0,1),
        bES ~ normal(0,1),
        sE ~ exponential(1.5),
        sS ~ exponential(1.5),
        sP ~ exponential(1.5),
        muE ~ normal(0, 2)
        
    ), data=dat, chains=4, cores=4, sample=T, cmdstan=T, log_lik = T ) 


precis(m1)
precis(m2)
compare(m1, m2)


######
# Model 1 (true)
bSP = .31
bEP = .68
bES = .69
sE = .53
sS = 1.11
sP = .48
muS = .5
set.seed(10)
N = 400
S = rnorm(N, muS, sS)
E = rnorm(N, bES * S, sE)
P = rnorm(N, bSP * S + bEP * E, sP)
Mat1 = cbind( S, E, P )

# Model 2 (E -> S, arrow reversed)
bSP = .31
bEP = .68
bES = .98
sE = .93
sS = .63
sP = .48
muE = .33
set.seed(10)
N = 400
E = rnorm(N, muE, sE)
S = rnorm(N, bES * E, sS)
P = rnorm(N, bSP * S + bEP * E, sP)
Mat2 = cbind( S, E, P )

cov(Mat0)
cov(Mat1)
cov(Mat2)
