#################################################################
# Simple interaction IRT model (differential rater functioning) #
#################################################################
#   [model]
#   R ~ Bernoulli( p )
#   logit(p) = W[Wid] + J[Jid] + Int[Oj, Ow]
#   matrix[2, 2]:Int ~ Normal( 0, 1 )
#   W ~ Normal( 0, sigma_W )
#   J ~ Normal( 0, sigma_J )
#   sigma_W, sigma_J ~ Exponential(1)
#################################################################

standardize = function(x) (x - mean(x)) / sd(x)
rbern = function( p ) rbinom(n = length(p), size = 1, prob = p)
logistic = function(x) 1/(1 + exp(-x))

#######################
### Data Simulation ###
#######################
set.seed(1023)
Nj = 40      # n judges
Nw = 40      # n wines
N = Nj * Nw  # n responses
Oj = rep( 1:2, each=Nj/2 )  # Judge origin (1 or 2)
Ow = rep( 1:2, each=Nw/2 )  # Wine origin (1 or 2)
J = rnorm(Nj)  # Judge leniency
W = rnorm(Nw)  # Wine quality
# Interaction: 
#   Rows: judge origin
#   Cols: wine origin
#   If Judge/Wine have same origin, an addition latent score of 1.3 is added.
Int = matrix(c(1.3, 0,
               0,  1.3 ), nrow=2, byrow=T )
d = expand.grid( Jid=1:Nj, Wid=1:Nw, KEEP.OUT.ATTRS=F )
d$J = J[d$Jid]
d$W = W[d$Wid]
d$Oj = Oj[d$Jid]
d$Ow = Ow[d$Wid]
d$L = sapply( 1:nrow(d), function(i) d$J[i] + d$W[i] + Int[d$Oj[i], d$Ow[i]] )
d$R = rbern( logistic(d$L) )


#####################
### Model Fitting ###
#####################
dat = list(
  N = Nj*Nw,
  Nw = Nw,
  Nj = Nj,
  R = d$R,
  L = d$L,
  Wid = d$Wid,
  Jid = d$Jid,
  Ow = d$Ow,
  Oj = d$Oj
)

library(cmdstanr)
m = cmdstan_model("wine0.stan")
fit = m$sample(data=dat,
  seed=123, chains=4, parallel_chains=4, refresh=500)
summ = fit$summary(variables=c("W", "J", "a"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
summ = as.data.frame(summ)
summ
# plot( W, summ[grepl("W", summ$variable), "mean"]); abline(0, 1)
# plot( J, summ[grepl("J", summ$variable), "mean"]); abline(0, 1)
# plot( Int, summ[grepl("Int", summ$variable), "mean"]); abline(0, 1)

