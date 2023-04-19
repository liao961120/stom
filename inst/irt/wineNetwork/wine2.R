#####################################################################
# Piped IRT model with interaction (differential rater functioning) #
#####################################################################
#   Recovery of interaction seems to depend on where it happens
#   In general, it seems that if an interaction term has the same direction
#   as wine origin's effect on wine quality. 
#   Currently (4.18.2023) only 1 interaction term is roughly explored. 
#   Needs further explorations to confirm this hypothesis.
# 
#   [Possible explorations]
#     1. Test multiple interaction terms acting on same/different directions
#     2. Use normal instead of logit link to check for boundary effect
#  
#   [model]
#   R ~ Bernoulli( p )
#   logit(p) = W[Wid] + J[Jid] + Int[Oj, Ow]
#   J ~ Normal( 0, sigma_J )
#   sigma_J ~ Exponential(1)
#   
#   W[Wid] ~ Normal( a[Ow], sigma_W )
#   vector[2]:a ~ Normal( 0, 1.5 )
#   sigma_W ~ Exponential(1)
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
W = ifelse( Ow == 1, rnorm(Nw, 2), rnorm(Nw, -2) )  # Wine quality
J = standardize(J)
W = standardize(W)
# Interaction: 
#   Rows: judge origin
#   Cols: wine origin
#   If Judge/Wine have same origin, an addition latent score of 1.3 is added.
Int = matrix(c(0, 0,
               1.3, 0 ), nrow=2, byrow=T )

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
m = cmdstan_model("wine2.stan")
fit = m$sample(data=dat,
               seed=123, chains=4, parallel_chains=4, refresh=500)
summ = fit$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
summ = as.data.frame(summ)
summ
plot( W, summ[grepl("W", summ$variable), "mean"]); abline(0, 1)
plot( J, summ[grepl("J", summ$variable), "mean"]); abline(0, 1)
plot( c(mean(W[1:Nw/2]), mean(W[-(1:Nw/2)])), summ[grepl("a", summ$variable), "mean"]); abline(0, 1)
plot( Int, summ[grepl("Int", summ$variable), "mean"]); abline(0, 1)
