#' Effect of Partial Pooling on 1PL IRT Model
#' 
#' This script demonstrates the fitting of 1PL model in Bayesian hierarchical
#' model. Three models (`m0`, `m0.1`, and `m0.2`) are compared to illustrate  
#' the effect of partial pooling on parameter estimation.
#' 
#' - `m0` is the base model without partial pooling. This model suffers from the
#'   issue of identifiability. To identify the parameters in `m0` (i.e.,
#'   preventing the estimates from floating around according to the given
#'   priors), a sum-to-zero constraint is added to the subject abilities.
#'
#' - `m0.1` could be conceptualized in two ways.
#'
#'   1. Utilizing hierarchical structure to identify the location of item
#'      parameters. 
#'
#'      In practice, when fitting IRT models to real data, we do not know the
#'      locations of the parameters. These need to be estimated from the data,
#'      not solely from the prior information given to the model (when the model
#'      is unidentified as in `m0`, parameter estimation will be overly
#'      sensitive to priors). This can be done through hierarchical
#'      parameterization: set hyper-priors in prior distributions and let the
#'      model finds the item parameters. The constraint on the space explored is
#'      imposed through the standard normal prior given to the person
#'      parameters. 
#' 
#'   2. Partial pooling on item parameters to improve fitting.
#'
#'      Through hierarchical parameterization, partial pooling is also done. And
#'      through partial pooling, the estimation of each item parameters
#'      incorporates information from all other items, which reduces overfitting 
#'      and increases out-of-sample prediction accuracy.
#'   
#' - `m0.2` follows naturally from `m0.1`. Now, both item and person parameters
#'   are partially pooled. `m0.2` identifies the parameters through constraining
#'   both item and person parameters to have a zero mean. An independent grand
#'   mean is estimated. This grand mean can be used to scale the locations of
#'   either the items or the persons, depending on which group is used as the
#'   reference. In the case here, the person parameters are used as the
#'   reference. Hence, the grand mean is added to the item parameters after
#'   estimation.
source("../utils.R")


#########################
#### Simulate data  #####
#########################
set.seed(13)
n_item = 20    # number of items
n_subj = 40    # number of subjects
n_resp = n_item * n_subj
n_param = n_item + n_subj

A = rnorm( n_subj )  # Person ability
E = seq( -1.6, 1, length=n_item )  # Item easiness

d = expand.grid( Sid=1:n_subj, Iid=1:n_item, KEEP.OUT.ATTRS = F )
d$mu = A[d$Sid] + E[d$Iid]
d$R = rbern( logistic( d$mu ) )

dat = list(
  R = as.integer(d$R),
  Sid = as.integer( d$Sid ),
  Iid = as.integer( d$Iid ),
  n_item = as.integer(n_item),
  n_subj = as.integer(n_subj)
)


library(rethinking)
library(rstan)

######################################
#### Prior predictive simulation #####
######################################
# Unpooled case
sim_prior_unpooled = function(sd_a=3, sd_e=1, add=F) {
  a = rnorm( n_subj, 0, sd_a )
  e = rnorm( n_item, 0, sd_e )
  d = expand.grid( Sid=1:n_subj, Iid=1:n_item, KEEP.OUT.ATTRS = F )
  d$mu = with(d, { a[Sid] + e[Iid] })
  d$p = logistic(d$mu)
  rethinking::dens( d$p, add = add )
}
sim_prior_unpooled(1.3)
sim_prior_unpooled(1.3, add=T)

# pooled case
sim_prior_pooled = function(sd_abar=1.5, sigma_rate=1, sd_e=1, add=F) {
  abar = rnorm( 1, 0, sd_abar )
  sigma = rexp(sigma_rate)
  e = rnorm( n_item, 0, sd_e )
  za = rnorm( n_subj, 0, 1 )
  a = abar + za*sigma
  d = expand.grid( Sid=1:n_subj, Iid=1:n_item, KEEP.OUT.ATTRS = F )
  d$mu = with(d, { a[Sid] + e[Iid] })
  d$p = logistic(d$mu)
  dens(d$p, add=add, col=col.alpha(2,.6))
}
sim_prior_pooled(1)
sim_prior_pooled(1, add=T)


#######################
#### Model Fitting ####
#######################
# No pooling (sum-to-zero constraint added)
m0 <- ulam(
  alist(
    R ~ bernoulli(p),
    logit(p) <- A[Sid] + E[Iid],
    transpars> vector[n_subj]:A <<- append_row(A_raw, -sum(A_raw)), # sum-to-zero constraint
    vector[n_subj-1]:A_raw ~ normal( 0, 1 ),
    vector[n_item]:E ~ normal( 0, 1.5 )
  ) , data=dat, chains=4, cores=4, log_lik=T, sample=T )
saveRDS(m0, "cache/m0.RDS")
p0 = extract.samples( m0 )


# Hierarchical model to locate E by implicit constraint of setting A ~ std_normal()
m0.1 <- ulam(
  alist(
    R ~ bernoulli(p),
    logit(p) <- A[Sid] + E[Iid],
    transpars> vector[n_item]:E <<- Ebar + zE*sigma,
    vector[n_item]:zE ~ normal( 0, 1 ),
    vector[n_subj]:A ~ normal( 0, 1 ),
    Ebar ~ normal(0, 1),
    sigma ~ exponential(1)
  ) , data=dat, chains=4, cores=4, log_lik=T, sample=T )
p0.1 = extract.samples( m0.1 )


# Hierarchical model on both A and E
m0.2 <- ulam(
  alist(
    R ~ bernoulli(p),
    logit(p) <- A[Sid] + Ec[Iid] + Mean,
    # Item effect with mean shifted to grand mean
    transpars> vector[n_item]:E <<- Ec + Mean,
    transpars> vector[n_item]:Ec <<- zE*sigma_E,
    transpars> vector[n_subj]:A <<- zA*sigma_A,
    vector[n_item]:zE ~ normal( 0, 1 ),
    vector[n_subj]:zA ~ normal( 0, 1 ),
    Mean ~ normal(0, 1),
    c(sigma_E,sigma_A) ~ exponential(1)
  ) , data=dat, chains=4, cores=4, sample=T, log_lik=T )
saveRDS(m0.2, "cache/m0.2.RDS")
p0.2 = extract.samples( m0.2 )


compare( m0, m0.1, m0.2 )


######################################################
#### Model comparison (effect of partial-pooling) ####
######################################################
s0 = cbind( post_summ( p0$A ), post_summ( p0$E ) )
s1 = cbind( post_summ( p0.2$A ), post_summ( p0.2$E ) )
truth = c(A, E)

# Plot parameter estimation
plot( truth, col="white", ylim=c(-2.5, 2.5), ylab="Effect" )
# Grand mean for A
segments(0, mean(p0.2$A), n_subj+1, mean(p0.2$A), lty=2, col="grey" )
# Grand mean for E
segments(n_subj+1, mean(p0.2$E), n_param+1 , mean(p0.2$E), lty=2, col="grey" )
points( c(A,E), pch=19 )
# No pooling
points( s0[1, ], col=2 )
for (i in 1:ncol(s0))
  lines( rep(i, 2), c(s0[2, i], s0[3, i]), col=col.alpha(2, .5), lwd=6 )
# With pooling
points( s1[1, ], col=4 )
for (i in 1:ncol(s0))
  lines( rep(i, 2), c(s1[2, i], s1[3, i]), col=col.alpha(4, .7), lwd=2 )


# Plot estimation error
plot( 1, type="n", ylim=c(-2, 2), xlim=c(0, length(truth)+1) )
abline( h=0, lty=2, col="grey" )
# for (i in 1:ncol(s0))
#   lines( rep(i, 2), c(s0[2, i], s0[3, i])-truth[i], col=col.alpha(2, .5), lwd=6 )
# for (i in 1:ncol(s0))
#   lines( rep(i, 2), c(s1[2, i], s1[3, i])-truth[i], col=col.alpha(4, 1), lwd=2 )
points( s0[1, ]-truth, col=2, pch=19 )
points( s1[1, ]-truth, col=4 )
