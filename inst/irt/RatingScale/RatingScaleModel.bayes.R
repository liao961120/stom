source("../utils.R")  # Load some functions

#######################
#### Simulate data ####
#######################
set.seed(13)
n_item = 10   # number of items
n_subj = 80   # number of subjects
n_judge = 5   # number of judges
n_resp = n_subj * n_item * n_judge  # number of responses
n_param = n_item + n_subj + n_judge

# True parameter values
true = list(
  alpha = c( -2.1972246, -0.4054651, 1.3862944, 2.9444390 ),  # Baseline cutpoints
  A = rnorm( n_subj, 0, 1 ),          # Subject ability (zero-centered)
  E = seq(-1.5, 1.5, length=n_item),  # Item easiness (zero-centered)
  J = c( -.5, 0, 0, 0, .5 )           # Judge leniency (zero-centered)
)

d = expand.grid( Sid=1:n_subj, Iid=1:n_item, Jid=1:n_judge, KEEP.OUT.ATTRS = F )
d$R = apply( d, 1, function(row) {
  s = row['Sid']
  i = row['Iid']
  j = row['Jid']
  phi = true$A[s] + true$E[i] + true$J[j]
  rordlogit( 1, true$alpha, phi )
})
dat = list(
  R = d$R,
  Sid = d$Sid,
  Iid = d$Iid,
  Jid = d$Jid,
  N = n_resp,
  n_item = n_item,
  n_subj = n_subj,
  n_judge = n_judge,
  n_cutpoint = length(true$alpha)
)
truth = with(true, c( A, E, J, alpha ) )

#######################
#### Model Fitting ####
#######################
library(rstan)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)


# Model 1: no pooling
m1 = "cache/unpooled.RDS"
if ( file.exists(m1) ) {
  m1 = readRDS(m1)
} else {
  m1 = rstan::stan( "unpooled.stan", data=dat, chains=4 )
  saveRDS(m1, "cache/unpooled.RDS")
}
p1 = extract( m1 )  # extract posterior sample
s1 = with( p1, {cbind( 
  post_summ(A), post_summ(E), post_summ(J), post_summ(cutpoints) 
)})
  

# Model 2: pooling on S/E/J
m2 = "cache/partial-pooled.RDS"
if ( file.exists(m2) ) {
  m2 = readRDS(m2)
} else {
  m2 = rstan::stan( "partial-pooled.stan", data=dat, chains=4 )
  saveRDS(m2, "cache/partial-pooled.RDS")
}
p2 = extract( m2 )
s2 = with( p2, {cbind( 
  post_summ(A), post_summ(E), post_summ(J), post_summ(cutpoints) 
)})


##########################
#### Model Comparison ####
##########################
# Effect of partial pooling
plot( truth, col="white", ylim=c(-3.3, 3.3), ylab="Effect" )
plot_post$param_vsep( c(n_subj, n_item, n_judge) )
plot_post$grand_mean_ref( p2, params=c("A","E","J","cutpoints") )
plot_post$ci( s1, col=col.alpha(2,.5), lwd=6 )
plot_post$ci( s2, col=col.alpha(4,.7), lwd=2 )
points( truth, pch=19 )
plot_post$mean( s1, col=2 )
plot_post$mean( s2, col=4 )
mtext( "Subject", at=40, padj=-.3 )
mtext( "Item", at=85.5, padj=-.3 )
mtext( "Judge", at=93, padj=-.3 )
mtext( "Cutpoint", at=99.5, padj=-1.8 )
mtext( "Red: Unpooled", adj=0, line=3, cex=.8 )
mtext( "Blue: Partial-pooled", adj=0, line=2, cex=.8 )
mtext( "Shade: 89% Probability mass", adj=0, line=1, cex=.8 )
