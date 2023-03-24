library(lme4)
source("../utils.R")

#########################
#### Simulate data  #####
#########################
set.seed(12)
n_item = 30    # number of items
n_subj = 60    # number of subjects
n_resp = n_item * n_subj
n_param = n_item + n_subj

A = rnorm( n_subj )  # Person ability
E = seq( -1.6, 1, length=n_item )  # Item easiness

d = expand.grid( Sid=1:n_subj, Iid=1:n_item, KEEP.OUT.ATTRS = F )
d$mu = A[d$Sid] + E[d$Iid]
d$R = rbern( logistic( d$mu ) )
d$Sid = factor(d$Sid)
d$Iid = factor(d$Iid)


# GLM: Fixed person / Fixed item
#' Note that depending on the simulated data, the model may or may NOT
#' converge to stable estimates of the parameters. When the model fails,
#' NO WARNING will be given, but the standard errors of the effects will
#' increase to unreasonable values.
d1 = d
contrasts(d1$Sid) = contr.sum( n_subj )
m1 = glm( R ~ -1 + Iid + Sid, data=d1, family=binomial("logit") )
summary(m1)
# Contrast matrix for computing effects and std.Error for all parameters
Cmat = diag(0, nrow=n_item+n_subj)[, -1]
diag(Cmat)[1:n_item] = 1
idxS = 1:n_subj + n_item
Cmat[idxS, idxS[-length(idxS)]] = contr.sum( n_subj )
m1_eff = (Cmat %*% coef(m1))[, 1]
Vmat = Cmat %*% vcov(m1) %*% t(Cmat)
m1_se = sqrt(diag(Vmat))


# Partial pooling on subj
m2 = glmer( R ~ -1 + Iid + (1|Sid), data=d, family=binomial('logit') )
item_eff.m2 = fixef(m2)
subj_eff.m2 = ranef(m2)$Sid[, 1]
m2_eff = c( item_eff.m2, subj_eff.m2 )
m2_se = arm::se.coef( m2 )
m2_se = c( m2_se$fixef, m2_se$Sid )


# Partial pooling on item & subj
m2.2 = glmer( R ~ 1 + (1|Sid) + (1|Iid), data=d, family=binomial('logit') )
subj_eff.m2.2 = ranef(m2.2)$Sid[, 1]
item_eff.m2.2 = ranef(m2.2)$Iid[, 1] + fixef(m2.2)[["(Intercept)"]]
m2.2_eff = c( item_eff.m2.2, subj_eff.m2.2 )
m2.2_se = arm::se.ranef( m2.2 )
m2.2_se = c( m2.2_se$Iid, m2.2_se$Sid )


# Plot for comparing model fit
plot( 1, type="n", ylim = c(-4.8, 4.8), xlim=c( 0, n_subj+n_item + 1 ), 
      ylab = "Estimated Difficulty", xlab = "Item Index" )
abline( v = n_subj + .5, lty=2, col="grey" )
abline( h = 0, lty=2, col="grey" )

points( c(E, A), pch=19 )
for (i in seq_along(m2_se)) {
  lines( c(i,i), m1_eff[i] + c(-2,2)*m1_se[i], col=col.alpha(4), lwd=6 )
  lines( c(i,i), m2_eff[i] + c(-2,2)*m2_se[i], col=col.alpha(2,.7), lwd=3 )
}
points( m1_eff, col=4 )
points( m2_eff, col=2 )

