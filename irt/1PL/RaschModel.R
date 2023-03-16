library(lme4)
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
d$Sid = factor(d$Sid)
d$Iid = factor(d$Iid)



d1 = d
# contrasts(d1$I) = contr.sum(n_item)
contrasts(d1$S) = contr.sum(n_subj)
m1 = glm( R ~ -1 + Iid + Sid, data=d1, family=binomial("logit") )
eff = coef(m1)
item_eff.m1 = eff[1:n_item]
subj_eff.m1 = eff[(n_item+1):(n_item+n_subj-1)]
subj_eff.m1 = c( subj_eff.m1, -sum(subj_eff.m1) )


# Partial pooling on subj
m2 = glmer( R ~ -1 + Iid + (1|Sid), data=d, family=binomial('logit') )
item_eff.m2 = fixef(m2)
subj_eff.m2 = ranef(m2)$Sid[, 1]


# Partial pooling on item & subj
m2.2 = glmer( R ~ 1 + (1|Sid) + (1|Iid), data=d, family=binomial('logit') )
subj_eff.m2.2 = ranef(m2.2)$Sid[, 1]
item_eff.m2.2 = ranef(m2.2)$Iid[, 1] + fixef(m2.2)[["(Intercept)"]]


# Plot for comparing model fit
plot( 1, type="n", ylim = c(-2.7, 2.7), xlim=c( 0, n_subj+n_item + 1 ), 
      ylab = "Estimated Difficulty", xlab = "Item Index" )
abline( v = n_subj + .5, lty=2, col="grey" )
abline( h = 0, lty=2, col="grey" )
points( c(A, E), pch=19 )
points( c(subj_eff.m1, item_eff.m1),   col=4)
# points( c(-item_eff.m2,   subj_eff.m2),   col="red"    , cex=1.2, lwd = 1.8 )
points( c(subj_eff.m2.2, item_eff.m2.2), col=2 )



