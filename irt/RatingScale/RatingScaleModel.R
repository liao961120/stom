library(lme4)
library(ordinal)
library(glue)
source("../utils.R")  # Load some functions
if (Sys.info()["sysname"] == "Windows") 
  windowsFonts(sans = "SF Pro Display")

#######################
#### Simulate data ####
#######################
set.seed(13)
n_item = 10   # number of items
n_subj = 80   # number of subjects
n_judge = 5   # number of judges
n_resp = n_subj * n_item * n_judge  # number of responses

# True parameter values
true = list(
  alpha = c( -2.1972246, -0.4054651, 1.3862944, 2.9444390 ),  # Baseline cutpoints
  A = rnorm( n_subj, 0, 1 ),          # Subject ability (zero-centered)
  E = seq(-1.5, 1.5, length=n_item),  # Item easiness (zero-centered)
  J = c( -.5, 0, 0, 0, .5 )           # Judge leniency (zero-centered)
)
# rating_prob = c( .1, .3, .4, .15, .05 )  # Prob of each score
# cum_rating_prob = cumsum( rating_prob )
# cum_rating_prob
# alpha = logit( cum_rating_prob[-length(cum_rating_prob)] )

d = expand.grid( Sid=1:n_subj, Iid=1:n_item, Jid=1:n_judge, KEEP.OUT.ATTRS = F )
d$R = apply( d, 1, function(row) {
  s = row['Sid']
  i = row['Iid']
  j = row['Jid']
  phi = true$A[s] + true$E[i] + true$J[j]
  rordlogit( 1, true$alpha, phi )
})
for (col in colnames(d))
  d[[col]] = factor( d[[col]] )
d$R = ordered( d$R )
# simple_hist(d$R)


#######################
#### Model 1: GLM #####
#######################
# GLM with Cumulative-odds link
d2 = d
contrasts(d2$Sid) = contr.sum(n_subj)
contrasts(d2$Iid) = contr.sum(n_item)
contrasts(d2$Jid) = contr.sum(n_judge)
m1 = MASS::polr( R ~ Sid + Iid + Jid, data=d2, Hess = F )
# coef(m1)
# summary(m1)

get_eff = function(x, prefix) {
  x = x[startsWith(names(x), prefix)]
  c( x, -sum(x) ) 
}
m1.eff = list(
  alpha = m1$zeta,  # cutpoints
  A = get_eff( coef(m1), "Sid" ),
  E = get_eff( coef(m1), "Iid" ),
  J = get_eff( coef(m1), "Jid" )
)


########################
#### Model 2: GLMM #####
########################
d2 = d
contrasts(d2$Iid) = contr.sum(n_item)
contrasts(d2$Jid) = contr.sum(n_judge)
m2.1 = clmm( R ~ Iid + Jid + (1|Sid) , data = d2 )
m2.1.eff = list(
  alpha = m2.1$alpha,
  A = ranef(m2.1)$Sid[[1]],
  E = get_eff( coef(m2.1), "Iid" ),
  J = get_eff( coef(m2.1), "Jid" )
)

m2.2 = clmm( R ~ (1|Iid) + (1|Jid) + (1|Sid) , data = d )
m2.2.eff = list(
  alpha = m2.2$alpha,
  A = ranef(m2.2)$Sid[[1]],
  E = ranef(m2.2)$Iid[[1]],
  J = ranef(m2.2)$Jid[[1]]
)

##################################
### Comparing Model Estimation ###
##################################

# Estimated parameter values
png("model-estimate-compare.png", 
    width = 1600, height = 1000, res = 450, pointsize = 4.5 )
d_abs = function(x, y) sprintf( "%.3f", mean(abs(x-y)) )
par_bound = cumsum( sapply(true, function(x) length(x)) )[1:3]
plot(1, type="n", ylab = "Effect", xlim=c(0, 100), ylim=c(-2.3, 3.3))
abline( h = 0, lty = "dashed" )
abline( v=par_bound + .5 )
points( unlist(true), pch=19 )
points( unlist(m1.eff), col=4 )
points( unlist(m2.2.eff), col=2 )
b = 1
for (x in names(m1.eff)) {
  est = c(
    d_abs( m1.eff[[x]], true[[x]] ),
    d_abs( m2.2.eff[[x]], true[[x]] )
  )
  l = length(m1.eff[[x]])
  s = l / 2
  if (b < 5)  s = s - 2.5
  if (b > 94) s = s + 1.6
  mtext(x, line = 2, at = b + s, cex = .9 )
  mtext(glue("m1: {est[1]}"), line = 1.2, at = b + s, cex = .75 )
  mtext(glue("m2: {est[2]}"), line = 0.4, at = b + s, cex = .75 )
  b = b + l
}
dev.off() 

# Distance to true value
png("model-estimate-compare2.png", 
    width = 1600, height = 1000, res = 450, pointsize = 4.5 )
plot(1, type="n", ylab = "Estimated - True", xlim=c(0, 100), ylim=c(-1, 1))
abline( h = 0, lty = "dashed" )
abline( v=par_bound + .5 )
points( unlist(m1.eff) - unlist(true), pch=19, col=4 )
points( unlist(m2.2.eff) - unlist(true), pch=19, col=2 )
b = 1
for (x in names(m1.eff)) {
  est = c(
    d_abs( m1.eff[[x]], true[[x]] ),
    d_abs( m2.2.eff[[x]], true[[x]] )
  )
  l = length(m1.eff[[x]])
  s = l / 2
  if (b < 5)  s = s - 2.5
  if (b > 94) s = s + 1.6
  mtext(x, line = 2, at = b + s, cex = .9 )
  mtext(glue("m1: {est[1]}"), line = 1.2, at = b + s, cex = .75 )
  mtext(glue("m2: {est[2]}"), line = 0.4, at = b + s, cex = .75 )
  b = b + l
}
dev.off() 
