library(stom)
library(cmdstanr)

sim_dat = function( Int=matrix(rep(0,4), nrow=2, byrow=T), diff_by_Ow=T ) {
  set.seed(1023)
  Nj = 40      # n judges
  Nw = 40      # n wines
  N = Nj * Nw  # n responses
  Oj = rep( 1:2, each=Nj/2 )  # Judge origin (1 or 2)
  Ow = rep( 1:2, each=Nw/2 )  # Wine origin (1 or 2)
  J = rnorm(Nj)  # Judge leniency
  if (diff_by_Ow)
    W = ifelse( Ow == 1, rnorm(Nw, 2), rnorm(Nw, -2) )  # Wine quality
  else
    W = rnorm(Nw)
  J = standardize(J)
  W = standardize(W)
  # Interaction:
  #   Rows: judge origin
  #   Cols: wine origin
  #   If Judge/Wine have same origin, an addition latent score of 1.3 is added.
  # Int = matrix(c(0, 0,
  #                1.3, 0 ), nrow=2, byrow=T )

  d = expand.grid( Jid=1:Nj, Wid=1:Nw, KEEP.OUT.ATTRS=F )
  d$J = J[d$Jid]
  d$W = W[d$Wid]
  d$Oj = Oj[d$Jid]
  d$Ow = Ow[d$Wid]
  d$L = sapply( 1:nrow(d), function(i) d$J[i] + d$W[i] + Int[d$Oj[i], d$Ow[i]] )
  d$R = rbern( logistic(d$L) )

  dat = list(
    N = Nj*Nw,
    Nw = Nw,
    Nj = Nj,
    R = d$R,
    L = d$L,
    C = rnorm(length(d$L), d$L ),
    Wid = d$Wid,
    Jid = d$Jid,
    Ow = d$Ow,
    Oj = d$Oj,
    W.ori = W,
    J.ori = J
  )
  return(dat)
}
###########################################



################################################
# Model 1
#
# [DGM]
#   - [x] interaction
#   - [0] Wine quality different by origin (Ow)
# [Model]
#   mu = W + J
#   - [x] interaction
#   - [x] Wine quality different by origin (Ow)
# [Expect]
#   W/J should be correctly recovered
# [Results]
#   As expected
################################################
m = cmdstan_model("wine2_normal_first_level.stan")
(Int = matrix( c( 0, 0,
                  0, 0), nrow=2, byrow=T ) )
dat = sim_dat(Int, diff_by_Ow = T)

f1 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s1 = f1$summary(variables=c("W", "J"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s1 = as.data.frame(s1) )
plot( dat$W.ori, s1[grepl("W", s1$variable), "mean"] ); abline(0, 1)
plot( dat$J.ori, s1[grepl("J", s1$variable), "mean"] ); abline(0, 1)



################################################
# Model 2
#
# [DGM]
#   - [o] interaction
#   - [x] Wine quality different by origin (Ow)
# [Model]
#   mu = W + J + Int
#   - [o] interaction
#   - [x] Wine quality different by origin (Ow)
# [Expect]
#   W/J/Int should be correctly recovered
# [Results]
#   As expected
################################################
m = cmdstan_model("wine2_normal_first_level2.stan")
(Int = matrix( c( 1.5, 0,
                  1.5, 0), nrow=2, byrow=T ) )
dat = sim_dat(Int, diff_by_Ow = F)
f2 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s = f2$summary(variables=c("W", "J", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s = as.data.frame(s) )
plot( dat$W.ori, s[grepl("W", s$variable), "mean"] ); abline(0, 1)
plot( dat$J.ori, s[grepl("J", s$variable), "mean"] ); abline(0, 1)
plot( Int, s[grepl("Int", s$variable), "mean"] ); abline(0, 1)


################################################
# Model 3
#
# [DGM]
#   - [o] interaction
#   - [o] Wine quality different by origin (Ow)
# [Model]
#   mu = W + J + Int
#   - [o] interaction
#   - [x] Wine quality different by origin (Ow)
# [Expect]
#   W/J/Int should be correctly recovered
# [Results]
#   Failed to recover W/J/Int
###############################################
m = cmdstan_model("wine2_normal_first_level2.stan")
(Int = matrix( c( 1.5, 0,
                  0,   0), nrow=2, byrow=T ) )
dat = sim_dat(Int, diff_by_Ow = T)
f3 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s = f3$summary(variables=c("W", "J", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s = as.data.frame(s) )
plot( dat$W.ori, s[grepl("W", s$variable), "mean"] ); abline(0, 1)
plot( dat$J.ori, s[grepl("J", s$variable), "mean"] ); abline(0, 1)
plot( Int, s[grepl("Int", s$variable), "mean"] ); abline(0, 1)






################################################
# Model 4
#
# [DGM]
#   - [o] interaction
#   - [o] Wine quality different by origin (Ow)
# [Model]
#   mu = W + J + Ow + Int
#   - [o] interaction
#   - [o] Wine quality different by origin (Ow)
# [Expect]
#   (W+Ow)/Ow/J/Int should be correctly recovered
# [Results]
#   As expected
#################################################
m = cmdstan_model("wine2_normal_first_level4.stan")
(Int = matrix( c( 1.5, 0,
                  0,   0), nrow=2, byrow=T ) )
dat = sim_dat(Int, diff_by_Ow = T)
f4 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s = f4$summary(variables=c("Wr", "J", "Int", "a"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s = as.data.frame(s) )
plot( dat$W.ori, s[grepl("Wr", s$variable), "mean"] ); abline(0, 1)
plot( dat$J.ori, s[grepl("J", s$variable), "mean"] ); abline(0, 1)
plot( Int, s[grepl("Int", s$variable), "mean"] ); abline(0, 1)


################################################
# Model 5
#
# [DGM]
#   - [o] interaction
#   - [x] Wine quality different by origin (Ow)
# [Model]
#   mu = W + J + Ow + Int
#   - [o] interaction
#   - [o] Wine quality different by origin (Ow)
# [Expect]
#   W/J/Ow(=0)/Int should be correctly recovered
# [Results]
#   As expected
#################################################
m = cmdstan_model("wine2_normal_first_level4.stan")
(Int = matrix( c( 1.5, 0,
                  0,   0), nrow=2, byrow=T ) )
dat = sim_dat(Int, diff_by_Ow = F)
f5 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s = f5$summary(variables=c("Wr", "W", "J", "Int", "a"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s = as.data.frame(s) )
plot( dat$W.ori, s[grepl("Wr", s$variable), "mean"] ); abline(0, 1)
plot( dat$W.ori, s[grepl("W[^r]", s$variable), "mean"] ); abline(0, 1)
plot( dat$J.ori, s[grepl("J", s$variable), "mean"] ); abline(0, 1)
plot( Int, s[grepl("Int", s$variable), "mean"] ); abline(0, 1)

