# Export this file with `stom::export_docs_pdf("wine.R", "README.pdf")`

#' ---
#' title: Joint Estimation of Interaction and Piped Effects in an Item Response Model
#' author: "Yongfu Liao"
#' ---
#'
#' <!-- date -->
#' \vspace{-15pt}\footnotesize\begin{center}\today\end{center}\vspace{12pt}
#'
#'
#' Description
#' ===========
#'
#' The Directed Acyclic Graph (DAG) shown below represent the data generating
#' process of the IRT model of interest. It can be conceptualized as a wine
#' rating context, where the rating of wine quality ($R$) is influenced by
#' three factors:
#'
#' 1. $J$: Judge leniency
#' 2. $W$: Wine quality
#' 3. $O_w$: Wine origin
#'
#' Furthermore, it is assumed that $O_w$ differentially influences $R$
#' based on the levels of another variable $O_j$, the origin of the judge
#' In stats jargon, there's an interaction between $O_w$ and $O_j$. In simpler
#' conceptual terms, consider the scenario that, for instance, French wines are
#' rated higher by French judges, in addition to the scores they should have
#' received based on their quality alone. The simulation code of this data
#' generating process is found in the `sim_dat()` function below.
#'
#' ```goat
#'       R <--- Ow
#'      ^ ^      |
#'     /   \     |
#'    J      W <-'
#' ```
#'
#'
#' The Original Model and its Problem
#' ==================================
#'
#' The specification of the original model is shown in the equations below.
#' A problem found in this model is that it cannot stably recover the wine
#' quality (`W`) and the interaction (`Int`) parameters.
#'
#' $$
#' \begin{aligned}
#' R & \sim Bernoulli( p )  \\
#' logit(p) &= W_{[Wid]} + J_{[Jid]} + Int_{[O_j, O_w]} \\
#' J & \sim Normal( 0, \sigma_J )  \\
#' \sigma_J & \sim Exponential(1) \\
#' \\
#' W_{[Wid]} & \sim Normal( a_{[O_w]}, \sigma_W )  \\
#' a & \sim Normal( 0, 1.5 )  \\
#' \sigma_W & \sim Exponential(1)
#' \end{aligned}
#' $$
#'
#'
#' Potential Causes
#' ================
#'
#' With some exploration on a simpler model (the response was modeled as normal
#' distributions generated from latent scores), it was found that the problem
#' seemed to arise from an identifiablity issue: the model cannot correctly
#' attribute the right amount of effect to wine quality (which is influenced by
#' wine origin) and the direct (interactive) influence of wine origin on rating
#' scores. The parameter estimates float around case by case when different
#' configurations of the interaction are set in the simulation.
#'
#'
#' Fixes
#' =====
#'
#' As illustrated in `wine2_normal.stan` (Case 4 & 5), the problem can be fixed
#' by imposing additional constraints on the model. Two of them are imposed
#' here to correctly identify the parameters:
#'
#' 1. A sum-to-zero constraint on the effects of wine origin on wine quality.
#'    That is, the effect through the path $O_w \rightarrow W$.
#' 2. One of a term in the interaction matrix (`(2, 2)` in the case here) is
#'    constraint to zero as the reference.
#'
#'
#' ToDo
#' ====
#'
#' Test whether the conclusion also holds with logit models (binary/ordinal
#' response models).



#######################
### Data Simulation ###
#######################
library(stom)
sim_dat = function( Int=matrix(rep(0,4), nrow=2, byrow=T) ) {
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


library(cmdstanr)
m = cmdstan_model("wine2.stan")

##########################################################
## Case 1: interaction increases score on wines of Ow=1 ##
##########################################################
# [ Ow --> W ]
#  Ow=1: positive effect (increase latent score)
#  Ow=2: negative effect (decrease latent score)
#
# [Int]
#        Ow=1 Ow=2
#  Oj=1    +    0
#  Oj=2    +    0
##########################################################
# I11 = 1.5
(Int = matrix( c( 1.5, 0,
                  0,   0), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f1 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s1 = f1$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s1 = as.data.frame(s1) )
plot( Int, s1[grepl("Int", s1$variable), "mean"] ); abline(0, 1)


# I21 = 1.5
(Int = matrix( c( 0,   0,
                  1.5, 0), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f2 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s2 = f2$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s2 = as.data.frame(s2) )
plot( Int, s2[grepl("Int", s2$variable), "mean"] ); abline(0, 1)


# I11, I21 = 1.5
(Int = matrix( c( 1.5, 0,
                  1.5, 0), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f3 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s3 = f3$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s3 = as.data.frame(s3) )
plot( Int, s3[grepl("Int", s3$variable), "mean"] ); abline(0, 1)



##########################################################
## Case 2: interaction decreases score on wines of Ow=2 ##
##########################################################
# [ Ow --> W ]
#  Ow=1: positive effect (increase latent score)
#  Ow=2: negative effect (decrease latent score)
#
# [Int]
#        Ow=1 Ow=2
#  Oj=1    0    -
#  Oj=2    0    -
##########################################################
# I12 = -1.5
(Int = matrix( c( 0, -1.5,
                  0,   0), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f4 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s4 = f4$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s4 = as.data.frame(s4) )
plot( Int, s4[grepl("Int", s4$variable), "mean"] ); abline(0, 1)


# I22 = -1.5
(Int = matrix( c( 0,   0,
                  0, -1.5), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f5 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s5 = f5$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s5 = as.data.frame(s5) )
plot( Int, s5[grepl("Int", s5$variable), "mean"] ); abline(0, 1)


# I12, I22 = -1.5
(Int = matrix( c( 0, -1.5,
                  0, -1.5), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f6 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s6 = f6$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s6 = as.data.frame(s6) )
plot( Int, s6[grepl("Int", s6$variable), "mean"] ); abline(0, 1)
# plot( dat$W.ori, s6[grepl("W", s6$variable), "mean"] ); abline(0, 1)



######################################################################
## Case 3: interaction acts in same direction as Ow on wine quality ##
######################################################################
# [ Ow --> W ]
#  Ow=1: positive effect (increase latent score)
#  Ow=2: negative effect (decrease latent score)
#
# [Int]
#        Ow=1 Ow=2
#  Oj=1    +    -
#  Oj=2    +    -
#####################################################################
(Int = matrix( c( 1.5,  0,
                  0, -1.5), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f7 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s7 = f7$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s7 = as.data.frame(s7) )
plot( Int, s7[grepl("Int", s7$variable), "mean"] ); abline(0, 1)
plot( dat$W.ori, s7[grepl("W", s6$variable), "mean"] ); abline(0, 1)

df = s7[grepl("W", s6$variable), ]
plot( dat$W.ori, pch=19, ylim=c(-3, 3) )
for ( i in 1:nrow(df) )
  lines( x=c(i,i), y=c(df[i,"q5"], df[i,"q95"]), col=col.alpha(2), lwd=3 )
points( df$mean, col=2 )



############# Normal Link ################
m =  cmdstan_model("wine2_normal.stan")
##########################################

#####################################################################
## Case 4: interaction act in same direction as Ow on wine quality ##
## Normal link function #############################################
#####################################################################
# [ Ow --> W ]
#  Ow=1: positive effect (increase latent score)
#  Ow=2: negative effect (decrease latent score)
#
# [Int]
#        Ow=1 Ow=2
#  Oj=1    +    -
#  Oj=2    +    -
#####################################################################
(Int = matrix( c( 0, -1.5,
                  0,    0), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f8 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s8 = f8$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s8 = as.data.frame(s8) )
plot( Int, s8[grepl("Int", s8$variable), "mean"] ); abline(0, 1)
plot( dat$W.ori, s8[grepl("W", s8$variable), "mean"] ); abline(0, 1)
plot( dat$J.ori, s8[grepl("J", s8$variable), "mean"] ); abline(0, 1)


##########################################################
## Case 5: interaction increases score on wines of Ow=1 ##
## Normal link function ##################################
##########################################################
# [ Ow --> W ]
#  Ow=1: positive effect (increase latent score)
#  Ow=2: negative effect (decrease latent score)
#
# [Int]
#        Ow=1 Ow=2
#  Oj=1    +    0
#  Oj=2    0    0
##########################################################
(Int = matrix( c( 1.3,  1,
                  -1,  0), nrow=2, byrow=T ) )
dat = sim_dat(Int)
f9 = m$sample(data=dat, seed=123, chains=4, parallel_chains=4, refresh=500)
s9 = f9$summary(variables=c("W", "J", "a", "Int"), "mean", "sd", "quantile2", "rhat", "ess_bulk")
( s9 = as.data.frame(s9) )
plot( Int, s9[grepl("Int", s9$variable), "mean"] ); abline(0, 1)
plot( dat$W.ori, s9[grepl("W", s9$variable), "mean"] ); abline(0, 1)
# plot( dat$J.ori, s9[grepl("J", s9$variable), "mean"] ); abline(0, 1)
