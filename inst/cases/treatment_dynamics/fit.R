library(stom)
source("simulation.R")

d = sim_data()


#############################################################
#' Model 0: simple rating scale model
#'
#' `m0.stan` fits an ordinal response IRT model, without other
#' explanatory variables.
#'
#' @Goal
#' To test if item(I)/person(E) parameters can be recovered.
#'
#' @Model
#' R[s,i,t] ~ ordered_logistic(phi, kappa);
#' phi = I[i] + E[s,t];
#'
#' $I$ are zero-mean constrained to anchor the parameters
#'
#' @Results
#' As expected.
#'
#' @FitTime
#' 723 secs on i7/Linux when $Ns=200$ (num of subjects)
#'
#' @ToDo
#' Construct full model.
#'
#############################################################
m = stan( "m0.stan", data=d$dat )
summ = precis(m, 3)

E = summ[ startsWith(summ$variable, "E"),  ]
E = cbind(
  E[endsWith(E$variable, ",1]"), "mean"],  # baseline
  E[endsWith(E$variable, ",2]"), "mean"],  # t1
  E[endsWith(E$variable, ",3]"), "mean"],  # t2
  E[endsWith(E$variable, ",4]"), "mean"]   # t3
)
# Test Efficacy recovery
for (t in 1:4) {
  plot(E[, t], d$param$E[, t], main=t); abline(0, 1)
}







########################
#' Model 1: Full model
#'
#' @FitTime
#' 2289 secs on i7/Linux
#'
#' @Problems
#' Effects in simulation seem to be too small for model to recover
#'
#' @ToDo
#' Change treatment effects in simulation.
#' Possibly model and independent global effect
#' to resonably scale the model to realistic outcomes.
m1 = stan( "m1.stan", data=d$dat )

library(bayesplot)
post = extract(m1)
color_scheme_set("viridis")
mcmc_rank_overlay(post, regex_pars="^A[EC]|^T[EC]|^tE")

summ = precis(m1, 3)
E = summ[ startsWith(summ$variable, "E"),  ]
E = cbind(
  E[endsWith(E$variable, ",1]"), "mean"],  # baseline
  E[endsWith(E$variable, ",2]"), "mean"],  # t1
  E[endsWith(E$variable, ",3]"), "mean"],  # t2
  E[endsWith(E$variable, ",4]"), "mean"]   # t3
)
# Test Efficacy recovery
for (t in 1:4) {
  plot(E[, t], d$param$E[, t], main=t); abline(0, 1)
}

precis(m1, 4, pars=c("AE", "AC", "TE", "TC", "tE"))
