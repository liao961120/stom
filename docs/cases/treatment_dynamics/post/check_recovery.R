source("simulation.R")
set.seed(1977)
d = sim_data()
m = readRDS("m1.RDS")
s = stom::precis(m, 5)  # Compute posterior summary


###### Sampling diagnostics ##########
library(bayesplot)
color_scheme_set("viridis")

plot(s$rhat)
s[s$rhat > 1.006, ]
s[s$ess_bulk < 500, ]



pars = c( 
    #s$variable[s$rhat > 1.006],
    s$variable[s$ess_bulk < 400],
    c("delta", "alpha")
)
mcmc_trace(m$draws(), pars=pars)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


####### Check IRT params recovery ########
for ( p in c("E", "I", "kappa") ) {
    mt = d$params[[p]]        # True value
    mm = get_pars(s, p)$mean  # Posterior mean
    u = get_pars(s, p)$q95    # Posterior .05 quantile 
    l = get_pars(s, p)$q5     # Posterior .95 quantile 
    plot( mt, mm , main=p, ylim=c(-4.6,4.6),
          xlab="True", ylab="Estimated")
    for ( i in seq_along(mm) )
        lines( c(mt[i],mt[i]), c(u[i],l[i]), lwd=3, col=col.alpha(2) )
    abline(0,1, lty="dashed", col="grey")
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


######## Check Beta params recovery #########
beta = c( "B_TE", "B_AE", "B_AD", "B_ED", "B_TD", "delta", "alpha", "tau", "sigma_I" )
b_true = lapply( beta, function(p) d$params[[p]] ) |> unlist()
b_est = lapply( beta, function(p) get_pars(s, p)$mean ) |> unlist()
b_est_upp = lapply( beta, function(p) get_pars(s, p)$q5 ) |> unlist()
b_est_low = lapply( beta, function(p) get_pars(s, p)$q95 ) |> unlist()

plot( b_true, pch=19, ylim=c(-2.6, 3.2), ylab="Parameter value", xlab="" )
abline(h = 0, lty="dashed", col="grey")
points( b_est, col=2 )
for ( i in seq_along(b_est) )
    lines( c(i,i), c(b_est_upp[i],b_est_low[i]), lwd=4, col=col.alpha(2) )
for ( v in c(6:9,12) )
    abline( v=v+.5, lty="dashed", col="grey" )
for ( v in 7:9 )
    mtext( beta[v-5], at=v )
mtext( beta[1], at = length(get_pars(d,"B_TE")) * .5 )
mtext( beta[5], at = 11 )
for ( i in 6:9 )
    mtext( beta[i], at = i+7 )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
