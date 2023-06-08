# ToDo: extract chains to proof non-identifiablility

library(stom)
source("simulation.R")
set.seed(1977)
d = sim_data( alpha=0,
              delta=-1.8,
              sigma_ET = .2,
              sigma_subj = .5 )

m = readRDS("m1-ncp.RDS")

s = stom::precis(m, 5)


d1 = m$draws()[,1,] |> posterior::as_draws_df()
d2 = m$draws()[,2:3,]|> posterior::as_draws_df()
s1 = apply( d1, 2, function(c_) c(mean = mean(c_), q5=quantile(c_,.05), q95=quantile(c_, .95)) )
s2 = apply( d2, 2, function(c_) c(mean=mean(c_), q5=quantile(c_,.05), q95=quantile(c_, .95)) )

######## Check Beta params recovery #########
beta = c( "B_TE", "B_AE", "B_AD", "B_ED", "B_TD", "delta", "alpha", "sigma_ET" )
pars = colnames(d1)
pars = pars[grepl(paste(beta,collapse = "|"), pars)]

b_true = lapply( beta, function(p) d$params[[p]] ) |> unlist()
b_est = lapply( pars, function(p) s1["mean",p] ) |> unlist()
b_est2 = lapply( pars, function(p) s2["mean",p] ) |> unlist()


# b_est_upp = lapply( beta, function(p) s1["q5",p] ) |> unlist()
# b_est_low = lapply( beta, function(p) s1["q95",p] ) |> unlist()

plot( b_true, pch=19, ylim=c(-4.8, 4.8) )
abline(h = 0, lty="dashed", col="grey")
points( b_est, col=2, lwd=2 )
points( b_est2, col=col.alpha(4), lwd=2 )
# for ( i in seq_along(b_est) )
#     lines( c(i,i), c(b_est_upp[i],b_est_low[i]), lwd=4, col=col.alpha(2) )
for ( v in c(6:9,12) )
    abline( v=v+.5, lty="dashed", col="grey" )
for ( v in 7:9 )
    mtext( beta[v-5], at=v )
mtext( beta[1], at = length(get_pars(d,"B_TE")) * .5 )
mtext( beta[5], at = 11 )
mtext( beta[6], at = 13 )
mtext( beta[7], at = 14 )
mtext( beta[8], at = 15 )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%





library(bayesplot)
color_scheme_set("viridis")
pars = stom:::parse_pars("alpha,delta,B_TD,B_ED,B_AE,B_AD,B_TE,sigma_ET")
mcmc_trace(m$draws()[,,], regex_pars = pars, facet_args = list(ncol=3))