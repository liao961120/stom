library(stom)
source("simulation.R")

d = sim_data()
m = stan( "m1.stan", data=d$dat, chains=3, parallel_chains=3 )
save_model(m, fp = "m1-wider_Item_difficulty.RDS")


s = stom::precis(m, 5)
####### Check IRT params recovery ########
for ( p in c("E", "I", "kappa") ) {
    mt = d$params[[p]]
    mm = get_pars(s, p)$mean
    u = get_pars(s, p)$q95
    l = get_pars(s, p)$q5
    plot( mt, mm , main=p, ylim=c(-8,9) )
    for ( i in seq_along(mm) )
        lines( c(mt[i],mt[i]), c(u[i],l[i]), lwd=3, col=col.alpha(2) )
    abline(0,1, lty="dashed", col="grey")
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


######## Check Beta params recovery #########
beta = c( "B_TE", "B_AE", "B_AD", "B_ED", "B_TD" )
b_true = lapply( beta, function(p) d$params[[p]] ) |> unlist()
b_est = lapply( beta, function(p) get_pars(s, p)$mean ) |> unlist()
b_est_upp = lapply( beta, function(p) get_pars(s, p)$q5 ) |> unlist()
b_est_low = lapply( beta, function(p) get_pars(s, p)$q95 ) |> unlist()
# b_true = c(b_true, rep(0,3) )  # no B_TD effect

plot( b_true, pch=19, ylim=c(-2, 4) )
abline(h = 0, lty="dashed", col="grey")
points( b_est, col=2 )
for ( i in seq_along(b_est) )
    lines( c(i,i), c(b_est_upp[i],b_est_low[i]), lwd=4, col=col.alpha(2) )
for ( v in seq(3.5, 6.5, by=1) )
    abline( v=v, lty="dashed", col="grey" )
for ( v in 4:6 )
    mtext( beta[v-2], at=v )
mtext( beta[1], at=2 )
mtext( beta[5], at=8 )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


############ Check subject varying effects recovery #############
Rho_est = stom:::bracket_to_array(get_pars(s, "Rho")$variable,
                        get_pars(s, "Rho")$mean)
Rho_true = d$params$subj$Rho

pars = d$params$subj$V_subj |> colnames()
V_subj_mean = sapply( pars, function(p) get_pars(s,p)$mean )
V_subj_upp = sapply( pars, function(p) get_pars(s,p)$q95 )
V_subj_low = sapply( pars, function(p) get_pars(s,p)$q5 )
V_subj_true = d$params$subj$V_subj

x = c( V_subj_upp, V_subj_low, V_subj_true )
for ( p in pars ) {
    plot( 1, type="n", ylim=c(min(x), max(x)), xlim=c(0,90),
          xlab = "Subject Effect", main=p )
    abline( h=0, lty="dashed" )
    for ( i in 1:90 )
        lines( c(i,i), c(V_subj_upp[i,p],V_subj_low[i,p]), lwd=3, col=col.alpha(2) )
    points( V_subj_true[,p], pch=19 )
    points( V_subj_mean[,p], col=2 )
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



library(stom)
source("simulation.R")
d = sim_data()
m = readRDS("m1-wider_Item_difficulty.RDS")
s = stom::precis(m, 5)
############ Subject-level Posterior Predictions #############
post = stom::extract(m)
E = as_posterior_array( post, "E" )
V_subj = as_posterior_array( post, "V_subj" )
B_TD = as_posterior_array( post, "B_TD" )
B_TE = as_posterior_array( post, "B_TE" )


empirical_obs = function(Sid, time=1:4) {
    d$others$D[Sid, time]
}
predict_obs = function(Sid, Tid=NULL, A=NULL, time=1:4, idx=1) {
    # # counter_factual = F
    # if ( !is.null(Tid) || !is.null(A) )
    #     counter_factual = T
    if ( is.null(Tid) )
        Tid = d$dat$Tx[ d$dat$Sid_O == Sid ][1]
    if ( is.null(A) )
        A = d$dat$A[ d$dat$Sid_O == Sid ][1]

    # Posterior sampling (parameters)
    alpha = post$alpha[idx]
    b_AD = post$B_AD[idx]
    b_ED = post$B_ED[idx]
    sigma_D = post$sigma_D[idx]

    delta = post$delta[idx]
    b_AE = post$B_AE[idx]
    sigma_ET = post$sigma_ET

    V = V_subj[[idx]]
    b_TD = B_TD[[idx]]
    b_TE = B_TE[[idx]]

    E = sapply( time, function(t) {
        mu = delta + (V[Sid,2] + V[Sid,1]*(t-1) ) + b_AE*A  + b_TE[Tid]*(t-1)
        rnorm( 1, mu, sigma_ET )
    })

    D = sapply( time, function(t) {
        mu = alpha + (V[Sid,4] + V[Sid,3]*(t-1) ) + b_TD[Tid]*(t-1) + b_AD*A + b_ED*E[t]
        rnorm(1, mu, sigma_D )
    })
    D
}
plot_model_prediction = function(Sid=85) {
    obs = empirical_obs(Sid)
    pred = sapply( sample(1:3000, 65), function(i) predict_obs(Sid=Sid, idx=i) )
    # Compute posterior means with all samples
    pred2 = sapply( 1:3000, function(i) predict_obs(Sid=Sid, idx=i) )
    pred2 = apply(pred2, 1, function(x)
                            c( mean(x), quantile(x,.05), quantile(x,.95) )
                  ) |> t()
    plot( 1, type="n", xlim=c(1,4), ylim=c(-15,15),
          xlab = "Time", ylab = "Treatment Outcome",
          main = paste("Subject",Sid,"model predictions") )
    # Empirical curve
    for ( t in 1:3 )
        lines( c(t,t+1), c(obs[t],obs[t+1]), lwd=2 )
    # Posterior mean curve
    for ( t in 1:3 )
        lines( c(t,t+1), c(pred2[t,1],pred2[t+1,1]), lwd=2, col=2 )
    # Bars of uncertainty around posterior mean
    for ( t in 1:4 )
        lines( c(t,t), c(pred2[t,2],pred2[t,3]), lwd=3, col=col.alpha(2,.4) )
    # Draw several posterior samples
    for ( s in 1:ncol(pred) ) {
        for ( t in 1:3 )
            lines( c(t,t+1), c(pred[t,s],pred[t+1,s]), col=col.alpha(2,.15) )
    }
}
set.seed(250)
plot_model_prediction(Sid=85)  # Treatment 3
set.seed(250)
plot_model_prediction(Sid=85)  # Treatment 3
plot_model_prediction(Sid=67)  # Treatment 3
plot_model_prediction(Sid=55)  # Treatment 2
plot_model_prediction(Sid=33)  # Treatment 2
plot_model_prediction(Sid=27)  # Treatment 1
plot_model_prediction(Sid=15)  # Treatment 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

