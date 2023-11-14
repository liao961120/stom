library(stom)
source("simulation.R")

d = sim_data(outcome="pois", alpha=-1, seed = 777)
# m = stan( "m2.stan", data=d$dat, chains=3, parallel_chains=3 )
# save_model(m)
m = readRDS("m2.RDS")

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
beta = c( "B_TE", "B_AE", "B_AD", "B_ED", "B_TD", "alpha" )
b_true = lapply( beta, function(p) d$params[[p]] ) |> unlist()
b_est = lapply( beta, function(p) get_pars(s, p)$mean ) |> unlist()
b_est_upp = lapply( beta, function(p) get_pars(s, p)$q5 ) |> unlist()
b_est_low = lapply( beta, function(p) get_pars(s, p)$q95 ) |> unlist()

plot( b_true, pch=19, ylim=c(-1.6, 1.6) )
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
mtext( beta[6], at = 13 )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


############ Check subject varying effects recovery #############
Rho_est = erect(s, "Rho")
Rho_true = d$params$subj$Rho

pars = d$params$subj$V_subj |> colnames()
V_subj_mean = erect(s, "V_subj", "mean")
V_subj_upp = erect(s, "V_subj", "q95")
V_subj_low = erect(s, "V_subj", "q5")
V_subj_true = d$params$subj$V_subj

x = c( V_subj_upp, V_subj_low, V_subj_true )
for ( j in seq_along(pars) ) {
    p = pars[j]
    plot( 1, type="n", ylim=c(min(x), max(x)), xlim=c(0,nrow(V_subj_true)),
          xlab = "Subject Effect", main=p )
    abline( h=0, lty="dashed" )
    for ( i in 1:nrow(V_subj_true) )
        lines( c(i,i), c(V_subj_upp[i,j],V_subj_low[i,j]), lwd=3, col=col.alpha(2) )
    points( V_subj_true[,j], pch=19 )
    points( V_subj_mean[,j], col=2 )
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%










source("simulation.R")
m = readRDS("m2.RDS")
d = sim_data(outcome="pois", alpha=-1, seed = 777)
s = stom::precis(m, 5)
############ Subject-level Posterior Predictions #############
post = stom::extract2(m)

empirical_obs = function(Sid, time=1:4) {
    d$others$D[Sid, time]
}
empirical_obs_treatment = function(Tid, G) {
    idx_subj = ( d$dat$Tx == Tid ) & ( d$dat$G == G )
    Sids = unique( d$dat$Sid_O[idx_subj] )
    mat = sapply( Sids, function(sid) {
        empirical_obs(sid) + rnorm(4, sd=.05)
    })
    lx = ncol(mat)
    flatten = lapply( 1:4, function(t) {
        x = rep(t, lx)
        y = mat[t,]
        cbind(x, y)
    })
    Reduce(rbind, flatten)
}
predict_obs = function(Sid, Tid=NULL, A=NULL, G=NULL, time=1:4, idx=1, latent=F) {
    idx_sid = (d$dat$Sid_O == Sid)
    if ( is.null(Tid) )
        Tid = d$dat$Tx[idx_sid][1]
    if ( is.null(G) )
        G = d$dat$G[idx_sid][1]
    if ( is.null(A) ) {
        A = d$dat$A[idx_sid][1]
    } else {
        A = (A - d$others$minA) / 10
    }

    # Posterior sampling (parameters)
    alpha   = post$alpha(idx)
    b_AD    = post$B_AD(idx)
    b_ED    = post$B_ED(idx)
    # sigma_D = post$sigma_D(idx)

    delta = post$delta(idx)
    b_AE = post$B_AE(idx)
    sigma_ET = post$sigma_ET(idx)

    V = post$V_subj(idx)
    b_TD = post$B_TD(idx)
    b_TE = post$B_TE(idx)

    D = sapply( time, function(t) {
        muE = delta + (V[Sid,2] + V[Sid,1]*(t-1) ) + b_AE*A + b_TE[G,Tid]*(t-1)
        E = rnorm( 1, muE, sigma_ET )
        muD = alpha + (V[Sid,4] + V[Sid,3]*(t-1) ) + b_TD[Tid]*(t-1) + b_AD*A + b_ED*E
        muD
    })
    if (!latent)
        D = rpois( length(time), lambda = 1.35^(-D) )
    D
}
j = function(jt=.1) {
    function() rnorm(1, mean=0, sd=jt)
}
plot_model_prediction = function(Sid=85, jt=.05) {
    j = j(jt)
    obs = empirical_obs(Sid)
    pred = sapply( sample(1:3000, 30), function(i) predict_obs(Sid=Sid, idx=i) )
    # Compute posterior means with all samples
    pred2 = sapply( 1:3000, function(i) predict_obs(Sid=Sid, idx=i) )
    pred2 = apply(pred2, 1, function(x)
                            c( mean(x), quantile(x,.05), quantile(x,.95) )
                  ) |> t()
    y = c(pred2,pred)
    plot( 1, type="n", xlim=c(1,4), ylim=c(min(y), max(y)),
          xlab = "Time", ylab = "Treatment Outcome\n(Num. of heavy drinks)",
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
            lines( c(t,t+1)+j(), c(pred[t,s],pred[t+1,s])+j(), col=col.alpha(2,.1) )
    }
}
set.seed(250)
par(mar=c(5, 6, 4, 2) + 0.1)
plot_model_prediction(Sid=73)  # Treatment 3
plot_model_prediction(Sid=61)  # Treatment 3
plot_model_prediction(Sid=53)  # Treatment 2
plot_model_prediction(Sid=31)  # Treatment 2
plot_model_prediction(Sid=27)  # Treatment 1
plot_model_prediction(Sid=15)  # Treatment 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



##### Counter factual prediction #############
#' The simulation below first simulates 3000 subjects from the
#' posterior. These subjects are then used for counter factual simulations
#' on the treatment-gender conditions. The "counter factual" here are the
#' manipulation of of subject's gender and treatment condition for each
#' simulation.
#'
#' @CurrentProblem
#' The simulation here assigned too large variation to the subjects, resulting
#' in some observations that could even reverse the treatment main effect. This
#' led to posterior predictive distributions to have very large variations such
#' that the treatment conditions cannot be reliably differentiated on the
#' outcome scale.
source("utils.R")
predict_treatment = function(Tid, G, idx=1) {
    idx_subj = ( d$dat$Tx == Tid ) & ( d$dat$G == G )
    Sids = unique( d$dat$Sid_O[idx_subj] )
    mat = sapply( Sids, function(sid) {
        mu = predict_obs(sid, idx=idx, time=seq(1,4,by=.05), latent=T)
        1.35^(-mu)
    })
    apply( mat, 1, function(r) mean(r) )
}
predict_treatment_same_subj = function(Tid, G, V, Age) {
    mat = sapply( 1:3000, function(idx) {
        mu = predict_new(Tid = Tid, V = V[idx,], G = G, Age = Age[idx],
                         time = seq(1,4,by=.05), latent = T)
        1.35^(-mu)
    })
    apply( mat, 1, function(r) c(mean(r), quantile(r,.05), quantile(r,.95)) )
}

Age = rtnorm(3000, 35, s=15, lower=20, upper=80)
V = sim_V_subj(3000)
T1G1_ = predict_treatment_same_subj(Tid=1, G=1, V=V, Age=Age)
T1G2_ = predict_treatment_same_subj(Tid=1, G=2, V=V, Age=Age)
T2G1_ = predict_treatment_same_subj(Tid=2, G=1, V=V, Age=Age)
T2G2_ = predict_treatment_same_subj(Tid=2, G=2, V=V, Age=Age)
T3G1_ = predict_treatment_same_subj(Tid=3, G=1, V=V, Age=Age)
T3G2_ = predict_treatment_same_subj(Tid=3, G=2, V=V, Age=Age)

time = seq(1,4,by=.05)
y = c(T1G1_,T1G2_,T2G1_,T2G2_,T3G1_,T3G2_)
plot( 1, type="n", xlim=c(1,4), ylim=c(min(y),max(y)))
abline(h=2, col="grey", lty="dashed")
lines( time, T1G1_[1,], col=6, lwd=1, lty=1  )
lines( time, T1G2_[1,], col=6, lwd=1, lty=2  )
lines( time, T2G1_[1,], col=4, lwd=1, lty=1  )
lines( time, T2G2_[1,], col=4, lwd=1, lty=2  )
lines( time, T3G1_[1,], col=1, lwd=1, lty=1  )
lines( time, T3G2_[1,], col=1, lwd=1, lty=2  )

rethinking::shade( T1G1_[-1,], time, col=col.alpha(6,.2), xpd=TRUE )
rethinking::shade( T1G2_[-1,], time, col=col.alpha(6,.1), xpd=TRUE )
rethinking::shade( T2G1_[-1,], time, col=col.alpha(4,.2), xpd=TRUE )
rethinking::shade( T2G2_[-1,], time, col=col.alpha(4,.1), xpd=TRUE )
rethinking::shade( T3G1_[-1,], time, col=col.alpha(1,.2), xpd=TRUE )
rethinking::shade( T3G2_[-1,], time, col=col.alpha(1,.1), xpd=TRUE )








# Old simulation based on different subjects for different treatment conditions
T1G1 = sapply( 1:3000, function(i) predict_treatment(Tid=1,G=1,idx=i) ) |>
    apply(1, function(r) c( m=mean(r), quantile(r,.05), quantile(r,.95) ) )

T1G2 = sapply( 1:3000, function(i) predict_treatment(Tid=1,G=2,idx=i) ) |>
    apply(1, function(r) c( m=mean(r), quantile(r,.05), quantile(r,.95) ) )

T2G1 = sapply( 1:3000, function(i) predict_treatment(Tid=2,G=1,idx=i) ) |>
    apply(1, function(r) c( m=mean(r), quantile(r,.05), quantile(r,.95) ) )

T2G2 = sapply( 1:3000, function(i) predict_treatment(Tid=2,G=2,idx=i) ) |>
    apply(1, function(r) c( m=mean(r), quantile(r,.05), quantile(r,.95) ) )

T3G1 = sapply( 1:3000, function(i) predict_treatment(Tid=3,G=1,idx=i) ) |>
    apply(1, function(r) c( m=mean(r), quantile(r,.05), quantile(r,.95) ) )

T3G2 = sapply( 1:3000, function(i) predict_treatment(Tid=3,G=2,idx=i) ) |>
    apply(1, function(r) c( m=mean(r), quantile(r,.05), quantile(r,.95) ) )


time = seq(1,4,by=.05)
y = c(T1G1,T1G2,T2G1,T2G2,T3G1,T3G2)
plot( 1, type="n", xlim=c(1,4), ylim=c(min(y),max(y)))
abline(h=2, col="grey", lty="dashed")
lines( time, T1G1[1,], col=6, lwd=1, lty=1  )
lines( time, T1G2[1,], col=6, lwd=1, lty=2  )
lines( time, T2G1[1,], col=4, lwd=1, lty=1  )
lines( time, T2G2[1,], col=4, lwd=1, lty=2  )
lines( time, T3G1[1,], col=1, lwd=1, lty=1  )
lines( time, T3G2[1,], col=1, lwd=1, lty=2  )

rethinking::shade( T1G1[-1,], time, col=col.alpha(6,.2), xpd=TRUE )
rethinking::shade( T1G2[-1,], time, col=col.alpha(6,.1), xpd=TRUE )
rethinking::shade( T2G1[-1,], time, col=col.alpha(4,.2), xpd=TRUE )
rethinking::shade( T2G2[-1,], time, col=col.alpha(4,.1), xpd=TRUE )
rethinking::shade( T3G1[-1,], time, col=col.alpha(1,.2), xpd=TRUE )
rethinking::shade( T3G2[-1,], time, col=col.alpha(1,.1), xpd=TRUE )
