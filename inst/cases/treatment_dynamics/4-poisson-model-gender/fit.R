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







##### ToDo: recode posterior prediction with GENDER effect ######








source("simulation.R")
s = stom::precis(m, 5)
############ Subject-level Posterior Predictions #############
post = stom::extract2(m)
# E = as_posterior_array( post, "E" )
# V_subj = as_posterior_array( post, "V_subj" )
# B_TD = as_posterior_array( post, "B_TD" )
# B_TE = as_posterior_array( post, "B_TE" )


empirical_obs = function(Sid, time=1:4) {
    d$others$D[Sid, time]
}
predict_obs = function(Sid, Tid=NULL, A=NULL, time=1:4, idx=1) {
    if ( is.null(Tid) )
        Tid = d$dat$Tx[ d$dat$Sid_O == Sid ][1]
    if ( is.null(A) )
        A = d$dat$A[ d$dat$Sid_O == Sid ][1]

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

    E = sapply( time, function(t) {
        mu = delta + (V[Sid,2] + V[Sid,1]*(t-1) ) + b_AE*A  + b_TE[Tid]*(t-1)
        rnorm( 1, mu, sigma_ET )
    })
    D = sapply( time, function(t) {
        mu = alpha + (V[Sid,4] + V[Sid,3]*(t-1) ) + b_TD[Tid]*(t-1) + b_AD*A + b_ED*E[t]
        # rnorm(1, mu, sigma_D )
        rpois( 1, lambda=1.35^(-mu) )
    })
    D
}
plot_model_prediction = function(Sid=85) {
    obs = empirical_obs(Sid)
    pred = sapply( sample(1:3000, 30), function(i) predict_obs(Sid=Sid, idx=i) )
    # Compute posterior means with all samples
    pred2 = sapply( 1:3000, function(i) predict_obs(Sid=Sid, idx=i) )
    pred2 = apply(pred2, 1, function(x)
                            c( mean(x), quantile(x,.05), quantile(x,.95) )
                  ) |> t()
    ylim = max( abs(c(pred2,pred)) )
    plot( 1, type="n", xlim=c(1,4), ylim=c(-ylim,ylim),
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
            lines( c(t,t+1), c(pred[t,s],pred[t+1,s]), col=col.alpha(2,.1) )
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





sigma = get_pars(s, "sigma_subj")$mean
Rho = get_pars(s, "Rho")$mean |> matrix(nrow=4,ncol=4)
diag(sigma) %*% Rho %*% diag(sigma)

####### Compute Estimands (treatment comparisons) #########

source("simulation.R")
d = sim_data(outcome="pois", seed = 1999)
m = readRDS("m2.RDS")
s = stom::precis(m, 5)
post = stom::extract2(m)

# generates to unseen population (simulate subjects from Rho)
sim_V_subj = function(Ns, N=3000) {
    S = matrix(0, nrow=4, ncol=4)
    for ( i in 1:N ) {
        Rho        = post$Rho(i)
        sigma_subj = post$sigma_subj(i)
        S = S + diag(sigma_subj) %*% Rho %*% diag(sigma_subj)
    }
    S = S / N
    V = mvrnorm( Ns, mu=rep(0,4), Sigma=S )
    colnames(V) = c('G_TE', 'Delta_TE', 'G_TD', 'Alpha_TD')
    V
}
predict_new = function(Tid, V, Age=20, time=1:4, idx=1, latent=F) {
    A = ( Age - d$other$minA ) / 10

    alpha   = post$alpha(idx)
    b_AD    = post$B_AD(idx)
    b_ED    = post$B_ED(idx)

    delta = post$delta(idx)
    b_AE = post$B_AE(idx)
    sigma_ET = post$sigma_ET(idx)

    b_TD = post$B_TD(idx)
    b_TE = post$B_TE(idx)


    E = sapply( time, function(t) {
        muE = delta + (V[2] + V[1]*(t-1) ) + b_AE*A + b_TE[Tid]*(t-1)
        rnorm( 1, muE, sigma_ET )
    })

    D = sapply( time, function(t) {
        muD = alpha + (V[4] + V[3]*(t-1) ) + b_TD[Tid]*(t-1) + b_AD*A + b_ED*E[t]
        if (latent) return(muD)
        rpois( 1, lambda=1.35^(-muD) )
    })
    D
}


set.seed(9878)
N_distr = 3000
age_y = rep(20:29, each=N_distr/10)
V_Subj = sim_V_subj(Ns=N_distr)

# Outcome (count) scale
T1 = sapply( 1:N_distr, function(i) {
    age = age_y[i]
    V = V_Subj[i, ]
    pred =  predict_new(Tid=1, V=V, Age=age, idx = i)
    pred[4] - pred[1]
})

T2 = sapply( 1:N_distr, function(i) {
    age = age_y[i]
    V = V_Subj[i, ]
    pred =  predict_new(Tid=2, V=V, Age=age, idx = i)
    pred[4] - pred[1]
})

T3 = sapply( 1:N_distr, function(i) {
    age = age_y[i]
    V = V_Subj[i, ]
    pred =  predict_new(Tid=3, V=V, Age=age, idx = i)
    pred[4] - pred[1]
})

rethinking::dens(T1, adj=1.8, ylim=c(0, .2), xlim=c(-15, 45))
rethinking::dens(T2, adj=1.8, add=T, col=4)
rethinking::dens(T3, adj=1.8, add=T, col=2)

mean( T1 < 0 )
mean( T2 < 0 )
mean( T3 < 0 )
mean( T1[T1 < 0] )
mean( T2[T2 < 0] )
mean( T3[T3 < 0] )


# Latent scale
T1_L = sapply( 1:N_distr, function(i) {
    age = age_y[i]
    V = V_Subj[i, ]
    pred =  predict_new(Tid=1, V=V, Age=age, idx = i, latent = T)
    pred[4] - pred[1]
})

T2_L = sapply( 1:N_distr, function(i) {
    age = age_y[i]
    V = V_Subj[i, ]
    pred =  predict_new(Tid=2, V=V, Age=age, idx = i, latent = T)
    pred[4] - pred[1]
})

T3_L = sapply( 1:N_distr, function(i) {
    age = age_y[i]
    V = V_Subj[i, ]
    pred =  predict_new(Tid=3, V=V, Age=age, idx = i, latent = T)
    pred[4] - pred[1]
})


rethinking::dens(T1_L, adj=.8, xlim=c(-20, 30))
rethinking::dens(T2_L, adj=.8, add=T, col=4)
rethinking::dens(T3_L, adj=.8, add=T, col=2)
