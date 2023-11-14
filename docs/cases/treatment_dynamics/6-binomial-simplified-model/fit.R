library(stom)
source("simulation.R")
set.seed(1977)
d = sim_data( alpha=0,
              delta=-1.8,
              sigma_ET = .2,
              sigma_subj = .5,
              subj_eff = F)
m = stan( "m1-ncp.stan", data=d$dat,
          chains=3, parallel_chains=3,
          # seed = 2038786619,
          init = NULL,        # Initial values for parameters
          #adapt_delta = .99, # default: .8 (larger results to smaller step sizes)
          step_size = NULL    # initial step size (default:1)
        )
save_model(m)
# m = readRDS("m1.RDS")

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
beta = c( "B_TE", "B_AE", "B_AD", "B_ED", "B_TD", "delta", "alpha", "sigma_ET" )
b_true = lapply( beta, function(p) d$params[[p]] ) |> unlist()
b_est = lapply( beta, function(p) get_pars(s, p)$mean ) |> unlist()
b_est_upp = lapply( beta, function(p) get_pars(s, p)$q5 ) |> unlist()
b_est_low = lapply( beta, function(p) get_pars(s, p)$q95 ) |> unlist()

plot( b_true, pch=19, ylim=c(-4.8, 4.8) )
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
mtext( beta[7], at = 14 )
mtext( beta[8], at = 15 )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Subject intercept
E_subj = get_pars(s, "E_subj")

plot(1, type="n", xlim=c(-3,3), ylim=c(-3,3),
     xlab="Estimate", ylab="True")
abline(0,1, col="grey")
points( E_subj$mean, d$params$E_subj, col=2)
for ( i in 1:d$dat$Ns ) {
    m_t = d$params$E_subj[i]
    q5 = E_subj$q5[i]
    q95 = E_subj$q95[i]
    lines( c(q5,q95), c(m_t,m_t), lwd=4, col=col.alpha(2,.3) )
}



############ Posterior Predictions #############
post = stom::extract2(m)

empirical_obs = function(Tid, G, sample_n=3) {
    Sids = d$dat$Sid_O[ d$dat$Tx == Tid & d$dat$G == G ]
    Sids = sample(unique(Sids), size=sample_n)
    dat = sapply( Sids, function(sid) {
        x = lapply( 1:4 , function(t) {
            cbind(t, d$others$D[sid, t] )
        })
        Reduce( rbind, x )
    }, simplify = "array")
    dimnames(dat)[[3]] = Sids
    dat
}
predict_obs = function(Tid, A, G, idx) {
    alpha    = post$alpha(idx)
    delta    = post$delta(idx)
    b_AD     = post$B_AD(idx)
    b_ED     = post$B_ED(idx)
    b_AE     = post$B_AE(idx)
    b_TD     = post$B_TD(idx)
    b_TE     = post$B_TE(idx)
    sigma_ET = post$sigma_ET(idx)

    time = seq( 1, 4, by=.01 )
    D = sapply( time, function(t) {
        muE = delta + b_AE*A + b_TE[G,Tid]*(t-1)
        E = rnorm( 1, muE, sigma_ET )
        alpha + b_TD[Tid]*(t-1) + b_AD*A + b_ED*E
    })
    D
}
predict_over_post = function(Tid, A, G) {
    # Compute posterior means with all samples
    pred = sapply( 1:3000, function(i) {
        D = predict_obs(Tid=Tid, G=G, A=A[i], idx=i)
        14 * logistic(-D)  # expected counts
    })
    pred2 = apply(pred, 1, function(x)
        c( mean(x), quantile(x,.05), quantile(x,.95) )
    )
    return(list(
        pred = pred,
        summ = pred2
    ))
}

plot_model_prediction = function(Tid, G, A=NULL, emp_sample_n=NULL,
                                 uncertainty = T, empirical = T,
                                 add=FALSE, col_main=2) {
    idx_subj = d$dat$G == G & d$dat$Tx == Tid
    if ( is.null(A) )
        A = d$dat$A[idx_subj]
    if ( length(A) < 3000 )
        A = cbind(1:3000, A)[,2]
    if ( is.null(emp_sample_n) )
        emp_sample_n = length( unique(d$dat$Sid_O[idx_subj]) )

    # Compute posterior means with all samples
    post_pred = predict_over_post(Tid=Tid, G=G, A=A)
    pred2 = post_pred$summ

    time = seq(1,4,.01)  # time sequence for smooth posterior mean prediction
    obs = empirical_obs( Tid=Tid, G=G, sample_n=emp_sample_n )  # draw empirical samples

    if (!add)
        plot( 1, type="n", xlim=c(1,4), ylim=c(0, 14),
              xlab = "Time", ylab = "Days of heavy drinking",
        )
    if (uncertainty)
        shade( pred2[-1,], time, col=col.alpha(2,.1) )  # 95% posterior samples
    if (empirical) {
        for ( s in 1:dim(obs)[3] ) {
            j = runif(2, -.16, .16)
            obs_ = obs[,,s]
            for ( t in 1:3 ) {
                lines( c(t,t+1), c(obs_[t,2],obs_[t+1,2])+j, lwd=1, col=col.alpha("grey",.3) )
                points( c(t,t+1), c(obs_[t,2],obs_[t+1,2])+j, col="grey", cex=.5 )
            }
        }
    }
    lines( time, pred2[1,], lwd=3, col=col_main )  # Posterior mean curve
}

# Prediction from recruited subjects
set.seed(250)
plot_model_prediction(Tid=1, G=1)  # Treatment 3
plot_model_prediction(Tid=1, G=2)  # Treatment 3
plot_model_prediction(Tid=2, G=1)  # Treatment 2
plot_model_prediction(Tid=2, G=2)  # Treatment 2
plot_model_prediction(Tid=3, G=1)  # Treatment 1
plot_model_prediction(Tid=3, G=2)  # Treatment 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


plot_model_prediction(Tid=1, G=1, col_main = 4, uncertainty = F, empirical = F)  # Treatment 3
plot_model_prediction(Tid=1, G=2, add = T, uncertainty = F, empirical = F)














######## Treatment mean curve  #############
# shade: covering 68% of subject trajectories
#        (generates to unseen population)
source("utils.R")
predict_treatment_same_subj = function(Tid, G, V, Age) {
    mat = sapply( 1:3000, function(idx) {
        mu = predict_new(Tid = Tid, V = V[idx,], G = G, Age = Age[idx], time = seq(1,4,by=.05))
        14 * logistic(-mu)
    })
    apply( mat, 1, function(r) c(mean(r), quantile(r,.16), quantile(r,.84)) )
}

Age = rtnorm(3000, 60, s=10, lower=40, upper=80)
V = sim_V_subj(3000)
T1G1_ = predict_treatment_same_subj(Tid=1, G=1, V=V, Age=Age)
T1G2_ = predict_treatment_same_subj(Tid=1, G=2, V=V, Age=Age)
T2G1_ = predict_treatment_same_subj(Tid=2, G=1, V=V, Age=Age)
T2G2_ = predict_treatment_same_subj(Tid=2, G=2, V=V, Age=Age)
T3G1_ = predict_treatment_same_subj(Tid=3, G=1, V=V, Age=Age)
T3G2_ = predict_treatment_same_subj(Tid=3, G=2, V=V, Age=Age)

time = seq(1,4,by=.05)
plot( 1, type="n", xlim=c(1,4), ylim=c(0,14) )
abline(h=c(0,14), col="grey", lty="dashed")
lines( time, T1G1_[1,], col=6, lwd=1, lty=1  )
lines( time, T1G2_[1,], col=6, lwd=1, lty=2  )
lines( time, T2G1_[1,], col=4, lwd=1, lty=1  )
lines( time, T2G2_[1,], col=4, lwd=1, lty=2  )
lines( time, T3G1_[1,], col=1, lwd=1, lty=1  )
lines( time, T3G2_[1,], col=1, lwd=1, lty=2  )

rethinking::shade( T1G2_[-1,], time, col=col.alpha(6,.05), xpd=TRUE )
rethinking::shade( T1G1_[-1,], time, col=col.alpha(6,.1), xpd=TRUE )
rethinking::shade( T2G1_[-1,], time, col=col.alpha(4,.1), xpd=TRUE )
rethinking::shade( T2G2_[-1,], time, col=col.alpha(4,.05), xpd=TRUE )
rethinking::shade( T3G1_[-1,], time, col=col.alpha(1,.1), xpd=TRUE )
rethinking::shade( T3G2_[-1,], time, col=col.alpha(1,.05), xpd=TRUE )






######### Treatment mean curve ##############
#### shade: covering 95% of treatment mean variation
predict_treatment = function(Tid, G, idx=1) {
    idx_subj = ( d$dat$Tx == Tid ) & ( d$dat$G == G )
    Sids = unique( d$dat$Sid_O[idx_subj] )
    mat = sapply( Sids, function(sid) {
        mu = predict_obs(sid, idx=idx, time=seq(1,4,by=.05))
        14 * logistic(-mu)
    })
    apply( mat, 1, function(r) mean(r) )
}

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
plot( 1, type="n", xlim=c(1,4), ylim=c(0,14))
abline(h=c(0,14), col="grey", lty="dashed")
lines( time, T1G1[1,], col=6, lwd=1, lty=1  )
lines( time, T1G2[1,], col=6, lwd=1, lty=2  )
lines( time, T2G1[1,], col=4, lwd=1, lty=1  )
lines( time, T2G2[1,], col=4, lwd=1, lty=2  )
lines( time, T3G1[1,], col=1, lwd=1, lty=1  )
lines( time, T3G2[1,], col=1, lwd=1, lty=2  )

rethinking::shade( T1G1[-1,], time, col=col.alpha(6,.1), xpd=TRUE )
rethinking::shade( T1G2[-1,], time, col=col.alpha(6,.05), xpd=TRUE )
rethinking::shade( T2G1[-1,], time, col=col.alpha(4,.1), xpd=TRUE )
rethinking::shade( T2G2[-1,], time, col=col.alpha(4,.05), xpd=TRUE )
rethinking::shade( T3G1[-1,], time, col=col.alpha(1,.1), xpd=TRUE )
rethinking::shade( T3G2[-1,], time, col=col.alpha(1,.05), xpd=TRUE )


######## Treatment distribution comparison ###########
set.seed(9878)
N = 3000
Age = rtnorm(N, 35, s=15, lower=20, upper=80)
V_Subj = sim_V_subj(N)

treatment_eff = function(Tid, G) {
    sapply( 1:N, function(i) {
        age = Age[i]
        V = V_Subj[i, ]
        pred =  predict_new(Tid=Tid, V=V, G=G, Age=age, idx = i, time = c(1,4) )
        pred = 14 * logistic(-pred)
        pred[2] - pred[1]  # difference in expected drinks pre/post treatment
    })
}

# Outcome (count) scale
T1G1_eff = treatment_eff(Tid=1, G=1)
T1G2_eff = treatment_eff(Tid=1, G=2)
T2G1_eff = treatment_eff(Tid=2, G=1)
T2G2_eff = treatment_eff(Tid=2, G=2)
T3G1_eff = treatment_eff(Tid=3, G=1)
T3G2_eff = treatment_eff(Tid=3, G=2)
T1_eff = c(T1G1_eff, T1G2_eff)
T2_eff = c(T2G1_eff, T2G2_eff)
T3_eff = c(T3G1_eff, T3G2_eff)

# Collapse gender
rethinking::dens(T1_eff, adj=1, ylim=c(0, .15), xlim=c(-15, 15), col=6)
rethinking::dens(T2_eff, adj=1, add=T, col=4)
rethinking::dens(T3_eff, adj=1, add=T, col=2)

# Separate gender
rethinking::dens(T2G1_eff, adj=1, ylim=c(0, .15), xlim=c(-15, 15), col=4, lwd=2)
rethinking::dens(T3G1_eff, adj=1, add=T, col=2, lwd=2)
rethinking::dens(T2G2_eff, adj=1, add=T, col=4, lty="dashed", lwd=2)
rethinking::dens(T3G2_eff, adj=1, add=T, col=2, lty="dashed", lwd=2)
legend(7, .152,
       legend = c("Tx: 2, Male",
                  "Tx: 2, Female",
                  "Tx: 3, Male",
                  "Tx: 3, Female"),
       col    =c(4,4,2,2),
       lty    =c(1,2,1,2),
       lwd    =2)
