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
predict_obs = function(Tid, A, G, idx, post) {
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
predict_over_post = function(Tid, A, G, post, N=1000) {
    # Compute posterior means with all samples
    pred = sapply( 1:N, function(i) {
        D = predict_obs(Tid=Tid, G=G, A=A[i], idx=i, post=post)
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

plot_model_prediction = function(Tid, G, post, A=NULL, emp_sample_n=NULL,
                                 uncertainty = T, empirical = T,
                                 add=FALSE, col_main=2, N=1000) {
    idx_subj = d$dat$G == G & d$dat$Tx == Tid
    if ( is.null(A) )
        A = d$dat$A[idx_subj]
    if ( length(A) < N )
        A = cbind(1:N, A)[,2]
    if ( is.null(emp_sample_n) )
        emp_sample_n = length( unique(d$dat$Sid_O[idx_subj]) )

    # Compute posterior means with all samples
    post_pred = predict_over_post(Tid=Tid, G=G, A=A, post=post, N=N)
    pred2 = post_pred$summ

    time = seq(1,4,.01)  # time sequence for smooth posterior mean prediction
    obs = empirical_obs( Tid=Tid, G=G, sample_n=emp_sample_n )  # draw empirical samples

    if (!add)
        plot( 1, type="n", xlim=c(1,4), ylim=c(0, 14),
              xlab = "", ylab = ""
        )
    if (uncertainty)
        shade( pred2[-1,], time, col=col.alpha(col_main,.1) )  # 95% posterior samples
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
