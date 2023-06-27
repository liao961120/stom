library(cowplot)
library(ggplot2)
library(ggridges)

##################################################
###### Functions for treatment trajectories ######
##################################################
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
# If A/G is NULL, use subjects' demographic data
predict_obs = function(Tid, Sid, A, G, idx) {
    alpha = post$alpha(idx)
    delta = post$delta(idx)
    b_AD  = post$B_AD(idx)
    b_ED  = post$B_ED(idx)
    b_AE  = post$B_AE(idx)
    b_TD  = post$B_TD(idx)
    b_TE  = post$B_TE(idx)
    subj  = post$subj(idx)
    if (is.null(A))
        A = d$dat$A[d$dat$Sid_O == Sid][1]
    if (is.null(G))
        G = d$dat$G[d$dat$Sid_O == Sid][1]

    time = seq( 1, 4, by=.01 )
    D = sapply( time, function(t) {
        E = delta + subj[Sid] + b_AE*A + b_TE[G,Tid]*(t-1)
        alpha + b_TD[Tid]*(t-1) + b_AD*A + b_ED*E
    })
    D
}
predict_over_post = function(Tid, A, G) {
    # Even out subjects across posterior samples
    Sids = rep( 1:d$dat$Ns, length=2970 ) |> sample()
    pred = sapply( 1:2970 , function(i) {
        D = predict_obs(Tid=Tid, Sid=Sids[i], G=G, A=A[i], idx=i)
        14 * logistic(-D)  # expected counts
    })
    pred2 = apply(pred, 1, function(x) {
        c( mean(x), quantile(x,.05), quantile(x,.95) )
    })
    return(list(
        pred = pred,
        summ = pred2
    ))
}

plot_model_prediction = function(Tid, G=NULL, A=NULL, emp_sample_n=NULL,
                                 uncertainty = T, empirical = T,
                                 add=FALSE, col_main=2) {
    idx_subj = d$dat$G == G & d$dat$Tx == Tid
    if ( is.null(emp_sample_n) )
        emp_sample_n = length( unique(d$dat$Sid_O[idx_subj]) )

    # Compute posterior means with all samples
    post_pred = predict_over_post(Tid=Tid, G=G, A=A)
    pred2 = post_pred$summ

    time = seq(1,4,.01)  # time sequence for smooth posterior mean prediction
    # draw empirical samples from treatment-gender group
    obs = empirical_obs( Tid=Tid, G=G, sample_n=emp_sample_n )

    if (!add)
        plot( 1, type="n", xlim=c(1,4), ylim=c(0, 14),
              xlab = "Time", ylab = "Days of heavy drinking" #,
              #main = paste("Treatment:", Tid, "Gender:", G)
        )
    if (uncertainty)
        shade( pred2[-1,], time, col=col.alpha(col_main,.1) )  # 90% posterior samples
    if (empirical) {
        for ( s in 1:dim(obs)[3] ) {
            j = runif(1, -.16, .16)
            obs_ = obs[,,s]
            for ( t in 1:3 ) {
                lines( c(t,t+1), c(obs_[t,2],obs_[t+1,2])+j, lwd=1, col=col.alpha("grey",.3) )
                points( c(t,t+1), c(obs_[t,2],obs_[t+1,2])+j, col="grey", cex=.5 )
            }
        }
    }
    lines( time, pred2[1,], lwd=3, col=col_main )  # Posterior mean curve
}



###############################################
###### Functions for treatment contrasts ######
###############################################
treatment_contrast = function(Tid, Tid_b, G) {
    t_snapshot = c(101, 201, 301)
    T_b = cf[[Tid_b]][[G]]
    T_c = cf[[Tid]][[G]]
    contr = T_c - T_b
    contr = t( contr[t_snapshot,] )
    colnames(contr) = paste( 10*seq_along(t_snapshot), "days" )

    as.data.frame(contr) |>
        tidyr::pivot_longer(everything(), names_to = "time", values_to = "D")
}
treatment_contrast2 = function(Tid, Tid_b) {
    G1 = treatment_contrast(Tid, Tid_b, G=1)
    G2 = treatment_contrast(Tid, Tid_b, G=2)
    G1$Gender = "Male"
    G2$Gender = "Female"
    rbind(G1, G2)
}
plot_treatment_contrast_noG = function(data, title="Treatment 3 - Treatment 1") {
    ggplot( data, aes(x = D, y = time ) ) +
        geom_density_ridges( scale = 1, fill=col.alpha(1, .15) ) +
        scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
        scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
        coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
        theme_ridges() +
        xlim(-11.5, 1) +
        labs( title = title, x = "Difference in Days of Drinking" )
}
plot_treatment_contrast = function(data, title="Treatment 3 - Treatment 1") {
    ggplot( data, aes(x = D, y = time, fill=Gender, color=Gender ) ) +
        geom_density_ridges( scale = 1 ) +
        scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
        scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
        scale_fill_manual(values = c(col.alpha(2,.2), col.alpha(4,.2)), labels = c("Female", "Male")) +
        scale_color_manual(values = c(2, 4), guide = "none") +
        coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
        theme_ridges() +
        xlim(-11.5, 1) +
        labs( title = title, x = "Difference in Days of Drinking" )
}
