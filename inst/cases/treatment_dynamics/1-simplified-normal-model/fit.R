library(stom)
source("simulation.R")

d = sim_data()
m = stan( "m0-1.stan", data=d$dat )
m$save_object("m0.RDS")
m = readRDS("m0.RDS")

pars = c("B_TE", "B_TD", "B_ED", "B_AE", "B_AD", "E", "I", "kappa", "sigma_D")
s = precis(m, 5, pars )

# get_pars(s, "sigma_D")


####### Check IRT params recovery ########
for ( p in c("E", "I", "kappa") ) {
    plot( d$params[[p]], get_pars(s, p)$mean, main=p )
    abline(0,1, lty="dashed", col="grey")
}



######## Check Beta params recovery #########
beta = c("B_TE", "B_ED", "B_AE", "B_AD", "B_TD")
b_true = lapply( beta, function(p) d$params[[p]] ) |> unlist()
b_est = lapply( beta, function(p) get_pars(s, p)$mean ) |> unlist()
b_est_upp = lapply( beta, function(p) get_pars(s, p)$q5 ) |> unlist()
b_est_low = lapply( beta, function(p) get_pars(s, p)$q95 ) |> unlist()
b_true = c(b_true, rep(0,3) )  # no B_TD effect

plot( b_true, pch=19, ylim=c(-2, 2) )
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



f = glue::glue
##### Check model prediction (posterior prediction check) ######
post = extract(m)
n_post = nrow(post)
# D_pred
post_draw = function(A, Tx, Sid, time) {
    idx = sample(1:n_post, size=1)
    p = post[idx, ]

    B_ED = p$B_ED
    E = p[[f("E[{Sid},{time+1}]")]]
    B_AD = p$B_AD
    B_TD = p[[f("B_TD[{Tx}]")]]
    sigma_D = p$sigma_D

    mu = B_ED*E + B_AD*A + B_TD*time
    D_pred = rnorm( 1, mu, sigma_D )

    return(
        D_pred
        # list(B_ED, E, B_AD, B_TD)
    )
    # B_ED*E[Sid[i],time[i]+1] + B_AD*A[i] + B_TD[Tx[i]]*time[i]

}

# post_draw(0.4, 1, 1, 1)
