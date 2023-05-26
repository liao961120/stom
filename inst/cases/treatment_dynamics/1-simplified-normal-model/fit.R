library(stom)
source("simulation.R")

d = sim_data(B_AE=1)
m = stan( "m1.stan", data=d$dat, chains=3, parallel_chains=3 )
save_model(m, fp = "m1.RDS")
# m = readRDS("model_4a5c159a2ff3_model.RDS")
# m = readRDS("m0-1.RDS")

pars = c("B_TE", "B_TD", "B_ED", "B_AE", "B_AD", "E", "I", "kappa", "sigma_D")
# m = readRDS("m1-noAge.RDS")



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


# m = readRDS("m1-noAge.RDS")
# d = sim_data(B_AE=0)
# s = stom::precis(m, 5)
######## Check Beta params recovery #########
beta = c( "B_TE", "B_AE", "B_AD", "B_ED", "B_TD" )
b_true = lapply( beta, function(p) d$params[[p]] ) |> unlist()
b_est = lapply( beta, function(p) get_pars(s, p)$mean ) |> unlist()
b_est_upp = lapply( beta, function(p) get_pars(s, p)$q5 ) |> unlist()
b_est_low = lapply( beta, function(p) get_pars(s, p)$q95 ) |> unlist()
b_true = c(b_true, rep(0,3) )  # no B_TD effect

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







### Compare order
# library(stom)
# m0 = readRDS("m0.RDS")
# m1 = readRDS("m0-1.RDS")
# s0 = stom::precis( m0, 5 )
# s1 = stom::precis( m1, 5 )
#
# plot( s0$mean, s1$mean )
# for ( i in 1:nrow(s0) ) {
#     lines( c(s0$mean[i], s0$mean[i]), c(s0$q5[i], s0$q95[i]), lwd=2, col=col.alpha(2) )
#     lines( c(s1$q5[i], s1$q95[i]), c(s1$mean[i], s1$mean[i]), lwd=2, col=col.alpha(4) )
# }
# abline(0, 1)
