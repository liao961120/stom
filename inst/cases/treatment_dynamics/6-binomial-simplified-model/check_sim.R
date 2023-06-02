library(dplyr)
source("simulation.R")
set.seed(987)
d = sim_data( alpha=-.5 )



empirical_obs = function(Tid, G, sample_n=3) {
    Sids = d$dat$Sid_O[ d$dat$Tx == Tid & d$dat$G == G ]
    Sids = sample(Sids, size=sample_n)
    dat = sapply( Sids, function(sid) {
        x = lapply( 1:4 , function(t) {
            cbind(t, d$others$D[sid, t] )
        })
        Reduce( rbind, x )
    }, simplify = "array")
    dimnames(dat)[[3]] = Sids
    dat
}


obs = empirical_obs(Tid=1, G=1, sample_n=5)
plot( 1, type="n", xlim=c(1,4), ylim=c(0, 14),
      xlab = "Time", ylab = "Treatment Outcome\n(Days of heavy drinking)",
    )
# Empirical curve
for ( s in 1:dim(obs)[3] ) {
    obs_ = obs[,,s]
    for ( t in 1:3 ) {
        lines( c(t,t+1), c(obs_[t,2],obs_[t+1,2]), lwd=1, col=col.alpha(s,.2) )
        points( c(t,t+1), c(obs_[t,2],obs_[t+1,2]), col=s, pch=s )
    }

}




d$dat$D_latent |> rethinking::simplehist()

dat = with(d$dat, {
    data.frame(
        Sid_O    = Sid_O   ,
        time_O   = time_O  ,
        G        = G       ,
        A        = A       ,
        Tx       = Tx      ,
        D        = D       ,
        D_latent = D_latent
    )
})



dat |>
    filter(Sid_O == 1)

