library(stom)
library(dplyr)

sim_data = function(viz=F, seed=10, Ns=100) {
    set.seed(seed)
    Ns = Ns
    Ntx = 2
    Nt = 4
    Tx = rep(1:Ntx, each=Ns/Ntx)
    Bt_Tx = c( 0, 1.5 )
    Bt = .7
    A_Tx = c( 0, 1 )
    A_s = rnorm( Ns, 0, 1.5 )
    Bt_s = rnorm( Ns, 0, 1 )
    alpha = 3
    
    d = expand.grid( Sid=1:Ns, time=(1:Nt)-1, KEEP.OUT.ATTRS=F )
    d$Tx = Tx[d$Sid]
    d$Y = rnorm( nrow(d), alpha + A_Tx[d$Tx] + Bt*d$time + Bt_Tx[d$Tx]*d$time + A_s[d$Sid] + Bt_s[d$Sid]*d$time )
    # str(d)
    
    if (!viz)
      return(list(
        dat = list(
          N = nrow(d),
          Ns = Ns,
          Ntx = Ntx,
          Nt = Nt,
          Y = d$Y,
          Tx = d$Tx,
          time = d$time,
          Sid = d$Sid
        ),
        Bt_Tx = Bt_Tx,
        Bt = Bt,
        A_Tx = A_Tx,
        A_s = A_s,
        Bt_s = Bt_s
      ))
      
    
    ## Plot trajectory
    plot( 1, type="n", xlim=c(-0,3), ylim=c(-10,10),
          xlab="time", ylab="Outcome")
    # Sampled subjects' trajectories
    for ( Sid in sample(1:Ns, 20) ) {
      ds = d[d$Sid == Sid, ]
      lines( ds$time, ds$Y, col=col.alpha(ds$Tx[1]*2), lwd=2 )
    }
    # Mean trajectory
    for ( Tx in 1:Ntx ) {
      ds = d[d$Tx == Tx, ] |>
        group_by(time) |>
        summarise( Y = mean(Y) )
      lines( ds$time, ds$Y, col=Tx*2, lwd=4 )
    }
    # Theoretical trend
    for ( Tx in 1:Ntx ) {
      lines( 0:3, A_Tx[Tx] + Bt*(0:3) + Bt_Tx[Tx] , col=col.alpha(Tx*2, .7), lwd=4, lty="dashed" )
    }
}
