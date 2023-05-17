#' Growth curve modeling of latent trait
library(stom)

sim_data = function() {
    set.seed(10)

    Ns = 3*10  # number of subjects
    Ntx = 3    # number of treatments
    Nt = 4     # number of time points
    Tx = rep(1:Ntx, each=Ns/3)
    A = rtnorm( Ns, m=27, lower=18, upper=80, s=20 )  # Age
    A = ( A - 18 ) / 80
    B_AE = .8
    B_TE = c( .5, 1, 2 )  # Treatment effect (slopes)
    t = 0:(Nt-1)  # time points of measure
    E = sapply( t, function(time) {  # latent trait across time points (including E0)
       rnorm( Ns, B_AE*A + B_TE[Tx]*time ) - 1.5
    })
    U = rnorm( Ns )  # unmeasured influence on D
    B_ED = 1.5
    B_AD = .8
    D = sapply( t, function(time) {  # Outcome across time (latent trait underlying days of drinking)
      rnorm( Ns, B_ED * E[, time+1] + B_AD*A + U )
    })


    Ni = 16  # number of items
    ei = seq(-3, 3, length=Ni)  # item easiness (sums to zero)
    kappa = logit( cumsum( simplex(c(1,2,3,3,2,1)) ) )
    kappa = kappa[-length(kappa)]

    d = expand.grid( Sid=1:Ns, Iid=1:Ni, time=t, KEEP.OUT.ATTRS=F )
    for ( i in 1:nrow(d) ) {
      # Item responses
      d$R[i] = with(d, {
        rordlogit( E[Sid[i],time[i]+1] + ei[Iid[i]], kappa=kappa )
      })
      d$D[i] = D[d$Sid[i], d$time[i]+1]  # Outcome
      d$Tx = Tx[d$Sid]  # Treatment received
      d$A = A[d$Sid]    # Age of subjects
    }

    # Gather data
    dat = list(
      N = nrow(d),
      Ns = Ns,
      Ntx = Ntx,
      Nt = Nt,
      Nk = length(kappa) + 1,
      Ni = Ni,

      A = d$A,
      Tx = d$Tx,
      R = d$R,
      D = d$D,
      Sid = d$Sid,
      Iid = d$Iid,
      time = d$time
    )
    true_params = list(
      B_TE = B_TE,
      B_ED = B_ED,
      B_AE = B_AE,
      B_AD = B_AD,
      E = E,
      I = ei,
      kappa = kappa
    )
    return( list(dat=dat, params=true_params) )
}


# d = sim_data()
#
# library(dplyr)
# library(ggplot2)
#
# dat = d$dat[-(1:6)] |> data.frame()
# dat$Sid = factor(dat$Sid)
# dat$Iid = factor(dat$Iid)
# dat$Tx = factor(dat$Tx)
# dat$R = ordered(dat$R)
#
# dat |> group_by(Tx, time) |> summarise(D=mean(D))
#
#
# dat |>
#     group_by(Sid, time) |>
#     summarise(D = mean(D),
#               R = mean(R)) |>
#     mutate(Sid = factor(Sid)) |>
#     ggplot()+
#         geom_line(aes(x=time, y=R, color=Sid))
#
