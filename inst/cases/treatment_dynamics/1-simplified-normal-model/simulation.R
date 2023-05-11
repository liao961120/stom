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
    E0 = rnorm( Ns, B_AE*A )  # baseline latent trait
    E0 = standardize(E0, m=-1)
    B_TE = c( .5, 1, 2 )  # Treatment effect (slopes)
    t = 0:(Nt-1)  # time points of measure
    E = sapply( t, function(time) {  # latent trait across time points (including E0)
       E0 + B_TE[Tx]*time
    })
    U = rnorm( Ns )  # unmeasured influence on D
    B_ED = 1.5
    B_AD = .8
    D = sapply( t, function(time) {  # Outcome across time (latent trait underlying days of drinking)
      rnorm( Ns, B_ED * E[, time+1] + B_AD*A + U )
    })


    Ni = 10  # number of items
    ei = seq(-3, 3, length=Ni)  # item easiness
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
      kappa = kappa
    )
    return( list(dat=dat, params=true_params) )
}


