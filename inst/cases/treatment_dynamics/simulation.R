#' Growth curve modeling of latent trait
library(stom)

Ns = 3*10
Ntx = 3
Nt = 4
Tx = rep(1:Ntx, each=Ns/3)
A = rtnorm( Ns, m = 27, lower=18, upper=80, s=20 )
A = ( A - 18 ) / 80
E0 = rnorm( Ns, .8*A )  # baseline latent trait
E0 = standardize(E0, m=-1)
B_Tx = c( .5, 1, 2 )  # Treatment effect (slopes)
t = 0:(Nt-1)  # time points of measure
E = sapply( t, function(time) {  # latent trait across time points (including E0)
   E0 + B_Tx[Tx]*time
})
U = rnorm( Ns )  # unmeasured influence on D
D = sapply( t, function(time) {  # Outcome across time
  rnorm( Ns, 1.5 * E[, time+1] + 0.8*A + U )
})


Ni = 10  # number of items
ei = seq(-3, 3, length=Ni)  # item easiness
kappa = logit( cumsum( simplex(c(1,2,3,3,2,1)) ) )
kappa = kappa[-length(kappa)]

d = expand.grid( Sid=1:Ns, Iid=1:Ni, time=t, KEEP.OUT.ATTRS=F )
for ( i in 1:nrow(d) ) {
  d$R[i] = with(d, {
    rordlogit( E[Sid[i], time[i]+1] + ei[Iid[i]], kappa=kappa )
  })
  d$D[i] = D[d$Sid[i], d$time[i]+1]
}
