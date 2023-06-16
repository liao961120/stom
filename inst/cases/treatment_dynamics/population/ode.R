library(stom)
library(deSolve)

f = function( t, x, params ) {
    # State
    D = x[1]
    E = x[2]

    # params
    rTD = params["rTD"]
    rED = params["rED"]
    rTE = params["rTE"]
    rDE = params["rDE"]

    # Differential equations
    dD_dt = rTD * (1 - D) + rED * (1 - E)*E
    dE_dt = rTE * (1 - E) + rDE * (1 - D)*D
    list( c(dD_dt, dE_dt) )
}

params = c(
    rTD = .5,
    rED = .5,
    rTE = .5,
    rDE = .7
)
init = c( D=.1, E=.1 )
times = seq(0, 10, by=.0001)

traj = ode(
    func=f,
    y=init,
    times=times,
    parms=params
)
d = as.data.frame(traj)

plot(1, type="n", xlab="time", ylab="value",
     xlim=c(0,10), ylim=c(0,1) )
lines(d$time, d$D, col=col.alpha(2) )
lines(d$time, d$E, col=col.alpha(1) )


