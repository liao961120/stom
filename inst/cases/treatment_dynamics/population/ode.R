library(deSolve)

f = function( t, x, params ) {
    # State
    D = x[1]
    E = x[2]
    
    # params
    b_TD = params["b_TD"]
    b_ED = params["b_ED"]
    b_TE = params["b_TE"]
    b_DE = params["b_DE"]
    
    # Differential equations
    dD_dt = b_TD + b_ED * E
    dE_dt = b_TE + b_DE * b_TD + b_DE * b_ED * E
    list( c(dD_dt, dE_dt) )
}

params = c(
    b_TD =  -.1, 
    b_ED =  .2, 
    b_TE = -1, 
    b_DE =   1
)
init = c( D=10.5, E=9.5 )
times = seq(0, 10, by=.0001)

traj = ode(
    func=f,
    y=init,
    times=times,
    parms=params
)
d = as.data.frame(traj)

plot(1, type="n", xlab="time", ylab="value",
     xlim=c(0,10), ylim=c(0,50) )
lines(d$time, d$D, col=col.alpha(2) )
lines(d$time, d$E, col=col.alpha(1) )


