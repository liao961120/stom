library(RWiener)

# Simulation of Wiener First Passage Time Distribution
rW = function(n, alpha=1, bias=0, drift=1, tau=.1, scale=1, dt=.001) {
    b = c("upper", "lower")
    d = list(q=rep(NA_real_,n), resp=rep(NA_character_,n))
    for (i in seq_along(n)) {
        t = 0
        w = alpha * bias  # initial condition
        while (0 < w && w < alpha) {
            w = w + drift*dt + scale*sqrt(dt)*rnorm(1)
            t = t + dt
        }
        d$q[i] = tau + t
        d$resp[i] = b[(w<0)+1]
    }
    data.frame(d)
}


# Params
alpha = 2
bias = beta = 0.6
drift = delta = 0.1
tau = 0.3

# Check simulation by comparing to functions in RWiener
N = 5000
x = rW(N, alpha=alpha, bias=bias, drift=drift, tau=tau)
x2 = rwiener(N, alpha=alpha,beta=bias,delta=drift,tau=tau)

plot(1, type="n", xlab="RT", ylab="Density", ylim=c(0,1.1), xlim=c(0,5))
lines(density(x$q[x$resp == "upper"]), lwd=2, col=2 )
lines(density(x$q[x$resp == "lower"]), lwd=2, col=2, lty=2)

lines(density(x2$q[x2$resp == "upper"]), lwd=2, col=4)
lines(density(x2$q[x2$resp == "lower"]), lwd=2, col=4, lty=2)

legend(3.5, 1, 
       legend = c("rW(): upper", "rW(): lower", "rwiener(): upper", "rwiener(): lower"),
       col    = c(2, 2, 4, 4),
       lty    = c(1, 2, 1, 2),
       lwd    = 2)
