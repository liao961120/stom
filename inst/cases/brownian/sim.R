##################################
#### Standard Brownian Motion ####
##################################
W = function(t=1, dt=.001) {
    ts = seq(0, t, by=dt)
    N = length(ts) - 1
    Z = rnorm(N)  # standard normal
    w = rep(0, N+1)
    for (i in 2:(N+1))
        w[i] = w[i-1] + sqrt(dt) * Z[i-1]
    return(list(w = w, ts = ts))
}

# Simulate positions at t=1 for 5000 Brownian motions
x = replicate(5000, rev(W()$w)[1])
plot(density(x))
curve(dnorm(x), from = -5, to=5, add=T, col=2)

# Simulate positions at t=2 for 5000 Brownian motions
x = replicate(5000, rev(W(t=2)$w)[1])
plot(density(x))
curve(dnorm(x,sd = sqrt(2)), from = -5, to=5, add=T, col=2)


##################################
### Brownian Motion with drift ###
##################################
W2 = function(drift=1, scale=1, t=1, dt=.001) {
    ts = seq(0, t, by=dt)
    N = length(ts) - 1
    Z = rnorm(N)  # standard normal
    w = rep(0, N+1)
    for (i in 2:(N+1))
        w[i] = w[i-1] + drift*dt + scale*sqrt(dt)*Z[i-1]
    return(list(w = w, ts = ts))
}

# Simulate positions at t=1 for 5000 Brownian motions (drift=1, scale=2)
x = replicate(5000, rev(W2(drift=1, scale=2, t=1)$w)[1])
plot(density(x))
curve(dnorm(x, 1, 2), from = -5, to=5, add=T, col=2)

# Simulate positions at t=2 for 5000 Brownian motions (drift=1, scale=2)
x = replicate(5000, rev(W2(drift=1, scale=2, t=2)$w)[1])
plot(density(x))
curve(dnorm(x, 1*2, 2*sqrt(2)), from = -15, to=15, add=T, col=2)



