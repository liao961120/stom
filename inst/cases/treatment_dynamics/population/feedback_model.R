#' Preliminary exploration of Control-Efficacy feed back model
#' 
#' @model
#' D[t+1] = D[t] + rTD + bED (E[t] - E[t-1]) + rS S[t]
#' E[t+1] = E[t] + rTE + bDE (D[t] - D[t-1])
#' 
#' @todo 
#' Model risk aversion?
#' Different bED/bDE for +/- rates

library(stom)

timesteps = 100
S = c(0,0, rbern(.1, 98)) #c(0,0, 1 , rep(0,97)) #  # Stressful life events

# Params
rTD = .005
bED = .8
rS =  -.15
rTE = .01
bDE = .99

E = rep( NA, timesteps )
D = rep( NA, timesteps )
# Initial conditions
E[1:2] = 0
D[1:2] = 0


for ( t in seq(timesteps) ) {
    if ( t %in% 1:2 ) next
    D[t] = D[t-1] + rTD + bED * (E[t-1] - E[t-2]) + rS * S[t]
    E[t] = E[t-1] + rTE + bDE * (D[t-1] - D[t-2])
}

rng = c(D,E)
rng = c( min(rng), max(rng) )
plot(1, type="n", xlim=c(1,timesteps), ylim=rng)
lines( 1:timesteps, D, col=col.alpha(2) )
lines( 1:timesteps, E, col=col.alpha(4) )
Sb = as.logical(S)
points( (1:timesteps)[Sb], D[Sb] )

