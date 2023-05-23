library(stom)
library(dplyr)
source("simulation.R")

d = sim_data()


dat = d$dat
dat = dat[-(1:6)] |> data.frame() 


E = d$params$E
plot(1, type="n", xlim=c(.5,4.5),
     ylim = c(min(E), max(E)) )
for ( i in 21:30) {
  for ( x in 1:3 )
    lines(c(x,x+1), c(E[i,x], E[i,x+1]), col=col.alpha(i))
}