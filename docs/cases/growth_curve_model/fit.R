
# Stan version of the model
library(stom)
source("simulation.R")
d = sim_data(seed=13, Ns=300)
m = stan("m1.stan", data=d$dat)
s = precis(m, 5, c("Bt_Tx", "A_Tx", "A_s", "Bt_s"))

plot( d$A_s, s[startsWith(s$variable, "A_s"), "mean"] ); abline(0, 1)
plot( d$Bt_s, s[startsWith(s$variable, "Bt_s"), "mean"] ); abline(0, 1)
plot( d$Bt_Tx, s[startsWith(s$variable, "Bt_Tx"), "mean"] ); abline(0, 1)


# lmer version of the model
library(lme4)
source("simulation.R")
d = sim_data(seed=1, Ns=5000)  # params converge on large sample size
d2 = d$dat
d2$Sid = factor(d2$Sid)
d2$Tx = factor(d2$Tx)
# contrasts(d2$Tx) = contr.treatment(n=2, base=1)
m2 = lmer( Y ~ -1 + Tx + time:Tx + (1 + time | Sid), data=d2 )
summary(m2)
