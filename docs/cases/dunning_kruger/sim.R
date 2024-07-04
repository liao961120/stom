library(dplyr)

N = 1e5       # num of people
A = rnorm(N)  # person abilities
B = 0         # bias

Ro = rnorm(N, A, 1)
Rp = rnorm(N, A + B, 1)


# Split by quantile based on Ro
#
# Regression towards the mean happens to Rp with this operation since
# Ro/Rp are positively correlated, and therefore in the more extreme quantiles
# (e.g., the 1th/bottom and 4th/top quantiles), Ro will tend to be more extreme
# compared to Rp. Thus, Rp are said to regress toward the mean.
# A flatter slope of Rp along the quantiles is hence observed.
#
# See http://haines-lab.com/post/2021-01-10-modeling-classic-effects-dunning-kruger/2021-01-10-modeling-classic-effects-dunning-kruger/
q = quantile(Ro, seq(.25,.75,by=.25))
q = sapply(Ro, \(r) {
    if (r < q[1]) return(1)
    if (r < q[2]) return(2)
    if (r < q[3]) return(3)
    return(4)
})


# Population level effects
d = data.frame(
    obj = Ro,
    per = Rp,
    q = q
)



d_summ = d |>
    group_by(q) |>
    summarise(gp_obj = mean(obj),
              gp_per = mean(per))

with(d_summ, {
    M = max(gp_obj,gp_per)
    m = min(gp_obj,gp_per)

    plot(1, type="n", xlim=c(1,4), ylim=c(m,M))
    lines(1:4, gp_obj, col=2, lwd=2)
    points(1:4, gp_obj, col=2, pch=19)
    lines(1:4, gp_per, col=4, lwd=2)
    points(1:4, gp_per, col=4, pch=19)
    legend(1,1, c("Obj", "Per"), col=c(2,4), lwd=2, pch=19)
})
