# Debunking debunking hot hand fallacy
# 
# Simulation based on:
# https://statmodeling.stat.columbia.edu/2015/07/09/hey-guess-what-there-really-is-a-hot-hand/
N = 1e5
s = 4

draw_seq = function() {
    one_draw = sample(0:1, size=4, replace=T)
    h1 = one_draw[1:(s-1)] == 1
    h2 = one_draw[2:s] == 1
    p = sum(h1 & h2) / sum(h1)
    p
}


x = replicate(N, draw_seq())
mean(x, na.rm=T)
