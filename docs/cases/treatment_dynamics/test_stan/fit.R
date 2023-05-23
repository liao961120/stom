library(stom)
set.seed(18)

N = 5000
beta = 1.5
sigma = 3
x = rnorm( N )
y = rnorm( N, beta*x, sigma )


dat = list(
    N = N,
    x = x,
    y = y
)




m0 = stan( "m0.stan", dat )
m0

m1 = stan( "m2.stan", dat )
m1
