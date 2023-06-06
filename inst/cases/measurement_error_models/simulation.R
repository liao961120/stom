N = 500

mu_z = 0
sigma_z = 1
sigma_x = .6
sigma_y = .6
tau = .3

b_xz = .8
a_xz = -1
b_yz = .7
b_yx = .5
a_yx = 3


z = rnorm( N, mu_z, sigma_z )
x_true = rnorm( N, b_xz*z + a_xz ,sigma_x )
x_obs = rnorm( N, x_true, tau )

y = rnorm(N, a_yx + b_yx * x_true + b_yz * z, sigma_y )

