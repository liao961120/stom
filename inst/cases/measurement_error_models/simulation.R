library(stom)

sim_data = function() {
    N = 500
    mu_z = 0
    sigma_z = 1
    sigma_x = 1
    sigma_y = 1
    tau = 1
    b_xz = .8
    a_xz = -1
    b_yz = .7
    b_yx = .5
    a_yx = 3

    z = rnorm( N, mu_z, sigma_z )
    x_true = rnorm( N, b_xz*z + a_xz ,sigma_x )
    x_obs = rnorm( N, x_true, tau )
    y = rnorm(N, a_yx + b_yx * x_true + b_yz * z, sigma_y )

    return(list(
        params = list(
            mu_z = mu_z,
            sigma_z = sigma_z,
            sigma_x = sigma_x,
            sigma_y = sigma_y,
            tau = tau,
            b_xz = b_xz,
            a_xz = a_xz,
            b_yz = b_yz,
            b_yx = b_yx,
            a_yx = a_yx,

            x_true = x_true
        ),
        dat = list(
            N = N,
            z = z,
            x_obs = x_obs,
            y = y
        )
    ))
}
