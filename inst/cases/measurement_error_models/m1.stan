data {
    int N;
    vector[N] z;
    vector[N] x_obs;
    vector[N] y;
}
parameters {
    vector[N] x_true;
    real mu_z;
    real<lower=0> sigma_z;
    real<lower=0> sigma_x;
    real<lower=0> sigma_y;
    real<lower=0> tau;
    real b_xz;
    real a_xz;
    real b_yz;
    real b_yx;
    real a_yx;
}
model {
    // Priors
    mu_z ~ std_normal();
    sigma_z ~ std_normal();
    sigma_x ~ std_normal();
    sigma_y ~ std_normal();
    tau ~ std_normal();
    b_xz ~ std_normal();
    a_xz ~ std_normal();
    b_yz ~ std_normal();
    b_yx ~ std_normal();
    a_yx ~ std_normal();


    y ~ normal( a_yx + b_yx * x_true + b_yz * z, sigma_y );
    x_true ~ normal( a_xz + b_xz * z, sigma_x );
    x_obs ~ normal( x_true, tau );
    z ~ normal(mu_z, sigma_z);
}
