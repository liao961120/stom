/*
    Reparameterization of m1.stan through change of variables (need Jacobian adjustments)
    See https://mc-stan.org/docs/stan-users-guide/changes-of-variables.html
*/

data {
    int N;
    vector[N] z;
    vector[N] x_obs;
    vector[N] y;
}
parameters {
    vector[N] x_true_std;
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
transformed parameters {
    vector[N] x_true = x_true_std * sigma_x + (a_xz + b_xz * z);
}
model {
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

    z ~ normal(mu_z, sigma_z);
    vector[N] x_obs_std = (x_obs - x_true) / tau;
    vector[N] J_diag = rep_vector(-2*log(tau), N);  // diagonals of Jacobian
    target += sum( log(J_diag) );  // Jacobian adjustment
    x_true_std ~ std_normal();
    x_obs_std ~ std_normal();
    y ~ normal( a_yx + b_yx * x_true + b_yz * z, sigma_y );
}
