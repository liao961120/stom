data {
   int N;
   vector[N] x;
   vector[N] y;
}
parameters {
   real beta;
   real<lower=0> sigma;
}
model {
    beta ~ std_normal();
    sigma ~ exponential(.5);
    vector[N] mu;
    for ( i in 1:N )
        mu[i] = beta * x[i];
    y ~ normal( mu, sigma );
}
