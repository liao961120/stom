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
    vector[N] y_std;
    for ( i in 1:N ) {
        y_std[i] = (y[i] - beta * x[i]);
    }
    target += normal_lpdf(y_std | 0, sigma );
    // ( / sigma) ~ std_normal();
}
