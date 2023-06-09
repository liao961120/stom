data {
   int N;
   vector[N] x;
   vector[N] y;
}
parameters {
   real beta;
   real<lower=0> sigma;
}
transformed parameters {
    // non-center parameterization
    vector[N] y_std;
    for ( i in 1:N )
        y_std[i] = ( y[i] - beta*x[i] ) / sigma;
}
model {
    beta ~ std_normal();
    sigma ~ exponential(.5);
    y_std ~ std_normal(); 
}
