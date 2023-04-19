/*
  See wine1.R for data and fitting.
*/
data{
    int N;    // # responses
    int Nj;   // # judged
    int Nw;   // # wines
    array[N] int R;    // Binary response
    array[N] int Jid;  // Judge ID
    array[N] int Wid;  // Wine ID
    array[N] int Oj;   // Judge origin
    array[N] int Ow;   // Wine origin
}
parameters{
  vector[2] a;  // mean wine quality for Ow=1 & Ow=2
  vector[Nj] zJ;
  vector[Nj] zW;
  real<lower=0> sigma_J;
  real<lower=0> sigma_W;
}
transformed parameters {
  vector[Nj] J;
  J = zJ * sigma_J;
}
model{
  // Submodel 1: Ow --> W
  vector[Nw] W;
  a ~ normal( 0, 1 );
  zW ~ normal( 0, 1 );
  sigma_W ~ exponential( 1 );
  for ( i in 1:N ) {
    W[Wid[i]] = a[Ow[i]] + zW[Wid[i]] * sigma_W;
  }
  
  // Submodel 2:  W --> R <-- J
  vector[N] mu;
  zJ ~ normal( 0 , 1 );
  sigma_J ~ exponential( 1 );
  for ( i in 1:N ) {
    mu[i] = W[Wid[i]] + J[Jid[i]];
  }
  R ~ bernoulli_logit( mu );
}
generated quantities {
  vector[Nw] W;
  for ( i in 1:N )
    W[Wid[i]] = a[Ow[i]] + zW[Wid[i]] * sigma_W;
}
