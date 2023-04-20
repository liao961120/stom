/*
  See wine0.R for data and fitting.
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
  vector[Nj] zJ;
  vector[Nw] zW;
  matrix[2, 2] Int;  // Interaction: Oj x Ow
  real<lower=0> sigma_J;
  real<lower=0> sigma_W;
}
transformed parameters {
    vector[Nj] J;
    vector[Nw] W;
    J = zJ * sigma_J;
    W = zW * sigma_W;
}
model{
  vector[N] mu;
  zW ~ normal( 0 , 1 );
  zJ ~ normal( 0 , 1 );
  sigma_W ~ exponential( 1 );
  sigma_J ~ exponential( 1 );
  for ( i in 1:N ) {
    mu[i] = W[Wid[i]] + J[Jid[i]] + Int[Oj[i], Ow[i]];
  }
  R ~ bernoulli_logit( mu );
}
