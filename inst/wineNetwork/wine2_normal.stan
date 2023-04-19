/*
  See wine2.R for data and fitting.
*/
data{
    int N;    // # responses
    int Nj;   // # judged
    int Nw;   // # wines
    vector[N] C;       // Continuous response
    array[N] int R;    // Binary response
    array[N] int Jid;  // Judge ID
    array[N] int Wid;  // Wine ID
    array[N] int Oj;   // Judge origin
    array[N] int Ow;   // Wine origin
}
parameters{
  vector[3] Int_raw;  
  vector[1] a_raw;
  vector[Nj] zJ;
  vector[Nj] zW;
  real<lower=0> sigma_J;
  real<lower=0> sigma_W;
}
transformed parameters {
  // mean wine quality for Ow=1 & Ow=2 (sum-zero-constraint)
  vector[2] a = append_row(a_raw, -sum(a_raw));
  // Interaction terms (fix 2,2 to zero)
  matrix[2,2] Int = [ [Int_raw[1],    Int_raw[2]], 
                      [Int_raw[3],    0] ];
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
  to_vector(Int) ~ normal( 0, 1 );
  zJ ~ normal( 0 , 1 );
  sigma_J ~ exponential( 1 );
  for ( i in 1:N ) {
    mu[i] = W[Wid[i]] + J[Jid[i]] + Int[Oj[i], Ow[i]];
  }
  C ~ normal( mu, 1 );
}
generated quantities {
  vector[Nw] W;
  for ( i in 1:N )
    W[Wid[i]] = a[Ow[i]] + zW[Wid[i]] * sigma_W;
}
