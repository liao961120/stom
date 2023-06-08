// Based on Lecture 17 Rethinking2023

data{
     int N;
     vector[50] D_sd;  // noise
     vector[50] D_obs;
     vector[50] M_sd;  // noise
     vector[50] M_obs;
     vector[50] A;
}
parameters{
     vector[N] D_true;
     real a;
     real bA;
     real bM;
     real<lower=0> sigma;
     vector[N] M_true;
     real aM;
     real bAM;
     real<lower=0> tau;
}
model{
    // M_true generative model
    vector[50] mu;
    vector[50] nu;
    tau ~ exponential( 1 );
    bAM ~ normal( 0 , 0.5 );
    aM ~ normal( 0 , 0.2 );
    for ( i in 1:50 ) {
        nu[i] = aM + bAM * A[i];
    }
    M_true ~ normal( nu , tau );

    // Measurement model
    M_obs ~ normal( M_true , M_sd );
    sigma ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bA ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 0.2 );
    
    for ( i in 1:50 ) {
        mu[i] = a + bA * A[i] + bM * M_true[i];
    }
    D_true ~ normal( mu , sigma );
    D_obs ~ normal( D_true , D_sd );
}
