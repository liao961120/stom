/* 
    Growth-Curve modeling on simulated data
*/ 

data {
    int N;             // num of observations
    int Ns;            // num of subjects
    int Nt;            // num of time points
    int Ntx;           // num of treatments
    array[N] int Sid;  // subject id
    array[N] int Tx;   // treatment group (1: control, 2: treatment)
    vector[N] time;    // time (0, 1, 2, 3)
    vector[N] Y;       // outcome (assume gausian)
}
parameters {
   vector[Ntx] A_Tx;    // treament main effect
   vector[Ntx] Bt_Tx;   // change rate by treatment (time:tx interaction)
   vector[Ns] zA_s;
   vector[Ns] zBt_s;
   real<lower=0> sigma_A_s;
   real<lower=0> sigma_Bt_s;
   real<lower=0> sigma_y;  // sd of outcome distribution
}
transformed parameters {
    vector[Ns] A_s;       // subject effect
    vector[Ns] Bt_s;      // change rate by subject (time:id interaction)
    A_s = zA_s * sigma_A_s;
    Bt_s = zBt_s * sigma_Bt_s;
}
model {
    sigma_y ~ exponential(1);
    A_Tx ~ normal(0, 1.5);
    Bt_Tx ~  normal(0, 1.5);
    // Partial-pool subject params
    zA_s ~  std_normal();
    zBt_s ~  std_normal();
    sigma_A_s ~ exponential(1);
    sigma_Bt_s ~ exponential(1);

    vector[N] mu;
    for ( i in 1:N )
        mu[i] = A_Tx[Tx[i]] + Bt_Tx[Tx[i]]*time[i] + A_s[Sid[i]] + Bt_s[Sid[i]]*time[i];
    Y ~ normal( mu, sigma_y );
}
