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
   real Bt;                   // time main effect
   vector[Ntx-1] A_Tx_raw;    // treament main effect
   vector[Ntx-1] Bt_Tx_raw;   // change rate by treatment (time:tx interaction)
   vector[Ns] zA_s;
   vector[Ns] zBt_s;
   real<lower=0> sigma_A_s;
   real<lower=0> sigma_Bt_s;
   real<lower=0> sigma_y;  // sd of outcome distribution
   real alpha;             // global intercept
}
transformed parameters {
    vector[Ntx] Bt_Tx = append_row( 0, Bt_Tx_raw );
    vector[Ntx] A_Tx = append_row( 0, A_Tx_raw );
    vector[Ns] A_s;       // subject effect
    vector[Ns] Bt_s;      // change rate by subject (time:id interaction)
    A_s = zA_s * sigma_A_s;
    Bt_s = zBt_s * sigma_Bt_s;
}
model {
    alpha ~ normal(0, 2);
    sigma_y ~ exponential(1);
    A_Tx ~ normal(0, 1.5);
    Bt ~  normal(0, 1.5);
    Bt_Tx ~  normal(0, 1.5);
    // Partial-pool subject params
    zA_s ~  std_normal();
    zBt_s ~  std_normal();
    sigma_A_s ~ exponential(1);
    sigma_Bt_s ~ exponential(1);

    vector[N] mu;
    for ( i in 1:N )
        mu[i] = alpha + A_Tx[Tx[i]] + Bt*time[i] + Bt_Tx[Tx[i]]*time[i] + A_s[Sid[i]] + Bt_s[Sid[i]]*time[i];
    Y ~ normal( mu, sigma_y );
}
