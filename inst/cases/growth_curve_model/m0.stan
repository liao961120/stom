/* 
    Growth-Curve modeling replicating Solomon Kurz's post
    found at https://solomonkurz.netlify.app/blog/2021-04-22-effect-sizes-for-experimental-trials-analyzed-with-multilevel-growth-models-two-of-two/
*/ 

data {
    int N;            // num of observations
    int Ns;           // num of subjects
    int Nt;           // num of time points
    int Ntx;          // num of treatments
    array[N] int id;  // subject id
    array[N] int tx;  // treatment group (1: control, 2: treatment)
    vector[N] time0;  // time (0, 1, 2, 3)
    vector[N] y;      // outcome (assume gausian)
}
parameters {
   vector[Ntx] Tx;         // treament main effect
   real Bt;                // time main effect
   vector[Ntx-1] Bt_Tx_raw;      // change rate by treatment (time:tx interaction)
   vector[Ns] zAs;
   vector[Ns] zBt_s;
   real<lower=0> sigma_As;
   real<lower=0> sigma_Bt_s;
   real<lower=0> sigma_y;  // sd of outcome distribution
   real alpha;  // global intercept
}
transformed parameters {
    vector[Ntx] Bt_Tx = append_row( Bt_Tx_raw, -sum(Bt_Tx_raw) );
    vector[Ns] As;          // subject effect
    vector[Ns] Bt_s;        // change rate by subject (time:id interaction)
    As = zAs * sigma_As;
    Bt_s = zBt_s * sigma_Bt_s;
}
model {
    alpha ~ normal(0, 5);
    sigma_y ~ exponential(1);
    Tx ~ std_normal();
    Bt ~  normal(0, 1.5);
    Bt_Tx ~  std_normal();
    // Partial-pool subject params
    zAs ~  std_normal();
    zBt_s ~  std_normal();
    sigma_As ~ exponential(.5);
    sigma_Bt_s ~ exponential(.5);

    vector[N] mu;
    for ( i in 1:N )
        mu[i] = alpha + Tx[tx[i]] + Bt*time0[i] + Bt_Tx[tx[i]]*time0[i] + As[id[i]] + Bt_s[id[i]]*time0[i];
    y ~ normal( mu, sigma_y );
}
