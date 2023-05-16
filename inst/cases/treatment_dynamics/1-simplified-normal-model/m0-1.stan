data {
    int N;   // num of obs.
    int Ns;  // num of subjects
    int Ni;  // num of items in the self-efficacy scale
    int Nt;  // num of time points
    int Nk;  // num of Likert scale points
    int Ntx; // num of treatments
    array[N] int<lower=0,upper=Nt-1> time;  // Time point of obs.
    array[N] int<lower=1,upper=Ns> Sid; // Subject ID
    array[N] int<lower=1,upper=Ni> Iid; // Item ID
    array[N] int<lower=1> R;            // Responses on self-efficacy scale
    array[N] int<lower=1> Tx;           // Treatment received 
    array[N] real<lower=0,upper=1> A;   // Age ( scaled: (x-18)/80 )
    array[N] real D;   // Outcome: observed heavy drinking tendency (coined, for scaffolding larger models later)
}
parameters {
    // IRT model params
    array[Ns] real muEs;
    array[Ns] real<lower=0> sigma_Es;
    array[Nt] real<lower=0> sigma_Et;
    real<lower=0> sigma_I;
    matrix[Ns,Nt] zE;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;

    // Mediation model params
    real B_AE;    // Age on Efficacy
    real B_AD;    // Age on Outcome
    real B_ED;    // Efficacy on Outcome (indirect effect)
    vector[Ntx] B_TE;    // Treatment on Efficacy (indirect effect)
    vector[Ntx] B_TD;    // Treatment on Outcome (direct effect)
    real<lower=0> sigma_D;  // Outcome std
}
transformed parameters {
    // IRT model params
    vector[Ni] I;     // Item params
    matrix[Ns,Nt] E;  // Efficacy
    // sum-to-0 contraint on I
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    for ( s in 1:Ns )
        E[s,] = sigma_Es[s] * zE[s,] + muEs[s];
}
model {
    to_vector(zE) ~ std_normal();
    muEs ~ normal(0, 2);
    sigma_Es ~ exponential(1);
    sigma_Et ~ exponential(1);

    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);
    sigma_D ~ exponential(1);

    B_AD ~ std_normal();
    B_ED ~ std_normal();
    B_TD ~ std_normal();  // treatment direct effect (vector)
    B_TE ~ std_normal();  // treatment indirect effect (vector)
    B_AE ~ std_normal();

    int t;
    real mu;
    for ( i in 1:N ) {
        // Mediation Submodel
        t = time[i];
        // pre-treatment Efficacy: affected by age
        if ( t == 0 ) 
            mu = B_AE*A[i];
        // post-treatment Efficacy
        else 
            mu = E[Sid[i],1] + B_TE[Tx[i]]*t;
        E[Sid[i],t+1] ~ normal( mu, sigma_Et[t+1] );

        // IRT Submodel (Efficacy Measure)
        R[i] ~ ordered_logistic( E[Sid[i],t+1] + I[Iid[i]], kappa );

        // link to (Normal) Outcome
        D[i] ~ normal( B_ED*E[Sid[i],t+1] + B_AD*A[i] + B_TD[Tx[i]]*t, 1 );
    }
}
