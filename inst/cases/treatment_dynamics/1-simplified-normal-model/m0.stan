// Untested yet (2023.5.11)
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
    array[N] real<lower=0,upper=1> As;  // Age ( scaled: (x-18)/80 )
    array[N] real D;   // Outcome: observed heavy drinking tendency (coined, for scaffolding larger models later)
}
parameters {
    // IRT model params
    real muE;
    real<lower=0> sigma_E;
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

    // Non-centered parameterization for TE/TC (partial-pooled)
    // vector[Ns] zC;   // self-control at Et
    // real<lower=0> sigma_C;  // self-control std at Et
    // vector[3] zTE;
    // vector[3] zTC;
    // real<lower=0> sigma_TE;
    // real<lower=0> sigma_TC;
    // real muTE;
    // real muTC;
    // real muEb;  // baseline grand mean Efficacy before treatment
    // array[Nt] real<lower=0> sigma_Et;  // SD of Efficacy at each time step;

}
transformed parameters {
    // IRT model params
    vector[Ni] I;     // Item params
    matrix[Ns,Nt] E;  // Efficacy
    // sum-to-0 contraint on I
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    E = sigma_E * zE + muE;

    // // Mediation model params
    // vector[3] TE;  // Treatment on Efficacy
    // vector[3] TC;  // Treatment on Efficacy
    // TE =  zTE * sigma_TE + muTE;  // Treatment on Efficacy
    // TC =  zTC * sigma_TC + muTC;  // Treatment on Efficacy
}
model {
    // IRT Submodel (Efficacy Measure)
    kappa ~ std_normal();
    to_vector(zE) ~ std_normal();
    zI_raw ~ std_normal();
    muE ~ normal(0, 1.5);
    sigma_I ~ exponential(1);
    sigma_E ~ exponential(1);
    for ( i in 1:N )
        R[i] ~ ordered_logistic( E[Sid[i], time[i]+1] + I[Iid[i]], kappa );


    // Mediation Model
    B_AD ~ std_normal();
    B_ED ~ std_normal();
    B_TD ~ std_normal();
    B_AE ~ std_normal();
    // pre-treatment Efficacy: affected by age
    for (s in 1:Ns)
        E[Sid[s], 1] ~ normal( B_AE*A[s], 1.5 );
    // post-treatment Efficacy
    vector[Ns] mu;
    for ( t in 1:(Nt-1) ) {
        for (s in 1:Ns) {
            mu[s] = E[s,1] + B_TE[Tx[s]]*t;
        }
        E[,t+1] ~ normal( mu, 1.5 );
    }
    // link to Outcome
    for ( i in 1:N )
        D[i] ~ normal( B_ED*E[Sid[i],time[i]+1] + B_AD*A[i] + B_TD[Tx[i]]*time[i], 3 );
}