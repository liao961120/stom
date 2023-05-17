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
    array[N] real<lower=0,upper=1> A;   // Age ( scaled: (x-18)/80 )
    array[N] real D;   // Outcome: observed heavy drinking tendency (coined, for scaffolding larger models later)
}
parameters {
    // IRT model params
    array[Nt] real muE;
    array[Nt] real<lower=0> sigma_E;
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
    // Outcome std
    real<lower=0> sigma_D;
}
transformed parameters {
    // IRT model params
    vector[Ni] I;     // Item params
    matrix[Ns,Nt] E;  // Efficacy
    // sum-to-0 contraint on I
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    for ( t in 1:Nt )
        E[,t] = sigma_E[t] * zE[,t] + muE[t];
}
model {
    // IRT Submodel (Efficacy Measure)
    to_vector(zE) ~ std_normal();
    zI_raw ~ std_normal();
    muE ~ normal(0, 2);
    sigma_I ~ exponential(1);
    sigma_E ~ exponential(1);
    for ( i in 1:N )
        R[i] ~ ordered_logistic( E[Sid[i], time[i]+1] + I[Iid[i]], kappa );

    // Mediation Model
    B_AD ~ std_normal();
    B_ED ~ std_normal();
    B_TD ~ std_normal();  // treatment direct effect (vector)
    B_TE ~ std_normal();  // treatment indirect effect (vector)
    B_AE ~ std_normal();
    // pre-treatment Efficacy: affected by age
    for (s in 1:Ns)
        E[Sid[s], 1] ~ normal( B_AE*A[s], sigma_E[1] );
    // post-treatment Efficacy
    vector[Ns] mu;
    for ( t in 1:(Nt-1) ) {
        for (s in 1:Ns) {
            mu[s] = E[s,1] + B_TE[Tx[s]]*t;
        }
        E[,t+1] ~ normal( mu, sigma_E[t+1] );
    }
    // link to (Normal) Outcome
    // sigma_D ~ exponential(1);
    // vector[N] muD;
    // for ( i in 1:N )
    //     muD[i] = B_ED*E[Sid[i],time[i]+1] + B_AD*A[i] + B_TD[Tx[i]]*time[i];
    // D ~ normal( muD, sigma_D );
}
