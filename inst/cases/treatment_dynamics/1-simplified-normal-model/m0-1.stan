data {
    int Ns;  // num of subjects
    int Ntx; // num of treatments
    int Nt;  // num of time points
    int Nk;  // num of Likert scale points
    int Ni;  // num of items in the self-efficacy scale

    // Item-level responses (N=Ns*Ni*Nt)
    int NI;
    array[NI] int<lower=1,upper=Ns> Sid_I;     // Subject ID
    array[NI] int<lower=1,upper=Ni> Iid_I;     // Item ID
    array[NI] int<lower=0,upper=Nt-1> time_I;  // time point of obs.
    array[NI] int<lower=1,upper=Nk> R;         // Responses on Efficacy scale

    // Outcome-level responses (N=Ns*Nt)
    int NO;
    array[NO] int<lower=1,upper=Ns> Sid_O;     // Subject ID
    array[NO] int<lower=0,upper=Nt-1> time_O;  // time point of obs.
    array[NO] real<lower=0,upper=1> A;         // Age ( scaled: (x-18)/80 )
    array[NO] int<lower=1> Tx;                 // Treatment received
    array[NO] real D;                          // Outcome: observed heavy drinking tendency (coined, for scaffolding larger models later)
}
parameters {
    // IRT model params
    matrix[Ns,Nt] zE;
    real muE;
    real<lower=0> sigma_E;   // global, IRT
    // real<lower=0> sigma_Et;  // conditional on treatment, mediation model
    real<lower=0> sigma_I;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;

    // Mediation model params
    real B_AE;    // Age on Efficacy
    vector[Ntx] B_TE;    // Treatment on Efficacy (indirect effect)
}
transformed parameters {
    // IRT model params
    vector[Ni] I;     // Item params
    matrix[Ns,Nt] E;  // Efficacy
    // sum-to-0 contraint on I
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    E = sigma_E * zE + muE;
}
model {
    to_vector(zE) ~ std_normal();
    muE ~ normal(0, 2.5);
    sigma_E ~ exponential(.5);
    // sigma_Et ~ exponential(1);
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    B_TE ~ std_normal();  // treatment indirect effect (vector)
    B_AE ~ std_normal();

    vector[NI] phi;
    // vector[N] mu;
    for ( i in 1:NI ) {
        // Mediation Submodel
        // pre-treatment Efficacy: affected by age
                                   //  E0          Treatment
        // mu[i] = E[Sid[i],time[i]+1] - (B_AE*A[i] + B_TE[Tx[i]] * time[i]);
        
        // IRT Submodel (Efficacy Measure)
        phi[i] = E[Sid_I[i],time_I[i]+1] + I[Iid_I[i]];
        // link to (Normal) Outcome
        // nu[i] = B_ED*E[Sid[i],time[i]+1] + B_AD*A[i] + B_TD[Tx[i]] * time[i];
    }
    // mu ~ normal(0, 1.5);  // discard sigma_Et
    // mu ~ normal( 0, sigma_Et );
    // D ~ normal( nu, sigma_D );
    R ~ ordered_logistic( phi, kappa );
}
