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
    matrix[Ns,Nt] E;
    real<lower=0> sigma_I;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;

    // Mediation model params
    real<lower=0> sigma_Et;  // conditional on treatment, mediation model
    real B_AE;               // Age on Efficacy
    vector[Ntx] B_TE;        // Treatment on Efficacy (indirect effect)
    real alpha;              // global intercept
}
transformed parameters {
    // IRT item params (sum-to-zero contrained)
    vector[Ni] I;
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
}
model {
    to_vector(E) ~ normal(0, 2.5);

    sigma_Et ~ exponential(1);
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    B_TE ~ std_normal(); 
    B_AE ~ std_normal();
    alpha ~ normal(0, 1.5);

    // Mediation submodel
    vector[NO] mu;
    for ( i in 1:NO )
        E[Sid_O[i],time_O[i]+1] ~ normal( B_AE*A[i] + B_TE[Tx[i]] * time_O[i] + alpha, sigma_Et );

    // IRT submodel
    vector[NI] phi;
    for ( i in 1:NI )
        phi[i] = E[Sid_I[i],time_I[i]+1] + I[Iid_I[i]];
    R ~ ordered_logistic( phi, kappa );
}


/*
    Next:
        1. Add Outcome model
        2. Random subject intercepts/slopes on alpha/B_TE
            -> correlation among random intercepts/slopes (bivariate normal)
*/
