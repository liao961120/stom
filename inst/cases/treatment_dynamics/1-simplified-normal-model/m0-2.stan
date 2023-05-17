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
    real alpha;  // global intercept for treatment-Efficacy linear model
    // IRT model params
    vector[Ns*Nt-1] E_raw;
    // matrix[Ns,Nt] E;
    // matrix[Ns,Nt] zE;
    // real muE;
    // real<lower=0> sigma_E;   // global, IRT
    real<lower=0> sigma_Et;  // conditional on treatment, mediation model
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
    // matrix[Ns,Nt] E;  // Efficacy
    // sum-to-0 contraint on I
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    matrix[Ns,Nt] E;
    E = to_matrix( append_row(E_raw, -sum(E_raw)), Ns, Nt );
    // E = sigma_E * zE + muE;
}
model {
    alpha ~ std_normal();
    // to_vector(E) ~ normal(0, 2);
    E_raw ~ normal(0, 2.5);
    // muE ~ normal(0, 2.5);
    // sigma_E ~ exponential(.5);
    sigma_Et ~ normal(1, 1);
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    B_TE ~ std_normal();  // treatment indirect effect (vector)
    B_AE ~ std_normal();

    vector[N] mu;
    vector[N] phi;
    // vector[N] mu;
    for ( i in 1:N ) {
        // Mediation Submodel
        // pre-treatment Efficacy: affected by age
                                   //  E0          Treatment
        mu[i] = E[Sid[i],time[i]+1] - (B_AE*A[i] + B_TE[Tx[i]] * time[i] + alpha);
        
        // IRT Submodel (Efficacy Measure)
        phi[i] = E[Sid[i],time[i]+1] + I[Iid[i]];
    }
    R ~ ordered_logistic( phi, kappa );
    mu ~ normal( 0, sigma_Et );
}
