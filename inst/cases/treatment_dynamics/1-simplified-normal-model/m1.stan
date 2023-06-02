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
    array[NO] real<lower=0,upper=100> A;       // Age scaled: (A-min(A)) / 10 
    array[NO] int<lower=1> Tx;                 // Treatment received
    array[NO] real D;                          // Outcome: observed heavy drinking tendency (coined, for scaffolding larger models later)
}
parameters {
    // IRT model params
    vector[Ns*Nt-1] E_raw;
    real<lower=0> sigma_I;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;

    // Mediation model params
    vector[Ntx] B_TE;        // Treatment on Efficacy (indirect effect)
    real B_AE;               // Age on Efficacy
    real alpha;              // global intercept (E linear model)
    real<lower=0> sigma_ET;  // std Efficacy, conditional on Treatment & Age

    // Outcome params
    vector[Ntx] B_TD;        // Treatment on Outcome (direct effect)
    real B_AD;               // Age on outcome
    real B_ED;               // Efficacy on outcome
    real gamma;              // global intercept (D linear model)
    real<lower=2> sigma_D;   // std outcome, conditional on Efficacy, Age & Treatment
}
transformed parameters {
    // IRT item params (sum-to-zero contrained)
    vector[Ni] I;
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    matrix[Ns,Nt] E;
    E = to_matrix( append_row(E_raw, -sum(E_raw)), Ns, Nt );
}
model {
    E_raw ~ normal(0, 2);
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    B_TE ~ normal(0, 1.5); 
    B_AE ~ normal(0, 1.5);
    alpha ~ normal(0, 1.5);
    sigma_ET ~ exponential(1);
    
    B_TD ~ normal(0, 1.5);
    B_AD ~ normal(0, 1.5);
    B_ED ~ normal(0, 1.5);
    gamma ~ normal(0, 1.5);
    sigma_D ~ exponential(1);

    // Mediation submodel
    vector[NO] mu;
    for ( i in 1:NO ) {
        E[Sid_O[i],time_O[i]+1] ~ normal( B_AE*A[i] + B_TE[Tx[i]] * time_O[i] + alpha, sigma_ET );
        mu[i] =  B_AD*A[i] + B_TD[Tx[i]] * time_O[i] + B_ED*E[Sid_O[i],time_O[i]+1] + gamma;
    }
    D ~ normal( mu, sigma_D );
        
    // IRT submodel
    vector[NI] phi;
    for ( i in 1:NI )
        phi[i] = E[Sid_I[i],time_I[i]+1] + I[Iid_I[i]];
    R ~ ordered_logistic( phi, kappa );
}

/*
    Next:
        1. Poisson outcome
        2. Random subject intercepts/slopes on alpha/B_TE
            -> correlation among random intercepts/slopes (bivariate normal)
*/
