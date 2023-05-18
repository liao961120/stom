data {
    int Ns;  // num of subjects
    int Ntx; // num of treatments
    int Nt;  // num of time points
    int Nk;  // num of Likert scale points
    int Ni;  // num of items in the self-efficacy scale
    // int Nk2; // for kappa

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
    // matrix[Ns,Nt] E;
    vector[Ns*Nt-1] E_raw;
    real<lower=0> sigma_I;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;
    // positive_ordered[Nk2] kappa_pos;
    // positive_ordered[Nk2] kappa_neg;

    // Mediation model params
    vector[Ntx] B_TE;        // Treatment on Efficacy (indirect effect)
    real B_AE;               // Age on Efficacy
    real alpha;              // global intercept (E linear model)
    real<lower=0> sigma_E;   // std Efficacy, conditional on Treatment & Age

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
    E = to_matrix( append_row(E_raw, -sum(E_raw)), Ns, Nt ) - 1; // -1-centered
    // vector[Nk-1] kappa;
    // if ( fmod(Nk-1, 2) == 0 )
    //     kappa = append_row( reverse(-kappa_neg), kappa_pos );
    // else
    //     kappa = append_row( 
    //         append_row( reverse(-kappa_neg), 0 ),
    //         kappa_pos
    //     );
}
model {
    // to_vector(E) ~ normal(0, 2.5);
    E_raw ~ normal(0, 2.5);
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    B_TE ~ normal(0, 1.5); 
    B_AE ~ normal(0, 1.5);
    alpha ~ normal(0, 1.5);
    sigma_E ~ exponential(1);

    B_TD ~ normal(0, 1.5);
    B_AD ~ normal(0, 1.5);
    B_ED ~ normal(0, 1.5);
    gamma ~ normal(0, 1.5);
    sigma_D ~ exponential(1);

    // Mediation submodel
    vector[NO] mu;
    for ( i in 1:NO ) {
        E[Sid_O[i],time_O[i]+1] ~ normal( B_AE*A[i] + B_TE[Tx[i]] * time_O[i] + alpha, sigma_E );
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
        1. Think harder on the scales:
            Scale of time: ranges from 0~3 
            Scale of age:  ranges from 0~1
            This results in different magnitute of beta parameters (slopes)
        2. Random subject intercepts/slopes on alpha/B_TE
            -> correlation among random intercepts/slopes (bivariate normal)
*/
