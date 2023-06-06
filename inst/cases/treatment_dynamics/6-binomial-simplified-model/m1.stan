/*
    Binomial Outcome model, with no varying effects of subjects
*/

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
    array[NO] real<lower=0,upper=20> A;        // Age scaled: (A-min(A))/10 
    array[NO] int<lower=1> Tx;                 // Treatment received
    array[NO] int<lower=1,upper=2> G;          // Gender
    array[NO] int<lower=0,upper=14> D;         // Binomial outcome (heavy drinkings in the last 14 days)
}
parameters {
    // IRT model params
    real<lower=0> sigma_I;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;
    matrix[Ns,Nt] E;
    real<lower=0> sigma_ET;

    // Mediation model params
    matrix[2,Ntx] B_TE;  // Treatment on Efficacy (indirect effect)
    real B_AE;           // Age on Efficacy
    real delta;          // global intercept (E linear model)
    
    // Subject baseline efficacy (varying intercepts)
    vector[Ns] Z_subj;
    real<lower=0> sigma_subj;

    // Outcome params
    vector[Ntx] B_TD;  // Treatment on Outcome (direct effect)
    real B_AD;         // Age on outcome
    real B_ED;         // Efficacy on outcome
    real alpha;        // global intercept (D linear model)
}
transformed parameters {
    // IRT item params (sum-to-zero contrained)
    vector[Ni] I = sigma_I * append_row( zI_raw,-sum(zI_raw) );

    // Partial pool subject intercepts
    vector[Ns] E_subj = sigma_subj * Z_subj;
}
model {
    // Priors for IRT parameters
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);
    kappa ~ std_normal();

    // Priors for indirect treatment effects (T -> E -> D)
    to_vector(B_TE) ~ normal(0, 1.5);
    B_AE ~ std_normal();
    B_ED ~ std_normal();
    delta ~ normal(0, 1.5);
    sigma_ET ~ normal(0.2, 0.5);

    // Priors for direct treament effects (T -> D)
    B_TD ~ std_normal();
    B_AD ~ std_normal();
    alpha ~ normal(0, 1.5);

    // Priors for subject varying intercepts
    Z_subj ~ std_normal();
    sigma_subj ~ std_normal();  // half-normal

    // Measurement model (IRT)
    to_vector(E) ~ normal(0, 2);
    vector[NI] phi;
    for ( i in 1:NI )
        phi[i] = E[Sid_I[i],time_I[i]+1] + I[Iid_I[i]];
    R ~ ordered_logistic( phi, kappa );

    // Causes of E
    int sid, time;
    real muE;
    for ( i in 1:NO ) {
        sid = Sid_O[i];
        time = time_O[i];
        muE = delta + B_AE*A[i] + B_TE[G[i],Tx[i]]*time;
        E[sid,time+1] ~ normal(muE, sigma_ET);
    }

    // Causes of D
    vector[NO] mu;
    for ( i in 1:NO ) {
        sid = Sid_O[i];
        time = time_O[i];
        mu[i] = alpha + B_TD[Tx[i]]*time + B_AD*A[i] + B_ED*E[sid,time+1];
    }
    D ~ binomial_logit( 14, -mu );
}
