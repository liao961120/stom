/*
    Binomial Outcome model, with no varying effects of subjects
    ToDo: 
        test subject partial pooling on T -> E submodel
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
    array[NO] int<lower=0,upper=14> D;         // Binomial outcome (days of heavy drinking in last 14 days)
}
parameters {
    // IRT model params
    // vector[Ns*Nt-1] E_raw;
    matrix[Ns,Nt] E_raw;
    real<lower=0> sigma_I;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;

    // Mediation model params
    matrix[2,Ntx] B_TE;        // Treatment on Efficacy (indirect effect)
    real B_AE;                 // Age on Efficacy
    real delta_raw;            // global intercept (E linear model)
    real<lower=0> sigma_ET_raw;    // std Efficacy, conditional on Treatment & Age
    
    // vector[Ns] E_subj;
    vector[Ns] z_E_subj;         // Baseline subject efficacy
    real<lower=0> sigma_E_subj;

    // Outcome params
    vector[Ntx] B_TD;        // Treatment on Outcome (direct effect)
    real B_AD;               // Age on outcome
    real B_ED;               // Efficacy on outcome
    real alpha;              // global intercept (D linear model)
}
transformed parameters {
    // IRT item params (sum-to-zero contrained)
    vector[Ni] I;
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    matrix[Ns,Nt] E;
    E = 2 * E_raw;

    real sigma_ET = .7 * sigma_ET_raw;
    real delta = 2 * delta_raw;

    // Partial pool subject intercept
    vector[Ns] E_subj;
    E_subj = z_E_subj * (1 + .5 * sigma_E_subj);
}
model {
    // Priors for IRT parameters
    to_vector(E_raw) ~ std_normal();
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    // Priors for indirect treatment effects (T -> E -> D)
    to_vector(B_TE) ~ normal(0, 1.5);
    B_AE ~ std_normal();
    B_ED ~ std_normal();
    delta_raw ~ std_normal();
    sigma_ET_raw ~ std_normal();   // half-normal
    z_E_subj ~ std_normal();
    sigma_E_subj ~ std_normal();  // half-normal
    
    // Priors for direct treament effects (T -> D)
    B_TD ~ std_normal();
    B_AD ~ std_normal();
    alpha ~ std_normal();

    // Mediation submodel
    int sid;
    real muET; 
    vector[NO] mu;
    for ( i in 1:NO ) {
        sid = Sid_O[i];
        // mu = delta + E_subj     + beta_AE*A +         beta_TE*t
        muET = delta + E_subj[sid] + B_AE*A[i] + B_TE[G[i],Tx[i]]*time_O[i];
        E[sid,time_O[i]+1] ~ normal( muET, sigma_ET );
        mu[i] =  alpha + B_TD[Tx[i]]*time_O[i] + B_AD*A[i] + B_ED*E[sid,time_O[i]+1];
    }
    D ~ binomial_logit( 14, -mu );

    // IRT submodel
    vector[NI] phi;
    for ( i in 1:NI )
        phi[i] = E[Sid_I[i],time_I[i]+1] + I[Iid_I[i]];
    R ~ ordered_logistic( phi, kappa );
}
