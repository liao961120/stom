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
    matrix[Ns,Nt] zE;
    real<lower=0> sigma_ET;

    // Mediation model params
    matrix[2,Ntx] B_TE;  // Treatment on Efficacy (indirect effect)
    real B_AE;           // Age on Efficacy
    real delta;          // global intercept (E linear model)
    
    // Subject baseline efficacy (varying intercepts)
    vector[Ns] Z_subj;
    real<lower=0> sigma_subj;

    // Outcome params
    vector[Ntx-1] zB_TD;   // Treatment on Outcome (direct effect): reference constrained
    real<lower=0> sigma_B_TD;  // Common std among direct treatment effect
    real B_AD;           // Age on outcome
    real B_ED;           // Efficacy on outcome
    real alpha;          // global intercept (D linear model)
}
transformed parameters {
    // IRT item params (sum-to-zero contrained)
    vector[Ni] I = sigma_I * append_row( zI_raw,-sum(zI_raw) );

    // Partial pool subject intercepts
    vector[Ns] E_subj = sigma_subj * Z_subj;

    matrix[Ns,Nt] E;
    // Transformed E
    for ( i in 1:NO ) {
        int sid = Sid_O[i];
        int time = time_O[i];
        real muE = delta + B_AE*A[i] + B_TE[G[i],Tx[i]]*time;
        E[sid,time+1] = fma(zE[sid,time+1], sigma_ET, muE);
    }

    // Direct treatment effect
    // partial-pool to allow model identification (by narrowing variance among treatment)
        /*
            Two strategies to constrain the model
                1. Partial-pool BOTH mu and sigma for B_TD
                2. Constrain one of B_TD to zero, set others as relative to zero + Partial-pool sigma
                
                The second strategy is easier to sample for MCMC, but the first 
                seems more intuitive within a Bayesian workflow.
                See `m1-ncp.stan` for 1 and `m1-ncp-ref_constrained.stan` for 2. 
        */
    vector[Ntx] B_TD = append_row(0, sigma_B_TD * zB_TD);
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
    sigma_ET ~ std_normal();

    // Priors for direct treament effects (T -> D)
    zB_TD ~ std_normal();
    sigma_B_TD ~ std_normal();
    B_AD ~ std_normal();
    alpha ~ normal(0, 1.5);

    // Priors for subject varying intercepts
    Z_subj ~ std_normal();
    sigma_subj ~ std_normal();  // half-normal

    // Causes of E (see transformed parameters block)
    to_vector(zE) ~ std_normal();
    
    // Causes of D
    vector[NO] mu;
    for ( i in 1:NO ) {
        int sid, time;
        sid = Sid_O[i];
        time = time_O[i];
        mu[i] = alpha + B_TD[Tx[i]]*time + B_AD*A[i] + B_ED*E[sid,time+1];
    }
    D ~ binomial_logit( 14, -mu );

    // Measurement model (IRT)
    vector[NI] phi;
    for ( i in 1:NI )
        phi[i] = E[Sid_I[i],time_I[i]+1] + I[Iid_I[i]];
    R ~ ordered_logistic( phi, kappa );
}
