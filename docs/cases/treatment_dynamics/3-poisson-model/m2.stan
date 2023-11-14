/*
    Poisson Outcome model, with logarithmic base of log link set to 1.35
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
    array[NO] int<lower=0> D;                  // Poisson outcome (days of heavy drinking)
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
    real delta;              // global intercept (E linear model)
    real<lower=0> sigma_ET;  // std Efficacy, conditional on Treatment & Age

    // Outcome params
    vector[Ntx] B_TD;        // Treatment on Outcome (direct effect)
    real B_AD;               // Age on outcome
    real B_ED;               // Efficacy on outcome
    real alpha;              // global intercept (D linear model)

    // Subject varying effects parameters (non-centered parameterization)
    array[4] real<lower=0> sigma_subj;
    matrix[4, Ns] Z_subj;
    cholesky_factor_corr[4] L_cholesky;
}
transformed parameters {
    // IRT item params (sum-to-zero contrained)
    vector[Ni] I;
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    matrix[Ns,Nt] E;
    E = to_matrix( append_row(E_raw, -sum(E_raw)), Ns, Nt );

    // Subjecct varying effects
    //   Order:   1       2      3       4
    //          G_TE, Delta_TE, G_TD, Alpha_TD
    matrix[Ns, 4] V_subj;
    V_subj = ( diag_pre_multiply( to_vector(sigma_subj), L_cholesky) * Z_subj )' ;
                                        // diag(sigma) %*%  L       %*%   Z   )^T
}
model {
    E_raw ~ normal(0, 2);
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    B_TE ~ std_normal(); 
    B_AE ~ std_normal();
    delta ~ std_normal();
    sigma_ET ~ exponential(1.5);
    
    B_TD ~ std_normal();
    B_AD ~ std_normal();
    B_ED ~ std_normal();
    alpha ~ std_normal();

    to_vector(Z_subj) ~ std_normal();
    sigma_subj ~ exponential(1.8);
    L_cholesky ~ lkj_corr_cholesky(2);

    // Mediation submodel
    int sid;
    real muET; 
    vector[NO] mu;
    for ( i in 1:NO ) {
        sid = Sid_O[i];
        // mu = delta + (  Delta_TE   +         G_TE * t       ) + beta_AE*A +     beta_TE*t
        muET = delta + (V_subj[sid,2] + V_subj[sid,1]*time_O[i]) + B_AE*A[i] + B_TE[Tx[i]]*time_O[i];
        E[sid,time_O[i]+1] ~ normal( muET, sigma_ET );
        // mu =  alpha + (  Alpha_TD    +         G_TD * t       ) +     beta_TD*t         + beta_AD*A + B_ED*E
        mu[i] =  alpha + (V_subj[sid,4] + V_subj[sid,3]*time_O[i]) + B_TD[Tx[i]]*time_O[i] + B_AD*A[i] + B_ED*E[sid,time_O[i]+1];
    }
    D ~ poisson( pow(1.35, -mu) );  // use base=1.35 instead of natural exponent

    // IRT submodel
    vector[NI] phi;
    for ( i in 1:NI )
        phi[i] = E[Sid_I[i],time_I[i]+1] + I[Iid_I[i]];
    R ~ ordered_logistic( phi, kappa );
}
generated quantities {
   corr_matrix[4] Rho;
   vector[Ns] G_TE, Delta_TE, G_TD, Alpha_TD;
   Rho       =  multiply_lower_tri_self_transpose(L_cholesky);  // R = LL'
   G_TE      =  V_subj[,1];
   Delta_TE  =  V_subj[,2];
   G_TD      =  V_subj[,3];
   Alpha_TD  =  V_subj[,4];
}
