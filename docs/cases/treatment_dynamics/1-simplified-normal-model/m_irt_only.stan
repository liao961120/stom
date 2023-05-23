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
    matrix[Ns,Nt] zE;
    real muE;
    real<lower=0> sigma_E;   // global, IRT
    real<lower=0> sigma_I;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;
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
    zI_raw ~ std_normal();
    sigma_I ~ exponential(1);

    vector[N] phi;
    for ( i in 1:N ) {
        // IRT Submodel (Efficacy Measure)
        phi[i] = E[Sid[i],time[i]+1] + I[Iid[i]];
    }
    R ~ ordered_logistic( phi, kappa );
}
