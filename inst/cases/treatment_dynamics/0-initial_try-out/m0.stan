data {
    int Ns;  // number of subjects
    int Ni;  // number of items in the self-efficacy scale
    int Nt;  // number of time points
    int Nk;  // number of Likert scale points
    array[Ns,Ni,Nt] int<lower=1> R;  // Responses on self-efficacy scale
    array[Ns] int<lower=1> Tr;       // received treatment
    vector[Ns] As;  // age (min-max scaled)
    array[Ns] int<lower=0> D ;  // outcome: days of heavy drinking
}
parameters {
    real muE;
    real<lower=0> sigma_E;
    real<lower=0> sigma_I;
    matrix[Ns,Nt] zE;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;
}
transformed parameters {
    vector[Ni] I;
    matrix[Ns,Nt] E;
    // sum-to-0 contraint on I
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    E = sigma_E * zE + muE;
}
model {
    real phi;
    kappa ~ normal(0, 1);
    to_vector(zE) ~ normal(0, 1);
    zI_raw ~ normal(0, 1);
    muE ~ normal(0, 1.5);
    sigma_I ~ exponential(1);
    sigma_E ~ exponential(1);

    for (t in 1:Nt) {
        for (s in 1:Ns) {
            for (i in 1:Ni) {
                phi = I[i] + E[s,t];
                R[s,i,t] ~ ordered_logistic(phi, kappa);
            }
        }
    }
}
