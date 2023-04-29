data {
    int Ns;  // number of subjects
    int Ni;  // number of items in the Efficacy scale
    int Nt;  // number of time points
    int Nk;  // number of Likert scale points
    int Nd;  // number of days for outcome evaluation
    array[Ns,Ni,Nt] int<lower=1> R;  // responses on Efficacy scale
    array[Ns] int<lower=1> Tr;       // received treatment
    vector[Ns] As;  // Age (min-max scaled)
    array[Ns] int<lower=0,upper=Nd> D ;  // Outcome: days of heavy drinking
}
parameters {
    // IRT model params
    real muE;
    real<lower=0> sigma_E;
    real<lower=0> sigma_I;
    matrix[Ns,Nt] zE;
    vector[Ni-1] zI_raw;
    ordered[Nk-1] kappa;

    // Mediation model params
    real AE;       // Age on Efficacy
    real AC;       // Age on Control
    real tE;       // Time on Efficacy
    // Non-centered parameterization for TE/TC (partial-pooled)
    vector[3] zTE;
    vector[3] zTC;
    real<lower=0> sigma_TE;
    real<lower=0> sigma_TC;
    real muTE;
    real muTC;
    real muEb;  // baseline grand mean Efficacy before treatment
    array[Nt] real<lower=0> sigma_Et;  // SD of efficacy of each time step;
}
transformed parameters {
    // IRT model params
    vector[Ni] I;
    matrix[Ns,Nt] E;
    // sum-to-0 contraint on I
    I = sigma_I * append_row( zI_raw,-sum(zI_raw) );
    E = sigma_E * zE + muE;

    // Mediation model params
    vector[3] TE;  // Treatment on Efficacy
    vector[3] TC;  // Treatment on Efficacy
    TE =  zTE * sigma_TE + muTE;  // Treatment on Efficacy
    TC =  zTC * sigma_TC + muTC;  // Treatment on Efficacy
}
model {
    // IRT Submodel (Efficacy Measure)
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

    // Mediation Model
    vector[Ns] mu;
    muEb ~ normal( 0, 1 );
    sigma_Et ~ exponential(1);
    tE ~ normal( 0, 1 );
    TE ~ normal( 0, 1 );
    AE ~ normal( 0, 1 );
    // pre-treatment Efficacy: affected by age
    for (s in 1:Ns) {
        mu[s] = AE * As[s] + muEb;
    }
    E[,1] ~ normal( mu, sigma_Et[1] );
    // post-treatment Efficacy
    for (t in 2:Nt) {
        for (s in 1:Ns) {
            mu[s] = E[s,t-1] + tE + TE[Tr[s]];
        }
        E[,t] ~ normal( mu, sigma_Et[t] );
    }
    // Link to outcome
    D ~ binomial_logit( Nd, E[,Nt] );
}
