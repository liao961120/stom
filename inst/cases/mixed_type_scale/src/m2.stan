data {
    int N ;  // num of test takers
    int Ns;  // num of subjective items
    int No;  // num of objective items
    int Np;  // num of rating-scale points
    int Ni;  // total number of items
    array[Ni] int<lower=1,upper=2> I;    // item type
    array[N,Ni] int<lower=0,upper=Np> R; // Item responses
}
parameters {
    // Hyperparameters (non-centered reparameterization)
    vector[N]  zA;
    vector[N]  zS;
    vector[Ni] zE;
    real muA;
    real muS;
    real muE;
    real<lower=0> sigmaA;
    real<lower=0> sigmaS;
    real<lower=0> sigmaE;
    // baseline cutpoints for ordered logit distribution
    ordered[Np-1] kappa;
}
transformed parameters {
    vector[N]  A;  // person ability
    vector[N]  S;  // person subjectivity
    vector[Ni] E;  // item easiness
    A = zA * sigmaA + muA;
    S = zS * sigmaS + muS;
    E = zE * sigmaE + muE;
}
model {
    // Priors
    muA ~ std_normal();
    muS ~ std_normal();
    muE ~ std_normal();
    sigmaA ~ std_normal();
    sigmaS ~ std_normal();
    sigmaE ~ std_normal();
    zA ~ std_normal();
    zS ~ std_normal();
    zE ~ std_normal();
    kappa ~ student_t(1, 0, 1);

    // Model
    {
        real phi;
        for (s in 1:N) {
            for (i in 1:Ni) {
                // Subjective Items
                if (I[i] == 1) {
                    phi = A[s] + S[s] + E[i];
                    R[s,i] ~ ordered_logistic(phi, kappa);
                }
                // Objective Items
                else {
                    phi = A[s] + E[i];
                    R[s,i] ~ bernoulli_logit(phi);
                }
            }
        }
    }
}
