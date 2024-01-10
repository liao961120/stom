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
    array[N]  real A;    // person ability
    array[N]  real S;    // person subjectivity
    array[Ni] real E;    // item easiness
    ordered[Np-1] kappa; // baseline cutpoints for ordered logit distribution
}
model {
    // Priors
    A ~ student_t(1, 0, 1);
    S ~ student_t(1, 0, 1);
    E ~ student_t(1, 0, 1);
    kappa ~ std_normal();

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
