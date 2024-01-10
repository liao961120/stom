data {
    int N ;  // num of test takers
    int Ns;  // num of subjective items
    int No;  // num of objective items
    int Np;  // num of rating-scale points
    int Ni;  // total number of items
}
generated quantities {
    // Simulated variables
    array[N]  real A = normal_rng( rep_array(0,N) , rep_array(1,N) );     // person ability
    array[N]  real S = normal_rng( rep_array(0,N) , rep_array(1,N) );     // person subjectivity
    array[Ni] real E = normal_rng( rep_array(0,Ni), rep_array(1,Ni) );    // item easiness
    array[Ni] int  I = append_array( rep_array(1,Ns), rep_array(2,No) );  // item type
    vector[Np-1] kappa = linspaced_vector(Np-1, -2, 2);                   // baseline cutpoints for ordered logit distribution
    array[N,Ni] int R;                                                   // Simulated Responses

    {
        real phi;
        for (s in 1:N) {
            for (i in 1:Ni) {
                // Subjective Items
                if (I[i] == 1) {
                    phi = A[s] + S[s] + E[i];
                    R[s,i] = ordered_logistic_rng(phi, kappa);
                }
                // Objective Items
                else {
                    phi = A[s] + E[i];
                    R[s,i] = bernoulli_logit_rng(phi);
                }
            }
        }
    }
}
