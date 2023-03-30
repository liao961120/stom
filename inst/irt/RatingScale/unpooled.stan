/*
    Rating scale model with person/item/judge intercepts 
      See RatingScaleModel.bayes.R for data and fitting.
      
      // Model
      R ~ ordered_logistic( phi, cutpoints )
      phi = A[Sid] + E[Iid] + J[Jid]
      
      // Prior
      cutpoints ~ normal( 0, 1 )
      A ~ normal( 0, 1 )
      E ~ normal( 0, 1 )
      J ~ normal( 0, 1 )
*/

data{
    int N; // Number of responses (nrow in long format)
    int n_cutpoint;
    int n_judge;
    int n_subj;
    int n_item;
    array[N] int R;
    array[N] int Jid;  // Judge ID
    array[N] int Iid;  // Item ID
    array[N] int Sid;  // Subject ID
}
parameters{
    ordered[n_cutpoint] cutpoints;
    vector[n_subj] A;  // Subject ability
    vector[n_item] E;  // Item easiness
    vector[n_judge] J; // Judge leniency
}
model{
    vector[N] phi;
    J ~ normal( 0 , 1 );
    E ~ normal( 0 , 1 );
    A ~ normal( 0 , 1 );
    cutpoints ~ normal( 0 , 1 );
    for ( i in 1:N ) {
        phi[i] = A[Sid[i]] + E[Iid[i]] + J[Jid[i]];
    }
    R ~ ordered_logistic( phi, cutpoints );
}
generated quantities{
    vector[N] log_lik;
    {
        vector[N] phi;
        for ( i in 1:N ) {
            phi[i] = A[Sid[i]] + E[Iid[i]] + J[Jid[i]];
            log_lik[i] = ordered_logistic_lpmf( R[i] | phi[i] , cutpoints );
        }
    }
}
