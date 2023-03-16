/*
    Partial-pooled version of `unpooled.stan`
      See RatingScaleModel.bayes.R for data and fitting.
      
      // Model
      R ~ ordered_logistic( phi, cutpoints )
      phi = A[Sid] + E[Iid] + J[Jid]
      
      // Prior
      cutpoints ~ normal( 0, 1 )
      A ~ normal( 0, sigma_A )
      E ~ normal( 0, sigma_E )
      J ~ normal( 0, sigma_J )
      sigma_A ~ exponential( 1 )
      sigma_E ~ exponential( 1 )
      sigma_J ~ exponential( 1 )
*/
data{
    int N;
    int n_cutpoint;
    int n_judge;
    int n_subj;
    int n_item;
    array[N] int R;
    array[N] int Jid;
    array[N] int Iid;
    array[N] int Sid;
}
parameters{
    ordered[n_cutpoint] cutpoints;
    vector[n_subj] zA;
    vector[n_item] zE;
    vector[n_judge] zJ;
    real<lower=0> sigma_A;
    real<lower=0> sigma_E;
    real<lower=0> sigma_J;
}
transformed parameters {
    vector[n_subj] A;
    vector[n_item] E;
    vector[n_judge] J;
    A = zA * sigma_A;
    E = zE * sigma_E;
    J = zJ * sigma_J;
}
model{
    vector[N] phi;
    zA ~ normal( 0 , 1 );
    zE ~ normal( 0 , 1 );
    zJ ~ normal( 0 , 1 );
    sigma_A ~ exponential( 1 );
    sigma_E ~ exponential( 1 );
    sigma_J ~ exponential( 1 );
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
