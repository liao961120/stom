functions {
    vector wiener_rng(real alpha, real beta, real delta, real tau) {
        vector[2] out;
        real dt = .0001;  // integration stepsize
        real t = 0;
        real w = alpha * beta;  // initial value
        while (0 < w && w < alpha) {
            w += delta * dt + sqrt(dt) * std_normal_rng();
            t += dt;
        }
        out[1] = tau + t;
        if (w >= alpha)
            out[2] = 1;  // Upper bound
        else
            out[2] = 2;  // Lower bound
        return out;  // {RT, boundary}
    }
}
data {
    int Ns;                              // num of subjects
    int N1;                              // num of trials with stim 1
    int N2;                              // num of trials with stim 2
    // array[N] real<lower=.1> RT;          // Reaction time
    // array[N] int<lower=1,upper=2> cond;  // Stimulus (1 or 2)
    // array[N] int<lower=1,upper=2> resp;  // Choice: 1 for stim1; 2 for stim2
    // array[N] int<lower=0,upper=1> hit;   // 1 if cond == resp; 0 otherwise

    /////////////// Hyper Params ////////////////////
    // Hyper-params for alpha(s)
    real mu_alpha;
    real<lower=0> s_alpha;
    vector[Ns] z_alpha;

    // Hyper-params for beta(s)
    real mu_beta;
    real<lower=0> s_beta;
    vector[Ns] z_beta;

    // Hyper-params for tau(s)
    real mu_tau;
    real<lower=0> s_tau;
    vector[Ns] z_tau;

    // Hyper-params for drift rates
    real mu_dr1;
    real<lower=0> s_dr1;
    vector[Ns] z_dr1;
    real mu_dr2;
    real<lower=0> s_dr2;
    vector[Ns] z_dr2;
    //////////////////////////////////////////////
}
transformed data {
   int N = Ns * (N1 + N2); // num of observed trials
}
generated quantities {
    // Simulated observations
    array[N] real<lower=.1> RT;   // Reaction time
    array[N] int cond;  // Stimulus (1 or 2)
    array[N] real resp;           // Choice: 1 for stim1; 2 for stim2
    array[N] int Sid;             // Subject ID

    ////////////////// Params ////////////////
    // Transformed params
    vector[Ns] alpha = exp(mu_alpha + s_alpha * z_alpha);    // Boundary separation        (0, Inf)
    vector[Ns] beta = inv_logit(mu_beta + s_beta * z_beta);  // Bias (0.5: non-biased)     (0, 1)
    vector[Ns] tau = exp(mu_tau + s_tau * z_tau);            // non-decision time (sec.)   [0, Inf)    
    array[2] vector[Ns] delta;                               // drift rates (1:stim1 / 2:stim2)
    delta[1] = mu_dr1 + s_dr1 * z_dr1;
    delta[2] = mu_dr2 + s_dr2 * z_dr2;
    ///////////////////////////////////////////////

    {
        vector[2] y;  // response holder
        int r = 1;    // row number
        int c;
        for ( s in 1:Ns ) {
            for ( i in 1:(N1+N2) ) {
                // Stimulus presentation
                if (i <= N1) 
                    c = 1;
                else 
                    c = 2;

                // Simulate diffusion
                y = wiener_rng(alpha[s], beta[s], delta[c,s], tau[s]);

                // Record data
                RT[r] = y[1];
                resp[r] = y[2];
                Sid[r] = s;
                cond[r] = c;

                r += 1;
            }
        }
    }

}