data {
    int N;                               // num of observed trials
    int Ns;                              // num of subjects
    array[N] int Sid;                    // Subject ID
    array[N] real<lower=.1> RT;          // Reaction time
    array[N] int<lower=1,upper=2> cond;  // Stimulus (1 or 2)
    array[N] int<lower=1,upper=2> resp;  // Choice: 1 for stim1; 2 for stim2
    array[N] int<lower=0,upper=1> hit;   // 1 if cond == resp; 0 otherwise
}
parameters {
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
}
transformed parameters {
    vector[Ns] alpha = exp(mu_alpha + s_alpha * z_alpha);    // Boundary separation        (0, Inf)
    vector[Ns] beta = inv_logit(mu_beta + s_beta * z_beta);  // Bias (0.5: non-biased)     (0, 1)
    vector[Ns] tau = exp(mu_tau + s_tau * z_tau);            // non-decision time (sec.)   [0, Inf)    
    array[2] vector[Ns] delta;                               // drift rates (1:stim1 / 2:stim2)
    delta[1] = mu_dr1 + s_dr1 * z_dr1;
    delta[2] = mu_dr2 + s_dr2 * z_dr2;
}
model {
    // Priors
    alpha ~ normal(0, 1.5);
    beta ~ beta(2, 2);
    tau ~ std_normal();
    dr1 ~ student_t(1, 1, 1);   
    dr2 ~ student_t(1, -1, 1);  

    // Model
    for (i in 1:N) {
        int r, c;
        r = resp[i];  // response (choice) at trial i
        c = cond[i];  // stimulus presented at trial i

        // Upper-bound passage time
        if (r == 1)
            RT[i] ~ wiener(alpha, tau, beta, delta[c]);
        // Lower-bound passage time
        else
            RT[i] ~ wiener(alpha, tau, 1 - beta, -delta[c]);
    }
    
}
