data {
    int N;                               // num of observed trials
    array[N] real<lower=0> RT;           // Reaction time
    array[N] int<lower=1,upper=2> cond;  // Stimulus (1 or 2)
    array[N] int<lower=1,upper=2> resp;  // Choice: 1 for stim1; 2 for stim2
    array[N] int<lower=0,upper=1> hit;   // 1 if cond == resp; 0 otherwise
}
parameters {
    real<lower=0> alpha;               // Boundary separation        (0, Inf)
    real<lower=0,upper=1> beta;        // Bias (0.5: non-biased)     (0, 1)
    real<lower=0> dr1;                 // drift rate for stimulus 1
    real<upper=0> dr2;                 // drift rate for stimulus 2
    real<lower=.1,upper=max(RT)> tau;  // non-decision time (sec.)   [0, Inf)
}
transformed parameters {
    array[2] real delta;
    delta[1] = dr1;
    delta[2] = dr2;
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
