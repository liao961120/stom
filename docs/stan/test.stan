// Simple model for testing stan compilation
data {
    int N;
    vector [N] y;
}
parameters {
    real mu;
}
model {
    y ~ normal( mu, 1 );
    mu ~ normal( 0, 1 );
}
