Change of Variables in hierarchical models to avoid HMC sampling difficulty
===========================================================================

Hi everyone!

I was trying to extend the measurement error model example provided in the 
[Stan User Guide](https://mc-stan.org/docs/stan-users-guide/bayesian-measurement-error-model.html#regression-with-measurement-error):

```stan
data {
  // ...
  array[N] real x_meas;   // measurement of x
  real<lower=0> tau;     // measurement noise
}
parameters {
  array[N] real x;    // unknown true value
  real mu_x;          // prior location
  real sigma_x;       // prior scale
  // ...
}
model {
  x ~ normal(mu_x, sigma_x);  // prior
  x_meas ~ normal(x, tau);    // measurement model
  y ~ normal(alpha + beta * x, sigma);
  // ...
}
```

What I would like to model is expressed in the Directed Acyclic Graph (DAG)
below. The extended model now assumes an influence on the unobserved $X$ by the
variable $Z$.

<img src="https://raw.githubusercontent.com/liao961120/stom/main/inst/cases/measurement_error_models/doc/dag.svg" width="200">

I constructed this model (`m1.stan`) in stan and found that the chains had 
difficulty mixing well.

```stan
data {
    int N;
    vector[N] z;
    vector[N] x_obs;
    vector[N] y;
}
parameters {
    vector[N] x_true;
    real mu_z;
    real<lower=0> sigma_z;
    real<lower=0> sigma_x;
    real<lower=0> sigma_y;
    real<lower=0> tau;
    real b_xz;
    real a_xz;
    real b_yz;
    real b_yx;
    real a_yx;
}
model {
    mu_z ~ std_normal();
    sigma_z ~ std_normal();
    sigma_x ~ std_normal();
    sigma_y ~ std_normal();
    tau ~ std_normal();
    b_xz ~ std_normal();
    a_xz ~ std_normal();
    b_yz ~ std_normal();
    b_yx ~ std_normal();
    a_yx ~ std_normal();

    z ~ normal(mu_z, sigma_z);
    x_true ~ normal( a_xz + b_xz * z, sigma_x );
    x_obs ~ normal( x_true, tau );
    y ~ normal( a_yx + b_yx * x_true + b_yz * z, sigma_y );
}
```

Here's the trace plot:

![](https://raw.githubusercontent.com/liao961120/stom/main/inst/cases/measurement_error_models/doc/trace-centered.png)


After some trial and error, I managed to re-express the model as `m2.stan` below
(with some help from Stan User's Guide on [Changes of
Variables](https://mc-stan.org/docs/stan-users-guide/changes-of-variables.html)).
The model now samples smoothly and correctly recovers the parameters from the
simulated data. But the way I get it to work worries me since it is nothing
similar to the suggestions on non-centered parameterizations I found online. In
particular, I found that transforming the variable `x_obs` (which is a data
variable) to get rid of `tau` and `x_true` in the sampling statement is required
for the model to sample smoothly. But online examples on reparameterizations
transform only parameters. I'm unsure whether this change of variable on
non-parameter variables is a justifiable way to make MCMC sampling smoother.
Hope anyone can provide some advice on this!

```stan
data {
    int N;
    vector[N] z;
    vector[N] x_obs;
    vector[N] y;
}
parameters {
    vector[N] x_true_std;
    real mu_z;
    real<lower=0> sigma_z;
    real<lower=0> sigma_x;
    real<lower=0> sigma_y;
    real<lower=0> tau;
    real b_xz;
    real a_xz;
    real b_yz;
    real b_yx;
    real a_yx;
}
transformed parameters {
    vector[N] x_true = x_true_std * sigma_x + (a_xz + b_xz * z);
}
model {
    mu_z ~ std_normal();
    sigma_z ~ std_normal();
    sigma_x ~ std_normal();
    sigma_y ~ std_normal();
    tau ~ std_normal();
    b_xz ~ std_normal();
    a_xz ~ std_normal();
    b_yz ~ std_normal();
    b_yx ~ std_normal();
    a_yx ~ std_normal();

    z ~ normal(mu_z, sigma_z);
    vector[N] x_obs_std = (x_obs - x_true) / tau;
    vector[N] J_diag = rep_vector(-2*log(tau), N);  // diagonals of Jacobian
    target += sum( log(J_diag) );  // Jacobian adjustment
    x_true_std ~ std_normal();
    x_obs_std ~ std_normal();
    y ~ normal( a_yx + b_yx * x_true + b_yz * z, sigma_y );
}
```



Code for Model Fitting
----------------------

All the source code can be found on [GitHub](https://github.com/liao961120/stom/blob/main/inst/cases/measurement_error_models).
I paste the code and output below for clarity.


### Data Simulation in R

```r
sim_data = function() {
    N = 500
    mu_z = 0
    sigma_z = 1
    sigma_x = 1
    sigma_y = 1
    tau = 1
    b_xz = .8
    a_xz = -1
    b_yz = .7
    b_yx = .5
    a_yx = 3

    z = rnorm( N, mu_z, sigma_z )
    x_true = rnorm( N, b_xz*z + a_xz ,sigma_x )
    x_obs = rnorm( N, x_true, tau )
    y = rnorm(N, a_yx + b_yx * x_true + b_yz * z, sigma_y )

    return(list(
        params = list(
            mu_z = mu_z,
            sigma_z = sigma_z,
            sigma_x = sigma_x,
            sigma_y = sigma_y,
            tau = tau,
            b_xz = b_xz,
            a_xz = a_xz,
            b_yz = b_yz,
            b_yx = b_yx,
            a_yx = a_yx,

            x_true = x_true
        ),
        dat = list(
            N = N,
            z = z,
            x_obs = x_obs,
            y = y
        )
    ))
}
```


### The Centered Model (`m1.stan`)

```r
library(cmdstanr)
set.seed(1977)
d = sim_data()  # Simulated data

m = stan( "m1.stan", data=d$dat,
          chains=3, parallel_chains=3,
          save_warmup = TRUE,
          iter_warmup  = 1000,
          iter_sampling = 1000,
          init = NULL,        # Initial values for parameters
          step_size = NULL    # initial step size (default:1)
)
#> ...
#> Chain 3 finished in 19.4 seconds.
#> 
#> All 3 chains finished successfully.
#> Mean chain execution time: 17.7 seconds.
#> Total execution time: 20.0 seconds.
#> 
#> Warning: 3 of 3 chains had an E-BFMI less than 0.2.
#> See https://mc-stan.org/misc/warnings for details.
```

```r
m$diagnostic_summary()
#> $num_divergent
#> [1] 0 0 0
#> 
#> $num_max_treedepth
#> [1] 0 0 0
#> 
#> $ebfmi
#> [1] 0.01786259 0.04330291 0.05912812
```

#### Trace Plot

```r
library(bayesplot)
color_scheme_set("viridis")
pars = c('mu_z', 'sigma_z', 'sigma_y', 'sigma_x', 'tau', 
         'b_yz', 'b_yx', 'b_xz', 'a_yx', 'a_xz')
mcmc_trace(m$draws(pars), facet_args=list(ncol=2))
```

![](https://raw.githubusercontent.com/liao961120/stom/main/inst/cases/measurement_error_models/doc/trace-centered.png)



### The Non-centered Model (`m2.stan`)

```r
library(cmdstanr)
set.seed(1977)
d = sim_data()

m = stan( "m2.stan", data=d$dat,
          chains=3, parallel_chains=3,
          save_warmup = TRUE,
          iter_warmup  = 1000,
          iter_sampling = 1000,
          init = NULL,        # Initial values for parameters
          step_size = NULL    # initial step size (default:1)
)
#> ...
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 18.1 seconds.
#> 
#> All 3 chains finished successfully.
#> Mean chain execution time: 17.6 seconds.
#> Total execution time: 18.4 seconds.
```

```r
> m$diagnostic_summary()
#> $num_divergent
#> [1] 0 0 0
#> 
#> $num_max_treedepth
#> [1] 0 0 0
#> 
#> $ebfmi
#> [1] 0.4620434 0.4613685 0.4234962
```

#### Trace Plot

```r
library(bayesplot)
color_scheme_set("viridis")
pars = c('mu_z', 'sigma_z', 'sigma_y', 'sigma_x', 'tau', 
         'b_yz', 'b_yx', 'b_xz', 'a_yx', 'a_xz')
mcmc_trace(m$draws(pars), facet_args=list(ncol=2))
```

![](https://raw.githubusercontent.com/liao961120/stom/main/inst/cases/measurement_error_models/doc/trace-noncentered.png)

