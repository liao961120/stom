---
title: Growth Curve Modeling with Bayes
author: Yongfu Liao
---


Description
-----------

Growth curve models are often applied to situations where there are repeated
observations for the same subjects at different time points, i.e., longitudinal
data. The structure of the data that can be fitted with growth curve models
resembles something like this:

| $Sid$ | $T_x$ | $t_0$ | $t_1$ | $t_2$ |
|-------|-------|-------|-------|-------|
| 1     | CBT   | 0.01  | 0.24  | 0.42  |
| 2     | CBT   | -0.74 | 1.38  | -1.12 |
| 3     | ACT   | -0.70 | 0.25  | 0.69  |
| 4     | ACT   | -0.26 | -0.29 | 1.01  |
| 1     | CBT   | 0.035 | 0.18  | 0.16  |
| 2     | CBT   | 0.75  | 1.11  | 0.68  |
| 3     | ACT   | -1.33 | 0.03  | -2.33 |
| 4     | ACT   | 2.08  | 0.38  | -0.60 |

Here, the $Sid$ column indicates the id of the participants; $T_x$ indicates
the treatment received; and the last three columns record the _outcome_ of the
treatment at three different time points. The intervals between adjacent time
points should be constant. 


### A Standard Model

A commonly seen formulation of the growth curve model is shown in (1).

$$
\begin{equation}
    \begin{aligned}
    y   & \sim Normal( \mu, \sigma )  \\
    \mu &  = \alpha + \alpha_{T_x} + \alpha_{Sid} + \beta_{t} time + \beta_{t {:} T_x} time
    \end{aligned}
\end{equation}
$$

The $\alpha$ parameters are intercepts, which estimate the effects discarding
the time dimension. The $\beta$ parameters are _slopes_. Or more intuitively,
the _rates of changes_, which are the amounts of increases added to the outcome
when moving one unit of time upwards. The effects of the treatments are found in
the $\beta_{t {:} T_x}$ parameters, which estimate the rate of change for
each treatment **in addition to the global rate of change**, estimated through
the parameter $\beta_{t}$.

Solomon Kurz provides a working example for fitting a growth curve model of this
kind in his blog post[^post].


### A Simplified Model

The model in (1) is parameterized in a somewhat **contrast-oriented** way. This
is the classic way to parameterize a model in many fields, which prefer to use
contrasts to understand the effects of interest (e.g., how much **better** is
_ACT_ compared to _CBT_.). This method, however, makes it harder to connect the
effects to real-world entities.

Below, I provide a simplified specification of the model from an **estimation
perspective**. Instead of asking how much better is _ACT_ than _CBT_, the
estimation perspective asks how good each of them is. If we need to compare the
two, simply subtract the estimated effect of _CBT_ from that of _ACT_. The
meaning of the parameters changes but should now become more intuitive. This
simplified model is shown in (2). Two modifications are applied here.

#### Dropping the global intercept $\alpha$

When the global intercept is dropped, the effect originally presented in it
will be picked up by the remaining intercepts. This makes more sense to me, as
the global intercept is rather abstract and its meaning changes greatly
according to what has been modeled. For the case here, the original global
intercept gets picked up by the treatment and the subject intercepts.

#### Dropping the global rate of change $\beta_t$

Similar to the global intercept, when the global rate of change is eliminated
from the model, its effect will be collected by the remaining slopes. In the
current example, it will be picked up by $\beta_{t {:} T_x}$. So now, the
interpretation of $\beta_{t {:} T_x}$ simplifies to "the rate of change for
each treatment".

$$
\begin{equation}
    \begin{aligned}
    y   & \sim Normal( \mu, \sigma )  \\
    \mu &  = \alpha_{T_x} + \alpha_{Sid} + \beta_{t{:}T_x} time
    \end{aligned}
\end{equation}
$$



Bayesian Growth Curve Models
----------------------------

To explore the fitting of growth curve models in *Stan*, the example provided in
Solomon Kurz's post mentioned above is refitted with *Stan* and `lme4::lmer()`.
The code is available in `verify.R` and `m0.stan` on GitHub[^repo].

The scripts `simulation.R`, `fit.R`, and `m1.stan`[^note] further explore the
simplified model discussed above in the Bayesian modeling framework. A
simulation is set up to test if the *Stan* and *lmer* models both correctly
recover the parameters. The models work as expected.



[^post]: <https://solomonkurz.netlify.app/blog/2021-04-22-effect-sizes-for-experimental-trials-analyzed-with-multilevel-growth-models-two-of-two>
[^repo]: <https://github.com/liao961120/stom/tree/main/inst/cases/growth_curve_model>
[^note]: Also found at the location above.
