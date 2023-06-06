library(bayesplot)
color_scheme_set("darkgray")

######## Visualize where divergences happen #######
# See <https://mc-stan.org/bayesplot/reference/MCMC-parcoord.html>
m = readRDS("m1.RDS")
s = stom::precis(m, 5)

draws = m$draws(
    c("sigma_ET",
      "alpha",
      "B_AE",
      "kappa",
      "delta"
      ))
np = bayesplot::nuts_params( m )

div_style <- parcoord_style_np(div_color = "green", div_size = 0.05, div_alpha = 0.4)
mcmc_parcoord(draws,
              transform = function(x) {(x - mean(x)) / sd(x)},
              size = 0.25, alpha = 0.1,
              np = np, np_style = div_style)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



###### Check which params caused Low E-BFMI ########
# See <https://mc-stan.org/misc/warnings.html#bfmi-low>
dg = m$sampler_diagnostics(format = "df")
s2 = extract(m)

x = sapply(colnames(s2), function(c_) {
    cor( s2[[c_]], dg$energy__ )
})
hist(x)
which( abs(x) > .5 )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
