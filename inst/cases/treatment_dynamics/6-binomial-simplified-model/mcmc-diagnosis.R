library(bayesplot)
color_scheme_set("viridis")

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
p = stom:::parse_pars("sigma_ET,E,I,alpha,delta,B_AE,B_AD,B_TE")
s2 = extract(m)

x = sapply(colnames(s2), function(c_) {
    cor( s2[[c_]], dg$energy__ )
})
hist(x)
x[which( abs(x) > .9 )]

x_cor = cor(s2$sigma_ET, s2)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pars = stom:::parse_pars("alpha,delta,B_TD,B_ED,B_AE,B_AD,B_TE,sigma_ET")
p = stom::extract(m, pars)

s = sample(1:3000, 300, replace=F)
p[s,-(12:14)] |> pairs()



library(bayesplot)
color_scheme_set("viridis")
mcmc_trace(m$draws(), regex_pars = pars, facet_args = list(ncol=3))

mcmc_trace(m$draws()[,,],
           pars="sigma_ET",
           transformations = "log",
           facet_args = NULL
           )
