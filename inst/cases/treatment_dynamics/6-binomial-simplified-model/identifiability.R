library(stom)
library(bayesplot)
library(ggplot2)
library(cowplot)
library(gridGraphics)
source("simulation.R")
source("post_predict.R")
color_scheme_set("viridis")
set.seed(1977)
d = sim_data( alpha=0,
              delta=-1.8,
              sigma_ET = .2,
              sigma_subj = .5 )
m = readRDS("m1-ncp0.RDS")


#### MCMC chains seem to suggest that the model is non-identifiable ####
# There are more than one solutions, or peaks, in the posterior. Different
# chains found different peaks.
pars = stom:::parse_pars("alpha,delta,B_TD,B_ED,B_AE,B_AD,B_TE,sigma_ET")
plt_trace = mcmc_trace(m$draws()[,,], regex_pars = pars,
                       facet_args = list(ncol=3),
                       window = c(800, 1000) ) +
    legend_none() +
    theme(
        plot.margin = margin(t=0, r=0, b=0, l=0, "cm")
    )



##### Compare posterior predictions of different chains #####
p1 = extract2(m, chains=1)
p2 = extract2(m, chains=2:3)
c1 = viridisLite::viridis(3, .85)[1]
c2 = viridisLite::viridis(3, .85)[3]

# c(bottom, left, top, right)
par( mfrow=c(3,1), mar=c(1,0,1,0) + 0.1, oma = c(2.5,2.5,0,0) + 0.1 ) # Create a 2 x 2 plotting matrix
for ( Tid in 1:3 ) {
    plot_model_prediction(Tid=Tid, G=1, post=p1, N=1000, uncertainty = T, empirical = T, col_main = c1)
    plot_model_prediction(Tid=Tid, G=1, post=p2, N=2000, uncertainty = T, empirical = F, col_main = c2, add=T)
}
title(xlab = "Time",
      ylab = "Days of heavy drinking",
      outer = T, line = 1)
p_base = recordPlot()

plot_grid(p_base, plt_trace,
          rel_widths = c(4.5, 7),
          scale = c(.9, .9),
          align = 'none',
          axis = "lb",
          ncol=2,
          labels = NULL)
