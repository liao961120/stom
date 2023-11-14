library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

source("simulation.R")
set.seed(1977)
d = sim_data()
m = readRDS("m1.RDS")

source("func_post_predict.R")
post = stom::extract2(m)


############ Posterior Predictions #############
## Prediction from recruited subjects
set.seed(250)
# plot_model_prediction(Tid=1, G=1)  # Treatment 3
# plot_model_prediction(Tid=1, G=2)  # Treatment 3
# plot_model_prediction(Tid=2, G=1)  # Treatment 2
# plot_model_prediction(Tid=2, G=2)  # Treatment 2
# plot_model_prediction(Tid=3, G=1)  # Treatment 1
# plot_model_prediction(Tid=3, G=2)  # Treatment 1
plot_model_prediction(Tid=3, G=1, col_main = 4, uncertainty = T, empirical = F)  # Treatment 3
plot_model_prediction(Tid=3, G=2, add = T, uncertainty = T, empirical = F)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


######## Estimand: improvements in days of drinking (Tx=3/2 - Tx=1)  #####
Sids = rep( 1:d$dat$Ns, length=2970 ) |> sample()
times = seq(1, 4, .01)
# table(Sids)

cf = list(
    T1 = list( G1=c(), G2=c(), all=c() ),
    T2 = list( G1=c(), G2=c(), all=c() ),
    T3 = list( G1=c(), G2=c(), all=c() )
)
for ( tx in 1:3 ) {
    for ( g in 1:2 )
        cf[[tx]][[g]] = sapply( 1:2970, function(i) {
            D = predict_obs(Tid=tx, Sid=Sids[i], A=NULL, G=g, idx=i) 
            14 * logistic(-D)
        })
    cf[[tx]][[3]] = sapply( 1:2970, function(i) {
        D = predict_obs(Tid=tx, Sid=Sids[i], A=NULL, G=NULL, idx=i) 
        14 * logistic(-D)
    })
}

# Collapse gender
T3T1_noG = treatment_contrast(3,1,3)
T2T1_noG = treatment_contrast(2,1,3)
plot_treatment_contrast_noG(T3T1_noG, "Treatment 3 - Treatment 1")
plot_treatment_contrast_noG(T2T1_noG, "Treatment 2 - Treatment 1")

# Consider gender
T3T1 = treatment_contrast2(3,1)
T2T1 = treatment_contrast2(2,1)
plot_treatment_contrast(T3T1, "Treatment 3 - Treatment 1")
plot_treatment_contrast(T2T1, "Treatment 2 - Treatment 1")

