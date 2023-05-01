################################################
#' Stan version of Growth Curve Modeling Script
#' 
#' @Data
#' src: https://solomonkurz.netlify.app/blog/2021-04-22-effect-sizes-for-experimental-trials-analyzed-with-multilevel-growth-models-two-of-two/
################################################

library(stom)
library(lme4)
library(dplyr)
library(tidyr)
library(stringr)

d <-
  tribble(
    ~id, ~tx, ~t1, ~t2, ~t3, ~t4,
    101, -0.5, 3, 5, 5,  7,
    102, -0.5, 4, 4, 6,  6,
    103, -0.5, 4, 5, 7,  8,
    104, -0.5, 5, 6, 6,  8,
    105, -0.5, 5, 6, 7,  8,
    106, -0.5, 5, 7, 7,  7,
    107, -0.5, 5, 6, 8,  8,
    108, -0.5, 6, 6, 7,  9,
    109, -0.5, 6, 8, 9,  10,
    110, -0.5, 7, 7, 8,  9,
    111,  0.5, 3, 5, 7,  9,
    112,  0.5, 4, 7, 9,  11,
    113,  0.5, 4, 6, 8,  11,
    114,  0.5, 5, 7, 9,  10,
    115,  0.5, 5, 6, 9,  11,
    116,  0.5, 5, 7, 10, 10,
    117,  0.5, 5, 8, 8,  11,
    118,  0.5, 6, 7, 9,  12,
    119,  0.5, 6, 9, 11, 13,
    120,  0.5, 7, 8, 10, 12
  ) %>% 
  mutate(`t4-t1`   = t4 - t1,
         condition = ifelse(tx == -0.5, "control", "treatment"))

d_long <-
  d %>% 
  pivot_longer(t1:t4, values_to = "y") %>% 
  mutate(time = str_extract(name, "\\d") %>% as.double()) %>% 
  mutate(time_f = (time * 2) - 5,
         time_c = time - mean(time),
         time0  = time - 1,
         time01 = (time - 1) / 3) |> 
  select( id, tx, time0, y, `t4-t1`)

dat = list(
  N = nrow(d_long),
  Ns = 20,
  Nt = 4,
  Ntx = 2,
  id = d_long$id-100,
  tx =  ifelse( d_long$tx == -.5, 1, 2 ),
  time0 = d_long$time0,
  y = d_long$y
)

######################
## Model fitting #####
######################
# lme4
m.f = lmer(y ~ 1 + time0 + tx + time0:tx + (1 + time0 | id), data = d_long)
summary(m.f)
m.f_subj_int = ranef(m.f)$id[,1]
m.f_subj_slp = ranef(m.f)$id[,2]

# stan
m.b = stan("m0.stan", data=dat)
post = stom::extract(m.b)
precis( m.b, pars=c("alpha", "Bt", "Tx", "Bt_Tx") )

m.b_subj_int = post[, startsWith(names(post), "As") ] |> 
  apply( 2, function(col) c( mean(col), quantile(col,.05), quantile(col,.95) ) )
m.b_subj_slp = post[, startsWith(names(post), "Bt_s") ] |> 
  apply( 2, function(col) c( mean(col), quantile(col,.05), quantile(col,.95) ) )


# lme4 vs. stan
plot(m.f_subj_int, m.b_subj_int[1,]);abline(0, 1)
plot(m.f_subj_slp, m.b_subj_slp[1,]);abline(0, 1)

plot( 1, type="n", ylim=c(.5,20.5), xlim=c(-2.6,2.6),
      xlab="Intercept", ylab="Subject ID" )
for (i in 1:ncol(m.b_subj_int))
  lines( c(m.b_subj_int[2,i],m.b_subj_int[3,i]), c(i,i), lwd=6, col=col.alpha(2) )
points( m.b_subj_int[1,], y=1:20, col=2, pch=19 )
points( m.f_subj_int    , y=1:20, col=4, pch= )

