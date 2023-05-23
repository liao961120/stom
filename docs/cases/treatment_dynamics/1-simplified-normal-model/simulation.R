#' Growth curve modeling of latent trait
library(stom)

sim_data = function(B_AE = 1,
                    B_AD = .8,
                    B_ED = 1.5,
                    B_TD = c(0, 0, 0),
                    B_TE = c(.5, 1, 2)) {

    set.seed(199)

    Ns = 3 * 10  # number of subjects
    Ntx = 3    # number of treatments
    Nt = 4     # number of time points
    Tx = rep(1:Ntx, each = Ns / Ntx)
    A = rtnorm(
        Ns,
        m = 38,
        lower = 18,
        upper = 83,
        s = 20
    )  # Age
    minA = min(A)
    A = (A - minA) / 10  # 1 unit of increase = 10 years of increase in original age
    t = 0:(Nt - 1)  # time points of measure
    E = sapply(t, function(time) {
        # latent trait across time points (including E0)
        rnorm(Ns, B_AE * A + B_TE[Tx] * time)
    })
    E = E - mean(E)  # 0-centered
    U = rnorm(Ns)  # unmeasured influence on D
    D = sapply(t, function(time) {
        # Outcome across time (latent trait underlying days of drinking)
        rnorm(Ns, B_TD[Tx]*time + B_ED * E[, time + 1] + B_AD * A + U)
    })

    Ni = 20  # number of items
    ei = seq(-3, 3, length = Ni)  # item easiness (sums to zero)
    kappa = logit(cumsum(simplex(c(1, 2, 3, 3, 2, 1))))
    kappa = kappa[-length(kappa)]

    # Item-level responses (subject-item-time observation)
    dI = expand.grid( Sid=1:Ns, Iid=1:Ni, time=t, KEEP.OUT.ATTRS=F )
    for (i in 1:nrow(dI)) {
        # Item responses
        dI$R[i] = with(dI, {
            rordlogit(E[Sid[i], time[i] + 1] + ei[Iid[i]], kappa = kappa)
        })
    }

    # Outcome-level responses (subject-time observation)
    dO = expand.grid( Sid=1:Ns, time=t, KEEP.OUT.ATTRS=F )
    dO$A = with( dO, A[Sid] )
    dO$Tx = with( dO, Tx[Sid] )
    for ( i in 1:nrow(dO) ) {
        s = dO$Sid[i]
        t_ = dO$time[i] + 1
        dO$E[i] = E[s, t_]
        dO$D[i] = D[s, t_]
    }

    # Gather data
    dat = list(
        Ns = Ns,                  # num. of subjects
        Ntx = Ntx,                # num. of treatments
        Nt = Nt,                  # num. of time-points
        Nk = length(kappa) + 1,   # num. of ordinal choices
        Ni = Ni,                  # num. of items
        Nk2 = floor( length(kappa)/2 ),

        # Item-level responses (N=Ns*Ni*Nt)
        NI = Ns * Ni * Nt,
        Sid_I  = dI$Sid,
        Iid_I  = dI$Iid,
        time_I = dI$time,
        R      = dI$R,            # item responses

        # Outcome-level responses (N=Ns*Nt)
        NO = Ns * Nt,
        Sid_O  = dO$Sid,
        time_O = dO$time,
        A      = dO$A,
        Tx     = dO$Tx,
        D      = dO$D             # outcome
    )
    true_params = list(
        B_TE = B_TE,
        B_ED = B_ED,
        B_AE = B_AE,
        B_AD = B_AD,
        E = E,
        I = ei,
        kappa = kappa,
        minA = minA
    )
    return(list(dat = dat, params = true_params))
}


# d = sim_data()
#
# library(dplyr)
# library(ggplot2)
#
# dat1 = d$dat[c("Sid_I", "time_I", "R")] |> data.frame()
# dat2 = d$dat[c("Sid_O", "time_O", "A", "Tx", "D")] |> data.frame()
# names(dat1) = gsub("_I$", "", names(dat1))
# names(dat2) = gsub("_O$", "", names(dat2))
#
# dat |> group_by(Tx, time) |> summarise(D=mean(D))
#
#
# dat2 |>
#     group_by(Sid, time) |>
#     summarise(D = mean(D)) |>
#     mutate(Tx = case_when(
#         Sid %in% 1:10 ~ "1",
#         Sid %in% 11:20 ~ "2",
#         Sid %in% 21:30 ~ "3"
#     )) |>
#     mutate(Sid = factor(Sid)) |>
#     ggplot()+
#         geom_line(aes(x=time, y=D, color=Tx, group=Sid))

