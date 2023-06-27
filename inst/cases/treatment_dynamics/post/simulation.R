library(stom)

sim_data = function(
        B_AE = .1,             # Effect of Age on Efficacy
        B_TE = c(              # Effect of Treatment on Efficacy (row: gender, col: treatment)
            .3, .7,  1.3,
            .3,  1,   .7
        ),
        B_AD = .2,             # Effect of Age on Outcome
        B_ED = 1,              # Effect of Efficacy on Outcome
        B_TD = c(0, 0, 0),     # Effect of Treatment on Outcome
        delta = -1.8,          # Intercept for "E model"
        tau = 0.8,             # std for subject baseline
        alpha = 0              # Intercept for "D model"
    ){

    Ntx = 3        # number of treatments
    Ns = Ntx * 30  # number of subjects
    Nt = 4         # number of time points

    Tx = rep(1:Ntx, each = Ns / Ntx)  # Treatment condition for each subj
    G = rep(1:2, times = Ns / 2)      # Gender of each subj
    A = rtnorm( Ns, m = 36, lower = 18, upper = 80, s = 20 )  # Age

    # Transform Age to a reasonable scale
    minA = min(A)
    A = (A - minA) / 10  # 1 unit of increase = 10 years of increase in original age

    B_TE = matrix( B_TE, nrow=2, byrow = T )
    row.names(B_TE) = c("male", "female")
    t = 0:(Nt - 1)  # time points of measure

    # Causes of E
    subj = rnorm(Ns, 0, tau)
    E = sapply(t, function(time) {
        b_TE = sapply( 1:Ns, function(i) B_TE[ G[i],Tx[i] ] )
        delta + subj + B_AE * A + b_TE * time
    })
    # Causes of D
    D_latent = sapply(t, function(time) {
        alpha + B_TD[Tx]*time + B_AD * A + B_ED * E[, time + 1]
    })
    # Latent to outcome
    D = sapply(t, function(time) {
        mu = -D_latent[, time+1]
        rbinom( Ns, size=14, prob=logistic(mu) )
    })

    # Item response model
    Ni = 20  # number of items
    I = seq(-4.5, 4.5, length = Ni)  # item easiness (sums to zero)
    kappa = logit(cumsum(simplex(c(1, 2, 4, 4, 2, 1))))
    kappa = kappa[-length(kappa)]
    # Item-level responses (subject-item-time)
    dI = expand.grid( Sid=1:Ns, Iid=1:Ni, time=t, KEEP.OUT.ATTRS=F )
    for (i in 1:nrow(dI)) {
        # Item responses
        dI$R[i] = with(dI, {
            rordlogit(E[Sid[i], time[i] + 1] + I[Iid[i]], kappa = kappa)
        })
    }

    # Outcome-level responses (subject-time)
    dO = expand.grid( Sid=1:Ns, time=t, KEEP.OUT.ATTRS=F )
    dO$A = with( dO, A[Sid] )
    dO$Tx = with( dO, Tx[Sid] )
    dO$G = with( dO, G[Sid] )
    dO$D = NA
    dO$D_latent = NA
    dO$E = NA
    for ( i in 1:nrow(dO) ) {
        s = dO$Sid[i]
        t_ = dO$time[i] + 1
        dO$E[i] = E[s, t_]
        dO$D_latent[i] = D_latent[s, t_]
        dO$D[i] = D[s, t_]
    }

    # Gather data (for Stan)
    dat = list(
        Ns = Ns,                 # num of subjects
        Ntx = Ntx,               # num of treatments
        Nt = Nt,                 # num of time-points
        Nk = length(kappa) + 1,  # num of ordinal choices
        Ni = Ni,                 # num of items

        # Item-level responses (N=Ns*Ni*Nt)
        NI = Ns * Ni * Nt,
        Sid_I  = dI$Sid,
        Iid_I  = dI$Iid,
        time_I = dI$time,
        R      = dI$R,           # item responses

        # Outcome-level responses (N=Ns*Nt)
        NO = Ns * Nt,
        Sid_O  = dO$Sid,
        time_O = dO$time,
        G      = dO$G,
        A      = dO$A,
        Tx     = dO$Tx,
        D      = dO$D,           # outcome (binomial)
        D_latent  = dO$D_latent  # outcome (latent)
    )
    true_params = list(
        alpha = alpha,
        delta = delta,
        B_AE = B_AE,
        B_TE = B_TE,
        B_AD = B_AD,
        B_ED = B_ED,
        B_TD = B_TD,
        E = E,
        I = I,
        sigma_I = sd(I),
        kappa = kappa,
        tau = tau,
        subj = subj
    )
    others = list(
        minA = minA,
        D = D,
        D_latent = D_latent
    )
    return(list(
        dat = dat,
        params = true_params,
        others = others
    ))
}
