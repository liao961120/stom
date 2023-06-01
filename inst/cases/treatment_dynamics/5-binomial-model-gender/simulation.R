# Simulation of subject varying effects

library(stom)

exponent = function(x, base=1.35) base^x

sim_data = function(alpha = 0,  # outcome global intercept ( to shift poisson to sensible location)
                    B_AE = .1,
                    B_TE = c(.3, .8,  1.3,
                             .3,  1.3, .8),
                    B_AD = .2,
                    B_ED = 1,
                    B_TD = c(0, 0, 0),
                    outcome = "binom") {

    Ns = 3 * 50  # number of subjects
    Ntx = 3      # number of treatments
    Nt = 4       # number of time points
    Tx = rep(1:Ntx, each = Ns / Ntx)
    G = rep(1:2, times = Ns / 2)
    B_TE = matrix( B_TE, nrow=2, byrow = T )
    row.names(B_TE) = c("male", "female")
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


    ###### Treatment-level parameters #######
    # B_AE = 1
    # B_TE = c(.5, 1, 2)
    # B_AD = .8
    # B_ED = 1.5
    # B_TD = c(0, 0, 0)
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    ##### Subject-level parameters (Varying effects) #####
    library(MASS)
    nm = "G_TE Delta_TE  G_TD  Alpha_TD"
    nm = strsplit(nm, "\\s+")[[1]]
            # G_TE delta_TE  G_TD  alpha_TD
    sigma = c( .3,    .3,     .3,     .3 )   # hyper-parameters
    Rho = matrix(c(                         # hyper-parameters
        # G_TE delta_TE  G_TD  alpha_TD
             1,   .1,      0,      0,   # G_TE
            NA,    1,      0,      0,   # delta_TE
            NA,   NA,      1,      0,   # G_TD
            NA,   NA,     NA,      1    # alpha_TD
    ), byrow = T, ncol=4 )
    # Record parameter names
    row.names(Rho) = nm
    colnames(Rho) = nm
    names(sigma) = nm
    Rho = Matrix::forceSymmetric(Rho, "U")
    S = diag(sigma) %*% Rho %*% diag(sigma)
    subj_params = mvrnorm( Ns, mu=rep(0,4), Sigma=S )  # sample from MVNormal
    colnames(subj_params) = nm

    G_TE = subj_params[, 1]
    Delta_TE = subj_params[, 2]
    G_TD = subj_params[, 3]
    Alpha_TD = subj_params[, 4]
    # Compare to true val
    # cor(G_TE, delta_TE)      # Rho[1, 2]
    # cor(G_TE, G_TD)          # Rho[1, 3]
    # cor(G_TE, alpha_TD)      # Rho[1, 4]
    # cor(delta_TE, G_TD)      # Rho[2, 3]
    # cor(delta_TE, alpha_TD)  # Rho[2, 4]
    # cor(G_TD, alpha_TD)      # Rho[3, 4]
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    E = sapply(t, function(time) {
        # latent trait across time points (including E0)
        b_TE = sapply( 1:Ns, function(i) B_TE[ G[i],Tx[i] ] )
        rnorm(Ns, (Delta_TE + G_TE*time) + B_AE * A + b_TE * time, 1)
    })
    E = E - mean(E)  # 0-centered
    D_raw = sapply(t, function(time) {
        # Outcome across time (latent trait underlying days of drinking)
        alpha + (Alpha_TD + G_TD * time) + B_TD[Tx]*time + B_AD * A + B_ED * E[, time + 1]
    })
    D_norm = sapply(t, function(time) {
        rnorm( Ns, D_raw[, time+1] )  # Normal outcome
    })
    D_pois = sapply(t, function(time) {
        lambda = exponent( -D_raw[, time+1] )
        rpois( Ns, lambda )
    })
    D_binom = sapply(t, function(time) {
        mu = -D_raw[, time+1]
        rbinom( Ns, size=14, prob=logistic(mu) )
    })

    Ni = 20  # number of items
    ei = seq(-6, 6, length = Ni)  # item easiness (sums to zero)
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
    dO$G = with( dO, G[Sid] )
    for ( i in 1:nrow(dO) ) {
        s = dO$Sid[i]
        t_ = dO$time[i] + 1
        dO$E[i] = E[s, t_]
        dO$D_pois[i] = D_pois[s, t_]
        dO$D_norm[i] = D_norm[s, t_]
        dO$D_binom[i] = D_binom[s, t_]
        dO$D_raw[i] = D_raw[s, t_]
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
        G      = dO$G,
        A      = dO$A,
        Tx     = dO$Tx,
        D_raw  = dO$D_raw         # outcome (latent)
        # D_norm  = dO$D_norm,    # outcome (Normal)
        # D_pois  = dO$D_pois     # outcome (Poisson)
    )
    outcome = paste0( "D_", outcome )
    if ( !outcome %in% colnames(dO) )
        stop("Unsupported outcome distribution: ", outcome )
    dat$D = dO[[outcome]]

    # Records only names that matches those used in stan
    true_params = list(
        alpha = alpha,
        B_AE = B_AE,
        B_TE = B_TE,
        B_AD = B_AD,
        B_ED = B_ED,
        B_TD = B_TD,
        E = E,
        I = ei,
        kappa = kappa,
        subj = list(
            sigma_subj = sigma,
            Rho = as.matrix(Rho),  # strip off attributes
            V_subj = subj_params,
            G_TE = G_TE,
            Delta_TE = Delta_TE,
            G_TD = G_TD,
            Alpha_TD = Alpha_TD
        )
    )
    others = list(
        D =  list(D_pois=D_pois, D_norm=D_norm, D_binom=D_binom)[[outcome]],
        D_raw = D_raw,
        minA = minA
    )
    return(list(
        dat = dat,
        params = true_params,
        others = others
    ))
}

