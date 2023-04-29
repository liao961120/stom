library(stom)

sim_data = function() {
    set.seed(1023)
    ## Subject-level params
    Ns = 50  # number of subj
    A = rtnorm( Ns, m=27, lower=18, upper=80, s=15 )  # age of subj
    As = (A - 18) / 80
    AE = -.3  # age effect on efficacy
    AC = -.3  # age effect on control
    Eb = standardize( rnorm( Ns, AE*As ), -1 )  # baseline self-efficacy before treatment
    Tr = sample( 1:3, size=Ns, replace=T )  # received treatment
    # before treatment outcomes
    C0 = rnorm( Ns, Eb + AC*As )
    D0 = rbinom( Ns, 90, logistic(-C0) )
    # hist(C0)
    # hist(D0)

    # Treatment-level params
    Nt = 4  # number of time points (include baseline)
          ## RP  MBRP TAU(ref)
    TE = c( .3,  .3,   0 )  # Treatment effect on self-efficacy (indirect)
    TC = c( 0,   .3,   0 )  # Treatment effect on self-control (direct)
    tE = .05  # Unexplained effect of time on self-efficacy
    E = matrix( NA, nrow=Ns, ncol=Nt )  # Self-Efficacy
    for ( t in 1:Nt ) {
      if ( t == 1 )
        E[, t] = Eb
      else
        E[, t] = rnorm( Ns, E[, t-1] + tE + TE[Tr])
    }
    C = rnorm( Ns, E[, Nt] + AC*As + TC[Tr] )  # Self-control at final time point
    D = rbinom( Ns, size=90, prob = logistic(-C) ) # Days of use (outcome)
    # hist(C)
    # hist(D)

    # rethinking::dens( C0 ); rethinking::dens( C, add=T, col=2 )
    # rethinking::dens( D0 ); rethinking::dens( D, add=T, col=2 )


    # Item-level params
    Ni = 8  # number of items (6-point Likert)
    I = seq( -1.5, 1.5, length=Ni )     # item easiness (Efficacy): sum-to-zero constraint
    kappa = c(-2.3, -1.1, 0, 1.1, 2.3 ) # baseline latent scores
    R = array( NA, dim=c(Ns, Ni, Nt) )  # Subj's responses on survey

    # Visualize rating responses across time points
    for ( t in 1:Nt ) {
      for ( i in 1:Ni ) {
            # person + item
        phi = E[, t] + I[i]
        R[ ,i,t] = stom::rordlogit( phi, kappa )
      }
    }
    # plot( 1, type="n", xlim=c(.5, 6.5), ylim=c(0, .5))
    # s = -.15
    # col = 1
    # for ( t in 1:Nt ) {
    #   count = table( R[,,t] )
    #   prob = count / sum(count)
    #   for ( x in seq_along(prob) )
    #     lines( c(x+s,x+s), c(0,prob[x]), lwd=8, col=col )
    #   s = s + .1
    #   col = col + 1
    # }


    # Collect data
    dat = list(
      Ns = Ns, # number of subjects
      Ni = Ni, # number of items in the self-efficacy scale
      Nt = Nt, # number of time points
      Nk = length(kappa) + 1,  # number of Likert scale points
      R = R,   # Responses on self-efficacy scale: array[Ns, Ni, Nt]
      Tr = Tr, # Received treatment Tr[Ns]
      As = As, # age (min-max scaled): A[Ns]
      D = D,   # outcome: days of heavy drinking: D[Ns]
      Nd = 90  # number of days for outcome evaluation
    )
    return(list(
      dat = dat,
      param = list(
        A     = A    ,   # Age (ori)
        # Efficacy mediation model
        AE    = AE   ,   # Age on Efficacy
        AC    = AC   ,   # Age on Control
        TE    = TE   ,   # Treatment on Efficacy
        TC    = TC   ,   # Treatment on Control
        tE    = tE   ,   # Time on on Efficacy
        # IRT submodel
        E     = E    ,   # Efficacy
        I     = I    ,   # Item easiness
        kappa = kappa    # baseline latent scores
      )
    ))
}
