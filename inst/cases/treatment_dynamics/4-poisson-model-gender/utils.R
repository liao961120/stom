# generates to unseen population (simulate subjects from Rho)
sim_V_subj = function(Ns=3000) {
    # need modification: one sample of covariance matrix from posterior
    #                    for each sampled subject (incorporating uncertainty)
    S = matrix(0, nrow=4, ncol=4)
    V = sapply(1:Ns, function(i) {
        Rho        = post$Rho(i)
        sigma_subj = post$sigma_subj(i)
        S = diag(sigma_subj) %*% Rho %*% diag(sigma_subj)
        mvrnorm( 1, mu=rep(0,4), Sigma=S )        
    })
    V = t(V)
    colnames(V) = c('G_TE', 'Delta_TE', 'G_TD', 'Alpha_TD')
    V
}

predict_new = function(Tid, V, G=1, Age=20, time=1:4, idx=1, latent=F) {
    A = ( Age - d$other$minA ) / 10
    
    alpha   = post$alpha(idx)
    b_AD    = post$B_AD(idx)
    b_ED    = post$B_ED(idx)
    
    delta = post$delta(idx)
    b_AE = post$B_AE(idx)
    sigma_ET = post$sigma_ET(idx)
    
    b_TD = post$B_TD(idx)
    b_TE = post$B_TE(idx)
    
    D = sapply( time, function(t) {
        muE = delta + (V[2] + V[1]*(t-1) ) + b_AE*A + b_TE[G,Tid]*(t-1)
        E = rnorm( 1, muE, sigma_ET )
        alpha + (V[4] + V[3]*(t-1) ) + b_TD[Tid]*(t-1) + b_AD*A + b_ED*E
    })
    if (!latent) 
        D = rpois( 1, lambda=1.35^(-D) )
    D
}

