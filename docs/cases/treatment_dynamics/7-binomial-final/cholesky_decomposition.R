set.seed(100)

N_samp = 1e6
DIM = 2



sigma = c( 1, 1.5 )
R = matrix(c(
      1, .65,
    .65,   1
), nrow=2 )
S = diag(sigma) %*% R %*% diag(sigma)


# Cholesky decomposition to lower triangular Cholesky factor L
L = t( chol(R) )
Z = sapply( 1:2, function(x) rnorm(N_samp) )  # Uncorrelated samples
V = ( diag(sigma) %*% L %*% t(Z) ) |> t()     # Reconstruction of correlated samples from L
cor(V)  # Should match R
cov(V)  # should match S
