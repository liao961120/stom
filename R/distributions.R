#' Bernoulli Random Number Generator
#'
#' Alias of `rbinom()` for a single trial (`size = 1`).
#'
#' @param prob Vector of probabilities.
#' @param n Numbers of sample, defaults to the length of `prob`.
#' @export
rbern = function(prob, n=length(prob)) rbinom( n=n, size=1, prob=prob )



#' Random Number Generator for Truncated Normal Distribution
#'
#' The parameters are set to generate numbers from a half-normal distribution
#' by default.
#'
#' @param n Number of samples.
#' @param m The mean of the untruncated normal distribution.
#' @param s The standard deviation of the untruncated normal distribution.
#' @param lower The lower bound for truncation.
#' @param upper The upper bound for truncation.
#' @export
rtnorm = function(n, m=0, s=1, lower=0, upper=Inf) {
  n_sampled = 0
  sampled = c()
  while (length(sampled) < n) {
    x = rnorm( n, m, s )
    sampled = c( sampled, x[x > lower & x < upper] )
  }
  sampled[seq(n)]
}


#' Random Number Generator from Ordered Logit Distribution
#'
#' @param phi Numeric. The linear term(s) to subtract from
#'        each ordered categories.
#' @param kappa A vector of cut-points for delineating the
#'        ordered categories on the logit scale. For generating
#'        `n` categories, `n-1` cut-points are needed. Do not
#'        include (negative) `Inf` values.
#' @examples
#' kappa = c( -1.5, 0, 1.5 )
#' rordlogit( phi=0, kappa=kappa )
#' x = replicate( 1e4, {rordlogit(0, kappa)})
#' table(x) / length(x)
#' logistic( c(kappa, Inf) ) - logistic( c(-Inf, kappa) )
#' @export
rordlogit = function( phi, kappa ) {
  a = c( -Inf, kappa, Inf )
  sapply(phi, function(phi_) {
    Pc = logistic( a - phi_ )
    P = Pc[-1] - Pc[-length(Pc)]
    sample( seq_along(P), size=1, prob=P )
  })
}



#' Random Number Generator from Gamma-Poisson Distribution
#'
#' @param n Number of samples.
#' @param mu Poisson mean (rate)
#' @param scale Parameter determining the dispersion of the outcomes.
#'        This parameter is equivalent to Stan's `neg_binomial_2(mu, phi)`'s `phi`.
#'
#' @details
#' See Chapter 12.1.2 of Statistical Rethinking 2th Edition, which details the
#' Gamma-Poisson distribution for modeling over-dispersed count data.
rgampois2 = function (n , mu , scale) {
    # Matches stan neg_binomial_2(mu, phi=scale)
    p_p = scale / (scale + mu)
    p_n = scale
    rnbinom(n, size = p_n, prob = p_p)
}

