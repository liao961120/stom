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
#' @param n Numbers of sample.
#' @param m The mean of the untruncated normal distribution.
#' @param s The standard deviation of the untruncated normal distribution.
#' @param lower The lower bound for truncation.
#' @param upper The upper bound for truncation.
#' @export
rtnorm = function(n, m=0, s=1, lower=0, upper=Inf) {
  n_sampled = 0
  sampled = c()
  while (n_sampled < n) {
    x = rnorm( n, m, s )
    sampled = c( sampled, x[x > lower & x < upper] )
    n_sampled = n_sampled + length(sampled)
  }
  sampled[seq_along(n)]
}
