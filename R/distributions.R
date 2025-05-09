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
#' @param lower The lower bound for the truncation.
#' @param upper The upper bound for the truncation.
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


#' Random Number Generator for Truncated Cauchy Distribution
#'
#' The parameters are set to generate numbers from a half-normal distribution
#' by default.
#'
#' @param n Number of samples.
#' @param location,scale Parameters of the Cauchy distribution, see `stats::rcauchy`.
#' @param lower The lower bound for the truncation.
#' @param upper The upper bound for the truncation.
#' @export
rtcauchy = function(n, location=0, scale=1, lower=0, upper=Inf) {
    n_sampled = 0
    sampled = c()
    while (length(sampled) < n) {
        x = rcauchy( n, location, scale )
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
#' @param phi Parameter determining the dispersion of the outcomes.
#'        This parameter is equivalent to Stan's `neg_binomial_2(mu, phi)`'s `phi`.
#'
#' @details
#' See Chapter 12.1.2 of Statistical Rethinking 2th Edition, which details the
#' Gamma-Poisson distribution for modeling over-dispersed count data.
#' @export
rgampois2 = function (n , mu , phi) {
    p_p = phi / (phi + mu)
    p_n = phi
    rnbinom(n, size = p_n, prob = p_p)
}


#' Random Number Generator for Cholesky LKJ Correlation Distribution
#'
#' @param n Number of samples.
#' @param K Dimension of matrix
#' @param eta Parameter controling the strengths of correlations
#' @export
#' @details
#' Function taken from https://github.com/rmcelreath/rethinking/blob/master/R/distributions.r#L214
rlkjcorr <- function ( n , K , eta = 1 ) {

    stopifnot(is.numeric(K), K >= 2, K == as.integer(K))
    stopifnot(eta > 0)
    #if (K == 1) return(matrix(1, 1, 1))

    f <- function() {
        alpha <- eta + (K - 2)/2
        r12 <- 2 * rbeta(1, alpha, alpha) - 1
        R <- matrix(0, K, K) # upper triangular Cholesky factor until return()
        R[1,1] <- 1
        R[1,2] <- r12
        R[2,2] <- sqrt(1 - r12^2)
        if(K > 2) for (m in 2:(K - 1)) {
            alpha <- alpha - 0.5
            y <- rbeta(1, m / 2, alpha)

            # Draw uniformally on a hypersphere
            z <- rnorm(m, 0, 1)
            z <- z / sqrt(crossprod(z)[1])

            R[1:m,m+1] <- sqrt(y) * z
            R[m+1,m+1] <- sqrt(1 - y)
        }
        return(crossprod(R))
    }
    R <- replicate( n , f() )
    if ( dim(R)[3]==1 ) {
        R <- R[,,1]
    } else {
        # need to move 3rd dimension to front, so conforms to array structure that Stan uses
        R <- aperm(R,c(3,1,2))
    }
    return(R)
}

