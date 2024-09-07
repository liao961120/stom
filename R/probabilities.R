#' log_mix from stan
#'
#' @param theta,lp1,lp2 Numeric. See
#' <https://mc-stan.org/docs/2_21/functions-reference/composed-functions.html>
#' for the documentation.
#' @param log_lp Logical. Defaults to TRUE, which complies with Stan's
#'        `log_mix()`. If `FALSE`, `lp1` and `lp2` is further converted to
#'        logarithms before the mixture. In such cases, the inputs
#'        `theta`, `lp1`, and `lp2` should all be in the probability space
#'        to make sense.
#' @export
#' @examples
#' theta = .3
#' lp1 = .99
#' lp2 = .1
#' log_mix(theta, lp1, lp2, log_lp=FALSE)
log_mix = function(theta, lp1, lp2, log_lp=TRUE) {
    lt = log(theta)
    l1t = log(1-theta)
    if (!log_lp) {
        lp1 = log(lp1)
        lp2 = log(lp2)
    }
    log( exp(lt + lp1) + exp(l1t + lp2) )
}
