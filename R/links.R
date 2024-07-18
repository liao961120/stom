#' Links and Inverse Links for Logistic Regression
#'
#' @param x Numeric vector. Values on the linear scale.
#' @param p Numeric vector. Values on the probability scale.
#' @export
logit = function(p) log(p) - log(1 - p)

#' @rdname logit
#' @export
inv_logit = function(x) 1 / (1 + exp(-x))  # Real -> P

#' @rdname logit
#' @export
logistic = inv_logit



#' Links for mapping reals to values between -1 and 1.
#'
#' @param x Double.
#' @details
#' `erf()`: <https://en.wikipedia.org/wiki/Error_function>.
#' @export
erf = function(x) 2 * pnorm(x * sqrt(2)) - 1


#' @rdname erf
#' @export
inv_erf = function (x) qnorm((1 + x)/2)/sqrt(2)

#' @rdname erf
#' @export
inv_tanh = function(x) .5 * (log(1 + x) - log(1 - x))


#' Numerically stable softmax function
#'
#' @param x Numeric. A vector of reals to map to probabilities.
#' @export
#' @examples
#' softmax(1:3)
#' softmax(1:3 + 9999999)
#' softmax(c(12345, 67890, 99999999))
softmax = function(x) {
    x = x - max(x)
    n = exp(x)
    n / sum(n)
}
