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


#' Numerically stable softmax function
#'
#' @param x Numeric. A vector of real values to convert to probabilities.
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
