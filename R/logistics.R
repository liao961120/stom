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



# start.time = Sys.time()
# x = replicate(1e4, {
#   # logit( 1e-323 )
#   inv_logit( seq(-10, 10, by=.01) )
# })
# end.time = Sys.time()
# end.time - start.time
#
#
# start.time = Sys.time()
# x = replicate(1e4, {
#   # qlogis( 1e-323 )
#   plogis( seq(-10, 10, by=.01) )
# })
# end.time = Sys.time()
# end.time - start.time
