#' Standardize a vector to fixed mean and std
#'
#' @param x Numeric vector.
#' @param m Mean.
#' @param s Standard deviation.
#' @export
standardize = function(x, m=0, s=1) {
  center = (x - mean(x)) / sd(x)
  center * s + m
}
