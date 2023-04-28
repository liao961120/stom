#' Commonly used statistical transformation
#'
#' @export
#' @examples
#' simplex( rep(1,3) )
simplex = function(x) {
  if (any(x < 0)) stop("Input values contain zero!")
  x / sum(x)
}
