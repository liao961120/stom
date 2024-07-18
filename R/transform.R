#' Commonly used statistical transformation
#'
#' @export
#' @examples
#' simplex( rep(1,3) )
simplex = function(x) {
  if (any(x < 0)) stop("Input values contain zero!")
  x / sum(x)
}

#' @rdname simplex
#' @export
#' @examples
#' min_max(-5:10)
min_max = function(x) {
    m = min(x, na.rm = T)
    M = max(x, na.rm = T)
    (x - m) / (M - m)
}
