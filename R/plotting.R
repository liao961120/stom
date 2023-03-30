#' RGB to RGBA
#'
#' Stolen from `rethinking::col.alpha()`.
#'
#' @export
col.alpha = function (acol, alpha = 0.5) {
  acol <- col2rgb(acol)
  acol <- rgb(acol[1]/255, acol[2]/255, acol[3]/255, alpha)
  acol
}

