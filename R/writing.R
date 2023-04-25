# Functions to facilitate authoring in R Markdown

#' Inline Formatting of Vectors
#'
#' @export
#' @examples
#' inline(1:2)
#' inline(1:3)
#' inline_zh(1:2)
#' inline_zh(1:3)
inline = function(x, sep=", ", last_sep=", and ", two_sep=" and ") {
  n = length(x)
  if (n == 2)
    return(paste0(x[1], two_sep, x[2]))
  x_last = x[n]
  x = x[-n]
  paste0( paste(x, collapse=sep), last_sep, x_last )
}

#' @rdname inline
#' @export
inline_zh = function(x, sep="、", last_sep="、", two_sep=" 與 ") inline(x, sep, last_sep, two_sep)
