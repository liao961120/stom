# Functions to facilitate authoring in R Markdown

#' Format vectors as inline text
#'
#' These functions are expect to work with R Markdown's inline R chunk syntax
#' `` `r ` ``, such that, for instance,
#' ``they are numbered `r inline(1:3)` respectively`` results in
#' `they are numbered 1, 2, and 3 respectively`.
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


#' Convert vector object to R syntax equivalent
#'
#' @export
#' @examples
#' as_c_num(1:3)
#' as_c_chr(1:3)
as_c_num = function(x) {
  x = paste(x, collapse = ", ")
  x = paste0( "c(", x, ")" )
  cat(x, "\n")
}

#' @export
#' @rdname num_as_concat
as_c_chr = function(x) {
  x = paste(x, collapse = "', '")
  x = paste0( "c('", x, "')" )
  cat(x, "\n")
}


