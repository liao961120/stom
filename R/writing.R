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


#' Vector object as R input string
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
#' @rdname as_c_num
as_c_chr = function(x) {
  x = paste(x, collapse = "', '")
  x = paste0( "c('", x, "')" )
  cat(x, "\n")
}


#' Handy vector construction from string
#'
#' @param x String. A string to construct a vector from (see examples).
#'          If `x` has length > 1, `stom::as_c_num` or `stom::as_c_chr`
#'          would be called (depending on input type) to print out the
#'          concatenation syntax `c(...)` in the console.
#' @param cast_num Boolean. Whether to convert string to numeric when
#'        applicable. See examples.
#' @export
#' @rdname as_vec
#' @examples
#' # Construct vector from string
#' as_vec("a b c")
#' as_vec("d, e  f")   # Commas have precedence over spaces
#'
#' # Auto typecasting
#' as_vec("1 2 -3.1")
#' as_vec("04, 05")
#' as_vec("04, 05", cast_num=FALSE)  # Disable typecasting
as_vec = function(x, cast_num=TRUE) {
    if ( is.null(x) ) return(NULL)

    x = trimws(x)
    if ( grepl(",",x) ) {
        x = trimws( strsplit(x, ",")[[1]] )
    } else if ( grepl("\\s+", x) ) {
        x = trimws( strsplit(x, "\\s+")[[1]] )
    }
    if ( all( grepl("^[0-9.-]+$",x) ) & cast_num )
        return( as.numeric(x) )
    return(x)
}

