fun_date_extractor = function(s="%Y") {
    function(x)
        sapply( x, function(e) {
            as.numeric( format(as.Date(e), s) )
        }, USE.NAMES = F )
}

#' Extract Year/Month/Day from date string
#'
#' @param x Character or Date vector.
#' @return Numeric vector
#' @examples
#' D = c("2021-1-1", "2015/9/25")
#' year(D)
#' month(D)
#' day(D)
#' @export
year = fun_date_extractor("%Y")

#' @rdname year
#' @export
month = fun_date_extractor("%m")

#' @rdname year
#' @export
day = fun_date_extractor("%d")
