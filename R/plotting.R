#' Functions stolen from the rethinking package
#'
#'
#' @export
col.alpha = function (acol, alpha = 0.5) {
  acol <- col2rgb(acol)
  acol <- rgb(acol[1]/255, acol[2]/255, acol[3]/255, alpha)
  acol
}


#' @rdname col.alpha
#' @export
shade = function (object, lim, label = NULL, col = col.alpha("black",
                                                             0.15), border = NA, ...)
{
    if (missing(lim))
        stop("Interval limits missing.")
    if (missing(object))
        stop("No density or formula object.")
    from <- lim[1]
    to <- lim[2]
    if (class(object)[1] == "formula") {
        x1 <- eval(object[[3]])
        y1 <- eval(object[[2]])
        x <- x1[x1 >= from & x1 <= to]
        y <- y1[x1 >= from & x1 <= to]
    }
    if (class(object)[1] == "density") {
        x <- object$x[object$x >= from & object$x <= to]
        y <- object$y[object$x >= from & object$x <= to]
    }
    if (class(object)[1] == "matrix" & length(dim(object)) ==
        2) {
        y <- c(object[1, ], object[2, ][ncol(object):1])
        x <- c(lim, lim[length(lim):1])
    }
    if (class(object)[1] == "matrix") {
        polygon(x, y, col = col, border = border, ...)
    }
    else {
        polygon(c(x, to, from), c(y, 0, 0), col = col, border = border,
                ...)
    }
    if (!is.null(label)) {
        lx <- mean(x)
        ly <- max(y)/2
        text(lx, ly, label)
    }
}
