#' Functions stolen from the rethinking package
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


#' Add multivariate normal contour to plot
#'
#' @param mu Vector of MVNormal center.
#' @param Sigma Covariance matrix of MVNormal.
#' @param col Color of the contours.
#' @param alpha Opacity of the contours.
#' @param level The values the of cumulative density to plot contours.
#' @param ... Further arguments passed to [graphics::lines()].
#' @examples
#' mu = c(.5, .7)
#' Rho = matrix(c(
#'     1, .2,
#'    .2,  1 ), nrow=2, byrow=T)
#' s = c( 1.5, 1.2 )
#' Sigma = diag(s) %*% Rho %*% diag(s)
#' x = MASS::mvrnorm(50, mu, Sigma)
#' plot(x)
#' curve_mvnorm(mu, Sigma)
#' @export
curve_mvnorm = function(mu, Sigma, level = c(seq(.1,.9,.1),.99),
                        col=1, alpha=.1, ... ) {
    for ( l in level)
        lines(
            ellipse::ellipse(Sigma, centre=mu, level=l),
            col = col.alpha("black",alpha, ...)
        )
}


#' Get graphical parameters
#'
#' @param x Parameter name.
#' @examples
#' par2("mar")
#' par2("omi")
#' @export
par2 = function(x, ...) {
    vals = par(x, ...)
    params = c( "omi", "oma", "mar", "mai" )
    if ( x %in% params )
        names(vals) = c("b", "l", "t", "r")
    vals
}


#' Convert numerical values to divergent colors
#'
#' @param x Numeric.
#' @param center Integer. Currently only `0` is valid.
#'        If `center == 0`, `0` is used as the center and corresponds to
#'        the central color (lightgoldenrod1).
#'        Otherwise, `x` must have values in the range [0, 1].
#' @return A vector of hex colors.
#' @export
#' @examples
#' x = rnorm(1e3)
#' plot(seq(x), x, col= vals2cols(x), pch=19)
vals2cols = function(x, center=0) {
    if (center == 0) {
        max_scale = max(abs(x))
        x = x / max_scale
    }
    p.pal = colorRamp(c("lightgoldenrod1", "orange", "orangered", "red"))
    n.pal = colorRamp(c("lightgoldenrod1", "yellowgreen", "green", "blue"))
    sapply(x, \(v) {
        if (v > 0)
            return(rgb(p.pal(v), max=255))
        else
            return(rgb(n.pal(-v), max=255))
    })
}
