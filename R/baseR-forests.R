#' Forest plot for multiple densities
#'
#' @param dat A list of numeric vectors with each representing
#'        a set of observations from a distribution.
#' @param labels Character vector. Labels plotted on the y-axis.
#' @param center String. The statistic used for drawing the central
#'        tendency line. Defaults to `mean`.
#' @param shade_inv Numeric vector of length 2. The central area to
#'        be shaded in the distributions By default, the central 50%
#'        regions are shaded.
#' @param vert_ref Numeric. The position of the vertical reference line.
#'        If NULL, nothing is plotted.
#' @param col Integer or String. A color code recognized by base R.
#'            Determines the color of the central tendency lines and
#'            shaded regions.
#' @param xlim Numeric. A vector of length 2 giving the plotted x range.
#' @param sep_fct,adj Parameters to tweak the positions of the distributions.
#'        `sep_fct` changes the distances between a pair of distributions.
#'        `adj` shifts the baselines of the distributions, with positive
#'        values shifting them upwards and negative values downwards.
#' @param ... Additional arguments passed to the main `plot()` function.
#'
#' @examples
#' # Set up the data
#' set.seed(100)
#' data1 <- rnorm(1e6, 0, 1.5)
#' data2 <- rnorm(1e6, 1, 1.5)
#' data3 <- rnorm(1e6, -1, 1.2)
#' data4 <- rnorm(1e6, 1.5, 1.8)
#'
#' plot_forest(list(data1,data2,data3,data4), labels=LETTERS[1:4],
#'             vert_ref = 0, center="mode", xlim=c(-10,10),
#'             main = "A comparision of four Normal distributions")
#'
#' @export
plot_forest = function(dat, labels, shade_inv=c(.25,.75),
                       center=c("mean", "median", "mode"), vert_ref=NULL,
                       col=2, xlim=NULL, sep_fct=.5, adj=-.00, ...) {
    dens = lapply(dat, \(x) density(x))
    n_distr = length(dat)
    n_distr_per_side = as.integer(n_distr / 2)
    ylim = 2 * sep_fct * c(-n_distr_per_side, n_distr_per_side)
    if (is_odd(n_distr))
        ylim = ylim + c(-1, 1)*sep_fct
    if (is.null(xlim))
        xlim = range( unlist(lapply(dens, \(x) x$x)))

    # Set up distribution positions
    y_centers = seq(from=ylim[1]+sep_fct, to=ylim[2]-sep_fct, length=n_distr)
    plot(1, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, axes=FALSE, ...)

    # Vertical ref line
    if (is.numeric(vert_ref))
        segments(x0 = vert_ref, x1 = vert_ref,
                 y0 = -100,
                 y1 = y_centers[n_distr] + max(dens[[n_distr]]$y) + adj + .15,
                 col="grey")

    # Plot densities
    n_dens = length(dens[[1]]$x)
    for (i in seq(y_centers)) {
        sft = y_centers[i] + adj
        x = dens[[i]]$x; y = dens[[i]]$y

        # Central tendency stats line
        if (center[1] == "median") {
            center_pos = which(x == median(x))[1]
        } else if (center[1] == "mode") {
            center_pos = which(y == max(y))[1]
        } else {
            x_sq_err = (x - mean(x))^2
            center_pos = which(x_sq_err == min(x_sq_err))[1]
        }

        mx = x[center_pos]; my = y[center_pos]

        # Shade center portion
        if (is.numeric(shade_inv)[1]) {
            qLow = quantile(dat[[i]], shade_inv[1])
            qUp = quantile(dat[[i]], shade_inv[2])
            idx = which(x >= qLow & x <= qUp)
            # Shade the central region
            for ( cl in c("white", stom::col.alpha(col,.2)) )
                polygon(c(x[idx], rev(x[idx])), c(y[idx] + sft, rep(sft, length(idx))),
                        col = cl, border = NA)
        }

        # Center line
        lines( rep(mx,2), c(sft,sft+my), col=col, lwd=2 )
        # Density curve
        lines(x, y + sft, col=1, lwd=1.5)
        # Bottom line
        lines(x, rep(sft,n_dens), col=1)

    }

    # Add axis labels
    axis(1)
    axis(2, at = y_centers, labels = labels, las=1)
}

is_odd = function(x) ifelse(x %% 2 == 1, T, F)
