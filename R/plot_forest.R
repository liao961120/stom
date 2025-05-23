#' Forest plot for multiple densities
#'
#' @param dat A list of numeric vectors with each representing
#'        a set of observations from a distribution.
#' @param labels Character vector. Labels plotted on the y-axis.
#' @param center String. The statistic used for drawing the central
#'        tendency line. Defaults to `mean`.
#' @param decreasing Logical. Whether to decreasingly sort the distributions
#'        according to their central tendency measures. If NULL, the
#'        distributions will be arranged according to the order in which
#'        they are passed in.
#' @param shade_inv Numeric vector of length 2. The central area to
#'        be shaded in the distributions By default, the central 50%
#'        regions are shaded. Set `shade_inv` to NULL to disable the
#'        shading.
#' @param vert_ref Numeric. The position of the vertical reference line.
#'        If NULL, nothing is plotted.
#' @param col Integer or String. A color code recognized by base R.
#'            Determines the color of the central tendency lines and
#'            shaded regions.
#' @param xlim Numeric. A vector of length 2 giving the plotted x range.
#' @param sep_fct,adj,ylim_base Parameters to tweak the positions of the
#'        distributions.
#'        `sep_fct` changes the distances between a pair of distributions.
#'        `ylim_base` determines the height of the density WITHIN a
#'         distribution.
#'        `adj` shifts the baselines of the distributions, with positive
#'        values shifting them upwards and negative values downwards.
#' @param plot_center Logical. Whether to plot the central tendency line.
#'        Defaults to TRUE.
#' @param ... Additional arguments passed to the main `plot()` function.
#'
#' @examples
#' # Set up the data
#' set.seed(100)
#' A <- rnorm(1e6,   0, 1.5)
#' B <- rnorm(1e6,   1, 1.5)
#' C <- rnorm(1e6,  -1, 1.2)
#' D <- rnorm(1e6, 1.5, 1.8)
#'
#' plot_forest(list(A,B,C,D), labels=LETTERS[1:4],
#'             vert_ref = 0, center="mode", xlim=c(-8,8),
#'             main = "A comparision of 4 normal distributions")
#' @export
plot_forest = function(dat, labels, shade_inv=c(.25,.75),
                       center=c("mean", "median", "mode"), decreasing=FALSE,
                       vert_ref=NULL, col=2, xlim=NULL, ylim_base=0, sep_fct=.5, adj=-.00,
                       dens_col=1, shade_pad_white=T, shade_col=col.alpha(2,.2),
                       center_col=2, plot_center=TRUE,
                       x_axis_at = NULL, ...) {
    # Sort distribution
    if (!is.null(decreasing)) {
        centers = sapply(dat, \(x) get_central_stat(x, center=center[1]))
        idx_reorder = order(rank(centers), decreasing = decreasing)
        dat = dat[idx_reorder]
        labels = labels[idx_reorder]
    }

    # Compute densities
    dens = lapply(dat, \(x) density(x, na.rm=T))
    n_distr = length(dat)
    n_distr_per_side = as.integer(n_distr / 2)
    ylim = 2 * sep_fct * c(-n_distr_per_side, n_distr_per_side)
    if (is_odd(n_distr))
        ylim = ylim + c(-1, 1)*sep_fct
    if (is.null(xlim))
        xlim = range( unlist(lapply(dens, \(x) x$x)))

    # Set up distribution positions
    y_centers = seq(from=ylim[1]+sep_fct, to=ylim[2]-sep_fct, length=n_distr)
    plot(1, type="n", xlab="", ylab="", xlim=xlim,
         ylim=ylim+c(-ylim_base,ylim_base), axes=FALSE, ...)

    # Vertical ref line
    if (is.numeric(vert_ref))
        segments(x0 = vert_ref, x1 = vert_ref,
                 y0 = -100,
                 y1 = y_centers[n_distr] + max(dens[[n_distr]]$y) + adj + .15,
                 col="darkgrey")

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
            center_pos = NULL
        }

        if (is.null(center_pos)) {
            mx = mean(dat[[i]], na.rm=T)
            x_err = abs(x - mx)
            center_pos = which(x_err == min(x_err))[1]
            my = y[center_pos]
        } else {
            mx = x[center_pos]; my = y[center_pos]
        }

        # Shade center portion
        if (is.numeric(shade_inv)[1]) {
            qLow = quantile(dat[[i]], shade_inv[1], na.rm=T)
            qUp = quantile(dat[[i]], shade_inv[2], na.rm=T)
            idx = which(x >= qLow & x <= qUp)
            # Shade the central region
            bg = "white"
            if (!shade_pad_white)
                bg = col.alpha(bg, 0)
            for ( cl in c(bg, shade_col) )
                polygon(c(x[idx], rev(x[idx])), c(y[idx] + sft, rep(sft, length(idx))),
                        col = cl, border = NA)
        }

        # Center line
        if (plot_center)
            lines( rep(mx,2), c(sft,sft+my), col=center_col, lwd=2 )
        # Density curve
        lines(x, y + sft, col=dens_col, lwd=1.2)
        # Bottom line
        lines(x, rep(sft,n_dens), col=dens_col)

        box(bty = "l")
    }

    # Add axis labels
    if (!is.null(x_axis_at)) {
        axis(1, at = x_axis_at)
    }
    axis(1)
    axis(2, at = y_centers, labels = labels, las=1)
}


is_odd = function(x) ifelse(x %% 2 == 1, T, F)


get_central_stat = function(x, center=c("mean", "median", "mode")) {
    if (center[1] == "mode")
        return(Mode(x))
    return(do.call(center[1], list(x, na.rm=T)))
}

Mode = function(x) {
    d = density(x, na.rm = T)
    d$x[which.max(d$y)]
}

