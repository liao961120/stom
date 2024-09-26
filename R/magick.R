#' Joining multiple images into a single one
#'
#' @param fps A character vector of paths to the images to be combined.
#' @param output The output file path. If `NULL`, the image is printed.
#' @param stack Logical. `TRUE` joins images in a top-to-bottom direction.
#'        `FALSE` joins images in a left-to-right direction.
#' @param width,height An integer specifying the common widths and/or heights of
#'        the images to be read in. Works only with SVG images. For joining the
#'        images side-by-side, setting a common height is recommended, whereas
#'        when the images are stacked, setting a common width results in a more
#'        desirable output image.
#' @param sep_fct Numeric. `sep_fct` determines the space between the joined
#'        images. The space between the joined images is determined by:
#'        \deqn{\textrm{Horizontal space} = \textrm{sep_fct} \times \textrm{max}(\textrm{widths of input images}) }
#'        \deqn{\textrm{Vertical space}   = \textrm{sep_fct} \times \textrm{max}(\textrm{heights of input images}) }
#' @param background Character. The color to add to background. By default, "transparent".
#' @export
image_combine = function(fps, output = NULL, stack = FALSE, height = NULL,
                         width = NULL, sep_fct = .1, background = "transparent") {
    n = length(fps)
    if (is.character(height)) height = as.numeric(height)
    if (is.character(width)) height = as.numeric(width)
    imgs = lapply(fps, \(fp) read_image(fp, height=height, width=width))
    d = get_max_dim(imgs)
    img_bk = magick::image_blank(width = sep_fct*d[1],
                                 height = sep_fct*d[2],
                                 color = "transparent")
    img_lst = vector("list", length = 2*n - 1 )
    for (i in 1:n) {
        img_lst[[2*i-1]] = imgs[[i]]
        if (i < n)
            img_lst[[2*i]] = img_bk
    }
    out = magick::image_append(magick::image_join(img_lst), stack = stack)
    if (!is.null(background))
        out = magick::image_background(out, color=background)
    if (is.null(output))
        return(out)
    magick::image_write(out, path = output)
}

# Read SVG or PNG images with magick
read_image = function(fp, ...) {
    if (endsWith(fp, ".svg") || endsWith(fp, ".SVG"))
        return(magick::image_read_svg(fp, ...))
    return(magick::image_read(fp))
}

get_max_dim = function(img) {
    w = sapply(img, \(x) magick::image_info(x)$width) |> max()
    h = sapply(img, \(x) magick::image_info(x)$height) |> max()
    return(c(width=w,height=h))
}
