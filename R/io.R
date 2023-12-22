#' I/O for data frames and \emph{typed CSV}s
#'
#' @param dt A data frame to be written to file.
#' @param fin Path to a typed CSV to be read in.
#' @param fout Output file path.
#' @param sep Character. The delimiter used for the typed CSV.
#'        Defaults to comma (`,`).
#' @export
#' @examples
#' (d = data.frame(
#'     A_int = c(1L:4L, NA, NaN),
#'     A_dbl = c(seq(0, 1, length=4), NA, NaN),
#'     A_cpl = c((1:4) + 1i, NA, NaN),
#'     A_lgl = c(T, F, F, T, T, NA),
#'     A_chr = c(LETTERS[1:2], NA, "", '"', '\t'),
#'     A_NA_int = rep(NA_integer_, 6)
#' ))
#' tmp = tempfile()
#' write_tcsv(d, tmp)
#' xfun::file_string(tmp)  # print file content
#' read_tcsv(tmp)
write_tcsv = function(dt, fout) {
    write_data(dt, fout)
}

#' @rdname write_tcsv
#' @export
read_tcsv = function(fin) {
    read_data(fin)
}


get_col_type = function(dt) {
    ctypes = sapply(dt, function(c_) {
        c_ = class(c_)
        if ( "ordered" %in% c_ | "factor" %in% c_ )
            stop("tcsv does not support columns with 'ordered' data type!\n\t",
                 "Please convert them to either 'character' or 'factor'\n\t",
                 "before writing to file.")
        return(c_)
    })
    return(ctypes)
}

# d0 = data.frame(a = 1:6,
#                 b=c(T,F,T,NA, T,T),
#                 c=c("a", "", '"', NA, ",", "\t"),
#                 e = NA_integer_, f = "哈", f2="d" )

write_data = function(dt, fout) {
    ctypes = get_col_type(dt)
    ctypes = paste0("<", ctypes, ">")
    data = apply(dt, 2, function(c_) as.character(c_))
    out = rbind( ctypes, data )
    row.names(out) = NULL
    out = as.data.frame(out)
    readr::write_csv(out, fout, col_names = TRUE,
                     quote = "all",
                     escape = "double",
                     na = "")
    return(invisible(out))
}

read_data = function(fin) {
    d = readr::read_csv(fin, col_types = "c",
                        na = "", quote = '"',
                        name_repair = "minimal")
    ctypes = trimws(unlist(d[1,]), whitespace = "<|>")
    d = d[2:nrow(d),]
    for (i in seq_along(ctypes))
        class(d[[i]]) = ctypes[i]
    return(d)
}
