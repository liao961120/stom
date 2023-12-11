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
#'     A_int = 1L:6L,
#'     A_dbl = seq(0, 1, length=6),
#'     A_cpl = (1:6) + 1i,
#'     A_lgl = rep(c(T,F), 3),
#'     A_fct = as.factor(head(LETTERS)),
#'     A_chr = head(LETTERS),
#'     A_utf8 = strsplit("你好我是大雄", "")[[1]])
#' )
#' tmp = tempfile()
#' write_tcsv(d, tmp)
#' xfun::file_string(tmp)  # print file content
#' read_tcsv(tmp)
#'
#' # May also set your preferred delimiter
#' write_tcsv(d, tmp, sep = "\t")
#' xfun::file_string(tmp)
#' read_tcsv(tmp, sep = "\t")
write_tcsv = function(dt, fout, sep=",") {
    write_col_type(dt, fout, sep)
    write_data(dt, fout, sep)
}

#' @rdname write_tcsv
#' @export
read_tcsv = function(fin, sep=",") {
    ctypes = read_col_type(fin, sep)
    read_data(fin, ctypes, sep)
}



write_col_type = function(dt, fout, sep) {
    ctypes = sapply(dt, function(c_) {
        c_ = class(c_)
        if ( "ordered" %in% c_ )
            stop("tcsv does not support columns with 'ordered' data type!\n\t",
                 "Please convert them to either 'character' or 'factor'\n\t",
                 "before writing to file.")
        return(c_)
    })
    ctypes = paste(ctypes, collapse = sep)
    writeLines(ctypes, fout, sep = "\n")
}

write_data = function(dt, fout, sep) {
    suppressWarnings({
        write.table(dt, fout, append=T,
                    sep = sep, row.names = F, col.names = T,
                    fileEncoding = "UTF-8" )
    })
}

read_col_type = function(fin, sep) {
    ctypes = strsplit(readLines(fin, n = 1), sep)[[1]]
    ctypes
}

read_data = function(fin, ctypes, sep) {
    read.table(fin, sep = sep,
               comment.char = "",
               colClasses = ctypes,
               fileEncoding = "UTF-8",
               header = T, skip = 1 )
}
