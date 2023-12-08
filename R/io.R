# ToDo:
#   1. write unit tests

# library(dplyr)
#
# d = tibble(
#         A_int = 1L:6L,
#         A_dbl = seq(0, 1, length=6),
#         A_cpl = (1:6) + 1i,
#         A_lgl = rep(c(T,F), 3),
#         A_fct = as.factor(head(LETTERS)),
#         A_chr = head(LETTERS),
#         A_utf8 = strsplit("你好我是大雄", "")[[1]]
#     )
# d
#
# write_tsv3(d, "test-stom.tsv")
# d2 = read_tsv3("test-stom.tsv")


write_tsv3 = function(dt, fout="test-base.csv") {
    write_col_type(dt, fout)
    write_data(dt, fout)
}

write_col_type = function(dt, fout) {
    ctypes = sapply(dt, function(c_) class(c_))
    ctypes = paste(ctypes, collapse="\t")
    writeLines(ctypes, fout, sep="\n")
}

write_data = function(dt, fout) {
    suppressWarnings({
        write.table(dt, fout, append=T,
                    sep="\t", row.names = F, col.names = T,
                    fileEncoding = "UTF-8" )
    })
}

read_tsv3 = function(fin) {
    ctypes = read_col_type(fin)
    read_data(fin, ctypes)
}
read_col_type = function(fin) {
    ctypes = strsplit(readLines(fin, n=1), "\t")[[1]]
    ctypes
}
read_data = function(fin, ctypes) {
    read.table(fin, sep="\t",
               comment.char = "",
               colClasses = ctypes,
               fileEncoding = "UTF-8",
               header = T, skip=1 )
}
