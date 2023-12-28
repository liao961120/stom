d = data.frame(
    A_int = c(1L:4L, NA, NaN),
    A_dbl = c(seq(0, 1, length=4), NA, NaN),
    A_cpl = c((1:4) + 1i, NA, NaN),
    A_lgl = c(T, F, F, T, T, NA),
    A_chr = c(LETTERS[1:2], NA, "", '"', '\t'),
    A_NA_int = rep(NA_integer_, 6)
)
tmp = tempfile()
write_tcsv(d, tmp)
d1 = read_tcsv(tmp)
write_tcsv(d1, tmp)
d2 = read_tcsv(tmp)

test_that("Consistent read & write for tcsv", {
    expect_identical(d1, d2)
})

d2$A_fct = rep("哈", nrow(d))
test_that("Test unicode", {
    write_tcsv(d2, tmp)
    dt = read_tcsv(tmp)
    expect_identical( dt$A_fct, d2$A_fct )
})

test_that("Test column type inference", {
    ctypes = sapply(d, \(x) class(x))
    expect_equal( get_col_type(d), ctypes )
})

d2$A_fct = factor(1:nrow(d))
test_that("Fail on factor", {
    testthat::expect_error(write_tcsv(d2))
})

d2$A_fct = ordered(1:nrow(d))
test_that("Fail on ordered", {
    testthat::expect_error(write_tcsv(d2))
})

d2$A_fct = list(1:nrow(d))
test_that("Fail on list column", {
    testthat::expect_error(write_tcsv(d2))
})
