if (identical(Sys.getenv("TESTTHAT"), "true")) {
    lgr::suspend_logging()
}

expect_equivalent = function(...) {
    testthat::expect_equal(..., ignore_attr = TRUE)
}

expect_error_fixed = function(...) {
    testthat::expect_error(..., fixed = TRUE)
}

expect_no_error = function(...) {
    testthat::expect_error(..., regexp = NA)
}

expect_no_warning = function(...) {
    testthat::expect_warning(..., regexp = NA)
}
