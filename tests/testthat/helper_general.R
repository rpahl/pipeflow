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

if (FALSE) {
    # This is a dummy code block to ensure that packages are added
    # to the DESCRIPTION file using attachment::att_amend_desc().
    require(visNetwork)
}
