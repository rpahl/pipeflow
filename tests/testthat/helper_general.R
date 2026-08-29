expect_equivalent <- function(...) {
    testthat::expect_equal(..., ignore_attr = TRUE)
}

expect_error_fixed <- function(...) {
    testthat::expect_error(..., fixed = TRUE)
}

expect_no_error <- function(...) {
    testthat::expect_error(..., regexp = NA)
}

expect_no_warning <- function(...) {
    testthat::expect_warning(..., regexp = NA)
}

get_run_state <- function(x) {
    as.character(.pip_get_pipenv(x)[[".run_state"]])
}
