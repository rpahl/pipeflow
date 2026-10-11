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
    as.character(.pip_pipenv(x)[[".run_state"]])
}

# Write `x` with saveRDS() and read it back with readRDS(). Objects that are
# saved together share their environments after reading, like a pipeline and
# its views.
save_and_read <- function(x) {
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f))
    saveRDS(x, f)
    readRDS(f)
}
