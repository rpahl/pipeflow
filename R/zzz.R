# nocov start
.onLoad <- function(libname, pkgname) {
    # Init text logger by default.
    # During test/check runs (including parallel workers), keep logging
    # suspended unless a test explicitly opts in with lgr::with_logging().
    lg <- set_log_layout("text")
    assign(pkgname, lg, envir = parent.env(environment()))

    if (
        identical(Sys.getenv("TESTTHAT"), "true") ||
            identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "pipeflow")
    ) {
        lgr::suspend_logging()
    }
}
# nocov end
