
# nocov start
.onLoad <- function(libname, pkgname)
{
    # Init text logger by default
    lg = set_log_layout("text")
    assign(pkgname, lg, envir = parent.env(environment()))
}
# nocov end
