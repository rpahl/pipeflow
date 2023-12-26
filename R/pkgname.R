
# nocov start
.onLoad <- function(libname, pkgname)
{
    # Init text logger by default
    lg = set_log_layout("text")
    assign(pkgname, lg, envir = parent.env(environment()))
}
# nocov end


.this_package_name <- function()
{
    methods::getPackageName()
}


.this_package_path <- function(...)
{
    system.file(package = .this_package_name()) |>
        file.path(...) |>
        normalizePath(winslash = "/", mustWork = FALSE)
}
