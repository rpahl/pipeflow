
.onLoad <- function(libname, pkgname)
{
    # Init text logger by default
    lg = set_log_layout("text")
    assign(pkgname, lg, envir = parent.env(environment()))
}


.this_package_path <- function(...)
{
    system.file(..., package = methods::getPackageName()) |>
        normalizePath(winslash = "/", mustWork = FALSE)
}


.this_package_name <- function()
{
    methods::getPackageName()
}
