
.onLoad <- function(libname, pkgname)
{
    # Init logger layout
    log_layout = Sys.getenv("log_layout", unset = "text")

    # If not yet set, set it now
    Sys.setenv("log_layout" = log_layout)

    layout <- switch(
        log_layout,
        "text" = lgr::LayoutFormat$new(),
        "myJson" = MyLogLayoutJson$new(),    # nolint
        stop("unknown log layout '", log_layout, "'")
    )

    # Init logger
    lg = lgr::get_logger(name = pkgname)
    lg$config(
        list(
            threshold = "info",
            propagate = FALSE,
            appenders = lgr::AppenderConsole$new(layout = layout)
        )
    )
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
