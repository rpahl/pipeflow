
.extract_default_values = function(fun)
{
    fargs <- formals(fun)

    hasDots <- "..." %in% names(fargs)
    if (hasDots) {
        fargs <- fargs[!names(fargs) %in% "..."]
    }

    # Signal args with no default values
    is_missing_default <- function(x) {
        identical(x, quote(expr = ))
    }
    undef <- names(Filter(fargs, f = is_missing_default))
    if (length(undef) > 0L) {
        stop_no_call(
            paste0("'", undef, "'", collapse = ", "),
            ifelse(length(undef) > 1L, " have ", " has "),
            "no default value"
        )
    }

    as.list(fargs)
}
