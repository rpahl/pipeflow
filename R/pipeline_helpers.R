
.extract_fun_args = function(fun)
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


.filter_dependencies = function(fargs = list())
{
    if (length(fargs) == 0) {
        return(character())
    }

    # Extract the dependency name from the formula, that is, ~x
    # becomes "x" and ~-1 becomes -1
    deps <- fargs |>
        Filter(f = function(x) methods::is(x, "formula")) |>
        sapply(FUN = function(x) deparse(x[[2]]))

    if (length(deps) == 0) {
        return(character()) # otherwise named list() would be returned
    }

    deps
}
