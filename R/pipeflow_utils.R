
is_string = function(x) {
    length(x) == 1 && isTRUE(is.character(x))
}

is_number = function(x) {
    length(x) == 1 && isTRUE(is.numeric(x))
}


unlist1 = function(x, ...)
{
    unlist(x, recursive = FALSE, ...)
}


pipeflow_replace_string = function(x, target, replacement) {

    if (length(x) == 0) {
        return(x)
    }

    stopifnot(
        is.character(x),
        is_string(target),
        is_string(replacement)
    )
    x[x %in% target] <- replacement
    x
}


pipe_filter_params <- function(pipe, ...)
{
    filters <- list(...)

    params <- pipe$get_params_unique() |>
        Filter(f = \(x) x |>
        methods::is("Param"))


    for (name in names(filters)) {
        value <- filters[[name]]
        params <- Filter(
            params,
            f = \(param) methods::slot(param, name) |> identical(value)
        )
    }

    params
}
