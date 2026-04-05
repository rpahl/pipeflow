
.is_single <- function(x, mode)
{
    if (length(x) != 1) {
        return(FALSE)
    }

    if (!(is.character(mode) && length(mode) == 1)) {
        stop("mode must be a single character string")
    }

    identical(data.class(x), mode)
}

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


stop_no_call <- function(...)
{
    stop(..., call. = FALSE)
}
