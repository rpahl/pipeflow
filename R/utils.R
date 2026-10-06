.is_single <- function(x, mode) {
    if (length(x) != 1) {
        return(FALSE)
    }

    if (!(is.character(mode) && length(mode) == 1)) {
        stop("mode must be a single character string")
    }

    identical(data.class(x), mode)
}

unlist1 <- function(x, ...) {
    unlist(x, recursive = FALSE, ...)
}

stop_no_call <- function(...) {
    stop(..., call. = FALSE)
}

is_one_sided_formula <- function(x) {
    inherits(x, "formula") && length(x) == 2L
}

# A step reference wrapped in try(), e.g. `~try(step)`, marks an argument
# that may receive a failed input (see pip_run()).
is_try_ref <- function(x) {
    is_one_sided_formula(x) &&
        is.call(x[[2L]]) &&
        identical(x[[2L]][[1L]], as.name("try")) &&
        length(x[[2L]]) == 2L
}

formula_deps <- function(x) {
    deps <- Filter(f = is_one_sided_formula, x = x) |>
        lapply(\(x) {
            ref <- if (is_try_ref(x)) x[[2L]][[2L]] else x[[2L]]
            trimws(deparse1(ref, backtick = TRUE))
        }) |>
        unlist()
    if (is.null(deps)) character(0) else deps
}

# Names of the arguments in `x` whose step reference is wrapped in try().
formula_try_args <- function(x) {
    as.character(names(Filter(f = is_try_ref, x = x)))
}
