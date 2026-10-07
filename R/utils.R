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

#' Check for a one-sided formula
#'
#' @param x Any object.
#' @return `TRUE` if `x` is a formula without a left-hand side, else `FALSE`.
#' @noRd
is_one_sided_formula <- function(x) {
    inherits(x, "formula") && length(x) == 2L
}

#' Check for a step reference wrapped in `try()`
#'
#' A step reference wrapped in `try()`, e.g. `~try(step)`, marks an argument
#' that may receive a failed input (see [pip_run()]). Only `try()` calls with
#' exactly one argument count.
#'
#' @param x Any object.
#' @return `TRUE` if `x` is a one-sided formula of the form `~try(<ref>)`,
#' else `FALSE`.
#' @noRd
is_try_ref <- function(x) {
    is_one_sided_formula(x) &&
        is.call(x[[2L]]) &&
        identical(x[[2L]][[1L]], as.name("try")) &&
        length(x[[2L]]) == 2L
}

#' Extract the step references of a list of parameters
#'
#' @param x List of parameters. Its one-sided formulas are step references,
#' where a reference wrapped in `try()` (see `is_try_ref()`) is unwrapped.
#' @return Character vector of the referenced step names or relative
#' positions (e.g. `"-1"`), named like the respective elements of `x`.
#' @noRd
formula_deps <- function(x) {
    deps <- Filter(f = is_one_sided_formula, x = x) |>
        lapply(\(x) {
            ref <- if (is_try_ref(x)) x[[2L]][[2L]] else x[[2L]]
            trimws(deparse1(ref, backtick = TRUE))
        }) |>
        unlist()
    if (is.null(deps)) character(0) else deps
}

#' Get the arguments with step references wrapped in `try()`
#'
#' @param x List of parameters.
#' @return Character vector of the names of the elements of `x` that are
#' step references wrapped in `try()` (see `is_try_ref()`).
#' @noRd
formula_try_args <- function(x) {
    as.character(names(Filter(f = is_try_ref, x = x)))
}
