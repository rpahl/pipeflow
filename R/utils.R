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

formula_deps <- function(x) {
    is_one_sided_formula <- function(x) {
        inherits(x, "formula") && length(x) == 2L
    }
    deps <- Filter(f = is_one_sided_formula, x = x) |>
        lapply(\(x) substring(trimws(deparse1(x)), 2L)) |>
        unlist()
    if (is.null(deps)) character(0) else deps
}
