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
