
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


`%!in%` <- Negate(`%in%`)


# Ternary NULL operator
`%||%` <- function(x, y) {

    if (is.null(x))
        y else x
}
