
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


.rel_pos_to_step_num <- function(relPos, startPos)
{
    if (!is.integer(relPos)) {
        stop("relPos must be an integer")
    }
    if (!is.integer(startPos)) {
        stop("startPos must be an integer")
    }
    if (startPos < 1) {
        stop("startPos must be at least 1")
    }

    stepNumber <- startPos - relPos

    if (stepNumber < 1) {
        stop("relative index -", relPos, " points outside pipeline")
    }

    stepNumber
}


.extract_references_to_steps = function(
    fargs,
    steps,
    toPos = as.integer(length(steps))
) {
    if (!is.list(fargs)) {
        stop("fargs must be a list")
    }
    if (!is.character(steps)) {
        stop("steps must be a character vector")
    }

    if(!is.integer(toPos)) {
        stop("toPos must be an integer")
    }
    if (toPos > length(steps)) {
        stop("toPos exceeds number of steps")
    }

    # References to other steps are marked using a formula and can be either
    # referencing earlier steps (e.g. x = ~step1) or using positional indices
    # by pointing backwards a certain number of steps (e.g. x = ~-1)
    refs <- fargs |>
        Filter(f = \(x) methods::is(x, "formula")) |>
        lapply(FUN = \(x) trimws(deparse(x[[2]]))) |>
        unlist()

    if (length(refs) == 0) {
        return(character(0))
    }

    iRelPos <- which(refs |> startsWith("-"))
    stepNumbers <- refs[iRelPos] |> lapply(
        FUN = \(x) .rel_pos_to_step_num(abs(as.integer(x)), toPos)
    )
    refs[iRelPos] <- steps[as.integer(stepNumbers)]

    unlist(refs)
}
