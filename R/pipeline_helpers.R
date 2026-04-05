
.empty_pipeline <- function() {
    data.table::data.table(
        step = character(0),
        fun = list(),
        fargs = list(),
        signature = character(0),
        refs = list(),
        out = list(),
        group = character(0),
        tags = list(),
        state = character(0),
        time = as.POSIXct(character(0)),
        lock = logical(0),
        skip = logical(0),
        meta = list(),
        .nodeId = integer()
    )
}


.new_step <- function(step, fun, fargs, refs, group, .nodeId)
{
    list(
        step = step,
        fun = list(fun),
        fargs = list(fargs),
        signature = trimws(substring(deparse(args(fun))[1], 10)),
        refs = list(refs),
        out = list(NULL),
        group = group,
        tags = list(character(0)),
        state = .step_states[["new"]][["name"]],
        time = Sys.time(),
        lock = FALSE,
        skip = FALSE,
        meta = list(list()),
        .nodeId = .nodeId
    )
}

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
        stop_no_call("relative index -", relPos, " points outside pipeline")
    }

    stepNumber
}


.extract_refs_to_steps = function(
    fargs,
    steps,
    toPos = as.integer(length(steps))
) {
    if (!is.list(fargs)) {
        stop_no_call("fargs must be a list")
    }
    if (!is.character(steps)) {
        stop_no_call("steps must be a character vector")
    }

    if(!is.integer(toPos)) {
        stop_no_call("toPos must be an integer")
    }
    if (toPos > length(steps)) {
        stop_no_call("toPos exceeds number of steps")
    }

    # References to other steps are marked using a formula and can be either
    # referencing earlier steps (e.g. x = ~step1) or using positional indices
    # by pointing backwards a certain number of steps (e.g. x = ~-1)
    refs <- lapply(fargs, FUN = \(x) trimws(deparse(x))) |>
        Filter(f = \(x) startsWith(x, "~")) |>
        lapply(\(x) substring(x, 2)) |>
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
