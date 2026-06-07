.empty_pipeline <- function() {
    data.table::data.table(
        step = character(0),
        group = character(0),
        fun = list(),
        params = list(),
        signature = character(0),
        depends = list(),
        out = list(),
        state = character(0),
        tags = list(),
        time = as.POSIXct(character(0)),
        locked = logical(0),
        exec = character(0),
        .nodeId = integer(),
        .indeps = list() # names of independent parameters
    )
}


.new_step <- function(
    step,
    group,
    fun,
    params,
    depends,
    .nodeId,
    tags = character(0),
    exec = "auto"
) {
    list(
        step = step,
        group = group,
        fun = list(fun),
        params = list(params),
        signature = trimws(substring(deparse(args(fun))[1], 10)),
        depends = list(depends),
        out = list(NULL),
        state = .step_states[["new"]][["name"]],
        tags = list(tags),
        time = Sys.time(),
        locked = FALSE,
        exec = exec,
        .nodeId = .nodeId,
        .indeps = list(setdiff(names(params), names(depends)))
    )
}

.extract_fun_params <- function(fun) {
    args <- formals(fun)

    # Remove potential "..." argument
    hasDots <- "..." %in% names(args)
    if (hasDots) {
        args <- args[!names(args) %in% "..."]
    }

    # First verify that all args have default values
    is_missing_default <- function(x) {
        identical(x, quote(expr = ))
    }
    undef <- names(Filter(args, f = is_missing_default))
    if (length(undef) > 0L) {
        stop_no_call(
            paste0("'", undef, "'", collapse = ", "),
            ifelse(length(undef) > 1L, " have ", " has "),
            "no default value"
        )
    }

    # Make sure default values are returned as resolved values by evaluating
    # them in the function's environment
    lapply(args, \(x) eval(x, envir = environment(fun)))
}


.rel_pos_to_step_num <- function(relPos, startPos) {
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


.extract_depends = function(
    params,
    steps,
    toPos = as.integer(length(steps))
) {
    if (!is.list(params)) {
        stop_no_call("params must be a list")
    }
    if (!is.character(steps)) {
        stop_no_call("steps must be a character vector")
    }

    if (!is.integer(toPos)) {
        stop_no_call("toPos must be an integer")
    }
    if (toPos > length(steps)) {
        stop_no_call("toPos exceeds number of steps")
    }

    # References to other steps are marked using a formula and can be either
    # referencing earlier steps (e.g. x = ~step1) or using positional indices
    # by pointing backwards a certain number of steps (e.g. x = ~-1)
    depends <- lapply(params, FUN = \(x) trimws(deparse(x))) |>
        Filter(f = \(x) startsWith(x, "~")) |>
        lapply(\(x) substring(x, 2)) |>
        unlist()

    if (length(depends) == 0) {
        return(character(0))
    }

    iRelPos <- which(depends |> startsWith("-"))
    stepNumbers <- depends[iRelPos] |>
        lapply(
            FUN = \(x) .rel_pos_to_step_num(abs(as.integer(x)), toPos)
        )
    depends[iRelPos] <- steps[as.integer(stepNumbers)]

    unlist(depends)
}
