# ---------------------
# Pipeline construction
# ---------------------
.empty_pipeline <- function() {
    data.table::data.table(
        step = character(0),
        fun = list(),
        params = list(),
        out = list(),
        state = character(0),
        tags = list(),
        locked = logical(0),
        exec = character(0), # execution mode
        time = as.POSIXct(character(0)),
        time_start = as.POSIXct(character(0)), # start of the last run
        depends = list(), # names of parameters referencing other steps
        unbound = list(), # names of unbound parameters
        nodeId = integer() # the unique node ID in the internal DAG
    )
}

.new_step <- function(
    step,
    fun,
    params,
    depends,
    nodeId,
    tags = character(0),
    exec = "auto"
) {
    list(
        step = step,
        fun = list(fun),
        params = list(params),
        out = list(NULL),
        state = .step_states[["new"]][["name"]],
        tags = list(tags),
        locked = FALSE,
        exec = exec,
        time = Sys.time(),
        time_start = as.POSIXct(NA),
        depends = list(depends),
        unbound = list(setdiff(names(params), names(depends))),
        nodeId = nodeId
    )
}

#' Clone a pipeline, deeply or lightly
#'
#' Both kinds of copy get their own step table, DAG and step to node map, so
#' adding, removing or running steps of the copy does not change `x`. A deep
#' copy also copies all values of the step table. A light copy shares them
#' (params, outputs, functions) with `x`: its columns are new vectors, but
#' their elements are the same objects. That is fast and takes no memory
#' for the outputs, as long as a value is replaced rather than modified in
#' place (e.g. with `:=` on a `data.table` output).
#'
#' @param x A pipeflow pipeline (not a view).
#' @param name The name of the copy.
#' @param deep `TRUE` for a deep copy, `FALSE` for a light one.
#' @return The copy, a pipeflow pipeline.
#' @noRd
.pip_clone <- function(x, name = x[["name"]], deep = TRUE) {
    out <- pip_new(name = name)
    env <- .pip_pipenv(out)

    env[[".dag"]] <- dag_clone(.pip_dag(.pip_pipenv(x)))
    dat <- .pip_pipenv(x)[["data"]]
    dat <- if (deep) data.table::copy(dat) else dat[seq_len(nrow(dat))]

    # Clone steps to nodes mapping
    stepsToNodes <- env[[".steps_to_nodes"]]
    for (k in seq_len(nrow(dat))) {
        step <- dat[["step"]][[k]]
        nodeId <- dat[["nodeId"]][[k]]
        stepsToNodes[[step]] <- nodeId
    }
    env[["data"]] <- dat

    out
}


# ---------------
# Type predicates
# ---------------
.is_pipeflow <- function(x) {
    inherits(x, "pipeflow")
}

# A view is a pipeflow whose `view` field is not NULL.
.is_pipeflow_view <- function(x) {
    inherits(x, "pipeflow") && !is.null(.subset2(x, "view"))
}

.is_pipeflow_partitioned <- function(x) {
    inherits(x, "pipeflow_partitioned")
}


# -------
# Asserts
# -------
.assert_exec_mode <- function(exec) {
    if (!.is_single(exec, "character") || is.na(exec)) {
        stop("exec must be a single string")
    }
    allowed <- c("auto", "split", "reduce", "plain")
    if (!(exec %in% allowed)) {
        stop("exec must be one of: ", toString(allowed))
    }
    invisible(exec)
}

#' Check the params of a step
#'
#' The params given to `pip_add()` or `pip_replace()` are merged with the
#' defaults of the step function by name, so each of them needs a unique name.
#'
#' @param params The params given for the step.
#' @return `params`, invisibly.
#' @noRd
.assert_step_params <- function(params) {
    parNames <- names(params)
    if (length(params) > 0L && is.null(parNames)) {
        parNames <- NA_character_
    }
    isValid <- is.list(params) &&
        !anyNA(parNames) &&
        all(nzchar(parNames)) &&
        !anyDuplicated(parNames)
    if (!isValid) {
        stop_no_call("params must be a list with unique names")
    }
    invisible(params)
}

#' A pipeline is no step function, but can be turned into one
#' @param fun The step function given to `pip_add()` or `pip_replace()`.
#' @noRd
.assert_fun_not_pipeline <- function(fun) {
    if (.is_pipeflow(fun)) {
        stop_no_call(
            "fun must be a function, not a pipeline - to use a pipeline as ",
            "a step, turn it into a function with as.function(), ",
            "see ?as.function.pipeflow"
        )
    }
    invisible(fun)
}

.assert_logger <- function(lgr) {
    if (!is.function(lgr)) {
        stop("lgr must be a function")
    }
    if (!all(c("level", "msg") %in% names(formals(lgr)))) {
        stop("lgr must be a function with arguments 'level' and 'msg'")
    }
    invisible(lgr)
}

.assert_pip_or_view <- function(x) {
    if (!.is_pipeflow(x)) {
        stop_no_call("x must be a pipeflow pip or view")
    }
    invisible(x)
}

# Structural operations require a full pipeline, not a view.
.assert_pip <- function(x) {
    if (!.is_pipeflow(x)) {
        stop_no_call("x must be a pipeflow pip")
    }
    if (.is_pipeflow_view(x)) {
        stop_no_call(
            "x must be a full pipeline, not a view - use ",
            "x[, view = FALSE] to turn the view into a standalone pipeline"
        )
    }
    invisible(x)
}

.assert_state <- function(state) {
    if (!.is_single(state, "character") || is.na(state)) {
        stop("state must be a single string")
    }
    allowed <- names(.step_states)
    if (!(state %in% allowed)) {
        stop("state must be one of: ", toString(allowed))
    }
    invisible(state)
}

# -------
# Wrapper
# -------

# Outer pipeline env wrapper that allows to create views as copied objects with
# different names and view specifications, while sharing (i.e. pointing to) the
# same underlying pipeline environment. The `view` of a view holds the node ids
# of the covered steps (see `.pip_rows_to_view()`).
.wrap_pipenv <- function(pipenv, name, view = NULL) {
    structure(
        list(name = name, view = view, pipenv = pipenv),
        class = "pipeflow"
    )
}

#' Provide `.self` to a step function that refers to it
#'
#' If the body of `fun` contains the name `.self` (also within functions
#' defined in the body), a copy of `fun` is returned whose environment is a
#' new child of the environment of `fun`, holding `self` as `.self`. `fun`
#' and its environment are not changed. Other functions are returned as they
#' are, so their outputs don't keep a reference to the pipeline. Indirect
#' access, such as `get(".self")`, is not detected.
#'
#' @param fun The step function.
#' @param self The pipeflow pipeline that `.self` refers to.
#' @return `fun`, or a copy of it that sees `self` as `.self`.
#' @noRd
.wrap_self <- function(fun, self) {
    if (!(".self" %in% all.names(body(fun)))) {
        return(fun)
    }
    env <- new.env(parent = environment(fun))
    env[[".self"]] <- self
    environment(fun) <- env
    fun
}

#' Release the pipeline that was provided as `.self` for one step call
#'
#' Removes `.self` from the environment that `.wrap_self()` created for a
#' single step call, so that whatever the step returned and that captured
#' the call frame doesn't keep the pipeline alive. The environment of the
#' user's function is never touched, since `.wrap_self()` only ever adds
#' `.self` to a child environment of its own.
#'
#' @param env The environment that `.wrap_self()` created.
#' @return `NULL`, invisibly.
#' @noRd
.release_self <- function(env) {
    if (exists(".self", envir = env, inherits = FALSE)) {
        rm(list = ".self", envir = env)
    }
    invisible()
}

# ------------------------------
# Parameter & dependency parsing
# ------------------------------
#' The params of a step: the defaults of its function, overridden by params
#'
#' @param fun The step function.
#' @param params Named list of param values. They override the defaults of
#' `fun` and give a value to arguments of `fun` that have no default. Names
#' that are no arguments of `fun` are added.
#' @return Named list: the arguments of `fun` (without `...`) in their order,
#' followed by the other `params`. The defaults of `fun` are resolved in its
#' environment, except for those that `params` override.
#' @noRd
.extract_fun_params <- function(fun, params = list()) {
    args <- formals(fun)

    # Remove potential "..." argument
    hasDots <- "..." %in% names(args)
    if (hasDots) {
        args <- args[!names(args) %in% "..."]
    }
    isGiven <- names(args) %in% names(params)

    # First verify that all args that params don't give have default values
    is_missing_default <- function(x) {
        identical(x, quote(expr = ))
    }
    undef <- names(Filter(args[!isGiven], f = is_missing_default))
    if (length(undef) > 0L) {
        stop_no_call(
            paste0("'", undef, "'", collapse = ", "),
            ifelse(length(undef) > 1L, " have ", " has "),
            "no default value - set one in fun or give a value in params"
        )
    }

    # Make sure default values are returned as resolved values
    values <- stats::setNames(vector("list", length(args)), names(args))
    values[!isGiven] <- lapply(
        args[!isGiven],
        \(x) eval(x, envir = environment(fun))
    )
    values[names(params)] <- params
    values
}

.extract_depends <- function(
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
    if (toPos < 1) {
        stop_no_call("toPos (", toPos, ") must be at least 1")
    }
    if (toPos > length(steps)) {
        stop_no_call(
            sprintf(
                "toPos (%d) exceeds number of steps (%d)",
                toPos,
                length(steps)
            )
        )
    }

    # References to other steps are marked using a formula and can be either
    # referencing earlier steps (e.g. x = ~step1) or using positional indices
    # by pointing backwards a certain number of steps (e.g. x = ~-2)
    depends <- formula_deps(params)
    if (length(depends) == 0) {
        return(character(0))
    }

    # Convert relative positional indices (e.g. x = ~-2) to step names.
    iRelPos <- which(depends |> startsWith("-"))
    if (length(iRelPos) > 0) {
        stepNumbers <- toPos + as.integer(depends[iRelPos])
        if (any(stepNumbers < 1)) {
            firstBad <- which(stepNumbers < 1)[1]
            badIdx <- depends[iRelPos[firstBad]]
            info <- "relative index %s = ~%s points outside pipeline"
            stop_no_call(sprintf(info, names(badIdx), badIdx))
        }
        depends[iRelPos] <- steps[as.integer(stepNumbers)]
    }

    unlist(depends)
}

# --------------
# Step execution
# --------------
.as_pipeflow_partitioned <- function(x) {
    if (!is.list(x)) {
        stop("split mode requires step output to be a list")
    }

    nm <- names(x)
    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm))) {
        stop("split output must be a named list with non-empty keys")
    }

    if (anyDuplicated(nm) > 0L) {
        stop("split output keys must be unique")
    }

    class(x) <- c(class(x), "pipeflow_partitioned")
    x
}

#' Create a step error condition
#'
#' Error signalled by `.pip_run_row()` when a step function fails. It keeps
#' the original condition as `parent`.
#'
#' @param step The name of the failed step.
#' @param parent The original condition raised by the step function.
#' @return A condition of class
#' `c("pipeflow_step_error", "error", "condition")` with the message
#' `"step '<step>': <message of parent>"` and the fields `step` and `parent`.
#' @noRd
.pip_step_error <- function(step, parent) {
    structure(
        class = c("pipeflow_step_error", "error", "condition"),
        list(
            message = sprintf("step '%s': %s", step, conditionMessage(parent)),
            call = NULL,
            step = step,
            parent = parent
        )
    )
}

#' Create a partition key error condition
#'
#' Error signalled by `.pip_execute_step_call()` when a step function fails
#' for one key of its partitioned inputs, and by `.pip_annotate_output()`
#' when `pip_output_value()` fails for one key of a partitioned output. It
#' keeps the original condition as `parent`.
#'
#' @param key The partition key for which the step function failed.
#' @param parent The original condition raised by the step function.
#' @return A condition of class
#' `c("pipeflow_key_error", "error", "condition")` with the message
#' `"key '<key>': <message of parent>"` and the fields `key` and `parent`.
#' @noRd
.pip_key_error <- function(key, parent) {
    structure(
        class = c("pipeflow_key_error", "error", "condition"),
        list(
            message = sprintf("key '%s': %s", key, conditionMessage(parent)),
            call = NULL,
            key = key,
            parent = parent
        )
    )
}

#' Create the error of a failed pip_set_params() call
#'
#' Error signalled by `pip_set_params()` when `pip_param_value<-()` failed
#' for one or more of the parameters that were to be set. All of them are
#' reported at once, and nothing is set.
#'
#' @param errors List of the collected failures, each a list with the
#' elements `step`, `name` and `parent`.
#' @return A condition of class
#' `c("pipeflow_set_params_error", "error", "condition")` with the fields
#' `errors` (a data frame with the columns `step`, `name` and `message`) and
#' `parents` (the list of the original conditions).
#' @noRd
.pip_set_params_error <- function(errors) {
    steps <- vapply(errors, FUN = `[[`, "step", FUN.VALUE = character(1))
    names <- vapply(errors, FUN = `[[`, "name", FUN.VALUE = character(1))
    parents <- lapply(errors, FUN = `[[`, "parent")
    messages <- vapply(
        parents,
        FUN = conditionMessage,
        FUN.VALUE = character(1)
    )

    msg <- paste0(
        sprintf(
            "Cannot set %s: nothing was set.\n",
            if (length(errors) == 1L) {
                "1 parameter"
            } else {
                sprintf("%d parameters", length(errors))
            }
        ),
        paste0(
            sprintf("  step '%s', parameter '%s': %s", steps, names, messages),
            collapse = "\n"
        )
    )

    structure(
        class = c("pipeflow_set_params_error", "error", "condition"),
        list(
            message = msg,
            call = NULL,
            errors = data.frame(
                step = steps,
                name = names,
                message = messages,
                stringsAsFactors = FALSE
            ),
            parents = parents
        )
    )
}

#' Create a failure object
#'
#' Failure object for the output of `step` in a run that continues after
#' errors. It is passed to the step arguments whose reference is wrapped in
#' `try()`, e.g. `x = ~try(step)`. A failure in one partition key of a
#' mapped step (see `.pip_execute_step_call()`) is stored as the output of
#' that key.
#'
#' @param step The step whose output the failure object replaces.
#' @param parent The original condition of the step that actually failed.
#' @param failedStep The step that actually failed. If it differs from
#' `step`, `step` was not run because of it.
#' @param key The partition key the failure belongs to, or `NULL` for the
#' output of the whole step.
#' @return A condition of class
#' `c("pipeflow_failure", "error", "condition")` with the fields `step`,
#' `failed_step` and `parent`, and `key` if given.
#' @noRd
.pip_failure <- function(step, parent, failedStep = step, key = NULL) {
    msg <- sprintf(
        "step '%s' failed: %s",
        failedStep,
        conditionMessage(parent)
    )
    if (step != failedStep) {
        msg <- sprintf("step '%s' was not run: %s", step, msg)
    }
    fields <- list(
        message = msg,
        call = NULL,
        step = step,
        failed_step = failedStep,
        parent = parent
    )
    if (!is.null(key)) {
        fields[["key"]] <- key
    }
    structure(class = c("pipeflow_failure", "error", "condition"), fields)
}

#' Get the partition keys of an output that hold a failure
#'
#' @param out A step output.
#' @return Character vector of the keys of a partitioned `out` whose output
#' is a failure object (see `.pip_failure()`), empty for any other `out`.
#' @noRd
.pip_failed_keys <- function(out) {
    if (!.is_pipeflow_partitioned(out)) {
        return(character())
    }
    isFailure <- vapply(
        out,
        FUN = inherits,
        what = "pipeflow_failure",
        FUN.VALUE = logical(1)
    )
    names(out)[isFailure]
}

#' Get the first failure of a partitioned output
#'
#' A failure is the step's own if the step failed for that key. A key can
#' also hold the failure of an input step: if the key was not run because
#' the input failed, or if the step got the failure via `~try()` and
#' returned it as-is.
#'
#' @param out A partitioned step output with at least one failed key.
#' @param step The name of the step whose output `out` is.
#' @return The failure object of the first key that failed in `step` itself,
#' or, if `step` failed for none of its keys itself, of the first failed key.
#' @noRd
.pip_first_failure <- function(out, step) {
    failures <- unname(out[.pip_failed_keys(out)])
    isOwn <- vapply(
        failures,
        FUN = \(f) identical(f[["failed_step"]], step),
        FUN.VALUE = logical(1)
    )
    failures[[if (any(isOwn)) which(isOwn)[[1L]] else 1L]]
}

#' Rename a step in the failures of a partitioned output
#'
#' @param out A partitioned step output.
#' @param from,to The old and the new name of the step.
#' @return `out`, with each failure of a key that names step `from` (as its
#' step or as the step that failed) rebuilt with `to` instead, including its
#' message.
#' @noRd
.pip_rename_failures <- function(out, from, to) {
    for (key in .pip_failed_keys(out)) {
        f <- out[[key]]
        if (!from %in% c(f[["step"]], f[["failed_step"]])) {
            next
        }
        swap <- \(name) if (identical(name, from)) to else name
        out[[key]] <- .pip_failure(
            swap(f[["step"]]),
            parent = f[["parent"]],
            failedStep = swap(f[["failed_step"]]),
            key = f[["key"]]
        )
    }
    out
}

#' Create the warning of a run that continued after errors
#'
#' @param pipname The name of the pipeline (used in the message).
#' @param failures Named list, by step name, of the failure objects of the
#' run (see `.pip_failure()`) and of the partitioned outputs of the steps
#' that failed for some of their keys.
#' @return A condition of class
#' `c("pipeflow_run_failed", "warning", "condition")` with the fields
#' `failed` (named list of the original conditions of the failed steps; for
#' a step that failed for some keys, of its first failed key, see
#' `.pip_first_failure()`), `skipped` (names of the steps not run because an
#' input failed) and `keys` (named list of the failed keys of the steps
#' that failed for some keys).
#' @noRd
.pip_run_failed_warning <- function(pipname, failures) {
    isPartial <- vapply(
        failures,
        FUN = .is_pipeflow_partitioned,
        FUN.VALUE = logical(1)
    )
    keys <- lapply(failures[isPartial], FUN = .pip_failed_keys)
    conds <- failures
    conds[isPartial] <- mapply(
        FUN = .pip_first_failure,
        out = failures[isPartial],
        step = names(failures)[isPartial],
        SIMPLIFY = FALSE
    )

    failedSteps <- vapply(conds, \(f) f[["failed_step"]], FUN.VALUE = "")
    isFailed <- isPartial | names(failures) == failedSteps
    failed <- lapply(conds[isFailed], \(f) f[["parent"]])
    skipped <- names(failures)[!isFailed]

    # A step that failed for some keys is listed with the message of its
    # first failed key, or with the step that failed for it.
    reasons <- vapply(
        names(failed),
        FUN = function(step) {
            if (!isPartial[[step]]) {
                return(conditionMessage(failed[[step]]))
            }
            f <- conds[[step]]
            reason <- if (identical(f[["failed_step"]], step)) {
                conditionMessage(f[["parent"]])
            } else {
                sprintf(
                    "key '%s': input step '%s' failed",
                    f[["key"]],
                    f[["failed_step"]]
                )
            }
            sprintf(
                "%s (%d of %d keys)",
                reason,
                length(keys[[step]]),
                length(failures[[step]])
            )
        },
        FUN.VALUE = ""
    )

    msg <- sprintf(
        "Run of pipeline '%s' continued after %d failed step%s:\n%s",
        pipname,
        length(failed),
        if (length(failed) == 1L) "" else "s",
        paste0("  '", names(failed), "': ", reasons, collapse = "\n")
    )
    if (length(skipped) > 0L) {
        msg <- paste0(
            msg,
            "\nSteps not run because an input failed: ",
            paste0("'", skipped, "'", collapse = ", ")
        )
    }

    structure(
        class = c("pipeflow_run_failed", "warning", "condition"),
        list(
            message = msg,
            call = NULL,
            failed = failed,
            skipped = skipped,
            keys = keys
        )
    )
}

#' The step call that runs
#'
#' @return A list of `step`, `key` and `keys`, see `.pip_call_step()`. All
#' of them are `NULL` outside of a run.
#' @noRd
.pip_step_call <- function() {
    mget(c("step", "key", "keys"), envir = .step_call)
}

#' Call a step function as the step call that runs
#'
#' Sets the step call (see `.pip_step_call()`) for the time of the call and
#' restores the one of the caller on exit, also on error, so that a nested
#' run (e.g. of an inner pipeline of a function made by
#' `as.function.pipeflow()`) leaves it as it was.
#'
#' @param fun The step function.
#' @param args Named list of the arguments.
#' @param step The name of the step.
#' @param key,keys For a call mapped over partitions, its partition key and
#' all keys; `NULL` for a single call.
#' @return The result of the call.
#' @noRd
.pip_call_step <- function(fun, args, step, key = NULL, keys = NULL) {
    caller <- .pip_step_call()
    on.exit(list2env(caller, envir = .step_call), add = TRUE)
    list2env(list(step = step, key = key, keys = keys), envir = .step_call)
    do.call(fun, args = args)
}

#' Call a step function according to its exec mode
#'
#' @param fun The step function.
#' @param args Named list of the arguments, with the outputs of the steps it
#' depends on.
#' @param exec The exec mode of the step.
#' @param step The name of the step.
#' @param onError The `on_error` of the run, see [pip_run()]. With
#' `"continue"`, a step mapped over partitioned inputs that fails for a key
#' stores a failure object (see `.pip_failure()`) as the output of that key
#' and goes on with the other keys.
#' @param tryArgs Names of the arguments whose step reference is wrapped in
#' `try()`. Only these get the failure of a key of a partitioned input; for
#' the other arguments the key is not run and its output is a failure too.
#' @param keep Named list of outputs by partition key, for a step mapped over
#' partitioned inputs: the function is not called for these keys, and their
#' outputs are kept as they are (see `pip_run()`, retry of failed keys).
#' @return The output of the step.
#' @noRd
.pip_execute_step_call <- function(
    fun,
    args,
    exec,
    step,
    onError = "stop",
    tryArgs = character(),
    keep = list()
) {
    # A partitioned argument is a list of per-key values produced by a
    # step with exec = "split" and tagged with class "pipeflow_partitioned".
    partIdx <- which(vapply(
        args,
        FUN = .is_pipeflow_partitioned,
        FUN.VALUE = logical(1)
    ))

    # Split mode: run the function once on the full (unpartitioned) inputs and
    # mark the result as partitioned so downstream steps can map over it.
    if (exec == "split") {
        out <- .pip_call_step(fun, args = args, step = step)
        return(.as_pipeflow_partitioned(out))
    }

    # Plain mode: only executes a single call, so partitioned inputs
    # (which would require mapping) are not allowed.
    if (exec == "plain" && length(partIdx) > 0L) {
        stop("plain mode does not accept partitioned inputs")
    }

    # Reduce mode: combines partitioned inputs in a single call,
    # so it needs at least one partitioned input to be meaningful. A failure
    # object counts as one, because a failed split step yields a single
    # failure instead of a partitioned output.
    if (exec == "reduce" && length(partIdx) == 0L) {
        hasFailure <- any(vapply(
            args,
            FUN = inherits,
            what = "pipeflow_failure",
            FUN.VALUE = logical(1)
        ))
        if (!hasFailure) {
            stop("reduce mode requires at least one partitioned input")
        }
    }

    # Single-call, which happens in three scenarios:
    # 1) there are no partitioned inputs, which is the standard case in a
    #    standard pipeline that runs without any split/reduce steps
    # 2) plain mode was explicitly requested (to ensure non-split mode)
    # 3) reduce mode was requested to re-combine partitioned inputs
    if (length(partIdx) == 0L || exec == "plain" || exec == "reduce") {
        return(.pip_call_step(fun, args = args, step = step))
    }

    # Auto mode with partitioned inputs: map the function over the partition
    # keys. Keys are taken from the first partitioned argument...
    keys <- names(args[[partIdx[[1]]]])
    for (k in partIdx[-1]) {
        kk <- names(args[[k]])
        if (!identical(kk, keys)) {
            stop("partitioned arguments must share identical keys")
        }
    }

    # With no key there is no call, which could tell a pipeline function that
    # the keys it keeps pipelines for are gone.
    if (length(keys) == 0L) {
        .fun_release(fun, step)
    }

    # ...and each key is computed separately by slicing all partitioned
    # arguments down to that key before calling the function.
    out <- stats::setNames(vector(mode = "list", length = length(keys)), keys)
    for (key in keys) {
        if (key %in% names(keep)) {
            out[key] <- keep[key]
            next
        }
        keyArgs <- args
        for (idx in partIdx) {
            keyArgs[[idx]] <- args[[idx]][[key]]
        }

        # A key whose input failed (in a continued run) is only allowed to run
        # if all arguments that get a failure refer to their step via try().
        isFailed <- vapply(
            keyArgs[partIdx],
            FUN = inherits,
            what = "pipeflow_failure",
            FUN.VALUE = logical(1)
        )
        notAllowed <- setdiff(names(keyArgs)[partIdx][isFailed], tryArgs)
        if (length(notAllowed) > 0L) {
            cause <- keyArgs[[notAllowed[[1L]]]]
            failure <- .pip_failure(
                step,
                parent = cause[["parent"]],
                failedStep = cause[["failed_step"]],
                key = key
            )
            if (onError == "stop") {
                stop(.pip_key_error(key, parent = failure))
            }
            out[[key]] <- failure
            next
        }

        # Errors are re-raised with the key name so a failing partition can
        # be located without inspecting the whole output. In a continued
        # run, the error is kept as the output of the key instead.
        out[[key]] <- tryCatch(
            expr = .pip_call_step(
                fun,
                args = keyArgs,
                step = step,
                key = key,
                keys = keys
            ),
            error = function(e) {
                keyError <- .pip_key_error(key, parent = e)
                if (onError == "stop") {
                    stop(keyError)
                }
                .pip_failure(step, parent = keyError, key = key)
            }
        )
    }

    .as_pipeflow_partitioned(out)
}

# `failed` is an optional named list of failure objects that replace the
# outputs of the referenced steps for the respective arguments. `onError` is
# the `on_error` of the run: with "continue", a mapped step that fails for
# some keys keeps the failures in its output, see .pip_execute_step_call().
# Such a step gets state "failed" and keeps its time. With `retry = TRUE`,
# such a step only calls its function for the keys that failed and keeps the
# outputs of the other keys (see pip_run()).
.pip_run_row <- function(
    x,
    i,
    lgr,
    failed = list(),
    onError = "stop",
    retry = FALSE
) {
    if (!.is_pipeflow(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.pip_is_indexed(x)) {
        .pip_reindex(x)
    }

    dat <- .pip_pipenv(x)[["data"]]
    fun <- dat[["fun"]][[i]]
    args <- dat[["params"]][[i]]
    depends <- dat[["depends"]][[i]]
    exec <- dat[["exec"]][[i]]

    # If calculation depends on results of earlier steps, get them from
    # respective referenced output slots of the pipeline.
    if (length(depends) > 0) {
        refsOut <- .pip_filter(x, on = "step", values = depends)[["out"]]
        args[names(depends)] <- refsOut
    }
    args[names(failed)] <- failed

    step <- dat[["step"]][[i]]

    # Provide `.self` (the full pipeline being run) to a step function that
    # refers to it, in an environment of its own for this call. That
    # environment is released once the step returned (or failed), so that
    # outputs capturing the call frame don't keep the pipeline alive.
    funSelf <- .wrap_self(fun, self = x)
    if (!identical(environment(funSelf), environment(fun))) {
        on.exit(.release_self(environment(funSelf)), add = TRUE)
    }
    fun <- funSelf

    unbound <- dat[["unbound"]][[i]]

    keep <- list()
    if (retry) {
        prev <- dat[["out"]][[i]]
        keep <- prev[setdiff(names(prev), .pip_failed_keys(prev))]
    }

    # The start of every attempt, also of one that fails. Row `i` is still
    # the step's row here, as only the step call can shift the rows.
    data.table::set(dat, i = i, j = "time_start", value = Sys.time())

    # `.steps_with_current_out` remembers the steps whose stored output comes
    # from their last attempt (an environment: step name -> TRUE/FALSE). Here,
    # at the start of an attempt, we unmark the current step and it will be
    # marked again only if this attempt successfully stores an output (see
    # below).
    pipenv <- .pip_pipenv(x)
    currentOut <- pipenv[[".steps_with_current_out"]]
    currentOut[[step]] <- FALSE
    # The step may change its own function, params or exec mode while it
    # runs (via `.self`), which makes the outputs of this attempt stale right
    # away.
    funAtStart <- dat[["fun"]][[i]]
    paramsAtStart <- dat[["params"]][[i]]

    out <- withCallingHandlers(
        {
            args <- .pip_unwrap_params(args, unbound)
            res <- .pip_execute_step_call(
                fun = fun,
                args = args,
                exec = exec,
                step = step,
                onError = onError,
                tryArgs = formula_try_args(dat[["params"]][[i]]),
                keep = keep
            )

            # One time stamp for the whole step, also for all keys of a
            # partitioned output, and the one that is stored below.
            runTime <- Sys.time()
            .pip_annotate_output(
                res,
                step = step,
                time = runTime,
                skip = names(keep)
            )
        },
        error = function(e) {
            data.table::set(
                dat,
                i = i,
                j = "state",
                value = .step_states[["failed"]][["name"]]
            )
            lgr(level = "error", msg = e$message)
            stop(.pip_step_error(step, parent = e))
        },
        warning = function(w) {
            lgr(level = "warn", msg = w$message)
        },
        message = function(m) {
            lgr(level = "info", msg = m$message)
        }
    )

    # Only the keys that failed in this step are logged, not those that were
    # not run because an input failed.
    failedKeys <- .pip_failed_keys(out)
    for (key in failedKeys) {
        f <- out[[key]]
        if (identical(f[["failed_step"]], step)) {
            lgr(level = "error", msg = conditionMessage(f[["parent"]]))
        }
    }

    # Re-read pipeline after execution to handle scenarios where the pipeline
    # modified itself at runtime. Since we update by step name, if the current
    # step does not exist anymore, we simply skip the update.
    dat <- .pip_pipenv(x)[["data"]]
    rowNow <- data.table::chmatch(step, dat[["step"]])
    stepStillExists <- !is.na(rowNow)
    if (stepStillExists && length(failedKeys) > 0L) {
        data.table::set(
            dat,
            i = rowNow,
            j = c("out", "state"),
            value = list(list(out), .step_states[["failed"]][["name"]])
        )
        # Remember that the step's output now comes from this (current)
        # attempt, unless the step changed itself during the attempt.
        isUnchanged <- identical(dat[["fun"]][[rowNow]], funAtStart) &&
            identical(dat[["params"]][[rowNow]], paramsAtStart) &&
            identical(dat[["exec"]][[rowNow]], exec)
        if (isUnchanged) {
            currentOut[[step]] <- TRUE
        }
    } else if (stepStillExists) {
        data.table::set(
            dat,
            i = rowNow,
            j = c("out", "time", "state"),
            value = list(
                list(out),
                runTime,
                .step_states[["done"]][["name"]]
            )
        )
    }

    out
}

# -------------
# State updates
# -------------
.pip_restart <- function(pipenv, force = TRUE, times = 1L) {
    if (!.is_single(force, "logical")) {
        stop("force must be a single logical value")
    }
    if (!.is_single(times, "numeric") || is.na(times) || times < 1L) {
        stop("times must be a single integer value >= 1")
    }

    count <- pipenv[[".restart_count"]]
    if (count >= times) {
        pipenv[[".restart_count"]] <- 0L
        return(invisible())
    }

    pipenv[[".run_state"]][] <- .run_states[["restart"]][["name"]]
    pipenv[[".restart_count"]] <- count + 1L
    pipenv[[".restart_force"]] <- force
    invisible()
}

.pip_halt <- function(pipenv) {
    pipenv[[".run_state"]][] <- .run_states[["halted"]][["name"]]
    invisible()
}

#' Mark steps and their downstream steps as outdated
#'
#' Marks `steps` and all steps downstream of them as `"outdated"`, except for
#' the steps in `keep`. Steps in state `"new"` have never run, so they stay
#' `"new"`.
#'
#' @param x A pipeflow pipeline or view.
#' @param steps Character vector of step names. Unknown steps are ignored.
#' @param keep Character vector of step names whose state is not changed.
#' @return `x`, invisibly. The step states are updated in place.
#' @noRd
.pip_outdate_downstream <- function(x, steps, keep = character()) {
    nodes <- unique(as.integer(unlist(.pip_get_reachable_nodes(x, steps))))
    if (length(nodes) == 0L) {
        return(invisible(x))
    }

    dat <- .pip_pipenv(x)[["data"]]
    rows <- dat[list(nodes), which = TRUE, on = "nodeId", nomatch = NULL]
    isNew <- dat[["state"]][rows] == .step_states[["new"]][["name"]]
    rows <- rows[!isNew & !(dat[["step"]][rows] %in% keep)]
    if (length(rows) > 0L) {
        data.table::set(
            dat,
            i = rows,
            j = "state",
            value = .step_states[["outdated"]][["name"]]
        )
    }

    invisible(x)
}


# ----------------------------------
# Pipeline data access and filtering
# ----------------------------------

# Convenience helper function to retrieve the shared inner environment.
.pip_pipenv <- function(x) {
    pipenv <- .subset2(x, "pipenv")
    dat <- pipenv[["data"]]
    if (is.data.frame(dat) && is.null(dat[["time_start"]])) {
        .pip_add_time_start(pipenv)
    }
    # Pipelines saved with older versions of pipeflow have no
    # `.steps_with_current_out` (see pip_new())
    if (
        is.environment(pipenv) && is.null(pipenv[[".steps_with_current_out"]])
    ) {
        pipenv[[".steps_with_current_out"]] <- new.env(parent = emptyenv())
    }
    pipenv
}

#' Add the `time_start` column to the step table of a saved pipeline
#'
#' Pipelines saved with pipeflow 0.5.0 or earlier have no `time_start`
#' column. It is added with `NA` values (no step has a recorded start)
#' after the `time` column. A table read with `readRDS()` has no spare
#' column slots, so adding a column returns a new table, which is stored
#' in `pipenv`.
#'
#' @param pipenv The inner pipeline environment.
#' @return The updated step table, invisibly.
#' @noRd
.pip_add_time_start <- function(pipenv) {
    dat <- data.table::set(
        pipenv[["data"]],
        j = "time_start",
        value = rep(as.POSIXct(NA), nrow(pipenv[["data"]]))
    )
    data.table::setcolorder(dat, "time_start", after = "time")
    pipenv[["data"]] <- dat
    invisible(dat)
}

#' Get the DAG of a pipeline
#'
#' All access to the DAG goes through this function. The DAG lives in C++
#' behind an external pointer, which `saveRDS()` stores as a null pointer.
#' In that case the DAG is rebuilt from the step table (see
#' `.pip_dag_restore()`) and stored in `pipenv`, so the pipeline and all
#' views on it use the rebuilt DAG from then on.
#'
#' @param pipenv The inner pipeline environment.
#' @return The external pointer to the DAG.
#' @noRd
.pip_dag <- function(pipenv) {
    d <- pipenv[[".dag"]]
    if (dag_is_null(d)) {
        d <- .pip_dag_restore(pipenv)
        pipenv[[".dag"]] <- d
    }
    d
}

#' Rebuild the DAG of a pipeline from its step table
#'
#' The nodes are the ids of the `nodeId` column, kept as they are since views
#' refer to them, in the order of the rows, which is the topological order.
#' The edges come from the `depends` column. The ids of removed steps are not
#' reused, as `dag_restore()` takes the number of ids from the (null) pointer.
#'
#' @param pipenv The inner pipeline environment.
#' @return An external pointer to the rebuilt DAG.
#' @noRd
.pip_dag_restore <- function(pipenv) {
    dat <- pipenv[["data"]]
    steps <- dat[["step"]]
    ids <- as.integer(dat[["nodeId"]])

    # Other functions find the nodes of steps via the step->node map, so it
    # must match the step table.
    stepsToNodes <- pipenv[[".steps_to_nodes"]]
    mapped <- mget(
        steps,
        envir = stepsToNodes,
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
    if (
        length(stepsToNodes) != length(steps) ||
            !identical(as.integer(unlist(mapped, use.names = FALSE)), ids)
    ) {
        stop_no_call(
            "cannot rebuild the pipeline DAG: the step to node map does not ",
            "match the step table"
        )
    }

    from <- lapply(
        dat[["depends"]],
        FUN = \(dep) unique(ids[data.table::chmatch(unname(dep), steps)])
    )
    to <- rep.int(ids, lengths(from))
    from <- as.integer(unlist(from))
    if (anyNA(from)) {
        stop_no_call(
            "cannot rebuild the pipeline DAG: steps depend on unknown steps"
        )
    }

    dag_restore(pipenv[[".dag"]], order = ids, from = from, to = to)
}

#' Build the view selector for rows of the step table
#'
#' Returns the node ids of the steps in the given rows. Unlike row numbers,
#' node ids don't change when steps are added, removed, renamed or replaced,
#' and they are never reused, so a view keeps covering its steps.
#'
#' @param pipenv The inner pipeline environment.
#' @param rows Absolute row indices of the step table.
#' @return An integer vector of node ids, to be used as the `view` of
#' `.wrap_pipenv()`.
#' @noRd
.pip_rows_to_view <- function(pipenv, rows) {
    as.integer(pipenv[["data"]][["nodeId"]][rows])
}

# The rows covered by `x`: all pipeline rows for a full pipeline, or the rows
# of the steps in the view's selector (in pipeline order) for a view. Steps
# that were removed from the pipeline are no longer covered.
.pip_view_rows <- function(x) {
    view <- .subset2(x, "view")
    nodeIds <- .pip_pipenv(x)[["data"]][["nodeId"]]
    if (is.null(view)) {
        return(seq_along(nodeIds))
    }
    which(nodeIds %in% view)
}

.pip_view_data <- function(x) {
    rows <- .pip_view_rows(x)
    .pip_pipenv(x)[["data"]][rows, ]
}

# TRUE if `step` is covered by the rows of `x`. Always TRUE for a full
# pipeline (where all rows are covered) and FALSE for a view that does not
# cover the step.
.pip_step_in_view <- function(x, step) {
    absRow <- data.table::chmatch(step, .pip_pipenv(x)[["data"]][["step"]])
    if (is.na(absRow)) {
        return(FALSE)
    }
    absRow %in% .pip_view_rows(x)
}

# Resolve the `i` selector of `[.pipeflow` to absolute row indices. `i_expr`
# is the unevaluated `i` expression; it is evaluated in the context of the
# covered rows, i.e. the data of a view or the full step table of a pipeline.
# `enclos` is used as the enclosing environment for boolean filter expressions.
.pip_select_rows <- function(x, i_expr, enclos, keep_order = FALSE) {
    dat <- .pip_pipenv(x)[["data"]]
    view <- .is_pipeflow_view(x)
    rows <- .pip_view_rows(x)
    sub <- if (view) dat[rows] else dat

    value <- eval(i_expr, envir = sub, enclos = enclos)

    if (is.logical(value)) {
        if (length(value) == 1L) {
            value <- rep(value, nrow(sub))
        }
        if (length(value) != nrow(sub)) {
            stop(sprintf(
                "logical filter has length %d but the %s has %d rows",
                length(value),
                if (view) "view" else "pipeline",
                nrow(sub)
            ))
        }
        if (anyNA(value)) {
            stop("logical filter must not contain NA")
        }
        return(rows[which(value)])
    }

    if (is.numeric(value)) {
        if (anyNA(value)) {
            stop("row indices in 'i' must not contain NA")
        }
        idx <- as.integer(value)
        if (any(idx < 0L)) {
            # Negative indices exclude the corresponding rows like in base R
            if (any(idx > 0L)) {
                stop("only 0's may be mixed with negative subscripts")
            }
            idx <- seq_len(nrow(sub))[idx]
        }
        if (!keep_order) {
            idx <- sort(unique(idx))
        }
        bad <- idx[idx < 1L | idx > nrow(sub)]
        if (length(bad) > 0L) {
            stop("Invalid row indices in 'i': ", toString(bad))
        }
        return(rows[idx])
    }

    if (is.character(value)) {
        if (anyNA(value)) {
            stop("step names must not contain NA")
        }
        if (!all(nzchar(value))) {
            stop("step names must be non-empty strings")
        }
        m <- data.table::chmatch(value, dat[["step"]])
        if (anyNA(m)) {
            unknown <- unique(value[is.na(m)])
            stop("Unknown step names: ", toString(unknown), call. = FALSE)
        }
        if (!keep_order) {
            m <- sort(unique(m))
        }
        outside <- m[!(m %in% rows)]
        if (length(outside) > 0L) {
            stop(
                "step '",
                dat[["step"]][[outside[[1L]]]],
                "' is not part of the view"
            )
        }
        return(m)
    }

    stop(sprintf(
        "`i` must evaluate to row indices, step names, or a logical filter, ",
        "not %s",
        typeof(value)
    ))
}

.pip_filter <- function(x, on, values) {
    .pip_pipenv(x)[["data"]][list(values), on = on]
}

.pip_filter_nodes <- function(x, nodes) {
    .pip_pipenv(x)[["data"]][list(nodes), on = "nodeId"]
}


# --------
# Indexing
# -------.
.pip_is_indexed <- function(x) {
    !is.null(data.table::indices(.pip_pipenv(x)[["data"]]))
}

.pip_reindex <- function(x) {
    data.table::setindexv(.pip_pipenv(x)[["data"]], list("step", "nodeId"))
}


# ---------------------------
# Step lookup & DAG traversal
# ---------------------------
.pip_step_exists <- function(x, step) {
    exists(
        step,
        where = .pip_pipenv(x)[[".steps_to_nodes"]],
        inherits = FALSE
    )
}

.pip_steps_to_nodes <- function(x, steps) {
    mget(
        steps,
        envir = .pip_pipenv(x)[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
}

.pip_steps_to_rows <- function(x, steps) {
    data <- .pip_pipenv(x)[["data"]]

    if (anyNA(steps)) {
        stop("step names must not contain NA", call. = FALSE)
    }
    if (!all(nzchar(steps))) {
        stop("step names must be non-empty strings", call. = FALSE)
    }

    i <- data.table::chmatch(steps, data[["step"]])
    if (anyNA(i)) {
        unknown <- unique(steps[is.na(i)])
        stop("Unknown step names: ", toString(unknown), call. = FALSE)
    }
    as.integer(i)
}

.pip_get_reachable_nodes <- function(x, steps, downstream = TRUE) {
    env <- .pip_pipenv(x)
    known <- intersect(steps, names(env[[".steps_to_nodes"]]))
    if (length(known) == 0L) {
        return(integer(0))
    }

    start_ids <- as.integer(mget(
        known,
        envir = env[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    ))
    start_ids <- start_ids[!is.na(start_ids)]
    if (length(start_ids) == 0L) {
        return(integer(0))
    }

    if (downstream) {
        dag_get_reachable_nodes_down(.pip_dag(env), start_ids)
    } else {
        dag_get_reachable_nodes_up(.pip_dag(env), start_ids)
    }
}


# -----------------
# Pipeline addition
# -----------------

.pip_append <- function(x, step, fun, tags, exec = "auto", params = list()) {
    params <- .extract_fun_params(fun, params)

    # Determine and verify potential links to existing steps
    env <- .pip_pipenv(x)
    steps <- c(env[["data"]][["step"]], step)
    depends <- .extract_depends(params = params, steps = steps)
    refNodes <- mget(
        depends,
        envir = env[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
    if (anyNA(refNodes)) {
        notFound <- Filter(is.na, refNodes)
        stop_no_call(
            "while adding step '",
            step,
            "' - cannot reference unknown steps: ",
            paste0("'", names(notFound), "'", collapse = ", ")
        )
    }

    # Update DAG
    d <- .pip_dag(env)
    nodeId <- as.integer(dag_add_node(d))
    if (length(refNodes) > 0) {
        dag_add_edges_to(d, from = unique(as.integer(refNodes)), to = nodeId)
    }

    # Create and append step
    newStep <- .new_step(
        step = step,
        fun = fun,
        params = params,
        depends = depends,
        tags = tags,
        exec = exec,
        nodeId = nodeId
    )

    env[["data"]] <- data.table::rbindlist(
        list(env[["data"]], newStep),
        use.names = TRUE
    )
    env[[".steps_to_nodes"]][[step]] <- nodeId
    x
}

# Insert a step at position `pos` (0 <= pos < number of steps) of the
# pipeline, i.e. after the first `pos` steps. The new row is inserted into
# the data table (one O(n) rbindlist) and the node is added to the DAG at
# the corresponding position of its topological order, leaving all existing
# steps (and their runtime state) untouched.
.pip_insert <- function(x, step, fun, tags, exec, params, pos) {
    params <- .extract_fun_params(fun, params)

    env <- .pip_pipenv(x)
    data <- env[["data"]]

    # Dependencies may only point to steps that precede the insertion point
    # (mirroring the previous step-by-step rebuild, this keeps the row order
    # a valid topological order).
    prefixSteps <- data[["step"]][seq_len(pos)]
    steps <- c(prefixSteps, step)
    depends <- .extract_depends(params = params, steps = steps)
    if (length(depends) > 0L) {
        forbidden <- unname(depends[!(depends %in% prefixSteps)])
        if (length(forbidden) > 0L) {
            later <- forbidden[forbidden %in% data[["step"]]]
            if (length(later) > 0L) {
                stop_no_call(
                    "while adding step '",
                    step,
                    "' - cannot reference steps that come after the ",
                    "insertion point: ",
                    paste0("'", later, "'", collapse = ", ")
                )
            }
            stop_no_call(
                "while adding step '",
                step,
                "' - cannot reference unknown steps: ",
                paste0("'", forbidden, "'", collapse = ", ")
            )
        }
    }

    refNodes <- mget(
        depends,
        envir = env[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
    if (anyNA(refNodes)) {
        notFound <- Filter(is.na, refNodes)
        stop_no_call(
            "while adding step '",
            step,
            "' - cannot reference unknown steps: ",
            paste0("'", names(notFound), "'", collapse = ", ")
        )
    }

    # Update DAG: add the node at the insertion position of its order
    d <- .pip_dag(env)
    nodeId <- as.integer(dag_add_node_at(d, pos))
    if (length(refNodes) > 0) {
        dag_add_edges_to(d, from = unique(as.integer(refNodes)), to = nodeId)
    }

    # Create the new step and insert its row after the first `pos` steps
    newStep <- .new_step(
        step = step,
        fun = fun,
        params = params,
        depends = depends,
        tags = tags,
        exec = exec,
        nodeId = nodeId
    )
    n <- nrow(data)
    env[["data"]] <- data.table::rbindlist(
        list(
            data[seq_len(pos)],
            newStep,
            data[seq_len(n - pos) + pos]
        ),
        use.names = TRUE
    )
    env[[".steps_to_nodes"]][[step]] <- nodeId
    x
}


# Bind two pipelines together by concatenating their steps.
.pip_bind <- function(x, y) {
    out <- pip_clone(x, name = paste0(x[["name"]], "-", y[["name"]]))
    yy <- pip_clone(y)
    yyDat <- .pip_pipenv(yy)[["data"]]

    # Resolve all name clashes directly on the cloned source pipeline.
    reserved <- .pip_pipenv(out)[["data"]][["step"]]

    `%chin%` <- data.table::`%chin%`
    for (k in seq_len(nrow(yyDat))) {
        step <- yyDat[["step"]][[k]]
        if (step %chin% reserved) {
            to <- step
            i <- 2L
            allSteps <- yyDat[["step"]]
            while (to %chin% reserved || to %chin% allSteps) {
                to <- paste0(step, i)
                i <- i + 1L
            }
            pip_rename(yy, from = step, to = to)
        }
        reserved <- c(reserved, yyDat[["step"]][[k]])
    }

    # Append all (potentially renamed) steps of y in one pass: transform each
    # row (re-create the formula dependencies after renaming), allocate a
    # fresh node id, wire the DAG edges, and collect the rows for a single
    # rbindlist.
    outEnv <- .pip_pipenv(out)
    d <- .pip_dag(outEnv)
    stepsToNodes <- outEnv[[".steps_to_nodes"]]
    rows <- vector("list", nrow(yyDat))
    for (k in seq_len(nrow(yyDat))) {
        step <- yyDat[["step"]][[k]]
        fun <- yyDat[["fun"]][[k]]
        tags <- yyDat[["tags"]][[k]]
        exec <- yyDat[["exec"]][[k]]
        params <- yyDat[["params"]][[k]]
        depends <- yyDat[["depends"]][[k]]

        # Point the formula dependencies to the (renamed) steps of y, which
        # also turns relative references into step names. References wrapped
        # in try() stay wrapped.
        for (arg in intersect(names(depends), names(params))) {
            params[[arg]] <- formula_set_ref(params[[arg]], depends[[arg]])
        }
        # Fold current parameter values into the defaults of the existing
        # function arguments. `[<-` with list() keeps a NULL value, which
        # `[[<-` would drop together with the argument.
        fml <- formals(fun)
        for (nm in intersect(names(params), setdiff(names(fml), "..."))) {
            fml[nm] <- list(params[[nm]])
        }
        formals(fun) <- fml

        # Update DAG: allocate the node, register the step, add edges to its
        # upstream steps (which are either in `out` or already appended).
        nodeId <- as.integer(dag_add_node(d))
        stepsToNodes[[step]] <- nodeId
        if (length(depends) > 0L) {
            refNodes <- as.integer(unlist(mget(
                unname(depends),
                envir = stepsToNodes,
                inherits = FALSE
            )))
            dag_add_edges_to(d, from = unique(refNodes), to = nodeId)
        }

        row <- .new_step(step, fun, params, depends, nodeId, tags, exec)
        rows[[k]] <- row
    }

    # rbind all appended rows and preserve runtime state of the source steps
    appRows <- data.table::rbindlist(rows, use.names = TRUE)
    data.table::set(
        appRows,
        j = c("out", "state", "time", "time_start", "locked"),
        value = list(
            yyDat[["out"]],
            yyDat[["state"]],
            yyDat[["time"]],
            yyDat[["time_start"]],
            yyDat[["locked"]]
        )
    )
    outEnv[["data"]] <- data.table::rbindlist(
        list(outEnv[["data"]], appRows),
        use.names = TRUE
    )
    out
}

# ---------------------------
# Exported pipeline functions
# ---------------------------

#' Create a pipeline
#'
#' Creates a new, empty pipeline. Add steps with [pip_add()] and execute
#' them with [pip_run()].
#'
#' @param name The name of the pipeline used for display and logging.
#' @return A pipeflow pipeline object.
#'
#' @section Saving pipelines:
#' A pipeline can be saved with [saveRDS()] and read back with [readRDS()]
#' (or serialized in other ways, e.g. to send it to another R process), with
#' its steps, parameters, outputs and states. The dependency graph of the
#' steps is kept outside of R and is not saved, but it is rebuilt from the
#' steps the first time it is needed. Views that are saved in the same object
#' as their pipeline, as in `saveRDS(list(p, v), file)`, are read as views of
#' the read pipeline. A view that is saved on its own is read with its own
#' copy of the pipeline.
#'
#' @examples
#' p <- pip_new("demo") |>
#'     pip_add("numbers", \(n = 5) seq_len(n)) |>
#'     pip_add("squared", \(x = ~numbers) x^2) |>
#'     pip_add("total",   \(x = ~squared) sum(x))
#' p
#' str(p)
#' p[["name"]]  # "demo"
#' p[["view"]]  # initially NULL
#'
#' # Inner pipeline environment (for advanced usage)
#' ls(p[["pipenv"]])                # shows "data"
#' ls(p[["pipenv"]], all = TRUE)    # also shows hidden variables
#'
#' # Save the pipeline and read it back
#' file <- tempfile(fileext = ".rds")
#' saveRDS(p, file)
#' p2 <- readRDS(file)
#' pip_run(p2)
#' p2
#' unlink(file)
#' @export
pip_new <- function(name = "pipe") {
    if (!.is_single(name, "character")) {
        stop("name must be a single string")
    }
    if (is.na(name)) {
        stop("name must not be NA")
    }

    # Main pipeline components live in an inner environment that is shared by
    # reference with all views of this pipeline.
    hash_map <- function() new.env(parent = emptyenv())
    env <- hash_map()
    env[["data"]] <- .empty_pipeline()
    env[[".dag"]] <- dag_new()
    env[[".steps_to_nodes"]] <- hash_map()

    # Pipeline states
    env[[".run_state"]] <- factor(
        .run_states[["ready"]][["name"]],
        levels = names(.run_states)
    )
    env[[".last_run"]] <- NULL

    # Restart tracking
    env[[".restart_count"]] <- 0L
    env[[".restart_force"]] <- TRUE

    # Steps whose stored output comes from their last attempt (see
    # .pip_run_row()), not copied by pip_clone()
    env[[".steps_with_current_out"]] <- hash_map()

    structure(
        list(name = name, view = NULL, pipenv = env),
        class = "pipeflow"
    )
}


#' Add a step
#'
#' Adds a named step to the pipeline. Each step is a function whose parameters
#' either hold constant defaults or reference the output of a prior step using
#' formula notation (`~step_name`). Dependencies are validated when the step
#' is added.
#'
#' @param x A pipeflow pipeline object.
#' @param step Unique step name.
#' @param fun Function to execute for the step. Each function parameter needs
#' a value: a default value in `fun` or an entry in `params`.
#' Default values that are simple constants are resolved immediately.
#' Default values that are formulas like `~other_step` are
#' treated as dependencies to those steps and resolved to the respective output
#' values at runtime once the step is executed. A reference wrapped in `try()`,
#' like `~try(other_step)`, accepts a failed input when the pipeline is run
#' with `on_error = "continue"`: instead of the output of the referenced step,
#' the argument then receives a condition of class `pipeflow_failure` (see
#' section 'Continuing after errors' in [pip_run()]). The function can refer
#' to the running pipeline as `.self` (see section 'The pipeline as `.self`'
#' in [pip_add()]).
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be adjusted later using `[pip_tag()]`.
#' @param after Optional position after which the new step should be inserted
#' (defaults to last position). Can be a step name or an integer index. If
#' set to 0, the new step will be inserted at the beginning of the pipeline.
#' @param params Optional named list of parameter values. They override the
#' defaults of `fun` and give a value to parameters of `fun` that have no
#' default. As in the defaults of `fun`, a formula like `~other_step` refers
#' to the output of another step. Names that are no parameters of `fun` add
#' params to the step, which are passed on to `fun` as well, so `fun` needs a
#' `...` argument for them. Use cases for `params` are:
#' 1. Set param values programmatically, e.g. when adding steps in a loop or
#'   at runtime.
#' 2. Use existing functions whose parameters have no defaults, e.g.
#'   `pip_add(p, "avg", mean, params = list(x = ~data, na.rm = TRUE))`.
#' 3. Provide extra param values to "mark" dependencies that are defined in
#'   pipelines nested in a step, which ensures that it is re-executed when
#'   one of the nested param values change. See the
#'   \href{../doc/v05b-nested-pipeline.html}{nested pipelines vignette}.
#' @param exec Execution mode for this step. One of "auto", "split",
#' "reduce" or "plain".
#' Using execution mode `exec = split`, the output of the step is marked as
#' partitioned output. In this mode, any step that depends on the split step
#' (directly or indirectly) will have its output automatically mapped
#' partition-wise during step execution. The `reduce` mode expects
#' partitioned input and passes it through without mapping, while `plain`
#' mode only accepts non-partitioned input and always intends to execute
#' a single call. In a run with `on_error = "continue"`, a mapped step that
#' fails for some partition keys keeps the outputs of the other keys (see
#' section 'Continuing after errors' in [pip_run()]). If a split step fails,
#' a `reduce` step also accepts the resulting failure object when it takes
#' it via `~try()`. In summary:
#' * auto: map if partitioned input appears, otherwise single call
#' * split: single call, then mark output as partitioned
#' * reduce: single call, but only valid with partitioned input or, via
#'   `~try()`, a failure object
#' * plain: single call, only valid with non-partitioned input
#'
#' @details
#' If `after` was specified, the new step will be inserted after the given
#' step or position. Be aware that in contrast to adding a step at the end,
#' inserting a step in the middle is a rather expensive operation as it
#' requires re-wiring parts of the internal pipeline structure, especially
#' if the new step is inserted at an early position.
#'
#' ## The pipeline as `.self`
#' A step function can refer to the pipeline that is being run as `.self`
#' without declaring it as an argument, for example to set parameters or add
#' steps at runtime, or to call `.self$restart()` or `.self$halt()` (see
#' [pip_run()]). `.self` is always the full pipeline, also when a view is
#' run. pipeflow provides it each time the step runs, but only if the body of
#' the step function contains the name `.self` (also within functions defined
#' in the body). Indirect access, such as `get(".self")`, is not detected.
#' `.self` is a reserved name: it can't be an argument of `fun` or a name in
#' `params`.
#'
#' `.self` is valid while the step runs only. It is released once the step
#' returned, so that outputs of the step don't keep the pipeline alive. A
#' step that returns a function which refers to `.self` therefore fails
#' with `object '.self' not found` when that function is called later.
#'
#' @return The updated pipeline, invisibly.
#' @examples
#' # --- Tags, and view filtering ---
#' p <- pip_new("analysis") |>
#'   pip_add("load", \(n = 5) seq_len(n), tags = c("io", "raw")) |>
#'   pip_add("clean", \(x = ~load) x * 2, tags = c("io", "process")) |>
#'   pip_add("fit", \(x = ~clean) sum(x), tags = c("model", "core", "daily")) |>
#'   pip_add("report", \(x = ~fit) paste("result:", x), tags = "report")
#'
#' pip_run(p)
#' p
#'
#' # Filter by tag using pip_view — keeps steps with any matching tag
#' pip_view(p, tags = "daily")
#' pip_view(p, tags = "core")
#' pip_view(p, tags = c("raw", "report"))
#'
#' # --- Split / reduce execution modes ---
#' q <- pip_new("split-demo") |>
#'   pip_add("data", \(x = iris) x) |>
#'   pip_add("split", \(x = ~data) split(x, x$Species),
#'     exec = "split"
#'   ) |>
#'   pip_add("stats", \(x = ~split) summary(x)) |>
#'   pip_add("combine", \(x = ~stats) do.call(rbind, x),
#'     exec = "reduce"
#'   )
#'
#' pip_run(q)
#' q[["stats", "out"]]   # partitioned list — one summary per species
#' q[["combine", "out"]] # combined table
#'
#' # --- Insert a step at a specific position with 'after' ---
#' p2 <- pip_new("insert-demo") |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#' p2
#' pip_add(p2, "clean", \(x = ~load) x * 2, after = "load")
#' p2  # "load", "clean", "fit" — inserted after "load"
#'
#' # after = 0 inserts the new step at the very beginning
#' pip_add(p2, "preload", \(x = 1) x, after = 0)
#' p2
#'
#' # --- Provide parameter values with 'params' ---
#' # 'params' override the defaults of the function ...
#' p3 <- pip_new("params-demo") |>
#'   pip_add("data", \(x = c(1, NA, 3)) x) |>
#'   pip_add("scale", \(x = ~data, factor = 1) x * factor,
#'     params = list(factor = 10)
#'   ) |>
#'   # ... and give values to parameters without default, e.g. of mean()
#'   pip_add("avg", mean, params = list(x = ~scale, na.rm = TRUE))
#' pip_run(p3)
#' p3
#' p3[["avg", "out"]] # 20
#'
#' # Extra params are passed via `...`
#' p4 <- pip_new("dots-demo") |>
#'   pip_add("load", \(x = 1, ...) c(x, ...), params = list(size = 42))
#' pip_run(p4)
#' p4[["load", "out"]] # c(1, size = 42)
#' @export
pip_add <- function(
    x,
    step,
    fun,
    tags = character(0),
    after = length(x),
    params = list(),
    exec = "auto"
) {
    .assert_pip(x)
    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }
    if (is.na(step)) {
        stop("step must not be NA")
    }
    if (!nzchar(step)) {
        stop("step must be a non-empty string")
    }
    if (.pip_step_exists(x, step)) {
        stop("step '", step, "' already exists in the pipeline")
    }
    .assert_fun_not_pipeline(fun)
    if (!is.function(fun)) {
        stop("fun must be a function")
    }
    if (is.primitive(fun)) {
        stop(
            "fun must not be a primitive function; wrap it, ",
            "e.g. \\(x = 1) sum(x)"
        )
    }
    if (".self" %in% names(formals(fun))) {
        stop_no_call(
            "'.self' is a reserved parameter name and must not be declared ",
            "in step '",
            step,
            "' - it is provided automatically"
        )
    }
    if (".self" %in% names(params)) {
        stop_no_call(
            "'.self' is a reserved parameter name and cannot be used in params"
        )
    }
    .assert_step_params(params)
    .assert_exec_mode(exec)

    n <- length(x)

    if (is.character(after)) {
        if (!.is_single(after, "character") || is.na(after) || !nzchar(after)) {
            stop("after must be a non-empty step name or integer index")
        }
        if (!.pip_step_exists(x, after)) {
            stop("step '", after, "' does not exist")
        }
        # Most of the time the new step is added at the end, so we check that
        # first to avoid the more expensive match() call in the common case.
        pos <- n
        last <- .pip_pipenv(x)[["data"]][["step"]][n]
        if (after != last) {
            pos <- data.table::chmatch(
                after,
                .pip_pipenv(x)[["data"]][["step"]]
            )
        }
    } else if (is.numeric(after)) {
        if (length(after) != 1 || is.na(after)) {
            stop("after must be a non-empty step name or integer index")
        }
        if (!is.finite(after) || after != as.integer(after)) {
            stop("after index must be a whole number")
        }
        pos <- as.integer(after)
        if (pos < 0L || pos > n) {
            stop("after index must be between 0 and ", n)
        }
    } else {
        stop("after must be a non-empty step name or integer index")
    }

    if (pos == n) {
        .pip_append(
            x,
            step = step,
            fun = fun,
            tags = tags,
            exec = exec,
            params = params
        )
    } else {
        .pip_insert(
            x,
            step = step,
            fun = fun,
            tags = tags,
            exec = exec,
            params = params,
            pos = pos
        )
    }
    invisible(x)
}


#' Clone a pipeline
#'
#' Creates an independent copy of the pipeline. Changes to the cloned
#' pipeline do not affect the original pipeline, and vice versa.
#'
#' @param x A pipeflow pipeline object.
#' @param name Optional name for the cloned pipeline. If `NULL`, the
#' original name is used.
#'
#' @return A cloned pipeflow pipeline object.
#' @examples
#' p <- pip_new("original") |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1)
#'
#' # Clone produces a fully independent copy
#' cp <- pip_clone(p, name = "copy")
#' pip_add(cp, "s3", \(x = ~s2) x * 10)
#'
#' # As a result, the clone has the new step ...
#' cp
#'
#' # ... while the original is left unchanged
#' p
#' @export
pip_clone <- function(x, name = NULL) {
    .assert_pip(x)
    if (!is.null(name) && (!.is_single(name, "character") || is.na(name))) {
        stop("name must be a single non-NA string")
    }

    newName <- if (is.null(name)) x[["name"]] else name
    .pip_clone(x, name = newName)
}


#' Collect step outputs
#'
#' Returns the outputs of the steps in a pipeline or view. With
#' `by = "step"` (the default) the result is a named list of step outputs.
#' With any other column, the outputs are grouped by the values of that
#' column (typically `"tags"`).
#'
#' The result always takes one of two shapes:
#'
#' * **Flat**: a named list whose elements are the step outputs directly,
#'   e.g. `list(s1 = 1, s2 = 2)`.
#' * **Grouped**: a named list whose elements are themselves named lists of
#'   step outputs, one per group, e.g.
#'   `list(io = list(s1 = 1, s2 = 2), model = list(s3 = 4))`.
#'
#' With `simplify = TRUE` (the default) the result is *flat* whenever every
#' group contains exactly one step, which, for example, is always the case
#' for `by = "step"` as step names are unique.
#' If any group contains more than one step (or `simplify = FALSE`) the
#' result is *grouped*. For list columns such as `tags`, a step with
#' several entries contributes its output to every
#' corresponding group, and steps without an entry (e.g. untagged steps)
#' are omitted.
#' You can use [pip_view()] to further narrow the selection before collecting.
#'
#' @param x A pipeflow pip or view.
#' @param by Single step-table column name to group by.
#' @param as.table If TRUE, return a `data.table` instead of a named list.
#' @param simplify If TRUE (default), if the list of collected outputs
#' contains exactly one step per group, the result is flattened by one level,
#' otherwise it is returned as a grouped list.
#' @return A named list of outputs
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x, tags = "io") |>
#'   pip_add("clean", \(x = ~load) x + 1, tags = "io") |>
#'   pip_add("model", \(x = ~clean) x * 2, tags = "model")
#' pip_run(p)
#'
#' # By default, a flat named list with one entry per step
#' pip_collect(p)
#'
#' # The same output as a data.table
#' pip_collect(p, as.table = TRUE)
#'
#' # Group the outputs by tag ...
#' pip_collect(p, by = "tags")
#'
#' # ... which is equivalent to
#' list(
#'   io = pip_view(p, tags = "io") |> pip_collect(),
#'   model = pip_view(p, tags = "model") |> pip_collect()
#' )
#'
#' # Grouped table output
#' pip_collect(p, by = "tags", as.table = TRUE)
#'
#' # Keep single-step groups nested
#' pip_collect(p, simplify = FALSE)
#'
#' # Collect output from a view
#' v <- p[step %in% c("clean", "model"), ]
#' pip_collect(v)
#' pip_collect(v, as.table = TRUE)
#' @export
pip_collect <- function(x, by = "step", as.table = FALSE, simplify = TRUE) {
    .assert_pip_or_view(x)
    if (!.is_single(as.table, "logical") || is.na(as.table)) {
        stop("as.table must be a single logical value")
    }
    if (!.is_single(simplify, "logical") || is.na(simplify)) {
        stop("simplify must be a single logical value")
    }
    if (is.null(by)) {
        by <- "step"
    }
    if (!.is_single(by, "character") || is.na(by) || !nzchar(by)) {
        stop("by must be a single column name")
    }

    dat <- .pip_view_data(x)

    if (!(by %in% colnames(dat))) {
        stop("unknown column: ", by)
    }
    # fmt: skip
    allowedBy <- c(
        "step", "state", "tags", "time", "time_start", "locked", "exec",
        "depends", "unbound", "nodeId"
    )
    if (!by %in% allowedBy) {
        stop(sprintf(
            "'%s' cannot be used as a grouping column - must be one of %s",
            by,
            toString(allowedBy)
        ))
    }

    byCol <- dat[[by]]
    steps <- dat[["step"]]
    outs <- dat[["out"]]

    emptyResult <- function() {
        if (!as.table) {
            return(list())
        }
        key0 <- if (is.list(byCol)) character(0) else byCol[0]
        tbl <- data.table::data.table(grp = key0, out = vector("list", 0L))
        data.table::setnames(tbl, new = c(by, "out"))
        tbl
    }

    if (nrow(dat) == 0L) {
        return(emptyResult())
    }

    tab <- if (is.list(byCol)) {
        # For list columns (like tags) the table is expanded by duplicating
        # rows with multiple entries so that each row contains one entry.
        # For example, tags = c("a", "b") will produce two rows, one for
        # tags = "a" and one for tags = "b".
        keysAll <- byCol
        lens <- lengths(keysAll)
        has <- which(lens > 0L)
        idx <- rep(has, lens[has])
        grp <- if (length(has) > 0L) {
            unlist(keysAll[has], use.names = FALSE)
        } else {
            character(0)
        }
        data.table::data.table(grp = grp, step = steps[idx], out = outs[idx])
    } else {
        # Scalar columns (like step) can stay as-is.
        data.table::data.table(grp = byCol, step = steps, out = outs)
    }

    tab <- tab[!is.na(grp)]
    if (nrow(tab) == 0L) {
        return(emptyResult())
    }

    # Delegate the actual grouping to data.table
    . <- out <- step <- NULL # silence R CMD check
    res <- tab[, .(collect = list(stats::setNames(out, step))), by = grp]

    # Keep the first-appearance order of the groups.
    groupNames <- res[["grp"]]
    first <- match(unique(tab[["grp"]]), groupNames)
    res <- res[first]
    collected <- res[["collect"]]

    # If all groups have a single element ...
    if (simplify && all(lengths(collected) == 1L)) {
        # ... list(grpA = list(x = 1), grpB = list(y = 2)) can be flattened
        # to  list(grpA = 1, grpB = 2)
        collected <- unlist1(collected)
    }

    if (as.table) {
        tbl <- data.table::data.table(grp = groupNames, out = collected)
        data.table::setnames(tbl, new = c(by, "out"))
        tbl
    } else {
        stats::setNames(collected, nm = groupNames)
    }
}


#' @rdname pip_collect
#' @section Lifecycle: Deprecated
#'
#' `pip_collect_out()` is a legacy alias for [pip_collect()]. It raises a
#' deprecation warning and will be removed in a future release.
#' @export
pip_collect_out <- function(
    x,
    by = "step",
    as.table = FALSE,
    simplify = TRUE
) {
    .Deprecated(
        new = "pip_collect",
        old = "pip_collect_out",
        package = "pipeflow"
    )
    pip_collect(x, by = by, as.table = as.table, simplify = simplify)
}


#' Access the underlying step table
#'
#' This is a convenience wrapper for accessing the internal `data.table`,
#' which normally is reachable via `p[["pipenv"]][["data"]]`.
#'
#' @details The internal `data.table` is holding the pipeline steps, one row
#' per step. Unless you know what you are doing, this table should not be
#' modified directly, as this can corrupt the pipeline including any views
#' that are derived from it. If you want to experiment, consider cloning the
#' pipeline first with [pip_clone()].
#'
#' @param x A pipeflow pipeline or view.
#' @return The underlying step table as a `data.table`.
#' @export
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 5) seq_len(n)) |>
#'   pip_add("model", \(x = ~load) sum(x))
#'
#' pip_data(p)
pip_data <- function(x) {
    .assert_pip_or_view(x)
    .pip_pipenv(x)[["data"]]
}


#' @name pip_set_params
#' @rdname pip_set_params
#' @export
pip_get_params <- function(x) {
    .assert_pip_or_view(x)
    dat <- .pip_view_data(x)

    params <- mapply(
        par = dat[["params"]],
        unbound = dat[["unbound"]],
        FUN = \(par, unbound) par[unbound],
        SIMPLIFY = FALSE
    ) |>
        Filter(f = \(x) length(x) > 0)

    parNames <- unlist(sapply(params, FUN = names))
    parValues <- stats::setNames(unlist1(params), parNames)
    as.list(parValues[!duplicated(names(parValues))])
}


#' Build pipeline graph data
#'
#' Builds graph data (nodes and edges) describing the pipeline's step
#' structure, suitable for visualisation with [visNetwork::visNetwork()].
#'
#' @details
#' Node shapes reflect execution mode:
#' * `auto`/`plain`: `hexagon`
#' * `reduce`: `dot`
#' * `split`: `star`
#'
#' @param x A pipeflow pip or view.
#' @param include_upstream Logical. Only relevant for views. If `TRUE`, add
#' all upstream dependencies of selected steps.
#'
#' @return A named list with two `data.frame`s: `nodes` and `edges`.
#' @export
#' @examples
#' p <- pip_new()
#' pip_add(p, "load", \(x = 1) x, tags = "io")
#' pip_add(p, "clean", \(x = ~load) x + 1, tags = "io")
#' pip_add(p, "fit", \(x = ~clean) x * 2, tags = "model")
#'
#' graph <- pip_graph(p)
#' graph$nodes # data.frame: id, label, shape, color
#' graph$edges # data.frame: from, to, arrows
#'
#' # For a view, include_upstream = TRUE adds upstream deps to the graph
#' v <- pip_view(p, step = "fit")
#' pip_graph(v, include_upstream = TRUE)
#'
#' if (require("visNetwork", quietly = TRUE)) {
#'   do.call(what = visNetwork::visNetwork, args = graph)
#' }
pip_graph <- function(x, include_upstream = FALSE) {
    .assert_pip_or_view(x)
    if (!.is_single(include_upstream, "logical")) {
        stop("include_upstream must be a single logical value")
    }

    isView <- .is_pipeflow_view(x)
    env <- .pip_pipenv(x)
    dat <- env[["data"]]

    rows <- .pip_view_rows(x)
    rows <- sort(unique(rows))

    if (isView && include_upstream && length(rows) > 0L) {
        startNodes <- dat[["nodeId"]][rows]
        keepNodes <- dag_get_reachable_nodes_up(
            .pip_dag(env),
            as.integer(unique(startNodes))
        )
        rows <- which(dat[["nodeId"]] %in% keepNodes)
        rows <- sort(unique(as.integer(rows)))
    }

    sub <- dat[rows]

    # Nodes
    colors <- vapply(
        sub[["state"]],
        FUN = \(st) .step_states[[st]][["color"]],
        FUN.VALUE = character(1)
    )

    ids <- as.integer(sub[["nodeId"]])
    shape <- rep("hexagon", nrow(sub))
    if ("exec" %in% names(sub)) {
        shape[sub[["exec"]] %in% "split"] <- "star"
        shape[sub[["exec"]] %in% "reduce"] <- "dot"
    }
    nodes <- data.frame(
        id = ids,
        label = sub[["step"]],
        shape = shape,
        color = colors
    )

    # Edges from direct dependencies only (no transitive links).
    stepToId <- stats::setNames(ids, sub[["step"]])
    edgeRows <- lapply(seq_len(nrow(sub)), FUN = \(k) {
        depSteps <- unname(sub[["depends"]][[k]])
        if (length(depSteps) == 0L) {
            return(NULL)
        }

        fromIds <- as.integer(stepToId[depSteps])
        fromIds <- fromIds[!is.na(fromIds)]
        if (length(fromIds) == 0L) {
            return(NULL)
        }

        data.frame(
            from = fromIds,
            to = rep.int(ids[[k]], length(fromIds)),
            stringsAsFactors = FALSE
        )
    })
    edgeRows <- Filter(f = Negate(is.null), x = edgeRows)

    edges <- if (length(edgeRows) == 0L) {
        data.frame(
            from = integer(0),
            to = integer(0),
            arrows = character(0),
            stringsAsFactors = FALSE
        )
    } else {
        edges <- do.call(what = rbind, args = edgeRows)
        edges <- unique(edges)
        cbind(edges, "arrows" = "to")
    }

    list(nodes = nodes, edges = edges)
}


#' @rdname pip_graph
#' @section Lifecycle: Deprecated
#'
#' `pip_get_graph()` is a legacy alias for [pip_graph()]. It raises a
#' deprecation warning and will be removed in a future release.
#' @export
pip_get_graph <- function(x, include_upstream = FALSE) {
    .Deprecated(new = "pip_graph", old = "pip_get_graph", package = "pipeflow")
    pip_graph(x, include_upstream = include_upstream)
}


#' Remove a step
#'
#' Removes a pipeline step by its name. If other steps depend on it,
#' an error is given and the removal of the step is blocked, unless
#' `force` was set to `TRUE`, which will remove the selected step
#' together with all its downstream dependent steps.
#'
#' @note
#' If called on a view, the step to be removed must be part of the view.
#' The view, as well as any other view on the same pipeline, stays valid:
#' the removed steps just drop out of it.
#'
#' @param x A pipeflow pip or view
#' @param step `string` the name of the step to be removed.
#' @param force `logical` if `TRUE` the step is removed together
#' with all its downstream dependencies.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("transform", \(x = ~load) x * 2) |>
#'   pip_add("model", \(x = ~transform) x + 10)
#'
#' # Removing a leaf step (nothing depends on it) works directly
#' pip_remove(p, "model")
#' p                        # "load", "transform"
#'
#' # Trying to remove a step that others depend on raises an error:
#' # pip_remove(p, "load")  # Error!
#'
#' # If a view is passed, the step must be part of the view.
#' v <- pip_view(p, step = "transform")
#' try(pip_remove(v, "load"))  # Error: "load" is not part of the view
#'
#' # Views stay valid, the removed steps just drop out of them
#' v <- pip_view(p, step = c("load", "transform"))
#' pip_remove(v, "transform")
#' v[["step"]]              # "load"
#' p                        # "load"
#'
#' # force = TRUE removes the step and all its downstream dependents
#' pip_remove(p, "load", force = TRUE)
#' p                        # pipeline is now empty
#' @export
pip_remove <- function(x, step, force = FALSE) {
    .assert_pip_or_view(x)
    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }
    if (is.na(step)) {
        stop("step must not be NA")
    }
    if (!.pip_step_exists(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!.pip_step_in_view(x, step)) {
        stop("step '", step, "' is not part of the view")
    }
    if (!is.logical(force) || length(force) != 1L || is.na(force)) {
        stop("force must be a single logical value")
    }

    pipenv <- .pip_pipenv(x)
    dat <- pipenv[["data"]]
    `%chin%` <- data.table::`%chin%`

    directDeps <- dat[["step"]][
        vapply(
            dat[["depends"]],
            FUN = \(dep) step %chin% dep,
            FUN.VALUE = logical(1)
        )
    ]

    if (length(directDeps) > 0L && !force) {
        stepsString <- paste0("'", directDeps, "'", collapse = ", ")
        stop(
            "cannot remove step '",
            step,
            "' because the following steps depend on it: ",
            stepsString
        )
    }

    stepsToRemove <- step
    if (force) {
        downNodes <- .pip_get_reachable_nodes(x, step)
        downNodes <- as.integer(downNodes)
        stepNode <- as.integer(.pip_steps_to_nodes(x, step)[[1]])

        downDeps <- dat[["step"]][
            dat[["nodeId"]] %in% setdiff(downNodes, stepNode)
        ]
        if (length(downDeps) > 0L) {
            stepsString <- paste0("'", downDeps, "'", collapse = ", ")
            message(
                "Removing step '",
                step,
                "' and its downstream dependencies: ",
                stepsString
            )
        }

        stepsToRemove <- dat[["step"]][dat[["nodeId"]] %in% downNodes]
    }

    nodesToRemove <- as.integer(unname(unlist(
        .pip_steps_to_nodes(x, stepsToRemove)
    )))

    # Remove DAG nodes first to keep node references stable during filtering.
    d <- .pip_dag(pipenv)
    for (nid in rev(nodesToRemove)) {
        ok <- dag_remove_node(d, nid, force = force)
        if (!ok) {
            stop("failed to remove node ", nid, " from DAG")
        }
    }

    dag_tidy_up(d)
    keep <- !(dat[["step"]] %chin% stepsToRemove)
    pipenv[["data"]] <- dat[keep]
    suppressWarnings(
        rm(
            list = stepsToRemove,
            envir = pipenv[[".steps_to_nodes"]],
            inherits = FALSE
        )
    )

    data.table::setindexv(pipenv[["data"]], list("step", "nodeId"))
    invisible(x)
}


#' Rename a step
#'
#' Renames the selected step and updates dependency references in
#' downstream steps.
#' @param x A pipeflow pip or view
#' @param from Existing step name
#' @param to New step name
#' @return The updated pipeline, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1)           # "s2" depends on "s1"
#'
#' # Downstream dependency references are updated automatically
#' pip_rename(p, from = "s1", to = "load_data")
#' p
#'
#' # Trying to rename to an existing step name raises an error:
#' try(pip_rename(p, "load_data", to = "s2"))  # step 's2' already exists!
#'
#' # If a view is passed, the step must be part of the view
#' v <- pip_view(p, step = c("load_data", "s2"))
#' pip_rename(v, from = "load_data", to = "input")
#' p[["step"]]                                 # "input", "s2"

#' v2 <- pip_view(p, step = "s2")
#' try(pip_rename(v2, from = "input", to = "data"))
#' @export
pip_rename <- function(x, from, to) {
    .assert_pip_or_view(x)

    if (!.is_single(from, "character")) {
        stop("from must be a single string")
    }
    if (is.na(from)) {
        stop("from must not be NA")
    }
    if (!nzchar(from)) {
        stop("from must be a non-empty string")
    }

    if (!.is_single(to, "character")) {
        stop("to must be a single string")
    }
    if (is.na(to)) {
        stop("to must not be NA")
    }
    if (!nzchar(to)) {
        stop("to must be a non-empty string")
    }

    if (!.pip_step_exists(x, from)) {
        stop("step '", from, "' does not exist")
    }
    if (!.pip_step_in_view(x, from)) {
        stop("step '", from, "' is not part of the view")
    }
    if (identical(from, to)) {
        return(invisible(x))
    }
    if (.pip_step_exists(x, to)) {
        stop("step '", to, "' already exists")
    }

    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    `%chin%` <- data.table::`%chin%`
    newSteps <- dat[["step"]]
    newSteps[newSteps %chin% from] <- to

    newDepends <- lapply(
        dat[["depends"]],
        FUN = \(dep) {
            if (length(dep) == 0L) {
                return(dep)
            }
            dep[dep %chin% from] <- to
            dep
        }
    )

    # Keep the formula references in params in sync with the renamed steps.
    # The formulas are informational (step resolution uses `depends`), but
    # they are user visible (e.g. when printing the params column).
    newParams <- dat[["params"]]
    for (i in seq_along(newParams)) {
        dep <- newDepends[[i]]
        if (length(dep) == 0L) {
            next
        }
        for (arg in names(dep)) {
            fml <- newParams[[i]][[arg]]
            if (inherits(fml, "formula")) {
                newParams[[i]][[arg]] <- formula_set_ref(fml, dep[[arg]])
            }
        }
    }

    data.table::set(dat, j = "step", value = newSteps)
    data.table::set(dat, j = "depends", value = newDepends)
    data.table::set(dat, j = "params", value = list(newParams))

    # The failures stored for the failed keys of mapped steps name the steps
    # they come from.
    for (i in seq_len(nrow(dat))) {
        out <- dat[["out"]][[i]]
        if (length(.pip_failed_keys(out)) > 0L) {
            out <- .pip_rename_failures(out, from = from, to = to)
            data.table::set(dat, i = i, j = "out", value = list(list(out)))
        }
    }

    stepsToNodes <- env[[".steps_to_nodes"]]
    nodeId <- stepsToNodes[[from]]
    stepsToNodes[[to]] <- nodeId
    rm(list = from, envir = stepsToNodes, inherits = FALSE)

    # Whether the stored output comes from the step's last attempt (see
    # .pip_run_row()) does not change with its name. The new name may still
    # carry a mark of a removed step, which is overwritten (also with NULL).
    currentOut <- env[[".steps_with_current_out"]]
    currentOut[[to]] <- currentOut[[from]]
    currentOut[[from]] <- NULL

    # The kept inner pipelines of a pipeline function are found by step name
    .fun_rename(dat[["fun"]][[match(to, newSteps)]], from = from, to = to)

    data.table::setindexv(dat, list("step", "nodeId"))
    invisible(x)
}


#' Replace a step
#'
#' Replaces a step's function while keeping it in the same position in the
#' pipeline. Downstream steps that have run before are automatically marked as
#' outdated and will re-run on the next [pip_run()].
#'
#' By default, the step keeps its tags and execution mode: `tags = NULL` and
#' `exec = NULL` mean "keep the current value". To change them, pass explicit
#' values, where `tags = character(0)` removes all tags and `exec = "auto"`
#' resets the execution mode to its default. Parameters, output, state and
#' lock status are always reset, like for a freshly added step.
#'
#' @inheritParams pip_add
#' @param x A pipeflow pipeline or view object.
#' @param tags Optional character vector of tags belonging to the step. If
#' `NULL` (the default), the current tags of the step are kept. Use
#' `character(0)` to remove all tags.
#' @param exec Execution mode for the step, see [pip_add()]. If `NULL` (the
#' default), the current execution mode of the step is kept.
#' @return The updated pipeline, invisibly.
#' @examples
#' p <- pip_new() |>
#'     pip_add("load", \(n = 5) seq_len(n)) |>
#'     pip_add("double", \(x = ~load) x * 2)
#' pip_run(p)
#' p
#'
#' # Replace "load" — downstream steps are automatically marked "outdated"
#' pip_replace(p, "load", \(n = 3) seq_len(n))
#' p
#'
#' # Re-run to bring everything up to date
#' pip_run(p)
#' p
#'
#' # If a view is passed, the step must be part of the view
#' v <- pip_view(p, step = "double")
#' pip_replace(v, "double", \(x = ~load) x * 3)
#' p[["state"]]                                # "double" is "new" again
#'
#' try(pip_replace(v, "load", \(n = 2) seq_len(n)))
#'
#' # Tags and execution mode are kept unless set explicitly
#' pip_tag(p, "io")
#' pip_replace(p, "load", \(n = 4) seq_len(n))
#' p[, "tags"]
#' pip_replace(p, "load", \(n = 4) seq_len(n), tags = character(0))
#' p[, "tags"]
#' @export
pip_replace <- function(
    x,
    step,
    fun,
    tags = NULL,
    params = list(),
    exec = NULL
) {
    .assert_pip_or_view(x)
    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }
    if (is.na(step)) {
        stop("step must not be NA")
    }
    if (!nzchar(step)) {
        stop("step must be a non-empty string")
    }
    if (!.pip_step_exists(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!.pip_step_in_view(x, step)) {
        stop("step '", step, "' is not part of the view")
    }
    .assert_fun_not_pipeline(fun)
    if (!is.function(fun)) {
        stop("fun must be a function")
    }
    if (is.primitive(fun)) {
        stop(
            "fun must not be a primitive function; wrap it, ",
            "e.g. \\(x = 1) sum(x)"
        )
    }
    if (".self" %in% names(formals(fun))) {
        stop_no_call(
            "'.self' is a reserved parameter name and must not be declared ",
            "in step '",
            step,
            "' - it is provided automatically"
        )
    }
    if (".self" %in% names(params)) {
        stop_no_call(
            "'.self' is a reserved parameter name and must not be set via ",
            "params - it is provided automatically"
        )
    }
    .assert_step_params(params)
    if (!is.null(exec)) {
        .assert_exec_mode(exec)
    }

    env <- .pip_pipenv(x)
    data <- env[["data"]]
    iStep <- data.table::chmatch(step, data[["step"]])

    # NULL means "keep the current value"
    if (is.null(tags)) {
        tags <- data[["tags"]][[iStep]]
    }
    if (is.null(exec)) {
        exec <- data[["exec"]][[iStep]]
    }

    params <- .extract_fun_params(fun, params)

    # Dependencies of the replacement may only point to earlier steps
    prefixSteps <- data[["step"]][seq_len(iStep - 1L)]
    steps <- c(prefixSteps, step)
    depends <- .extract_depends(params = params, steps = steps)
    if (length(depends) > 0L) {
        bad <- unname(depends[!(depends %in% prefixSteps)])
        # As in .pip_insert(), but a reference to the step itself is unknown
        later <- bad[bad %in% data[["step"]][-seq_len(iStep)]]
        if (length(later) > 0L) {
            stop_no_call(
                "while replacing step '",
                step,
                "' - cannot reference steps that come after the ",
                "replaced step: ",
                paste0("'", later, "'", collapse = ", ")
            )
        }
        if (length(bad) > 0L) {
            stop_no_call(
                "while replacing step '",
                step,
                "' - cannot reference unknown steps: ",
                paste0("'", bad, "'", collapse = ", ")
            )
        }
    }

    refNodes <- mget(
        depends,
        envir = env[[".steps_to_nodes"]],
        inherits = FALSE
    )

    # Update the incoming DAG edges of the replaced step (its node and all
    # outgoing edges stay the same).
    d <- .pip_dag(env)
    nodeId <- env[[".steps_to_nodes"]][[step]]
    oldDeps <- data[["depends"]][[iStep]]
    oldRefs <- if (length(oldDeps) > 0L) {
        as.integer(unlist(mget(
            unname(oldDeps),
            envir = env[[".steps_to_nodes"]],
            inherits = FALSE
        )))
    } else {
        integer()
    }
    newRefs <- as.integer(refNodes)

    for (from in setdiff(oldRefs, newRefs)) {
        dag_remove_edge(d, from = from, to = nodeId, force = TRUE)
    }
    toAdd <- setdiff(newRefs, oldRefs)
    if (length(toAdd) > 0L) {
        dag_add_edges_to(d, from = toAdd, to = nodeId)
    }

    # Reset the step row: new function and params, fresh runtime state (like
    # a freshly added step), but with the given or kept tags and exec.
    values <- list(
        "fun" = list(fun),
        "params" = list(params),
        "out" = list(NULL),
        "state" = .step_states[["new"]][["name"]],
        "tags" = list(tags),
        "locked" = FALSE,
        "exec" = exec,
        "time" = Sys.time(),
        "time_start" = as.POSIXct(NA),
        "depends" = list(depends),
        "unbound" = list(setdiff(names(params), names(depends)))
    )
    data.table::set(data, i = iStep, j = names(values), value = values)

    # Mark downstream dependent steps as outdated (the replaced step itself
    # is "new" and therefore stays "new").
    .pip_outdate_downstream(x, steps = step)

    invisible(x)
}


#' Run a pipeline
#'
#' Executes all pending steps in order. On repeated runs, steps that are
#' already `"done"` are skipped and only steps that are still `"new"` or were
#' marked `"outdated"` (because one of their dependencies changed) are
#' executed. Use `force = TRUE` to re-execute every step.
#'
#' @param x A pipeflow pip or view
#' @param lgr A logging function with the arguments `level` (`"info"`,
#' `"warn"` or `"error"`) and `msg`, which is called as
#' `lgr(level = level, msg = msg)`. The default is [pipeflow_lgr()], which
#' can also be used to build custom loggers. To suppress logging, you can set
#' `lgr = NULL`.
#' @param force Logical indicating if all steps should be forced to run,
#' regardless of whether they are outdated or not.
#' @param progress Optional callback of the form
#' `function(value, detail)` called before each step.
#' @param on_error What to do when a step fails: `"stop"` (the default)
#' aborts the run, `"continue"` goes on with all steps that do not depend on
#' a failed step (see section 'Continuing after errors' below).
#' @return The updated pipeline or view, invisibly.
#'
#' @details
#'
#' ## Step states
#'
#' A step can take the following states:
#'
#' * new: the step has been added but not yet executed, or was reset via
#'   [pip_reset()].
#' * outdated: a step can get outdated in the following scenarios:
#'   - one of the step's parameters were changed (e.g. via [pip_set_params()])
#'   - the step's execution mode was changed (`p[[step, "exec"]] <- mode`)
#'   - a step it depends on was re-executed or replaced, got a new
#'     execution mode, or its output was set (`p[[step, "out"]] <- value`)
#'   - a step it depends on was executed, but the run did not reach the step
#'     itself, either on purpose (see section 'Running views' below) or
#'     because the run was aborted early due to a failed step
#' * done: the step was executed successfully with its current inputs
#' * failed: the step raised an error during its last execution
#'
#' Only steps that have run before can become outdated: a step in state
#' `"new"` stays `"new"` until it is executed.
#' A "done" step is skipped unless `force = TRUE` was set. For all other
#' states, the step will be re-executed in the next [pip_run()].
#'
#' ## Time stamps
#'
#' The step table has two time stamps per step:
#'
#' * `time_start`: the start of the step's last run. It is set each time the
#'   step is run, also if the run fails. It is `NA` for a step that has not
#'   run since it was added, replaced (see [pip_replace()]) or reset (see
#'   [pip_reset()]). A skipped step (because it is `"done"`, locked or has a
#'   failed input) keeps its value.
#' * `time`: the time the step was added or replaced, or the end of its last
#'   successful run. A failed run does not change it. It is the time stamp
#'   that [pip_output_value()] gets in `ctx`.
#'
#' With `t0 <- Sys.time()` taken before `pip_run()`, a step was run in that
#' run if `time_start >= t0`. Its last run succeeded if its state is
#' `"done"` and `time >= time_start`, and then `time - time_start` is the
#' duration of that run.
#'
#' ## Running views
#' When `x` is a view, the requested rows are run together with their
#' upstream dependencies, so the steps covered by the view are brought up to
#' date even if their inputs come from steps outside the view. The rest of
#' the pipeline is not executed; downstream steps of executed steps are
#' marked `"outdated"`. Steps whose inputs were not re-executed (for example,
#' because they were already `"done"`) keep their state.
#'
#' ## Runtime errors
#' By default, a failing step stops the run: the step gets state `"failed"`,
#' the pipeline run state becomes `"failed"`, and steps after it that were
#' not reached are marked `"outdated"`, so the next run retries them.
#'
#' The error message names the step, and for a step mapped over partitions
#' (see `exec` in [pip_add()]) also the partition key, e.g.
#' `"step 'fit': key 'b': negative"`. The error has class
#' `pipeflow_step_error` and keeps the original error in its field
#' `parent`:
#' `tryCatch(pip_run(p), pipeflow_step_error = function(e) e$parent)`.
#'
#' ## Continuing after errors
#' With `on_error = "continue"`, the run does not stop at a failed step:
#'
#' * The failed step gets state `"failed"`, and its error is stored as its
#'   output.
#' * Steps that depend on a failed step are not run, and neither are their
#'   own dependents. All other steps are run.
#' * To run a step despite a failed input, refer to that input via `try()`,
#'   e.g. `x = ~try(fit)` (see [pip_add()]). The argument then receives a
#'   failure object (an error condition) whose message tells which step
#'   failed, e.g. `"step 'fit' failed: negative"`.
#' * Locked steps are never run, so a failed input does not affect them.
#'
#' A step mapped over partitions fails only for the keys whose call failed:
#'
#' * Its output keeps the results of the other keys and holds a failure
#'   object for each failed key, e.g. `"step 'fit' failed: key 'b':
#'   negative"`. The step gets state `"failed"`, and printing the pipeline
#'   shows its output as e.g. `<partitioned: 1/3 failed>`.
#' * A mapped step after it is not run for the failed keys, whose output is
#'   then a failure object too (`"step 'show' was not run: step 'fit'
#'   failed: key 'b': negative"`), unless it takes the input via `try()`.
#' * A step that is not mapped (e.g. `exec = "reduce"`) is not run, unless
#'   it takes the input via `try()`. Then it receives the results of all
#'   keys, including the failure objects.
#'
#' At the end of the run, the pipeline run state is `"continued"`, and a
#' warning of class `pipeflow_run_failed` lists the failed steps (with the
#' number of failed keys) and the steps that were not run.
#'
#' The next run retries the failed steps and the steps that were not run;
#' once a failed step succeeds, the steps after it, including those that
#' received failure objects, are run again. A mapped step is only run again
#' for its failed keys, keeping the results of the other keys, unless
#' something changed that affects all keys: its parameters, its execution
#' mode or an input step (which marks it `"outdated"`), an input step that is
#' run again in the same run, or `force = TRUE`. See
#' `vignette("v05c-error-handling", package = "pipeflow")` for examples.
#'
#' ## Runtime control flow via restart and halt
#'
#' A running pipeline can be interrupted via the `restart()` and `halt()`
#' virtual methods. They are intended for advanced, self-modifying pipelines
#' and are most often called from within a step function via `.self` (see
#' section 'The pipeline as `.self`' in [pip_add()]).
#'
#' - `.self$restart(force = TRUE, times = 1L)`: aborts the current run after the
#'   current step has finished and restarts it from the first step.
#'   The default parameters are `force = TRUE` and `times = 1L`, that is, the
#'   above call is the same as just invoking .self$restart().
#'   To skip steps that are already in state `"done"`, set `force = FALSE`.
#'   The `times` parameter limits the number of consecutive restarts within a
#'   single `pip_run()` call.
#'   If a view is being run, a restart covers the view steps together with
#'   their upstream dependencies.
#' - `p$halt()`: aborts the current run after the current step has finished.
#'   This is a *controlled halt* and deliberately distinct from [base::stop()]:
#'   no error is raised and the pipeline is not marked as `"failed"`. The run
#'   simply ends with run state `"halted"` (see [pip_run_state()]) and a
#'   subsequent [pip_run()] will continue where the run left off.
#'
#' In both cases steps downstream of executed steps that have not been
#' executed until the restart or halt happens are marked as `"outdated"`.
#'
#' @seealso `vignette("v06-self-modify-pipeline", package = "pipeflow")`
#'   for an advanced example of dynamic pipelines.
#' @examples
#' p <- pip_new() |>
#' pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("prep", \(x = ~load, weight = 1) x * weight) |>
#'   pip_add("square", \(x = ~prep) x^2) |>
#'   pip_add("total", \(x = ~square) sum(x))
#'
#' pip_run(p)
#' p
#'
#' pip_set_params(p, list(weight = 2))
#' p
#'
#' # Already-done steps are skipped on a second run
#' pip_run(p) # first step skipped
#'
#' # lgr = NULL suppresses log output
#' pip_run(p, lgr = NULL)
#'
#' # force = TRUE re-executes every step regardless of state
#' pip_run(p, force = TRUE)
#' p
#'
#' # Run only a subset of steps via a view;
#' # upstream dependencies are automatically included
#' v <- pip_view(p, step = "total")
#' pip_run(v)
#'
#' # Halt or restart pipeline at runtime (for advanced usage)
#' p <- pip_new("restart") |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("check", \(x = ~load) {
#'     if (length(x) > 10L) .self$halt()
#'   }) |>
#'   pip_add("model", \(x = ~load) {
#'     if (length(x) == 3L) {
#'       .self$set_params(list(n = 5))
#'       .self$restart()
#'     }
#'     x * 2
#'   })
#'
#' pip_run(p)
#' p
#'
#' pip_set_params(p, list(n = 15)) # now halt() in 'check' step is triggered
#' pip_run(p)
#'
#' # Continue after errors and pass failures to the steps that allow them
#' p <- pip_new("continue") |>
#'   pip_add("data", \(n = 5) seq_len(n)) |>
#'   pip_add("check", \(x = ~data) stop("check failed")) |>
#'   pip_add("count", \(x = ~check) length(x)) |>
#'   pip_add("total", \(x = ~data) sum(x)) |>
#'   pip_add("report", \(total = ~total, n = ~try(count)) {
#'     if (inherits(n, "pipeflow_failure")) conditionMessage(n) else n
#'   })
#'
#' withCallingHandlers(
#'   pip_run(p, lgr = NULL, on_error = "continue"),
#'   pipeflow_run_failed = \(w) {
#'     message("failed: ", toString(names(w$failed)))
#'     invokeRestart("muffleWarning")
#'   }
#' )
#' p
#' p[["report", "out"]]
#' @export
pip_run <- function(
    x,
    lgr = pipeflow_lgr,
    force = FALSE,
    progress = NULL,
    on_error = c("stop", "continue")
) {
    .assert_pip_or_view(x)
    if (!.is_single(force, "logical")) {
        stop("force must be a single logical value")
    }
    onError <- match.arg(on_error)
    if (!is.null(progress) && !is.function(progress)) {
        stop("progress must be a function")
    }
    if (is.null(lgr)) {
        lgr <- function(level, msg, ...) {}
    } else {
        .assert_logger(lgr)
    }
    log_info <- function(msg) lgr(level = "info", msg = msg)

    isView <- .is_pipeflow_view(x)
    pipenv <- .pip_pipenv(x)
    pipname <- x[["name"]]
    dat <- pipenv[["data"]]
    rowsToRun <- seq_len(nrow(dat))

    if (isView) {
        requested <- .pip_view_rows(x)
        reqSteps <- dat[["step"]][requested]
        upNodes <- .pip_get_reachable_nodes(
            x,
            reqSteps,
            downstream = FALSE
        )
        upRows <- as.integer(dat[list(upNodes), which = TRUE, on = "nodeId"])
        rowsToRun <- as.integer(sort(unique(c(requested, upRows))))
        upstreamRows <- setdiff(rowsToRun, requested)
        names(rowsToRun)[match(requested, rowsToRun)] <- "view"
        names(rowsToRun)[match(upstreamRows, rowsToRun)] <- "upstream"
    }
    # Mark the steps downstream of the executed steps as outdated if they were
    # not executed themselves, which can happen in two different ways:
    # a) when running a view that does not cover the entire pipeline or
    # b) the run was aborted in the middle (due to an error or manual halt).
    # Steps downstream of steps that were only skipped keep their state, as
    # do locked steps.
    executedSteps <- character()

    # Failure objects of the steps that failed or were not run because an
    # input failed, by step name (only used with on_error = "continue").
    failures <- list()
    outdate_downstream_of_executed <- function() {
        dat <- pipenv[["data"]]
        locked <- dat[["step"]][dat[["locked"]]]
        .pip_outdate_downstream(
            x,
            steps = executedSteps,
            keep = c(executedSteps, locked)
        )
        executedSteps <<- character()
    }
    on.exit(outdate_downstream_of_executed())

    state <- pipenv[[".run_state"]]
    isRestart <- state == .run_states[["restart"]][["name"]]
    # A restart hands over the start of the run, so that the total time
    # logged at the end covers the whole run.
    runStart <- pipenv[[".run_start"]]
    if (!isRestart || is.null(runStart)) {
        runStart <- Sys.time()
    }
    if (exists(".run_start", envir = pipenv, inherits = FALSE)) {
        rm(".run_start", envir = pipenv)
    }
    stepsToRun <- dat[["step"]][rowsToRun]
    action <- if (isRestart) "Restarting" else "Starting"
    log_info(sprintf("%s run of %s '%s'", action, data.class(x), pipname))
    pipenv[[".run_state"]][] <- .run_states[["running"]][["name"]]
    halted <- FALSE
    tryCatch(
        {
            for (i in seq_along(rowsToRun)) {
                row <- rowsToRun[[i]]
                step <- dat[["step"]][[row]]
                if (!is.null(progress)) {
                    progress(value = i, detail = step)
                }
                msg <- if (isView) {
                    marker <- names(rowsToRun)[[i]]
                    # fmt: skip
                    sprintf("Step %i/%i [%s] %s", i, length(rowsToRun), marker, step) # nolint
                } else {
                    sprintf("Step %i/%i %s", i, length(rowsToRun), step)
                }

                # Arguments whose input step failed or was not run in this
                # run (only possible with on_error = "continue"). Unless all
                # of them refer to their step via try(), the step is not run
                # and counts as failed for its own dependents. An input that
                # failed for some keys only counts for a step that is not
                # mapped over the keys; a mapped step handles it per key.
                depends <- dat[["depends"]][[row]]
                failedArgs <- names(depends)[depends %in% names(failures)]
                isPartial <- vapply(
                    failures[unname(depends[failedArgs])],
                    FUN = .is_pipeflow_partitioned,
                    FUN.VALUE = logical(1)
                )
                if (dat[["exec"]][[row]] == "auto") {
                    failedArgs <- failedArgs[!isPartial]
                }
                hasFailedInput <- length(failedArgs) > 0L &&
                    !dat[["locked"]][[row]]
                if (hasFailedInput) {
                    tryArgs <- formula_try_args(dat[["params"]][[row]])
                    notAllowed <- setdiff(failedArgs, tryArgs)
                    if (length(notAllowed) > 0L) {
                        cause <- failures[[depends[[notAllowed[[1L]]]]]]
                        if (.is_pipeflow_partitioned(cause)) {
                            cause <- .pip_first_failure(
                                cause,
                                step = depends[[notAllowed[[1L]]]]
                            )
                        }
                        failures[[step]] <- .pip_failure(
                            step,
                            parent = cause[["parent"]],
                            failedStep = cause[["failed_step"]]
                        )
                        log_info(sprintf(
                            "%s - skipping step, input '%s' failed",
                            msg,
                            notAllowed[[1L]]
                        ))
                        next()
                    }
                }

                # A done step is re-run if one of its inputs was executed in
                # this run, since its output may then be stale.
                inputsChanged <- hasFailedInput ||
                    any(depends %in% executedSteps)
                isDone <- dat[["state"]][[row]] == "done"
                if (isDone && !force && !inputsChanged) {
                    log_info(sprintf("%s - skipping done step", msg))
                    next()
                }
                if (dat[["locked"]][[row]]) {
                    log_info(sprintf("%s - skipping locked step", msg))
                    next()
                }

                # A step that failed for some keys and is still "failed"
                # (its params and upstream steps did not change since, which
                # would have outdated it) only re-runs these keys, unless an
                # input was run again in this run or the run is forced.
                retryKeys <- character()
                isFailed <- dat[["state"]][[row]] == "failed"
                # To ensure that we don't use outdated outputs for the other
                # keys (the ones that did not fail), their output is only
                # re-used if they come from the most recent step's attempt.
                isOutCurrent <- isTRUE(
                    pipenv[[".steps_with_current_out"]][[step]]
                )
                if (isFailed && isOutCurrent && !force && !inputsChanged) {
                    retryKeys <- .pip_failed_keys(dat[["out"]][[row]])
                }
                if (length(retryKeys) > 0L) {
                    msg <- sprintf(
                        "%s - re-running failed keys %s",
                        msg,
                        paste0("'", retryKeys, "'", collapse = ", ")
                    )
                }

                # Always pass the full pipeline object (not a view) to the
                # step function, so that it can modify itself if needed.
                self <- .wrap_pipenv(pipenv, pipname, view = NULL)
                log_info(msg)

                # Take the name from the current data, since a step that
                # modified the pipeline may have shifted the rows.
                execStep <- pipenv[["data"]][["step"]][[row]]
                executedSteps <- c(executedSteps, execStep)

                # Prepare the list of failed inputs for the current step. An
                # input that failed for some keys is passed as it is.
                failed <- stats::setNames(
                    failures[unname(depends[failedArgs])],
                    failedArgs
                )
                failed <- Filter(Negate(.is_pipeflow_partitioned), failed)
                out <- NULL
                cond <- tryCatch(
                    {
                        out <- .pip_run_row(
                            x = self,
                            i = row,
                            lgr = lgr,
                            failed = failed,
                            onError = onError,
                            retry = length(retryKeys) > 0L
                        )
                        NULL
                    },
                    pipeflow_step_error = function(e) {
                        if (onError == "stop") {
                            stop(e)
                        }
                        e[["parent"]]
                    }
                )

                # Keep the condition of a failed step as its output and
                # continue with the next step.
                if (!is.null(cond)) {
                    failures[[execStep]] <- .pip_failure(execStep, cond)
                    datNow <- pipenv[["data"]]
                    data.table::set(
                        datNow,
                        i = data.table::chmatch(execStep, datNow[["step"]]),
                        j = "out",
                        value = list(list(cond))
                    )
                }

                # A step that failed for some keys keeps its output, which
                # its dependents get per key (see .pip_execute_step_call()).
                if (length(.pip_failed_keys(out)) > 0L) {
                    failures[[execStep]] <- out
                }
                stateAfterStep <- pipenv[[".run_state"]]

                # Check for restart or stop signals
                if (stateAfterStep == .run_states[["restart"]][["name"]]) {
                    log_info("Restarting pipeline execution.")
                    doForce <- pipenv[[".restart_force"]]

                    # Mark what has run so far before handing the remaining
                    # work to a nested run, which then re-runs the outdated
                    # steps and does its own marking at the end.
                    outdate_downstream_of_executed()
                    pipenv[[".run_start"]] <- runStart
                    pip_run(
                        x,
                        lgr = lgr,
                        force = doForce,
                        progress = progress,
                        on_error = onError
                    )
                    return(invisible(x))
                }

                if (stateAfterStep == .run_states[["halted"]][["name"]]) {
                    log_info("Aborting pipeline execution on manual halt.")
                    halted <- TRUE
                    break
                }
            }

            # Determine the total number of done, skipped, and failed steps.
            datNow <- pipenv[["data"]]
            timeStart <- datNow[["time_start"]]
            timeEnd <- datNow[["time"]]
            ran <- !is.na(timeStart) & timeStart >= runStart
            done <- ran & !is.na(timeEnd) & timeEnd >= timeStart
            skipped <- !ran & datNow[["step"]] %in% stepsToRun

            log_info(sprintf(
                "Finished in %s (%d done, %d skipped, %d failed)",
                .formatted_duration(difftime(Sys.time(), runStart)),
                sum(done),
                sum(skipped),
                sum(ran & !done)
            ))
            runState <- if (halted) {
                "halted"
            } else if (length(failures) > 0L) {
                "continued"
            } else {
                "ready"
            }
            pipenv[[".run_state"]][] <- .run_states[[runState]][["name"]]
            pipenv[[".last_run"]] <- Sys.time()
        },
        error = function(e) {
            pipenv[[".run_state"]][] <- .run_states[["failed"]][["name"]]
            pipenv[[".last_run"]] <- Sys.time()

            # Re-signal step errors as they are, so that callers can get the
            # failed step and its original condition.
            if (inherits(e, "pipeflow_step_error")) {
                stop(e)
            }
            stop_no_call(e$message)
        }
    )

    # Report the failures of a continued run once all states are final, so
    # that condition handlers already see them.
    if (length(failures) > 0L) {
        outdate_downstream_of_executed()
        w <- .pip_run_failed_warning(pipname, failures)
        lgr(level = "warn", msg = conditionMessage(w))
        warning(w)
    }

    invisible(x)
}


#' Reset a pipeline to its initial state
#'
#' Resets all unlocked steps of a pipeline (or a subset of steps defined by a
#' view) to state `"new"` and clears their outputs, so a subsequent
#' [pip_run()] re-executes the cleaned steps from scratch.
#' Any pending restart counters are cleared, but parameters, tags, and
#' locked flags are left unchanged.
#'
#' @details Locked steps are skipped: their state and output are preserved.
#' If all selected steps are locked, a warning is issued and nothing is
#' changed.
#'
#' @param x A pipeflow pip or view. If a view is given, only the steps covered
#' by the view are reset.
#'
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("square", \(x = ~load) x^2)
#'
#' pip_run(p)
#' p[["state"]] # "done", "done"
#'
#' # Locked steps keep their state and output when resetting
#' pip_lock(pip_view(p, step = "square"))
#' pip_reset(p)
#' p
#' p[["state"]] # "new", "done"
#' p[["out"]]   # NULL, (x^2 result)
#'
#' pip_unlock(p)
#' pip_reset(p)
#' p
#' p[["state"]] # "new", "new"
#' p[["out"]]   # NULL, NULL
#' @export
pip_reset <- function(x) {
    .assert_pip_or_view(x)
    env <- .pip_pipenv(x)
    dat <- env[["data"]]

    rows <- .pip_view_rows(x)
    if (length(rows) == 0L) {
        return(invisible(x))
    }
    rowsConsidered <- setdiff(rows, which(dat[["locked"]]))

    if (length(rowsConsidered) == 0L) {
        message("No steps to update: all selected steps are locked")
        return(invisible(x))
    }

    data.table::set(
        dat,
        i = rowsConsidered,
        j = c("out", "state", "time_start"),
        value = list(
            rep(list(NULL), length(rowsConsidered)),
            rep(.step_states[["new"]][["name"]], length(rowsConsidered)),
            rep(as.POSIXct(NA), length(rowsConsidered))
        )
    )

    env[[".run_state"]][] <- .run_states[["ready"]][["name"]]
    env[[".restart_count"]] <- 0L
    env[[".last_run"]] <- NULL

    invisible(x)
}


#' Get the run state of a pipeline
#'
#' Returns the state of the last (or current) run of a pipeline. Views share
#' the run state of their pipeline.
#'
#' @details The run state is one of
#' * `"ready"`: the pipeline is new, was reset, or its last run finished
#'   without failures.
#' * `"running"`: a run is in progress (seen from within a step).
#' * `"restart"`: a step requested a restart, which [pip_run()] carries out
#'   right after the step.
#' * `"halted"`: a step halted the run (see section 'Runtime control flow
#'   via restart and halt' in [pip_run()]).
#' * `"failed"`: the last run stopped on an error (`on_error = "stop"`).
#' * `"continued"`: the last run had failed steps but went on
#'   (`on_error = "continue"`).
#'
#' [pip_reset()] sets the run state back to `"ready"`.
#'
#' @param x A pipeflow pip or view.
#' @return A single character string.
#' @seealso [pip_run()], [pip_reset()]
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("fail", \(x = ~load) stop("boom"))
#'
#' pip_run_state(p) # "ready"
#' try(pip_run(p, lgr = NULL))
#' pip_run_state(p) # "failed"
#'
#' suppressWarnings(pip_run(p, on_error = "continue", lgr = NULL))
#' pip_run_state(p) # "continued"
#'
#' pip_reset(p)
#' pip_run_state(p) # "ready"
#' @export
pip_run_state <- function(x) {
    .assert_pip_or_view(x)
    as.character(.pip_pipenv(x)[[".run_state"]])
}


#' Annotated parameter values
#'
#' `pip_param_value()` is an extension point for handling non-trivial
#' parameter values (e.g. values with units or labels, or
#' parameter objects that carry UI metadata) as step parameters.
#' It can be thought of as a hook that allows you to preprocess or
#' unwrap parameter values before they are passed to the step function.
#' When a step runs, the generic is applied to each unbound argument of the
#' step, that is, to the values that come from `params` or from the
#' defaults of the step function, and its result is what the step
#' function receives. The stored parameters are not changed, and a method
#' should not change them either: it is called with the stored value, so a
#' method that modifies a reference-backed `x` such as an environment or
#' an R6 object in place changes the step's parameter on every run.
#'
#' The default method returns `x` unchanged. Methods are meant for your own
#' classes, not for base classes such as `numeric` or `character`. Note that
#' this is a hook that unwraps a stored value, not an accessor: use
#' [pip_get_params()] to read parameters from a pipeline.
#'
#' The generic is *not* applied to the outputs of other steps (arguments
#' defined as `~step`), to partitions or to failure objects. An error in a
#' method is an error of the step and is handled like any other step error,
#' including `pip_run(on_error = "continue")`.
#'
#' @param x The parameter value as stored in the step.
#' @param ... Currently unused, reserved for future extensions. Methods
#' should accept it.
#' @return The value the step function receives for the argument.
#' @examples
#' # A value that carries annotations
#' my_number <- structure(
#'     list(value = 1.23, label = "Number", descr = "I am an example"),
#'     class = "foo"
#' )
#'
#' # Step functions then simply see the plain value
#' pip_param_value.foo <- function(x, ...) x$value
#' registerS3method("pip_param_value", "foo", pip_param_value.foo)
#'
#' p <- pip_new() |>
#'     pip_add("double", \(x = 1) x * 2)
#' pip_set_params(p, list(x = my_number))
#' pip_run(p, lgr = NULL)
#' pip_collect(p)
#' @seealso [pip_set_params()], [pip_output_value()], [pip_run()]
#' @export
pip_param_value <- function(x, ...) {
    UseMethod("pip_param_value")
}

#' @export
pip_param_value.default <- function(x, ...) {
    x
}

#' @details
#' `` `pip_param_value<-`() `` is the write side of the same extension point.
#' It turns a plain value into the value that is stored in the step, so that
#' [pip_set_params()] can keep the annotations of a stored object when it is
#' given a plain value. Dispatch is on `x`, the value as it is currently
#' stored, and `value` is the requested value. The default method just returns
#' `value`.
#' A method decides what a plain `value` and what an object of its own class
#' means, for example whether to merge it into the stored object or to use
#' it as is.
#'
#' Like `pip_param_value()`, this is a hook, not an accessor: `x` is a stored
#' value, and the parameters of a pipeline are changed with
#' [pip_set_params()]. The two sides belong together, so that applying
#' `pip_param_value()` to the result of the setter gives back the plain value
#' that was set:
#'
#' ```r
#' pip_param_value(`pip_param_value<-`(x, value = v))   # v
#' ```
#'
#' [pip_set_params()] computes the stored value of every affected step *before*
#' it changes any parameters in the pipeline. If one or more methods fail,
#' an error of class `pipeflow_set_params_error` reports all failures. This
#' two-phase approach ensures that either all parameter changes are applied
#' successfully or, in case of an error, the pipeline stays unchanged.
#' For that to hold, a method should not modify `x` in place as this would
#' change pipeline parameters directly. A reference-backed `x` such as an
#' environment or an R6 object therefore should be copied first.
#'
#' @param value The plain value to set into the stored value `x`.
#' @examples
#'
#' # The write side keeps the annotations when a plain value is set
#' `pip_param_value<-.foo` <- function(x, ..., value) {
#'     if (inherits(value, "foo")) {
#'         return(value)
#'     }
#'     x$value <- value
#'     x
#' }
#' registerS3method("pip_param_value<-", "foo", `pip_param_value<-.foo`)
#'
#' pip_set_params(p, list(x = 42))
#' pip_get_params(p) # still a "foo" object, now holding 42
#' pip_run(p, lgr = NULL)
#' pip_collect(p)
#' @rdname pip_param_value
#' @export
`pip_param_value<-` <- function(x, ..., value) {
    UseMethod("pip_param_value<-")
}

#' @export
`pip_param_value<-.default` <- function(x, ..., value) {
    value
}

#' Apply pip_param_value() to the unbound arguments of a step call
#'
#' @param args Named list of all arguments of the step call.
#' @param unbound Names of the unbound arguments.
#' @return `args`, with the unbound arguments passed through
#'   [pip_param_value()].
#' @noRd
.pip_unwrap_params <- function(args, unbound) {
    unbound <- intersect(unbound, names(args))
    if (length(unbound) > 0L) {
        args[unbound] <- lapply(args[unbound], FUN = pip_param_value)
    }
    args
}

#' Annotated step outputs
#'
#' `pip_output_value()` is an extension point for annotating the outputs
#' of a pipeline, for example to stamp a result with the step it came
#' from or the time it was computed.
#' It can be thought of as a hook that allows you to postprocess or annotate
#' the output of a step before it is stored.
#' The generic is applied to the output of a step right after the step
#' function returns, and its result is what is stored as the step's output.
#'
#' The default method returns `x` unchanged. Methods are meant for your own
#' classes, not for base classes such as `numeric` or `character`.
#'
#' The generic is applied once to the output as a whole, so a method sees
#' the whole object a step returned. The one exception is a partitioned
#' output (a step with `exec = "split"`, or a step that was mapped over a
#' partitioned input): there the generic is applied once per partition, and
#' each partition gets its key in `ctx$key`.
#'
#' If you want to annotate an output element-wise instead of the whole
#' object, just write a method that walks over each element. For example,
#' for a plain list, a method could look something like this:
#'
#' ```r
#' pip_output_value.list <- function(x, ctx = list(), ...) {
#'     x[] <- lapply(x, FUN = pip_output_value, ctx = ctx)
#'     x
#' }
#' ```
#'
#' The generic is *not* applied to the outputs of failed steps (see
#' `pip_run(on_error = "continue")`), which remain the condition of the
#' failure, nor to the inputs a step receives from other steps: it acts on
#' what a step returns, once. An error in a method is an error of the step
#' and is handled like any other step error; for a partitioned output it
#' names the partition key the method failed for.
#'
#' @param x The output value of a step, or one of its partitions if the
#' output is partitioned.
#' @param ctx Named list describing the context of the value:
#' * `step`: the name of the step, as it was before the step ran.
#' * `key`: the partition key, or `NULL` if the output is not partitioned.
#' * `time`: the time stamp of the step's run, which is the same for all
#'   partition keys and is also stored as the step's `time`.
#' @param ... Currently unused, reserved for future extensions. Methods
#' should accept it.
#' @return The value that is stored as the step's output.
#' @examples
#' # Stamp every output with the step it came from
#' stamped <- function(x) structure(x, class = "stamped")
#'
#' pip_output_value.stamped <- function(x, ctx = list(), ...) {
#'     attr(x, "step") <- ctx[["step"]]
#'     x
#' }
#' registerS3method("pip_output_value", "stamped", pip_output_value.stamped)
#'
#' p <- pip_new() |>
#'     pip_add("make", \(n = 3) stamped(seq_len(n)))
#' pip_run(p, lgr = NULL)
#' attr(p[["make", "out"]], "step")
#' @seealso [pip_param_value()], [pip_run()]
#' @export
pip_output_value <- function(x, ctx = list(), ...) {
    UseMethod("pip_output_value")
}

#' @export
pip_output_value.default <- function(x, ctx = list(), ...) {
    x
}

#' Apply pip_output_value() to the output of a step
#'
#' A partitioned output is annotated per key, so that each partition gets
#' its key in `ctx$key`, and every other output is annotated as a whole. The
#' failure of a key in a continued run is not annotated.
#' `[<-` is used to rebuild a partitioned output, because `[[<-` would
#' drop a partition whose result is `NULL`. An error for one key is
#' re-raised as a `pipeflow_key_error`, as in `.pip_execute_step_call()`.
#'
#' @param out The output of the step call.
#' @param step The name of the step.
#' @param time The time stamp of the step's run.
#' @param skip Keys of a partitioned output that are not annotated, because
#' their outputs were kept from an earlier run (and annotated then).
#' @return `out`, passed through [pip_output_value()].
#' @noRd
.pip_annotate_output <- function(out, step, time, skip = character()) {
    if (.is_pipeflow_partitioned(out)) {
        keys <- setdiff(names(out), skip)
        out[keys] <- lapply(keys, FUN = function(key) {
            # The failure of a key in a continued run is kept as it is.
            if (inherits(out[[key]], "pipeflow_failure")) {
                return(out[[key]])
            }
            # As in `.pip_execute_step_call()`, errors are re-raised with the
            # key name, so a failing partition can be located.
            tryCatch(
                expr = pip_output_value(
                    out[[key]],
                    ctx = list(step = step, key = key, time = time)
                ),
                error = function(e) {
                    stop(.pip_key_error(key, parent = e))
                }
            )
        })
        return(out)
    }

    pip_output_value(out, ctx = list(step = step, key = NULL, time = time))
}


#' Get or set unbound parameters
#'
#' [pip_get_params()] returns the current default values of all unbound
#' parameters of a pipeline or view (parameters wired to another step's
#' output via `~step_name` are excluded).
#' [pip_set_params()] updates these parameters for the whole pipeline or
#' view except for locked steps. A one-sided formula binds a parameter to
#' the output of a step instead (see below).
#' Steps whose values change are marked as outdated, together with
#' their downstream dependents (steps that have never run stay `"new"`).
#' Values that are [identical()] to the stored ones are skipped and don't
#' outdate any step, unless `force = TRUE`.
#'
#' @section Storing annotated values:
#' The value that is stored for a step is the result of
#' `` `pip_param_value<-`() `` applied to the value the step currently
#' holds, which by default is the given value. A method of that generic
#' can merge the given value into an annotated object instead, so that
#' its annotations survive (see [pip_param_value()]). The merged values
#' of all affected steps are computed before anything is changed: if a
#' method fails, nothing is set and an error of class
#' `pipeflow_set_params_error` reports every failure at once. The merged
#' value is also what is compared with the stored one, so setting a plain
#' value that an annotated object already holds doesn't outdate the step.
#'
#' @section Binding parameters to steps:
#' A one-sided formula binds a parameter to the output of a step, e.g.
#' `pip_set_params(p, list(model = ~fit))`, as it does in the defaults of a
#' step function or in `params` of [pip_add()]. The parameter is bound in
#' all selected steps that have it unbound, except for locked steps. These
#' steps must come after the step that the formula refers to, otherwise an
#' error is signalled and nothing is changed. If you want to bind parameters
#' only for certain steps, use a view to select the desired steps.
#' A relative reference such as `~-1` is not allowed, but
#' `~try(fit)` works as in [pip_add()].
#'
#' A bound parameter is no longer unbound: [pip_get_params()] doesn't return
#' it, and [pip_set_params()] can't change it any more. Setting the same
#' binding again does nothing, and any other value is taken as an unknown
#' parameter (see `unknown`). To unbind or rebind it, replace the step with
#' [pip_replace()].
#' Therefore, a one-sided formula can't be set as value. Give it as a string
#' instead and turn it into a formula in the step, e.g. with
#' [stats::as.formula()].
#'
#' @note Parameters of locked steps are never changed and their state
#' remains unchanged. [pip_get_params()] still returns them, so if a locked
#' step comes first among the steps that share a parameter name,
#' [pip_get_params()] keeps returning the locked step's value after the
#' parameter was set.
#' To read the parameters of the unlocked steps only, use a view that
#' excludes the locked ones, `pip_get_params(p[!locked])`.
#'
#' An environment (or a reference object such as an R6 object) that was
#' modified in place is identical to the stored one, so setting it again
#' doesn't outdate the step unless `force = TRUE`.
#'
#' @param x A pipeflow pip or view.
#' @param params Named list of parameters to set. A one-sided formula binds
#' the parameter to a step, see Binding parameters to steps.
#' @param unknown How to handle `params` that are not defined in any of the
#' selected steps, including locked ones: `"warn"` (default) signals a
#' warning and sets the remaining parameters, `"ignore"` does so silently, and
#' `"error"` fails before anything is changed.
#' @param force Logical. If `TRUE`, every step that has one of the `params`
#' is outdated (together with its downstream steps), even if the values are
#' [identical()] to the stored ones. This refreshes steps whose inputs are
#' environments or reference objects that were modified in place.
#' @param ... Passed on to the methods of `` `pip_param_value<-`() ``,
#' which [pip_set_params()] uses to determine the value to store.
#' @return For [pip_get_params()], a named list of unbound parameter values;
#' if the same parameter name appears in multiple steps, the first
#' occurrence in pipeline order is returned. For [pip_set_params()], the
#' updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 10) seq_len(n)) |>
#'   pip_add("scale", \(x = ~load, factor = 0.5) x * factor)
#'
#' # See all unbound/adjustable parameters before running
#' pip_get_params(p) # list(n = 10, factor = 0.5)
#' (pip_run(p))
#'
#' # Updating params marks affected steps (and their dependents) outdated
#' pip_set_params(p, params = list(n = 5, factor = 2.0))
#' p
#' (pip_run(p))
#'
#' # Setting the same values again doesn't outdate any step
#' pip_set_params(p, params = list(n = 5, factor = 2.0))
#' p
#'
#' # Use force = TRUE to outdate the steps anyway
#' pip_set_params(p, params = list(n = 5, factor = 2.0), force = TRUE)
#' p
#'
#' # Setting a parameter that is not defined in the pipeline yields a warning
#' \donttest{
#' pip_set_params(p, params = list(nope = 1))
#' }
#'
#' # ... which can be silenced or turned into an error
#' pip_set_params(p, params = list(nope = 1), unknown = "ignore")
#' try(pip_set_params(p, params = list(nope = 1), unknown = "error"))
#'
#' # pip_get_params() includes locked steps; a view can exclude them
#' pip_lock(p["load"])
#' pip_get_params(p) # list(n = 5, factor = 2.0)
#' pip_get_params(p[!locked]) # list(factor = 2.0)
#'
#' # A formula binds a parameter to the output of a step
#' q <- pip_new() |>
#'   pip_add("load", \(n = 5) seq_len(n)) |>
#'   pip_add("fit_mean", \(x = ~load) mean(x)) |>
#'   pip_add("fit_max", \(x = ~load) max(x)) |>
#'   pip_add("report", \(model = NA) paste("result:", model)) |>
#'   pip_add("report_mean", \(model = NA) paste("mean:", model))
#'
#' # A view selects the steps to bind, here only "report_mean"
#' pip_set_params(q["report_mean"], params = list(model = ~fit_mean))
#'
#' # Without a view, the param is bound in all steps that have it unbound
#' pip_set_params(q, params = list(model = ~fit_max))
#' q
#' pip_run(q)
#' q[["report", "out"]]
#' q[["report_mean", "out"]]
#' @export
pip_set_params <- function(
    x,
    params = list(),
    unknown = c("warn", "ignore", "error"),
    force = FALSE,
    ...
) {
    # Input checking
    .assert_pip_or_view(x)
    unknown <- match.arg(unknown)
    if (!is.list(params)) {
        stop("params must be a list")
    }
    if (!is.logical(force) || length(force) != 1L || is.na(force)) {
        stop("force must be TRUE or FALSE")
    }
    parNames <- names(params)
    if (length(params) == 0) {
        return(invisible(x))
    }
    allNamed <- length(parNames) == length(params) && all(nzchar(parNames))
    if (!allNamed) {
        stop("All parameters must be named")
    }

    # The options in `...` go to the methods of `pip_param_value<-`() only,
    # so that they can't clash with the arguments of .pip_set_params()
    setValue <- \(current, value) {
        `pip_param_value<-`(current, ..., value = value)
    }
    .pip_set_params(
        x,
        params,
        unknown = unknown,
        force = force,
        setValue = setValue
    )
}


#' Set the unbound params of the selected steps
#'
#' Does the work of [pip_set_params()], whose inputs are checked.
#'
#' @param x A pipeflow pipeline or view.
#' @param params Named list of the params to set.
#' @param unknown,force As for [pip_set_params()].
#' @param bind If `TRUE`, a one-sided formula in `params` binds the param to
#' the step that it refers to. If `FALSE`, all params are set as values,
#' which a call of the function made by [as.function.pipeflow()] needs: its
#' arguments are values, also if they are formulas.
#' @param setValue Function of the current and the given value that returns
#' the value to store, by default `` `pip_param_value<-`() ``.
#' @return `x`, invisibly.
#' @noRd
.pip_set_params <- function(
    x,
    params,
    unknown = "warn",
    force = FALSE,
    bind = TRUE,
    setValue = `pip_param_value<-`
) {
    if (length(params) == 0L) {
        return(invisible(x))
    }
    parNames <- names(params)
    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    rows <- .pip_view_rows(x)

    # A one-sided formula binds the param to the step it refers to. The
    # references are checked only for the params that are bound below.
    isRef <- bind & vapply(params, is_one_sided_formula, logical(1))
    refs <- params[isRef]
    targets <- formula_deps(refs)

    # Signal parameters that are not defined in any of the selected steps,
    # including the locked ones. A param that is bound as given already is
    # not unknown, so that setting the binding again just does nothing.
    defined <- unique(unlist(dat[["unbound"]][rows]))
    isBound <- vapply(
        names(refs),
        \(name) {
            .pip_has_binding(dat, rows, name, refs[[name]], targets[[name]])
        },
        logical(1)
    )
    undefined <- setdiff(parNames, c(defined, names(refs)[isBound]))
    if (length(undefined) > 0L && unknown != "ignore") {
        msg <- paste0(
            "Trying to set parameters not defined in the target: ",
            toString(undefined)
        )
        if (unknown == "error") {
            stop_no_call(msg)
        }
        warning(msg, call. = FALSE)
    }

    # Narrow down the considered rows
    rowsConsidered <- setdiff(rows, which(dat[["locked"]]))
    if (length(rowsConsidered) == 0L) {
        message("No steps to update: all selected steps are locked")
        return(invisible(x))
    }

    # Determine which steps/rows are affected, i.e. have intersecting params
    unbound <- dat[["unbound"]][rowsConsidered] # names of unbound params
    intersects <- lapply(unbound, FUN = intersect, y = parNames)
    hasOverlap <- lengths(intersects) > 0
    namesAffected <- intersects[hasOverlap]
    rowsAffected <- rowsConsidered[hasOverlap]

    if (any(hasOverlap)) {
        # Bindings must refer to a step that comes before the bound step,
        # which is checked before anything is changed.
        namesBound <- lapply(namesAffected, FUN = intersect, y = names(refs))
        toBind <- unique(unlist(namesBound))
        .pip_assert_bind_targets(x, refs[toBind])
        .pip_check_bind_order(
            dat,
            rowsAffected,
            namesBound,
            targets[toBind]
        )

        # Determine the value to store for every affected row and name
        # before anything is changed, so that a failing method leaves the
        # pipeline untouched and all failures are reported at once.
        merged <- vector("list", length(rowsAffected))
        errors <- list()
        for (j in seq_along(rowsAffected)) {
            i <- rowsAffected[[j]]
            rowPars <- dat[["params"]][[i]]
            names <- setdiff(namesAffected[[j]], namesBound[[j]])
            values <- stats::setNames(vector("list", length(names)), names)
            for (name in names) {
                values[name] <- list(tryCatch(
                    setValue(rowPars[[name]], value = params[[name]]),
                    error = function(e) {
                        errors[[length(errors) + 1L]] <<- list(
                            step = dat[["step"]][[i]],
                            name = name,
                            parent = e
                        )
                        NULL
                    }
                ))
            }
            merged[[j]] <- values
        }
        if (length(errors) > 0L) {
            stop(.pip_set_params_error(errors))
        }

        # Update parameters in all affected rows, skipping values that are
        # identical to the stored ones (unless forced), and bind params,
        # which always changes the step
        isChanged <- logical(length(rowsAffected))
        for (j in seq_along(rowsAffected)) {
            i <- rowsAffected[[j]]
            rowPars <- dat[["params"]][[i]]
            values <- merged[[j]]
            names <- names(values)
            if (!force) {
                names <- Filter(
                    \(name) !identical(rowPars[[name]], values[[name]]),
                    names
                )
            }
            if (length(names) > 0L) {
                rowPars[names] <- values[names]
                value <- list(list(rowPars)) # need to wrap in list() for set
                data.table::set(dat, i = i, j = "params", value = value)
                isChanged[[j]] <- TRUE
            }
            bound <- namesBound[[j]]
            if (length(bound) > 0L) {
                .pip_bind_row(env, i, refs[bound], targets[bound])
                isChanged[[j]] <- TRUE
            }
        }

        # Update states of changed steps and their downstream steps
        rowsChanged <- rowsAffected[isChanged]
        if (length(rowsChanged) > 0L) {
            .pip_outdate_downstream(x, steps = dat[["step"]][rowsChanged])
        }
    }

    invisible(x)
}


#' Check the steps that the bindings of pip_set_params() refer to
#'
#' @param x A pipeflow pipeline or view.
#' @param refs Named list of one-sided formulas, e.g. `list(model = ~fit)`.
#' @return `refs`, invisibly. Signals an error for a relative reference
#' (e.g. `~-1`) or a step that is not in the pipeline.
#' @noRd
.pip_assert_bind_targets <- function(x, refs) {
    targets <- formula_deps(refs)
    if (length(targets) == 0L) {
        return(invisible(refs))
    }
    text <- paste0(names(refs), " = ", vapply(refs, deparse1, character(1)))
    isRelative <- startsWith(targets, "-")
    if (any(isRelative)) {
        stop_no_call(
            "params must refer to steps by name, not by relative position: ",
            toString(text[isRelative])
        )
    }
    isUnknown <- !(targets %in% .pip_pipenv(x)[["data"]][["step"]])
    if (any(isUnknown)) {
        stop_no_call(
            "params refer to steps that are not in the pipeline: ",
            toString(text[isUnknown]),
            " - to set a formula as value, give it as a string and turn it ",
            "into a formula in the step, e.g. with as.formula()"
        )
    }
    invisible(refs)
}


#' Is a param bound as given in one of the rows?
#'
#' @param dat The step table.
#' @param rows Row indices of `dat`.
#' @param name Name of the param.
#' @param ref The one-sided formula that binds the param.
#' @param target The step that `ref` refers to.
#' @return `TRUE` if the param of one of the `rows` refers to `target`, and
#' is wrapped in `try()` just like `ref`.
#' @noRd
.pip_has_binding <- function(dat, rows, name, ref, target) {
    for (i in rows) {
        depends <- dat[["depends"]][[i]]
        if (identical(unname(depends[name]), target)) {
            isTry <- is_try_ref(dat[["params"]][[i]][[name]])
            if (isTry == is_try_ref(ref)) {
                return(TRUE)
            }
        }
    }
    FALSE
}


#' Check that params are bound to steps that come before
#'
#' @param dat The step table.
#' @param rows Row indices of the steps whose params are bound.
#' @param bound List with the names of the bound params of each of the
#' `rows`.
#' @param targets Named character vector: the step that each param is bound
#' to.
#' @return `NULL`, invisibly. Signals an error if a param is bound to a step
#' that is the step itself or comes after it.
#' @noRd
.pip_check_bind_order <- function(dat, rows, bound, targets) {
    for (name in names(targets)) {
        target <- targets[[name]]
        isBound <- vapply(bound, \(nms) name %in% nms, logical(1))
        bad <- rows[
            isBound & rows <= data.table::chmatch(target, dat[["step"]])
        ]
        if (length(bad) > 0L) {
            stop_no_call(
                "cannot bind '",
                name,
                "' to step '",
                target,
                "' in steps that don't come after it: ",
                paste0("'", dat[["step"]][bad], "'", collapse = ", "),
                " - use a view to select the steps to bind"
            )
        }
    }
    invisible(NULL)
}


#' Bind params of a step to other steps
#'
#' The params become references, like those in the defaults of the step
#' function: they are added to `depends`, removed from `unbound`, and the DAG
#' gets edges from the steps they refer to.
#'
#' @param env The inner pipeline environment.
#' @param i Row index of the step.
#' @param refs Named list of one-sided formulas, the bound params.
#' @param targets Named character vector, the steps that `refs` refer to.
#' @return `NULL`, invisibly.
#' @noRd
.pip_bind_row <- function(env, i, refs, targets) {
    dat <- env[["data"]]
    params <- dat[["params"]][[i]]
    params[names(refs)] <- refs
    oldDepends <- dat[["depends"]][[i]]
    depends <- c(oldDepends, targets)
    depends <- depends[order(match(names(depends), names(params)))]

    newSteps <- setdiff(unique(targets), oldDepends)
    if (length(newSteps) > 0L) {
        from <- mget(
            newSteps,
            envir = env[[".steps_to_nodes"]],
            inherits = FALSE
        )
        dag_add_edges_to(
            .pip_dag(env),
            from = as.integer(unlist(from)),
            to = dat[["nodeId"]][[i]]
        )
    }

    values <- list(
        "params" = list(params),
        "depends" = list(depends),
        "unbound" = list(setdiff(dat[["unbound"]][[i]], names(refs)))
    )
    data.table::set(dat, i = i, j = names(values), value = values)
    invisible(NULL)
}


#' Add or remove tags
#'
#' Adds tags to, or removes tags from, all steps of a pipeline or a subset
#' of steps defined by a view. Tagged steps can later be selected via
#' [pip_view()].
#' @param x A pipeflow pip or view.
#' @param tags Character vector of tags to add to or remove from each
#' selected step.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#'
#' # Tag every step in the pipeline at once
#' pip_tag(p, c("daily", "core"))
#' p
#' p[, "tags"] # both steps have c("daily", "core")
#'
#' # Add an extra tag to only one step via a view
#' p[step == "fit"] |> pip_tag("model")
#' p
#' p[, "tags"] # "fit" also has "model"
#'
#' # Remove "daily" from all steps
#' pip_untag(p, "daily")
#' p[, "tags"]
#' @export
pip_tag <- function(x, tags = character()) {
    .assert_pip_or_view(x)
    if (!is.character(tags)) {
        stop("tags must be a character vector")
    }

    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    rows <- .pip_view_rows(x)

    if (length(rows) == 0L || length(tags) == 0L) {
        return(invisible(x))
    }

    for (i in rows) {
        if (isTRUE(dat[["locked"]][[i]])) {
            next
        }

        oldTags <- dat[["tags"]][[i]]
        newTags <- unique(c(oldTags, tags))
        data.table::set(dat, i = i, j = "tags", value = list(list(newTags)))
    }

    invisible(x)
}


#' @rdname pip_tag
#' @export
pip_untag <- function(x, tags = character()) {
    .assert_pip_or_view(x)
    if (!is.character(tags)) {
        stop("tags must be a character vector")
    }

    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    rows <- .pip_view_rows(x)

    if (length(rows) == 0L || length(tags) == 0L) {
        return(invisible(x))
    }

    for (i in rows) {
        if (isTRUE(dat[["locked"]][[i]])) {
            next
        }

        oldTags <- dat[["tags"]][[i]]
        newTags <- setdiff(oldTags, tags)
        data.table::set(dat, i = i, j = "tags", value = list(list(newTags)))
    }

    invisible(x)
}


#' Lock or unlock steps
#'
#' Locks or unlocks all steps of a pipeline, or a subset of steps defined by
#' a view. Locked steps are skipped during [pip_run()] and are protected
#' against modification from [pip_set_params()], [pip_tag()] or
#' [pip_untag()]. Calling [pip_unlock()] removes the lock again.
#' @param x A pipeflow pip or view.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("x", \(x = 1) x) |>
#'   pip_add("y", \(y = 2) y) |>
#'   pip_add("sum", \(x = 1, y = 2) x + y)
#' (pip_run(p))
#' p[["sum", "out"]] # 3
#'
#' # Lock "sum" step via a view so it cannot be overwritten
#' pip_set_params(p, params = list(x = 10, y = 20))
#' p[["sum", "params"]] # x = 10, y = 20
#' pip_lock(p["sum", ])
#' (pip_run(p))
#' p[["sum", "out"]] # still 3
#'
#' # Note that locking also prevents any parameter updates
#' pip_set_params(p, params = list(x = 100, y = 200))
#' p[["x", "params"]] # x = 100
#' p[["y", "params"]] # y = 200
#' p[["sum", "params"]] # still x = 10, y = 20
#'
#' # Unlock everything to allow updates again
#' pip_unlock(p)
#' pip_set_params(p, params = list(x = 100, y = 200))
#' (pip_run(p))
#' @export
pip_lock <- function(x) {
    .assert_pip_or_view(x)

    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    rows <- .pip_view_rows(x)

    if (length(rows) == 0L) {
        return(invisible(x))
    }

    data.table::set(dat, i = rows, j = "locked", value = TRUE)
    invisible(x)
}


#' @rdname pip_lock
#' @export
pip_unlock <- function(x) {
    .assert_pip_or_view(x)

    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    rows <- .pip_view_rows(x)

    if (length(rows) == 0L) {
        return(invisible(x))
    }

    data.table::set(dat, i = rows, j = "locked", value = FALSE)
    invisible(x)
}


#' Create a pipeline view
#'
#' Creates a filtered view showing only a selected subset of steps.
#' A view references the underlying pipeline without copying it, so
#' operations like [pip_run()] and [pip_set_params()] applied to a view
#' work directly on the underlying pipeline, but are restricted to the
#' steps defined by the view.
#'
#' A view keeps covering its steps when steps are added, removed, renamed or
#' replaced, also through the pipeline or another view. Removed steps drop
#' out of the view, and new steps are not added to it, even if they match
#' the filters.
#'
#' @param x A pipeflow pipeline or view.
#' @param ... Named filters, which can be one or more of `step`, `params`,
#' `state`, `tags`, `exec`, and `depends`. Each filter value is a character
#' vector of values to keep, or - if `fixed` is `FALSE` - a regular
#' expression. The `params` filter matches against the actual parameter
#' names of each step.
#' @param join How individual filters are combined:
#' * `"intersect"` (the default) keeps steps that match *all* filters,
#' * `"union"` keeps steps that match *any* filter. Within a single
#'    filter, multiple values are always treated as alternatives (OR).
#' @param fixed If TRUE, values in `...` are treated as fixed strings,
#' otherwise they are treated as regular expressions.
#'
#' @return A view: a `pipeflow` object that shares the pipeline's state and
#' covers the selected steps.
#' @export
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(a = 1) a, tags = c("io", "core", "daily")) |>
#'   pip_add("fit", \(b = 2) b + 1, tags = c("model")) |>
#'   pip_add("eval_fit", \(fit = ~fit) fit,
#'     tags = c("model", "daily", "report")
#'   )
#' p
#'
#' # Filter by one or more column values
#' pip_view(p, state = "new")
#' pip_view(p, step = c("load", "fit"))
#'
#' # Filter by tag — keeps steps that have *any* of the given tags
#' pip_view(p, tags = "daily")
#'
#' # Combine filters: step pattern AND state (logical AND)
#' pip_view(p, step = "fit", state = "new")
#'
#' # Combine filters as a union (step OR state)
#' pip_view(p, step = "load", tags = "report", join = "union")
#'
#' # Use a regex pattern
#' pip_view(p, step = "fit$", fixed = FALSE)
#'
#' # Filter by parameter names — steps with any of the given parameters
#' pip_view(p, params = c("a", "fit"))
#'
#' # Views are composable: create a view-of-view for progressive narrowing
#' v1 <- pip_view(p, tags = "daily")
#' print(v1) # load, eval_fit
#' v2 <- pip_view(v1, tags = "report")
#' print(v2) # eval_fit only
pip_view <- function(x, ..., join = c("intersect", "union"), fixed = TRUE) {
    .assert_pip_or_view(x)
    join <- match.arg(join)
    if (!.is_single(fixed, "logical")) {
        stop("fixed must be a single logical value")
    }
    env <- x[["pipenv"]]
    dat <- env[["data"]]

    filters <- list(...)
    validFilters <- c("step", "params", "depends", "state", "tags", "exec")
    unknown <- setdiff(names(filters), validFilters)
    if (length(unknown) > 0) {
        stop(sprintf(
            "Invalid filter name: '%s' - can be one of: %s",
            unknown[[1]],
            paste(validFilters, collapse = ", ")
        ))
    }

    # For view-of-view, filter only within parent view rows and map local
    # matches back to absolute row indices of the underlying pipeline.
    parent_rows <- .pip_view_rows(x)
    sub <- dat[parent_rows]

    # Identity element for the join: TRUE for intersect, FALSE for union.
    keep <- rep(join == "intersect", nrow(sub))

    # Resolve each filter name to the column it filters on.
    for (name in names(filters)) {
        values <- filters[[name]]
        if (!is.character(values)) {
            stop(sprintf(
                "filter '%s' must be a character vector, not %s",
                name,
                typeof(values)
            ))
        }
        col <- if (name == "params") {
            # Special case "params": it matches against the parameter *names*
            lapply(sub[["params"]], names)
        } else {
            sub[[name]]
        }
        matchFun <- if (fixed) {
            function(x) any(x %in% values)
        } else {
            function(x) {
                any(vapply(values, FUN = \(p) any(grepl(p, x = x)), logical(1)))
            }
        }
        hasMatch <- vapply(col, FUN = matchFun, logical(1))
        join_op <- if (join == "intersect") `&` else `|`
        keep <- join_op(keep, hasMatch)
    }

    rows <- parent_rows[which(keep)]
    .wrap_pipenv(env, name = x[["name"]], view = .pip_rows_to_view(env, rows))
}
