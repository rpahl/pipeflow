# A pipeline as a function
#
# `as.function()` of a pipeline gives a function whose formals are the unbound params of
# the pipeline. Added with `pip_add()`, the params become params of the outer step and
# the exec modes map the function over partitions.
#
# The function keeps the inner pipelines it has run, so that a later call only runs the
# steps that its changed params or inputs outdate. Every call is served by a slot: the
# pipeline of its step and partition key. A call that is not mapped over partitions uses
# the single key, a direct call (outside of a step) the step reserved for direct calls.
# The kept pipelines belong to the function (they live in the environment of its
# runner): they are dropped with it, and serialising the function copies them.

# ======================================================================================
# Making the function
# ======================================================================================

# -------
# Asserts
# -------
#' Check the `out` of `as.function.pipeflow()`
#'
#' @param x The pipeline.
#' @param out `NULL` (all outputs), a character vector of steps of `x`, or a function
#' that takes at least one argument, the run inner pipeline. A primitive, e.g.
#' `length()`, counts with the arguments that `args()` gives it.
#' @return `out`, invisibly.
#' @noRd
.assert_fun_out <- function(x, out) {
    if (is.null(out)) {
        return(invisible(out))
    }
    msg <- paste(
        "out must be NULL, a character vector of step names or a function",
        "with at least one argument"
    )
    if (is.function(out)) {
        # args() also gives the formals of a primitive, e.g. length(), and NULL for a
        # language construct such as `if`
        outArgs <- args(out)
        if (is.null(outArgs) || length(formals(outArgs)) == 0L) {
            stop_no_call(msg)
        }
        return(invisible(out))
    }
    steps <- .pip_pipenv(x)[["data"]][["step"]]
    if (!is.character(out) || anyNA(out) || length(out) == 0L) {
        stop_no_call(msg)
    }
    missingSteps <- setdiff(out, steps)
    if (length(missingSteps) > 0L) {
        stop_no_call(
            "out has steps that are not in the pipeline: ",
            toString(missingSteps)
        )
    }
    invisible(out)
}


.assert_fun_cache <- function(cache) {
    if (!.is_single(cache, "logical") || is.na(cache)) {
        stop_no_call("cache must be a single logical value (TRUE or FALSE)")
    }
    invisible(cache)
}


# ---------------------
# Function construction
# ---------------------
#' Protect symbols and calls from being evaluated
#'
#' The stored params have been evaluated already (by `pip_add()`). A symbol or call
#' among them, as default of a formal, would be evaluated once more when the function is
#' called, in addition to the evaluation that the step does when it gets the value as
#' its default, like in a plain pipeline.
#'
#' @param args Named list of param values.
#' @return `args`, with the symbols and calls wrapped in `quote()`.
#' @noRd
.fun_quote <- function(args) {
    isLang <- vapply(args, is.language, logical(1))
    args[isLang] <- lapply(args[isLang], \(value) call("quote", value))
    args
}


#' The body of the function
#'
#' The body hands all formals to the runner: `runner(list(a = a, ...))`. The functions
#' are put into the call as objects, not as names, so that a formal with the name of a
#' function (`list`, ...) cannot shadow them.
#'
#' @param runner The function that runs the pipeline, see `.fun_runner()`.
#' @param formalNames Names of all formals.
#' @return A call.
#' @noRd
.fun_body <- function(runner, formalNames) {
    values <- lapply(stats::setNames(formalNames, formalNames), as.name)
    as.call(list(runner, as.call(c(list(base::list), values))))
}


#' The function that runs the pipeline
#'
#' Its environment holds the kept pipelines, `slots`: a list by step name of the slots
#' of the step, see `.fun_slots()`. They belong to the function and not to a registry,
#' so that they are dropped with it, also if a step closure refers to the function.
#' `active` holds the pipelines that run, see `.fun_pip()`. The class marks the
#' environment as the cache of a function made by `as.function.pipeflow()`, see
#' `.fun_cache()`.
#'
#' @param ctx The context made by `as.function.pipeflow()`.
#' @return A function of the named list of all arguments of a call, which returns the
#' result of the call, see `.fun_call()`.
#' @noRd
.fun_runner <- function(ctx) {
    force(ctx)
    cache <- new.env(parent = emptyenv())
    class(cache) <- "pipeflow_fun_cache"
    cache$slots <- list()
    cache$active <- list()
    function(args) .fun_call(args, ctx, cache)
}


# ======================================================================================
# Calling the function
# ======================================================================================

# ----
# Call
# ----
#' One call of the function: find or make the pipeline, set, run, extract
#'
#' @param args All arguments of the call, as a named list.
#' @param ctx The context made by `as.function.pipeflow()`.
#' @param cache The kept pipelines of the function.
#' @return The result of the call, see `.fun_out()`.
#' @noRd
.fun_call <- function(args, ctx, cache) {
    # Read before the inner run, whose steps set their own
    stepCall <- .pip_step_call()

    # Kept before running: after a failure, the steps that did run are reused
    pip <- .fun_pip(cache, stepCall, ctx)
    cache$active <- c(cache$active, list(.pip_pipenv(pip)))
    on.exit(cache$active <- cache$active[-length(cache$active)])
    # The arguments are values, also if they are formulas
    .pip_set_params(pip, args, unknown = "ignore", bind = FALSE)

    pip_run(pip, lgr = ctx$lgr, on_error = ctx$onError)
    .fun_check_run(pip, ctx$out)
    .fun_out(pip, ctx$out)
}


#' The pipeline for a call: its slot, which is made if it is not there yet
#'
#' A slot is the pipeline of a step and a partition key, updated in place when the input
#' of the key changes. A call that is not mapped over partitions uses the single key
#' (`.fun_key_single`), a direct call, which is not made by a step, the step
#' `.fun_step_direct`. The slots of the step whose key is not among the keys of the call
#' are dropped, so that the step keeps one pipeline per partition, and a single one if
#' it is no longer mapped (or the other way round), see `.fun_keep_keys()`. Nothing is
#' kept with `cache = FALSE`.
#'
#' A slot whose pipeline is running (see `cache$active`) is not handed out a second
#' time: a call that the run itself makes (a recursive function) gets a new pipeline,
#' which is not kept. Setting the params of the running pipeline would change the steps
#' of the call that is running it.
#'
#' @param cache The kept pipelines of the function.
#' @param stepCall The step call that calls the function, see `.pip_step_call()`.
#' @param ctx The context made by `as.function.pipeflow()`.
#' @return A pipeline.
#' @noRd
.fun_pip <- function(cache, stepCall, ctx) {
    if (!ctx$keep) {
        return(pip_clone(ctx$template))
    }
    step <- stepCall$step
    if (is.null(step)) {
        step <- .fun_step_direct
    }
    key <- stepCall$key
    keys <- stepCall$keys
    mapped <- !is.null(key)
    if (!mapped) {
        key <- keys <- .fun_key_single
    }

    slots <- cache$slots[[step]]
    if (is.null(slots)) {
        slots <- .fun_slots()
        cache$slots[[step]] <- slots
    }
    .fun_keep_keys(slots, keys, mapped)
    pip <- slots$pips[[key]]
    if (is.null(pip)) {
        pip <- pip_clone(ctx$template)
        assign(key, pip, envir = slots$pips)
    }
    isActive <- vapply(
        cache$active,
        identical,
        logical(1),
        .pip_pipenv(pip)
    )
    if (any(isActive)) {
        return(pip_clone(ctx$template))
    }
    pip
}


# -----
# Slots
# -----
# Placeholder names of the slots of calls without a partition key and without a step.
# They must be non-empty, since `x[[""]]` never finds an element of a list. A clash with
# a real step name or partition key only makes the calls share a pipeline of the same
# template: that is slower, never wrong, since every call sets all params and inputs.
.fun_key_single <- "<single>"
.fun_step_direct <- "<direct>"


#' The slots of a step
#'
#' @return An environment with `pips`, a hashed environment of the kept pipelines by
#' partition key, `keys`, the keys of the last call, which give the order of the
#' pipelines, and `mapped`, whether the last call was mapped over partitions. A
#' partition may be named as the single key, so `mapped` and not the key tells a mapped
#' call from a single one.
#' @noRd
.fun_slots <- function() {
    slots <- new.env(parent = emptyenv())
    slots$pips <- new.env(parent = emptyenv(), hash = TRUE)
    slots$keys <- character()
    slots$mapped <- FALSE
    slots
}


#' Keep the slots of the keys of a call and drop the others
#'
#' All calls of one run of a mapped step get the same `keys` object, for which
#' `identical()` returns at once, so the slots are reconciled once per run of the step
#' and not once per partition.
#'
#' @param slots The slots of the step, see `.fun_slots()`.
#' @param keys All partition keys of the call, or the single key.
#' @param mapped Whether the call is mapped over partitions.
#' @return `NULL`, invisibly.
#' @noRd
.fun_keep_keys <- function(slots, keys, mapped) {
    if (identical(keys, slots$keys) && identical(mapped, slots$mapped)) {
        return(invisible(NULL))
    }
    gone <- setdiff(names(slots$pips), keys)
    rm(list = gone, envir = slots$pips)
    slots$keys <- keys
    slots$mapped <- mapped
    invisible(NULL)
}


#' The kept pipelines of a step, in the order of its partition keys
#'
#' @param slots The slots of the step, see `.fun_slots()`.
#' @return A list of pipelines, named by partition key.
#' @noRd
.fun_slot_pips <- function(slots) {
    keys <- intersect(slots$keys, names(slots$pips))
    mget(keys, envir = slots$pips)
}


# ------------------
# Run check & result
# ------------------
#' Signal the failure of a requested step of a continued inner run
#'
#' With `on_error = "continue"` a failing step does not end the inner run, so the
#' requested steps are checked afterwards: if one of them failed, or was not run because
#' an input failed, the error of the step that failed is signalled as
#' `pip_run(on_error = "stop")` would have signalled it.
#'
#' @param pip The inner pipeline, after the run.
#' @param out The requested steps, `NULL` or a function for all of them.
#' @return `NULL`, invisibly, if no requested step is affected.
#' @noRd
.fun_check_run <- function(pip, out) {
    dat <- .pip_pipenv(pip)[["data"]]
    isFailed <- dat[["state"]] == .step_states[["failed"]][["name"]] &
        !dat[["locked"]]
    if (!any(isFailed)) {
        return(invisible(NULL))
    }
    causes <- .fun_failure_causes(dat)
    requested <- if (is.null(out) || is.function(out)) {
        dat[["step"]]
    } else {
        out
    }
    affected <- intersect(names(causes), requested)
    if (length(affected) == 0L) {
        return(invisible(NULL))
    }
    step <- affected[[1L]]
    stepOut <- pip[[step, "out"]]

    # If the requested step failed for some keys in this run, its output holds a failure
    # for each of them, naming the step and key that failed. That is more precise than
    # the output of its cause, which may also hold failed keys the requested step
    # recovered from via ~try(). A step that was skipped in this run still has its
    # output of an earlier run, which does not count.
    wasSkipped <- pip[[step, "state"]] != .step_states[["failed"]][["name"]]
    if (!wasSkipped && length(.pip_failed_keys(stepOut)) > 0L) {
        f <- .pip_first_failure(stepOut, step = step)
        stop(.pip_step_error(f[["failed_step"]], parent = f[["parent"]]))
    }

    cause <- causes[[step]]
    parent <- pip[[cause, "out"]]
    if (.is_pipeflow_partitioned(parent)) {
        parent <- .pip_first_failure(parent, step = cause)[["parent"]]
    }
    stop(.pip_step_error(cause, parent = parent))
}


#' The step that failed, for each step that has no output of its own
#'
#' The failures are followed step by step as the run followed them (see `pip_run()`),
#' and not along all edges of the DAG: a step that got the failure through `~try()` has
#' run, and a locked step is neither run nor skipped, so neither of them hands the
#' failure to its own dependents.
#'
#' @param dat The `data` of the inner pipeline, after the run.
#' @return A named list: step name -> name of the step that failed for it, in the order
#' of the steps.
#' @noRd
.fun_failure_causes <- function(dat) {
    causes <- list()
    for (i in seq_len(nrow(dat))) {
        step <- dat[["step"]][[i]]
        if (dat[["locked"]][[i]]) {
            next
        }
        if (dat[["state"]][[i]] == .step_states[["failed"]][["name"]]) {
            # A mapped step whose failed keys were all not run because an input failed
            # has the step of that input as its cause.
            out <- dat[["out"]][[i]]
            causes[[step]] <- if (length(.pip_failed_keys(out)) > 0L) {
                .pip_first_failure(out, step = step)[["failed_step"]]
            } else {
                step
            }
            next
        }
        depends <- dat[["depends"]][[i]]
        failedArgs <- names(depends)[depends %in% names(causes)]
        tryArgs <- formula_try_args(dat[["params"]][[i]])
        blocked <- setdiff(failedArgs, tryArgs)
        if (length(blocked) > 0L) {
            causes[[step]] <- causes[[depends[[blocked[[1L]]]]]]
        }
    }
    causes
}


#' The result of a call: the outputs of the requested steps
#'
#' A function `out` gets a light copy of the pipeline, so that the later calls that
#' update the kept pipeline don't change what it returned. The copy is made before `out`
#' is called: `out` may not use its argument at once, e.g. if it returns a closure.
#'
#' @param pip The inner pipeline, after the run.
#' @param out The `out` of `as.function.pipeflow()`.
#' @return For `out = NULL`, the outputs of all steps (see [pip_collect()]), for a
#' function `out`, its result, for a single step, its output, and for several steps, a
#' list of their outputs, named by step.
#' @noRd
.fun_out <- function(pip, out) {
    if (is.null(out)) {
        return(pip_collect(pip))
    }
    if (is.function(out)) {
        snapshot <- .fun_snapshot(pip)
        return(out(snapshot))
    }
    outs <- stats::setNames(lapply(out, \(s) pip[[s, "out"]]), out)
    if (length(out) == 1L) outs[[1L]] else outs
}


#' A light copy of the run inner pipeline, for a function `out`
#'
#' Unlike a new pipeline, the copy has the run state and the time and duration of the
#' last run of `pip`, since it shows `pip` as that run left it.
#'
#' @param pip The inner pipeline, after the run.
#' @return A light copy of `pip`, see `.pip_clone()`.
#' @noRd
.fun_snapshot <- function(pip) {
    snapshot <- .pip_clone(pip, deep = FALSE)
    from <- .pip_pipenv(pip)
    to <- .pip_pipenv(snapshot)
    to[[".run_state"]] <- from[[".run_state"]]
    to[[".last_run"]] <- from[[".last_run"]]
    to[[".last_run_duration"]] <- from[[".last_run_duration"]]
    snapshot
}


# ======================================================================================
# Kept pipelines
# ======================================================================================

# ------------
# Cache access
# ------------
#' The cache of a function made by `as.function.pipeflow()`
#'
#' @param fun Any object.
#' @return The environment with the kept pipelines, or `NULL` if `fun` is not made by
#' `as.function.pipeflow()`. The body of such a function is a call of the runner, which
#' is part of the call as an object, see `.fun_body()`.
#' @noRd
.fun_cache <- function(fun) {
    if (!is.function(fun) || !is.call(body(fun))) {
        return(NULL)
    }
    runner <- body(fun)[[1L]]
    cache <- if (is.function(runner)) environment(runner)[["cache"]]
    if (inherits(cache, "pipeflow_fun_cache")) cache
}


#' Whether a function made by `as.function.pipeflow()` keeps its pipelines
#'
#' @param fun Any object.
#' @return `TRUE` if `fun` was made by `as.function.pipeflow()` with `cache = TRUE`,
#' else `FALSE`. A function made with `cache = FALSE` runs a new copy of the pipeline
#' for each call and keeps nothing.
#' @noRd
.fun_keeps <- function(fun) {
    if (is.null(.fun_cache(fun))) {
        return(FALSE)
    }
    isTRUE(environment(body(fun)[[1L]])[["ctx"]][["keep"]])
}


# -------------------------
# Updates on pipeline edits
# -------------------------
#' Drop the kept pipelines of a step that is mapped over no partition
#'
#' The step does not call the function then, so the call that drops the pipelines of the
#' keys that are gone (see `.fun_pip()`) does not happen.
#'
#' @param fun The function of the step.
#' @param step The name of the step.
#' @return `NULL`, invisibly. Nothing happens if `fun` was not made by
#' `as.function.pipeflow()`.
#' @noRd
.fun_release <- function(fun, step) {
    cache <- .fun_cache(fun)
    if (!is.null(cache)) {
        cache$slots[[step]] <- NULL
    }
    invisible(NULL)
}


#' Move the kept pipelines of a renamed step to its new name
#'
#' @param fun The function of the step.
#' @param from,to The old and the new name of the step.
#' @return `NULL`, invisibly. Nothing happens if `fun` was not made by
#' `as.function.pipeflow()`.
#' @noRd
.fun_rename <- function(fun, from, to) {
    cache <- .fun_cache(fun)
    if (!is.null(cache) && !is.null(cache$slots[[from]])) {
        cache$slots[[to]] <- cache$slots[[from]]
        cache$slots[[from]] <- NULL
    }
    invisible(NULL)
}


# ======================================================================================
# Exported API
# ======================================================================================

# ----------------------
# Pipeline as a function
# ----------------------
#' Turn a pipeline into a function
#'
#' Returns a function that runs a copy of the pipeline `x` and returns the output of
#' selected steps. The function arguments are the unbound parameters of `x`, with their
#' current values as defaults. As any other function, it can be added to an "outer"
#' pipeline with [pip_add()], in which case the unbound parameters of `x` become
#' parameters of that pipeline. To bind parameters of `x` to output of steps of the
#' outer pipeline, pass them as references in `params` of [pip_add()], e.g.
#' `pip_add(outer, "inner", f_inner, params = list(df = ~outer_step))`.
#'
#' @note
#' The conversion is done by taking a snapshot of `x`. Later changes of `x` therefore
#' don't have an impact on the function. Also all steps start as new, so the pipeline
#' function result does not depend on whether `x` has been run before, except for locked
#' steps, which keep their outputs.
#'
#' @details
#' By default, the inner pipeline is run silently, but warnings and messages of its
#' steps are still passed on to the caller. A failing inner step results in an error of
#' the caller which in turn makes the outer step fail. Param values of the outer
#' pipeline reach the function unwrapped (see [pip_param_value()]). The arguments of the
#' function are values, also if they are formulas: unlike in [pip_set_params()], a
#' formula argument does not bind a param of `x` to a step.
#'
#' ## Choosing the output
#' With step names in `out`, the function returns their outputs. If `out` is a function,
#' it is called with the run inner pipeline, and its result is the result of the
#' function, e.g. `out = \(p) pip_collect(p, by = "tags")` groups the outputs by tags.
#' The pipeline that `out` gets is a light copy of the kept one: it has its own steps,
#' but the values (params and outputs) are the same objects. Returning it, or a view of
#' it, therefore keeps no extra copy of the data, and later calls of the function don't
#' change it. Since the values are shared, don't modify them in place (e.g. with `:=` on
#' a `data.table` output).
#'
#' ## Errors of inner steps
#' If an inner step fails, the error names it, e.g. `step 'fit': negative`. As a step of
#' an outer pipeline, the error also names the outer step and, if that step is mapped
#' over partitions, the key: `step 'by_batch': key 'D': step 'fit': negative`. Nested
#' pipeline functions add one step per level:
#' `step 'outer': step 'middle': step 'fit': negative`.
#'
#' ## Continuing after errors of inner steps
#' With `on_error = "continue"` the inner pipeline is run like
#' `pip_run(on_error = "continue")`, that is, a failing inner step gets state
#' `"failed"`, the inner steps that do not depend on it still run, and an inner step
#' that refers to it via `~try(step)` runs and receives the failure.
#'
#' The function still fails if one of the steps named in `out` failed or was not run
#' because an input failed. Since with `out = NULL` or a function `out` *all* steps are
#' requested, you need to name the relevant steps in `out` to profit from a continuing
#' inner run.
#'
#' ## Pipeline functions with partitioned inputs
#' If the step with the pipeline function receives partitioned inputs (from an earlier
#' step with `exec = "split"`), the function is called once per partition caching one
#' inner pipeline of `x` per step and partition key. A call updates its parameters and
#' inputs in the respective pipeline, so that (as usual in pipelines) only outdated
#' inner steps are run again. This works for inputs bound via `params` of [pip_add()] as
#' well as for those bound in the step function.
#'
#' * A partition caches one pipeline for each key, which hold their inputs and outputs,
#'   so they need memory.
#' * A step without partitioned inputs keeps a single pipeline.
#' * Pure calls of the pipeline function (outside of a step) always share one pipeline,
#'   e.g. `lapply(subsets, f_pip)` don't cache anything.
#' * Do *not* use the same pipeline function in two different pipelines - this avoids
#'   name clash of keys, which would prevent proper caching (not wrong but slower).
#' * Cached pipelines are stored in the function. If you save the function or send it to
#'   another process, they are copied, including their data and output, so new runs work
#'   independently from the original. A function that has not been called yet, or one
#'   made with `cache = FALSE`, has no pipelines and therefore is small to copy.
#' * In case of an error, the error message includes the key of the partition and the
#'   name of the failed inner step, see 'Errors of inner steps'.
#'
#' ## Limits
#' * Params that several steps of `x` share by name become one argument, with the
#'   default of the first step as in [pip_get_params()]. Every call sets this value in
#'   all of these steps, so a step whose own default differs from it runs with the value
#'   of the argument.
#' * Inputs are compared as they are. A change made in place to an environment or
#'   another reference object (e.g. R6) is not seen: as for [pip_set_params()] of a
#'   pipeline, which needs `force = TRUE`, the steps stay up to date. A forced run of
#'   the outer pipeline (`pip_run(force = TRUE)`) does not force the inner pipelines
#'   either, but you can force them by setting `cache = FALSE`.
#'
#' @param x A pipeflow pipeline (not a view; use `x[i, view = FALSE]` to turn a part of
#' a pipeline into a function).
#' @param out The result of the function. `NULL` (the default) returns the outputs of
#' all steps as a named list, see [pip_collect()]; a step name returns the output of
#' that step; several step names return a named list of their outputs. A function with
#' at least one argument is called with (a light copy of) the run inner pipeline and
#' returns the result, see Details.
#' @param cache Whether to keep the run copies of `x` for reuse, `TRUE` (the default) or
#' `FALSE`. The function keeps one copy per outer step and partition, see Details. With
#' `FALSE`, a step mapped over partitions can run the function in parallel, see the
#' `map` argument of [pip_run()].
#' @param on_error What to do when an inner step fails: `"stop"` (the default) ends the
#' inner run at that step, `"continue"` runs the inner steps that do not depend on it,
#' see Details and [pip_run()].
#' @param lgr The logger of the inner run, see [pip_run()]. `NULL` (the default) runs
#' the inner pipeline silently. To activate logging, set e.g. `lgr = pipeflow_lgr`.
#' @param ... Not used.
#' @return A function with the unbound params of `x` as arguments.
#' @seealso [pip_add()], [pip_inner()] to look at the kept inner pipelines
#' @examples
#' inner <- pip_new("inner") |>
#'   pip_add("input", \(input_df = NULL) input_df) |>
#'   pip_add("fit", \(data = ~input, degree = 1) {
#'     lm(mpg ~ poly(wt, degree, raw = TRUE), data = data)
#'   }) |>
#'   pip_add("coefs", \(fit = ~fit, digits = 2) round(coef(fit), digits))
#'
#' # As a plain function: the unbound params are its arguments
#' f_inner <- as.function(inner, out = "coefs")
#' f_inner(input_df = mtcars, digits = 1)
#'
#' # As a step: one inner pipeline
#' single <- pip_new("single") |>
#'   pip_add("df", \(df = mtcars) df) |>
#'   pip_add("coefs", f_inner, params = list(input_df = ~df))
#'
#' pip_run(single, lgr = NULL)
#' single[["coefs", "out"]]
#'
#' # Only the inner steps after the changed param run again
#' pip_set_params(single, list(digits = 3))
#' pip_run(single, lgr = NULL)
#' single[["coefs", "out"]]
#'
#' # With partioned inputs: one inner pipeline per cylinder number
#' outer <- pip_new("outer") |>
#'   pip_add("df", \(df = mtcars) df) |>
#'   pip_add("by_cyl", \(df = ~df) split(df, df$cyl), exec = "split") |>
#'   pip_add(
#'     "coefs",
#'     as.function(inner, out = "coefs"),
#'     params = list(input_df = ~by_cyl)
#'   ) |>
#'   pip_add("table", \(coefs = ~coefs) do.call(rbind, coefs), exec = "reduce")
#'
#' pip_get_params(outer)
#' pip_run(outer, lgr = NULL)
#' outer[["table", "out"]]
#'
#' # Only the steps after the changed param run again, for each partition
#' pip_set_params(outer, list(digits = 3))
#' print(outer)
#' pip_run(outer, lgr = NULL)
#' outer[["table", "out"]]
#'
#' # Don't use the same function for the same step name in two pipelines:
#' # this breaks the caching (results are still correct, but it is slower)
#' a <- pip_new("a") |>
#'   pip_add("df", \(df = mtcars) df) |>
#'   pip_add("coefs", f_inner, params = list(input_df = ~df))
#' b <- pip_new("b") |>
#'   pip_add("df", \(df = mtcars[mtcars$am == 1, ]) df) |>
#'   pip_add("coefs", f_inner, params = list(input_df = ~df))
#'
#' pip_run(a, lgr = NULL)
#' pip_run(b, lgr = NULL)
#'
#' # The inner pipeline now holds the input of `b`, so all its steps run again
#' # for `a`, not only the ones after `digits`
#' pip_set_params(a, list(digits = 3))
#' pip_run(a, lgr = NULL)
#' a[["coefs", "out"]]
#'
#' # Better: make one function per pipeline
#' pip_replace(
#'   a, "coefs", as.function(inner, out = "coefs"),
#'   params = list(input_df = ~df)
#' )
#'
#' # A function as out: the outputs grouped by tags
#' tagged <- pip_new("tagged") |>
#'   pip_add("input", \(input_df = NULL) input_df) |>
#'   pip_add("n", \(data = ~input) nrow(data), tags = "summary") |>
#'   pip_add("mpg", \(data = ~input) mean(data$mpg), tags = "summary") |>
#'   pip_add("fit", \(data = ~input) lm(mpg ~ wt, data = data), tags = "model")
#'
#' f_tagged <- as.function(tagged, out = \(p) pip_collect(p, by = "tags"))
#' res <- f_tagged(input_df = mtcars)
#' names(res)
#' res$summary
#'
#' # With on_error = "continue", the inner pipeline handles a failed inner
#' # step itself: `coefs` is run with the failure of `fit` and reports it.
#' robust <- pip_new("robust") |>
#'   pip_add("input", \(input_df = NULL) input_df) |>
#'   pip_add("fit", \(data = ~input) {
#'     if (nrow(data) < 3) stop("too few rows to fit")
#'     lm(mpg ~ wt, data = data)
#'   }) |>
#'   pip_add("coefs", \(fit = ~try(fit), digits = 2) {
#'     if (inherits(fit, "pipeflow_failure")) NULL else round(coef(fit), digits)
#'   })
#'
#' f_robust <- as.function(robust, out = "coefs", on_error = "continue")
#' f_robust(input_df = mtcars)
#'
#' # the continued inner run signals a warning instead of an error
#' suppressWarnings(f_robust(input_df = mtcars[1:2, ]))
#' @export
as.function.pipeflow <- function(
    x,
    out = NULL,
    cache = TRUE,
    on_error = c("stop", "continue"),
    lgr = NULL,
    ...
) {
    .assert_pip(x)
    .assert_fun_out(x, out)
    .assert_fun_cache(cache)
    onError <- match.arg(on_error)
    if (!is.null(lgr)) {
        .assert_logger(lgr)
    }

    template <- pip_clone(x)
    suppressMessages(pip_reset(template))
    args <- .fun_quote(pip_get_params(template))

    ctx <- list(
        template = template,
        out = out,
        onError = onError,
        lgr = lgr,
        keep = cache
    )
    body <- .fun_body(.fun_runner(ctx), names(args))
    as.function(c(args, list(body)), envir = new.env(parent = baseenv()))
}


# ---------------
# Inner pipelines
# ---------------
#' Inner pipelines of a pipeline function step
#'
#' Returns the inner pipelines that the function of `step`, made by
#' [as.function.pipeflow()], has kept, e.g. to debug an inner run or to look at the
#' steps behind the returned output.
#'
#' @param x A pipeflow pipeline object, the "outer" pipeline.
#' @param step Name of a step of `x` whose function was made by
#' [as.function.pipeflow()].
#' @return For a step that is mapped over partitions, a list of pipelines named by
#' partition key, otherwise the pipeline of the last call of the step. The pipelines are
#' copies that share the values of the kept ones, so don't modify the values in place.
#' An empty list is returned if the function has kept nothing (yet), e.g. before the
#' first run or with `cache = FALSE`.
#' @seealso [as.function.pipeflow()]
#' @examples
#' inner <- pip_new("inner") |>
#'   pip_add("data", \(input_df = NULL) input_df) |>
#'   pip_add("fit", \(data = ~data) lm(mpg ~ wt, data = data))
#'
#' outer <- pip_new("outer") |>
#'   pip_add("df", \(df = mtcars) df) |>
#'   pip_add("by_cyl", \(df = ~df) split(df, df$cyl), exec = "split") |>
#'   pip_add(
#'     "fits",
#'     as.function(inner, out = "fit"),
#'     params = list(input_df = ~by_cyl)
#'   )
#' pip_run(outer, lgr = NULL)
#'
#' fits <- pip_inner(outer, "fits")
#' names(fits)
#' fits[["6"]]
#' @export
pip_inner <- function(x, step) {
    .assert_pip_or_view(x)
    if (!.pip_step_exists(x, step)) {
        stop_no_call("step '", step, "' does not exist")
    }
    cache <- .fun_cache(x[[step, "fun"]])
    if (is.null(cache)) {
        stop_no_call(
            "the function of step '",
            step,
            "' was not made by as.function()"
        )
    }
    slots <- cache$slots[[step]]
    if (is.null(slots) || length(slots$pips) == 0L) {
        return(list())
    }
    pips <- lapply(.fun_slot_pips(slots), .fun_snapshot)
    if (!slots$mapped) {
        return(pips[[1L]])
    }
    pips
}
