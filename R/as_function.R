# A pipeline as a function ---------------------------------------------------
#
# `as.function()` of a pipeline gives a function whose formals are the unbound
# params of the pipeline. Added with `pip_add()`, the params become params of
# the outer step and the exec modes map the function over partitions.
#
# The function keeps the inner pipelines it has run, so that a later call only
# runs the steps that its changed params or inputs outdate. Every call is
# served by a slot: the pipeline of its step and partition key. A call that is
# not mapped over partitions uses the single key, a direct call (outside of a
# step) the step reserved for direct calls. The kept pipelines belong to the
# function (they live in the environment of its runner): they are dropped with
# it, and serialising the function copies them.

# Placeholder names of the slots of calls without a partition key and without
# a step. They must be non-empty, since `x[[""]]` never finds an element of a
# list. A clash with a real step name or partition key only makes the calls
# share a pipeline of the same template: that is slower, never wrong, since
# every call sets all params and inputs.
.fun_key_single <- "<single>"
.fun_step_direct <- "<direct>"

#' Turn a pipeline into a function
#'
#' Returns a function that runs a copy of the pipeline `x` and returns the
#' output of selected steps. The function arguments are the unbound
#' parameters of `x`, with their current values as defaults. As any other
#' function, it can be added to an "outer" pipeline with [pip_add()],
#' in which case the unbound parameters of `x` become parameters of that
#' pipeline.
#' To bind parameters of `x` to output of steps of the outer pipeline,
#' use the `bind` argument.
#'
#' @note
#' The conversion is done by taking a snapshot of `x`. Later changes of `x`
#' therefore don't have an impact on the function. Also all steps start as new,
#' so the pipeline function result does not depend on whether `x` has been run
#' before, except for locked steps, which keep their outputs.
#'
#' @details
#' By default, the inner pipeline is run silently, but warnings and messages
#' of its steps are still passed on to the caller.
#' A failing inner step results in an error of the caller which in turn
#' makes the outer step fail.
#' Param values of the outer pipeline reach the function unwrapped
#' (see [pip_param_value()]). The arguments of the function are values, also
#' if they are formulas: unlike in [pip_set_params()], a formula argument
#' does not bind a param of `x` to a step.
#'
#' ## Choosing the output
#' With step names in `out`, the function returns their outputs. If `out` is
#' a function, it is called with the run inner pipeline, and its result is
#' the result of the function, e.g. `out = \(p) pip_collect(p, by = "tags")`
#' groups the outputs by tags. The pipeline that `out` gets is a light copy of
#' the kept one: it has its own steps, but the values (params and outputs)
#' are the same objects. Returning it, or a view of it, therefore keeps no
#' extra copy of the data, and later calls of the function don't change it.
#' Since the values are shared, don't modify them in place (e.g. with `:=` on
#' a `data.table` output).
#'
#' ## Continuing after errors of inner steps
#' With `on_error = "continue"` the inner pipeline is run like
#' `pip_run(on_error = "continue")`, that is, a failing inner step gets state
#' `"failed"`, the inner steps that do not depend on it still run, and an
#' inner step that refers to it via `~try(step)` runs and receives the
#' failure.
#'
#' The function still fails if one of the steps named in `out` failed or was
#' not run because an input failed. Since with `out = NULL` or a function
#' `out` *all* steps are requested, you need to name the relevant steps in
#' `out` to profit from a continuing inner run.
#'
#' ## Pipeline functions with partitioned inputs
#' If the step with the pipeline function receives partitioned inputs (from
#' an earlier step with `exec = "split"`), the function is called once per
#' partition caching one inner pipeline of `x` per step and partition key.
#' A call updates its parameters and inputs in the respective pipeline, so that
#' (as usual in pipelines) only outdated inner steps are run again.
#' This works for inputs bound with `bind` as well as for those bound via
#' `params` of [pip_add()].
#'
#' * A partition caches one pipeline for each key, which hold their inputs
#'   and outputs, so they need memory.
#' * A step without partitioned inputs keeps a single pipeline.
#' * Pure calls of the pipeline function (outside of a step) always share
#'   one pipeline, e.g. `lapply(subsets, f_pip)` don't cache anything.
#' * Do *not* use the same pipeline function in two different pipelines -
#'   this avoids name clash of keys, which would prevent proper caching
#'   (not wrong but slower).
#' * Cached pipelines are stored in the function. If you save the function
#'   or send it to another process, they are copied, including their data
#'   and output, so new runs work independently from the original.
#'   A function that has not been called yet, or one made with
#'   `cache = FALSE`, has no pipelines and therefore is small to copy.
#' * In case of an error, the error message includes the key of the partition
#'   and the name of the failed inner step.
#'
#' ## Limits
#' * Params that several steps of `x` share by name become one argument, with
#'   the default of the first step as in [pip_get_params()]. Every call sets
#'   this value in all of these steps, so a step whose own default differs
#'   from it runs with the value of the argument.
#' * Inputs are compared as they are. A change made in place to an
#'   environment or another reference object (e.g. R6) is not seen: as for
#'   [pip_set_params()] of a pipeline, which needs `force = TRUE`, the
#'   steps stay up to date. A forced run of the outer pipeline
#'   (`pip_run(force = TRUE)`) does not force the inner pipelines either,
#'   but you can force them by setting `cache = FALSE`.
#'
#' @param x A pipeflow pipeline (not a view; use `x[i, view = FALSE]` to turn
#' a part of a pipeline into a function).
#' @param out The result of the function. `NULL` (the default) returns the
#' outputs of all steps as a named list, see [pip_collect()]; a step name
#' returns the output of that step; several step names return a named list
#' of their outputs. A function with at least one argument is called with
#' (a light copy of) the run inner pipeline and returns the result, see
#' Details.
#' @param bind A named list of one-sided formulas, e.g.
#' `list(df = ~outer_step)`, that bind unbound params of `x` to steps
#' of the outer pipeline (`~try(outer_step)` also works).
#' Binding them via `pip_add("inner", f_inner, params = list(df = ~outer_step))`
#' works as well.
#' @param cache Whether to keep the run copies of `x` for reuse, `TRUE` (the
#' default) or `FALSE`. The function keeps one copy per outer step and
#' partition, see Details.
#' @param on_error What to do when an inner step fails: `"stop"` (the
#' default) ends the inner run at that step, `"continue"` runs the inner
#' steps that do not depend on it, see Details and [pip_run()].
#' @param lgr The logger of the inner run, see [pip_run()]. `NULL` (the
#' default) runs the inner pipeline silently. To activate logging,
#' set e.g. `lgr = pipeflow_lgr`.
#' @param ... Not used.
#' @return A function with the unbound params of `x` as arguments.
#' @seealso [pip_add()]
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
#'     as.function(inner, out = "coefs", bind = list(input_df = ~by_cyl))
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
    bind = list(),
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
    params <- pip_get_params(template)
    .assert_bind(bind, unbound = names(params))
    args <- .fun_bind(params, bind)

    ctx <- list(
        template = template,
        out = out,
        onError = onError,
        lgr = lgr,
        bindText = vapply(
            bind,
            \(f) paste(deparse(f), collapse = " "),
            character(1)
        ),
        keep = cache
    )
    body <- .fun_body(.fun_runner(ctx), names(args), names(bind))
    as.function(c(args, list(body)), envir = new.env(parent = baseenv()))
}


.assert_fun_out <- function(x, out) {
    if (is.null(out)) {
        return(invisible(out))
    }
    msg <- paste(
        "out must be NULL, a character vector of step names or a function",
        "with at least one argument"
    )
    if (is.function(out)) {
        # args() also gives the formals of a primitive, e.g. length(), and
        # NULL for a language construct such as `if`
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


#' Put the bindings into the formals
#'
#' The default is a call to `~`, so that the formula is made in the
#' function's own environment when the step is added, and does not capture
#' the environment of the caller.
#'
#' @param args The unbound params of the pipeline.
#' @param bind Named list of one-sided formulas, checked by `.assert_bind()`.
#' @return `args`, with the bound ones as `~step` calls.
#' @noRd
.fun_bind <- function(args, bind) {
    args <- .fun_quote(args)
    for (name in names(bind)) {
        args[[name]] <- call("~", bind[[name]][[2L]])
    }
    args
}


#' Protect symbols and calls from being evaluated
#'
#' The stored params have been evaluated already (by `pip_add()`). A symbol
#' or call among them, as default of a formal, would be evaluated once more
#' when the function is called, in addition to the evaluation that the step
#' does when it gets the value as its default, like in a plain pipeline.
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
#' The body hands all formals and, for the bound ones, whether they were
#' omitted to the runner: `runner(list(a = a, ...), c(b = missing(b)))`. The
#' functions are put into the call as objects, not as names, so that a
#' formal with the name of a function (`list`, `c`, ...) cannot shadow them.
#'
#' @param runner The function that runs the pipeline, see `.fun_runner()`.
#' @param formalNames Names of all formals.
#' @param bound Names of the bound formals.
#' @return A call.
#' @noRd
.fun_body <- function(runner, formalNames, bound) {
    values <- lapply(stats::setNames(formalNames, formalNames), as.name)
    omitted <- lapply(
        stats::setNames(bound, bound),
        \(name) as.call(list(base::missing, as.name(name)))
    )
    as.call(list(
        runner,
        as.call(c(list(base::list), values)),
        as.call(c(list(base::c), omitted))
    ))
}


#' The function that runs the pipeline
#'
#' Its environment holds the kept pipelines, `slots`: a list by step name of
#' lists of pipelines by partition key. They belong to the function and not
#' to a registry, so that they are dropped with it, also if a step closure
#' refers to the function. `active` holds the pipelines that run, see
#' `.fun_pip()`. The class marks the environment as the cache of a function
#' made by `as.function.pipeflow()`, see `.fun_cache()`.
#'
#' @param ctx The context made by `as.function.pipeflow()`.
#' @noRd
.fun_runner <- function(ctx) {
    force(ctx)
    cache <- new.env(parent = emptyenv())
    class(cache) <- "pipeflow_fun_cache"
    cache$slots <- list()
    cache$active <- list()
    function(args, omitted) .fun_call(args, omitted, ctx, cache)
}


#' One call of the function: find or make the pipeline, set, run, extract
#'
#' @param args All arguments of the call, as a named list.
#' @param omitted Named logical: was a bound argument omitted?
#' @param ctx The context made by `as.function.pipeflow()`.
#' @param cache The kept pipelines of the function.
#' @noRd
.fun_call <- function(args, omitted, ctx, cache) {
    .assert_fun_supplied(omitted, ctx$bindText)
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
#' A slot is the pipeline of a step and a partition key, updated in place
#' when the input of the key changes. A call that is not mapped over
#' partitions uses the single key (`.fun_key_single`), a direct call, which
#' is not made by a step, the step `.fun_step_direct`. The slots of the step
#' whose key is not among the keys of the call are dropped, so that the step
#' keeps one pipeline per partition, and a single one if it is no longer
#' mapped (or the other way round). Nothing is kept with `cache = FALSE`.
#'
#' A slot whose pipeline is running (see `cache$active`) is not handed out a
#' second time: a call that the run itself makes (a recursive function) gets
#' a new pipeline, which is not kept. Setting the params of the running
#' pipeline would change the steps of the call that is running it.
#'
#' @param cache The kept pipelines of the function.
#' @param stepCall The step call that calls the function, see
#' `.pip_step_call()`.
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
    if (is.null(key)) {
        key <- keys <- .fun_key_single
    }

    slots <- cache$slots[[step]]
    pip <- slots[[key]]
    if (is.null(pip)) {
        pip <- pip_clone(ctx$template)
        slots <- c(slots, stats::setNames(list(pip), key))
    }
    cache$slots[[step]] <- slots[intersect(keys, names(slots))]
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


#' The cache of a function made by `as.function.pipeflow()`
#'
#' @param fun Any object.
#' @return The environment with the kept pipelines, or `NULL` if `fun` is not
#' made by `as.function.pipeflow()`. The body of such a function is a call of
#' the runner, which is part of the call as an object, see `.fun_body()`.
#' @noRd
.fun_cache <- function(fun) {
    if (!is.function(fun) || !is.call(body(fun))) {
        return(NULL)
    }
    runner <- body(fun)[[1L]]
    cache <- if (is.function(runner)) environment(runner)[["cache"]]
    if (inherits(cache, "pipeflow_fun_cache")) cache
}


#' Drop the kept pipelines of a step that is mapped over no partition
#'
#' The step does not call the function then, so the call that drops the
#' pipelines of the keys that are gone (see `.fun_pip()`) does not happen.
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


#' A bound argument has the formula as default, which only works as part of
#' an outer pipeline: if it is omitted in a direct call, there is no input
#'
#' @param omitted Named logical: was a bound argument omitted?
#' @param bindText The bound formulas as text, for the message.
#' @noRd
.assert_fun_supplied <- function(omitted, bindText) {
    notSupplied <- names(omitted)[omitted]
    if (length(notSupplied) > 0L) {
        name <- notSupplied[[1L]]
        stop_no_call(
            "argument '",
            name,
            "' is bound to a step (",
            bindText[[name]],
            ") and must be supplied when the function is called directly"
        )
    }
    invisible(omitted)
}


#' Signal the failure of a requested step of a continued inner run
#'
#' With `on_error = "continue"` a failing step does not end the inner run, so
#' the requested steps are checked afterwards: if one of them failed, or was
#' not run because an input failed, the error of the step that failed is
#' signalled as `pip_run(on_error = "stop")` would have signalled it.
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
    cause <- causes[[affected[[1L]]]]
    stop(.pip_step_error(cause, parent = pip[[cause, "out"]]))
}


#' The step that failed, for each step that has no output of its own
#'
#' The failures are followed step by step as the run followed them (see
#' `pip_run()`), and not along all edges of the DAG: a step that got the
#' failure through `~try()` has run, and a locked step is neither run nor
#' skipped, so neither of them hands the failure to its own dependents.
#'
#' @param dat The `data` of the inner pipeline, after the run.
#' @return A named list: step name -> name of the step that failed for it,
#' in the order of the steps.
#' @noRd
.fun_failure_causes <- function(dat) {
    causes <- list()
    for (i in seq_len(nrow(dat))) {
        step <- dat[["step"]][[i]]
        if (dat[["locked"]][[i]]) {
            next
        }
        if (dat[["state"]][[i]] == .step_states[["failed"]][["name"]]) {
            causes[[step]] <- step
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
#' A function `out` gets a light copy of the pipeline, so that the later calls
#' that update the kept pipeline don't change what it returned. The copy is
#' made before `out` is called: `out` may not use its argument at once, e.g.
#' if it returns a closure.
#'
#' @param pip The inner pipeline, after the run.
#' @param out The `out` of `as.function.pipeflow()`.
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
#' Unlike a new pipeline, the copy has the run state and the time of the last
#' run of `pip`, since it shows `pip` as that run left it.
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
    snapshot
}


#' The kept pipelines of a function made by `as.function.pipeflow()`
#'
#' @return A list of the pipelines of all slots, named by partition key.
#' @noRd
.fun_pips <- function(fun) {
    cache <- .fun_cache(fun)
    as.list(unlist(unname(cache$slots), recursive = FALSE))
}
