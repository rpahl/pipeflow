# A pipeline as a function ---------------------------------------------------
#
# `as.function()` of a pipeline gives a function whose formals are the unbound
# params of the pipeline. Added with `pip_add()`, the params become params of
# the outer step and the exec modes map the function over partitions.
#
# The function keeps the inner pipelines it has run, so that a later call only
# runs the steps that its changed params or inputs outdate. The kept
# pipelines belong to the function (they live in the environment of its
# runner): they are dropped with it, and serialising the function copies them.

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
#' ## Continuing after errors of inner steps
#' With `on_error = "continue"` the inner pipeline is run like
#' `pip_run(on_error = "continue")`, that is, a failing inner step gets state
#' `"failed"`, the inner steps that do not depend on it still run, and an
#' inner step that refers to it via `~try(step)` runs and receives the
#' failure.
#'
#' The function still fails if one of the steps named in `out` failed or was
#' not run because an input failed. Since with `out = NULL` *all* steps are
#' requested, you need to name the relevant steps in `out` to profit from
#' a continuing inner run.
#'
#' ## Pipeline functions with partitioned inputs
#' If the step with the pipeline function happens to receive partitioned inputs
#' (from an earlier step with `exec = "split"`), changing inner pipeline
#' parameters often would require multiple reruns of the whole inner pipeline,
#' because of the different partitioned inputs.
#' For this reason, the function keeps up to `cache` inner pipelines of `x`,
#' for different partitioned inputs so that each partition should get its
#' own inner pipeline.
#' Then a call is served by the inner pipeline that has already worked on the
#' same partitioned input passed from the outer pipeline (i.e., passed
#' to the arguments named in `bind`).
#'
#' * `cache` must be at least the number of partitions, otherwise the least
#'   recently used copies are dropped before they are used again and nothing
#'   is reused. The kept copies hold their inputs and outputs, so the memory
#'   grows with `cache`.
#' * The copies belong to the function and are dropped with it. Serialising a
#'   function that has been called copies them as well (e.g. when sending it
#'   to another process): the copy starts with the same cache, but does not
#'   share it. A function that has not been called yet, or one made with
#'   `cache = 0`, is small.
#' * In case of an error, the error message includes the key of the partition
#'   and the name of the failed inner step.
#' * If the bound steps are not partitioned, cache = 1L is always enough.
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
#'   but you can force them by setting `cache = 0`.
#'
#' @param x A pipeflow pipeline (not a view; use `x[i, view = FALSE]` to turn
#' a part of a pipeline into a function).
#' @param out The result of the function. `NULL` (the default) returns the
#' outputs of all steps as a named list, see [pip_collect()]; a step name
#' returns the output of that step; several step names return a named list
#' of their outputs.
#' @param bind A named list of one-sided formulas, e.g.
#' `list(df = ~outer_step)`, that bind unbound params of `x` to steps
#' of the outer pipeline (`~try(outer_step)` also works).
#' Binding them via `pip_add("inner", f_inner, params = list(df = ~outer_step))`
#' would also work, but only the arguments named in `bind` tell the partitions
#' apart (see Details), that is, caching will not be available this way.
#' @param cache The number of run copies of `x` to keep for reuse, a whole
#' number: `0` keeps none, `Inf` keeps all.
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
#' # As a step: one inner pipeline per cylinder number
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
    cache = 16L,
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
        size = cache
    )
    body <- .fun_body(.fun_runner(ctx), names(args), names(bind))
    as.function(c(args, list(body)), envir = new.env(parent = baseenv()))
}


.assert_fun_out <- function(x, out) {
    if (is.null(out)) {
        return(invisible(out))
    }
    steps <- .pip_pipenv(x)[["data"]][["step"]]
    if (!is.character(out) || anyNA(out) || length(out) == 0L) {
        stop_no_call("out must be NULL or a character vector of step names")
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
    isCount <- .is_single(cache, "numeric") &&
        !is.na(cache) &&
        cache >= 0 &&
        (is.infinite(cache) || cache == floor(cache))
    if (!isCount) {
        stop_no_call(
            "cache must be a single whole number >= 0 (Inf keeps all)"
        )
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
#' Its environment holds the kept pipelines (`entries`, a list of `pip`,
#' `key` and `used`, and the counter `tick`). They belong to the function and
#' not to a registry, so that they are dropped with it, also if a step
#' closure refers to the function.
#'
#' @param ctx The context made by `as.function.pipeflow()`.
#' @noRd
.fun_runner <- function(ctx) {
    force(ctx)
    cache <- new.env(parent = emptyenv())
    cache$entries <- list()
    cache$tick <- 0L
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
    cache$tick <- cache$tick + 1L
    key <- args[names(omitted)]

    i <- Position(\(e) identical(e$key, key), cache$entries)
    pip <- if (is.na(i)) {
        pip_clone(ctx$template)
    } else {
        cache$entries[[i]]$pip
    }
    # The arguments are values, also if they are formulas
    .pip_set_params(pip, args, unknown = "ignore", bind = FALSE)

    # Keep before running: after a failure, the steps that did run are reused
    entry <- list(pip = pip, key = key, used = cache$tick)
    .fun_keep(cache, entry, i, ctx$size)
    pip_run(pip, lgr = ctx$lgr, on_error = ctx$onError)
    .fun_check_run(pip, ctx$out)
    .fun_out(pip, ctx$out)
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


#' Store the entry (new or updated), drop the least recently used
#'
#' @param cache The cache of the function.
#' @param entry The entry to store.
#' @param i Position of the entry in the cache, `NA` for a new one.
#' @param size Maximum number of entries.
#' @noRd
.fun_keep <- function(cache, entry, i, size) {
    if (size < 1L) {
        return(invisible(NULL))
    }
    if (is.na(i)) {
        i <- length(cache$entries) + 1L
    }
    cache$entries[[i]] <- entry
    if (length(cache$entries) > size) {
        used <- vapply(cache$entries, \(e) e$used, integer(1))
        cache$entries <- cache$entries[-which.min(used)]
    }
    invisible(NULL)
}


#' Signal the failure of a requested step of a continued inner run
#'
#' With `on_error = "continue"` a failing step does not end the inner run, so
#' the requested steps are checked afterwards: if one of them failed, or was
#' not run because an input failed, the error of the step that failed is
#' signalled as `pip_run(on_error = "stop")` would have signalled it.
#'
#' @param pip The inner pipeline, after the run.
#' @param out The requested steps, `NULL` for all of them.
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
    requested <- if (is.null(out)) dat[["step"]] else out
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


.fun_out <- function(pip, out) {
    if (is.null(out)) {
        return(pip_collect(pip))
    }
    outs <- stats::setNames(lapply(out, \(s) pip[[s, "out"]]), out)
    if (length(out) == 1L) outs[[1L]] else outs
}


#' The kept pipelines of a function made by `as.function.pipeflow()`
#' @noRd
.fun_pips <- function(fun) {
    cache <- environment(body(fun)[[1L]])[["cache"]]
    lapply(cache$entries, `[[`, "pip")
}
