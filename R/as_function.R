# A pipeline as a function ---------------------------------------------------
#
# `as.function()` of a pipeline gives a function whose formals are the unbound
# params of the pipeline. Added with `pip_add()`, the params become params of
# the outer step and the exec modes map the function over partitions.
#
# The function keeps the inner pipelines it has run, so that a later call only
# runs the steps that its changed params or inputs outdate. The kept
# pipelines live in `.fun_registry`, keyed by the id of the function, and not
# in the closure: the function only holds the id, so serialising it never
# copies the cache, and a copy in another process starts cold.

# Kept inner pipelines: one environment per function id (see `.fun_cache()`).
# An entry is removed when the function is garbage collected.
.fun_registry <- new.env(parent = emptyenv())


#' Turn a pipeline into a function
#'
#' Returns a function that runs a copy of the pipeline `x` and returns the
#' output of selected steps. The formals of the function are the unbound
#' params of `x`, with their current values as defaults. As any other
#' function, it can be added to another pipeline with [pip_add()]. Then the
#' unbound params of `x` become params of that step: [pip_get_params()] lists
#' them, [pip_set_params()] sets them and a change outdates the step.
#' To bind params of `x` to output of steps of the outer pipeline,
#' use the `bind` argument.
#'
#' @note
#' The conversion is done by taking a snapshot of `x`. Later changes of `x`
#' therefore don't have an impact on the function. Also all steps start as new,
#' so the pipeline function result does not depend on whether `x` has been run
#' before, except for locked steps, which keep their outputs.
#'
#' @details
#' The inner pipeline is run silently (`lgr = NULL`).
#' Warnings and messages of its steps are passed on to the caller.
#' A failing inner step results in an error of the caller, which in turn
#' makes the outer step fail.
#' Param values of the outer pipeline reach the function unwrapped
#' (see [pip_param_value()]).
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
#' * The copies are kept in the session that made the function and are
#'   dropped when the function is garbage collected. Serialising the function
#'   (e.g. to run it in another process) does not copy them: the copy of the
#'   function starts with an empty cache.
#' * In case of an error, the error message includes the key of the partition
#'   and the name of the failed inner step.
#' * If the bound steps are not partitioned, cache = 1L is always enough.
#'
#' @param x A pipeflow pipeline (not a view; use `x[i, view = FALSE]` to turn
#' a part of a pipeline into a function).
#' @param out The result of the function. `NULL` (the default) returns the
#' outputs of all steps as a named list, see [pip_collect()]; a step name
#' returns the output of that step; several step names return a named list
#' of their outputs.
#' @param bind A named list of one-sided formulas, e.g.
#' `list(input_df = ~split_df)`, that bind unbound params of `x` to steps
#' of the outer pipeline the function is added to.
#' The formal gets the formula as its default, as
#' if it was written in the step function (`~try(step)` is possible as
#' well).
#' @param cache The number of run copies of `x` to keep for reuse; `0` keeps
#' none.
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
#' pip_run(outer, lgr = NULL)
#' outer[["table", "out"]]
#' @export
as.function.pipeflow <- function(
    x,
    out = NULL,
    bind = list(),
    cache = 16L,
    ...
) {
    .assert_pip(x)
    .assert_fun_out(x, out)
    .assert_fun_bind(bind)
    if (!.is_single(cache, "numeric") || is.na(cache) || cache < 0) {
        stop_no_call("cache must be a single non-negative number")
    }

    template <- pip_clone(x)
    suppressMessages(pip_reset(template))
    args <- .fun_bind(pip_get_params(template), bind)

    id <- basename(tempfile("pip_fun_"))
    ctx <- list(
        id = id,
        template = template,
        out = out,
        bindText = vapply(
            bind,
            \(f) paste(deparse(f), collapse = " "),
            character(1)
        ),
        size = as.integer(cache)
    )
    env <- new.env(parent = baseenv())
    reg.finalizer(env, .fun_finalizer(id), onexit = FALSE)

    body <- .fun_body(.fun_runner(ctx), names(args), names(bind))
    as.function(c(args, list(body)), envir = env)
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


.assert_fun_bind <- function(bind) {
    isOneSided <- \(f) inherits(f, "formula") && length(f) == 2L
    bindNames <- names(bind)
    hasNames <- !is.null(bindNames) &&
        !anyNA(bindNames) &&
        all(nzchar(bindNames)) &&
        !anyDuplicated(bindNames)
    hasValidNames <- length(bind) == 0L || hasNames
    if (
        !is.list(bind) ||
            !hasValidNames ||
            !all(vapply(bind, isOneSided, logical(1)))
    ) {
        stop_no_call(
            "bind must be a named list of one-sided formulas, ",
            "e.g. list(data = ~step)"
        )
    }
    invisible(bind)
}


#' Put the bindings into the formals
#'
#' The default is a call to `~`, so that the formula is made in the
#' function's own environment when the step is added, and does not capture
#' the environment of the caller.
#'
#' @param args The unbound params of the pipeline.
#' @param bind Named list of one-sided formulas.
#' @return `args`, with the bound ones as `~step` calls.
#' @noRd
.fun_bind <- function(args, bind) {
    unknown <- setdiff(names(bind), names(args))
    if (length(unknown) > 0L) {
        stop_no_call(
            "bind has names that are no unbound params of the pipeline: ",
            toString(unknown)
        )
    }
    for (name in names(bind)) {
        args[[name]] <- call("~", bind[[name]][[2L]])
    }
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


.fun_runner <- function(ctx) {
    force(ctx)
    function(args, omitted) .fun_call(args, omitted, ctx)
}


#' Finalizer that only knows the id
#'
#' It must not see the environment of the function, or the finalizer would
#' keep it alive.
#'
#' @param id The id of the function.
#' @noRd
.fun_finalizer <- function(id) {
    force(id)
    function(e) {
        if (exists(id, envir = .fun_registry, inherits = FALSE)) {
            rm(list = id, envir = .fun_registry)
        }
    }
}


#' The cache of a function, created on first use (so it is cold after
#' serialisation)
#'
#' @param id The id of the function.
#' @return An environment with `entries` (list of `pip`, `key`, `used`) and
#'   the counter `tick`.
#' @noRd
.fun_cache <- function(id) {
    cache <- .fun_registry[[id]]
    if (is.null(cache)) {
        cache <- new.env(parent = emptyenv())
        cache$entries <- list()
        cache$tick <- 0L
        .fun_registry[[id]] <- cache
    }
    cache
}


#' One call of the function: find or make the pipeline, set, run, extract
#'
#' @param args All arguments of the call, as a named list.
#' @param omitted Named logical: was a bound argument omitted?
#' @param ctx The context made by `as.function.pipeflow()`.
#' @noRd
.fun_call <- function(args, omitted, ctx) {
    .assert_fun_supplied(omitted, ctx$bindText)
    cache <- .fun_cache(ctx$id)
    cache$tick <- cache$tick + 1L
    key <- args[names(omitted)]

    i <- Position(\(e) identical(e$key, key), cache$entries)
    pip <- if (is.na(i)) {
        pip_clone(ctx$template)
    } else {
        cache$entries[[i]]$pip
    }
    pip_set_params(pip, args, unknown = "ignore")

    # Keep before running: after a failure, the steps that did run are reused
    entry <- list(pip = pip, key = key, used = cache$tick)
    .fun_keep(cache, entry, i, ctx$size)
    pip_run(pip, lgr = NULL)
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


.fun_out <- function(pip, out) {
    if (is.null(out)) {
        return(pip_collect(pip))
    }
    outs <- stats::setNames(lapply(out, \(s) pip[[s, "out"]]), out)
    if (length(out) == 1L) outs[[1L]] else outs
}


#' The id of a function made by `as.function.pipeflow()`
#' @noRd
.fun_id <- function(fun) {
    environment(body(fun)[[1L]])[["ctx"]][["id"]]
}


#' The kept pipelines of a function made by `as.function.pipeflow()`
#' @noRd
.fun_pips <- function(fun) {
    lapply(.fun_cache(.fun_id(fun))$entries, `[[`, "pip")
}
