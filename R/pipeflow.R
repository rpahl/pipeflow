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
        exec = character(0),
        time = as.POSIXct(character(0)),
        depends = list(),
        unbound = list(), # names of independent parameters
        nodeId = integer()
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
        depends = list(depends),
        unbound = list(setdiff(names(params), names(depends))),
        nodeId = nodeId
    )
}


# ---------------
# Type predicates
# ---------------
.is_pipeflow <- function(x) {
    inherits(x, "pipeflow")
}

# A view is a pipeflow whose `rows` field is not NULL.
.is_pipeflow_view <- function(x) {
    inherits(x, "pipeflow") && !is.null(x[["view"]])
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
        stop_no_call("x must be a full pipeline, not a view")
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
# same underlying pipeline environment.
.wrap_pipenv <- function(pipenv, name, view = NULL) {
    structure(
        list(name = name, view = view, pipenv = pipenv),
        class = "pipeflow"
    )
}

# Wrap a step function so that `.self` is available in its body. The wrapper
# gets its own environment holding the pipeline reference, so the original
# function object is never mutated.
.wrap_self <- function(fun, self) {
    env <- new.env(parent = environment(fun))
    env[[".self"]] <- self
    eval(call("function", formals(fun), body(fun)), envir = env)
}

# ------------------------------
# Parameter & dependency parsing
# ------------------------------
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

    # Make sure default values are returned as resolved values
    lapply(args, \(x) eval(x, envir = environment(fun)))
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

.pip_execute_step_call <- function(fun, args, exec) {
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
        out <- do.call(fun, args = args)
        return(.as_pipeflow_partitioned(out))
    }

    # Plain mode: only executes a single call, so partitioned inputs
    # (which would require mapping) are not allowed.
    if (exec == "plain" && length(partIdx) > 0L) {
        stop("plain mode does not accept partitioned inputs")
    }

    # Reduce mode: combines partitioned inputs in a single call,
    # so it needs at least one partitioned input to be meaningful.
    if (exec == "reduce" && length(partIdx) == 0L) {
        stop("reduce mode requires at least one partitioned input")
    }

    # Single-call, which happens in three scenarios:
    # 1) there are no partitioned inputs, which is the standard case in a
    #    standard pipeline that runs without any split/reduce steps
    # 2) plain mode was explicitly requested (to ensure non-split mode)
    # 3) reduce mode was requested to re-combine partitioned inputs
    if (length(partIdx) == 0L || exec == "plain" || exec == "reduce") {
        return(do.call(fun, args = args))
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

    # ...and each key is computed separately by slicing all partitioned
    # arguments down to that key before calling the function.
    out <- stats::setNames(vector(mode = "list", length = length(keys)), keys)
    for (key in keys) {
        keyArgs <- args
        for (idx in partIdx) {
            keyArgs[[idx]] <- args[[idx]][[key]]
        }

        # Errors are re-raised with the key name so a failing partition can
        # be located without inspecting the whole output.
        out[[key]] <- tryCatch(
            expr = do.call(fun, args = keyArgs),
            error = function(e) {
                stop_no_call("key '", key, "': ", e$message)
            }
        )
    }

    .as_pipeflow_partitioned(out)
}

.pip_run_row <- function(x, i, lgr) {
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

    step <- dat[["step"]][[i]]

    # Keep `.self` pointing at the pipeline object being run, so steps still
    # reference the correct pipeline after cloning, subsetting or replacing.
    environment(fun)[[".self"]] <- x

    out <- withCallingHandlers(
        .pip_execute_step_call(fun = fun, args = args, exec = exec),
        error = function(e) {
            data.table::set(
                dat,
                i = i,
                j = "state",
                value = .step_states[["failed"]][["name"]]
            )
            lgr(level = "error", msg = e$message)
            stop_no_call(e$message)
        },
        warning = function(w) {
            lgr(level = "warn", msg = w$message)
        },
        message = function(m) {
            lgr(level = "info", msg = m$message)
        }
    )

    # Re-read pipeline after execution to handle scenarios where the pipeline
    # modified itself at runtime. Since we update by step name, if the current
    # step does not exist anymore, we simply skip the update.
    dat <- .pip_pipenv(x)[["data"]]
    rowNow <- data.table::chmatch(step, dat[["step"]])
    stepStillExists <- !is.na(rowNow)
    if (stepStillExists) {
        data.table::set(
            dat,
            i = rowNow,
            j = c("out", "time", "state"),
            value = list(
                list(out),
                Sys.time(),
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

    pipenv[[".run_state"]][] <- "restart"
    pipenv[[".restart_count"]] <- count + 1L
    pipenv[[".restart_force"]] <- force
    invisible()
}

.pip_stop <- function(pipenv) {
    pipenv[[".run_state"]][] <- "stop"
    invisible()
}

.pip_update_downstream <- function(x, steps, what, value) {
    nodes <- .pip_get_reachable_nodes(x, steps)
    .pip_pipenv(x)[["data"]][list(nodes), (what) := value, on = "nodeId"]

    invisible(x)
}


# ----------------------------------
# Pipeline data access and filtering
# ----------------------------------

# Convenience helper function to retrieve the shared inner environment.
.pip_pipenv <- function(x) {
    .subset2(x, "pipenv")
}

# The rows covered by `x`: all pipeline rows for a full pipeline, or the
# view's `rows` selector for a view.
.pip_view_rows <- function(x) {
    view <- .subset2(x, "view")
    if (is.null(view)) {
        seq_len(nrow(.pip_pipenv(x)[["data"]]))
    } else {
        as.integer(view)
    }
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
        dag_get_reachable_nodes_down(env[[".dag"]], start_ids)
    } else {
        dag_get_reachable_nodes_up(env[[".dag"]], start_ids)
    }
}


# -----------------
# Pipeline addition
# -----------------

.pip_append <- function(x, step, fun, tags, exec = "auto", params = list()) {
    funParams <- .extract_fun_params(fun)
    params[names(funParams)] <- funParams

    # Provide `.self` to the step via a dedicated wrapper environment.
    fun <- .wrap_self(fun, x)

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
    d <- env[[".dag"]]
    nodeId <- as.integer(dag_add_node(d))
    if (length(refNodes) > 0) {
        dag_add_edges_to(d, from = as.integer(refNodes), to = nodeId)
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
    funParams <- .extract_fun_params(fun)
    params[names(funParams)] <- funParams

    # Provide `.self` to the step via a dedicated wrapper environment.
    fun <- .wrap_self(fun, x)

    env <- .pip_pipenv(x)
    data <- env[["data"]]

    # Dependencies may only point to steps that precede the insertion point
    # (mirroring the previous step-by-step rebuild, this keeps the row order
    # a valid topological order).
    prefixSteps <- data[["step"]][seq_len(pos)]
    steps <- c(prefixSteps, step)
    depends <- .extract_depends(params = params, steps = steps)
    if (length(depends) > 0L) {
        forbidden <- depends[!(depends %in% prefixSteps)]
        if (length(forbidden) > 0L) {
            stop_no_call(
                "while adding step '",
                step,
                "' - cannot reference unknown steps: ",
                paste0("'", unname(forbidden), "'", collapse = ", ")
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
    d <- env[[".dag"]]
    nodeId <- as.integer(dag_add_node_at(d, pos))
    if (length(refNodes) > 0) {
        dag_add_edges_to(d, from = as.integer(refNodes), to = nodeId)
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
    # row (re-create the formula dependencies after renaming and re-bind
    # `.self` to the target), allocate a fresh node id, wire the DAG edges,
    # and collect the rows for a single rbindlist.
    outEnv <- .pip_pipenv(out)
    d <- outEnv[[".dag"]]
    stepsToNodes <- outEnv[[".steps_to_nodes"]]
    rows <- vector("list", nrow(yyDat))
    for (k in seq_len(nrow(yyDat))) {
        step <- yyDat[["step"]][[k]]
        fun <- yyDat[["fun"]][[k]]
        tags <- yyDat[["tags"]][[k]]
        exec <- yyDat[["exec"]][[k]]
        params <- yyDat[["params"]][[k]]
        depends <- yyDat[["depends"]][[k]]

        # Re-create the formula dependencies in the target pipeline's context
        # so that they point to the (renamed) steps of y.
        for (arg in intersect(names(depends), names(params))) {
            params[[arg]] <- stats::as.formula(paste("~", depends[[arg]]))
        }
        # Fold current parameter values into the defaults of the existing
        # function arguments.
        fml <- formals(fun)
        for (nm in intersect(names(params), setdiff(names(fml), "..."))) {
            fml[[nm]] <- params[[nm]]
        }
        formals(fun) <- fml

        fun <- .wrap_self(fun, out)

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
            dag_add_edges_to(d, from = refNodes, to = nodeId)
        }

        row <- .new_step(step, fun, params, depends, nodeId, tags, exec)
        rows[[k]] <- row
    }

    # rbind all appended rows and preserve runtime state of the source steps
    appRows <- data.table::rbindlist(rows, use.names = TRUE)
    data.table::set(
        appRows,
        j = c("out", "state", "time", "locked"),
        value = list(
            yyDat[["out"]],
            yyDat[["state"]],
            yyDat[["time"]],
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
        "ready",
        levels = c("ready", "restart", "running", "stop", "failed")
    )
    env[[".last_run"]] <- NULL

    # Restart tracking
    env[[".restart_count"]] <- 0L
    env[[".restart_force"]] <- TRUE

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
#' @param fun Function to execute for the step. Each function parameter must
#' have a default value. Default values that are simple constants are resolved
#' immediately. Default values that are formulas like `~other_step` are
#' treated as dependencies to those steps and resolved to the respective output
#' values at runtime once the step is executed.
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be adjusted later using `[pip_tag()]`.
#' @param after Optional position after which the new step should be inserted
#' (defaults to last position). Can be a step name or an integer index. If
#' set to 0, the new step will be inserted at the beginning of the pipeline.
#' @param params Optional named list of parameter values, which will be merged
#' with the defaults of `fun` (if overlapping names, the default values in `fun`
#' take precedence). There are two use cases for `params`:
#' 1. Provide param values programmatically when adding steps at runtime
#' 2. Provide extra param values to "mark" dependencies that are defined in
#'   pipelines nested in a step, which ensures that the step (and with that
#'   the pipeline in the step) is re-executed when one of the respective
#'   param values change.
#' @param exec Execution mode for this step. One of "auto", "split",
#' "reduce" or "plain".
#' Using execution mode `exec = split`, the output of the step is marked as
#' partitioned output. In this mode, any step that depends on the split step
#' (directly or indirectly) will have its output automatically mapped
#' partition-wise during step execution. The `reduce` mode expects
#' partitioned input and passes it through without mapping, while `plain`
#' mode only accepts non-partitioned input and always intends to execute
#' a single call. In summary:
#' * auto: map if partitioned input appears, otherwise single call
#' * split: single call, then mark output as partitioned
#' * reduce: single call, but only valid with partitioned input
#' * plain: single call, only valid with non-partitioned input
#'
#' @details
#' If `after` was specified, the new step will be inserted after the given
#' step or position. Be aware that in contrast to adding a step at the end,
#' inserting a step in the middle is a rather expensive operation as it
#' requires re-wiring parts of the internal pipeline structure, especially
#' if the new step is inserted at an early position.
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
#' # --- Provide parameter values programmatically with 'params' ---
#' p3 <- pip_new("params-demo") |>
#'   pip_add("load", \(x = 1, ...) c(x, ...), params = list(size = 42))
#' pip_run(p3)
#' p3
#' p3[["load", "out"]] # c(1, size = 42) — extra params passed via `...`
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
    if (!is.function(fun)) {
        stop("fun must be a function")
    }
    if (".self" %in% names(formals(fun))) {
        stop_no_call(
            "'.self' is a reserved parameter and cannot be used as a step name"
        )
    }
    if (".self" %in% names(params)) {
        stop_no_call(
            "'.self' is a reserved parameter name and cannot be used in params"
        )
    }
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
    out <- pip_new(name = newName)
    env <- .pip_pipenv(out)

    env[[".dag"]] <- dag_clone(.pip_pipenv(x)[[".dag"]])
    dat <- data.table::copy(.pip_pipenv(x)[["data"]])

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
#'
#'
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
        "step", "state", "tags", "time", "locked", "exec",
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


#' Get independent parameters
#'
#' Returns the current default values of all unbound (non-dependency)
#' parameters across the pipeline. These are the parameters that can be
#' updated via [pip_set_params()]. Parameters wired to another step's output
#' via `~step_name` are excluded.
#' @param x A pipeflow pip or view
#' @return Named list of unbound parameter values. If the same parameter
#' name appears in multiple steps, the first occurrence in pipeline order
#' is returned.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 100, seed = 42) seq_len(n)) |>
#'   pip_add("model", \(x = ~load, lambda = 0.1) x * lambda)
#'
#' # ~load is a dependency — only non-dependency params are returned
#' pip_get_params(p) # list(n = 100, seed = 42, lambda = 0.1)
#'
#' # Useful as a guide for pip_set_params()
#' pip_set_params(p, params = list(n = 20, lambda = 0.5))
#' pip_run(p) |> pip_collect()
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
    dag <- env[[".dag"]]

    rows <- .pip_view_rows(x)
    rows <- sort(unique(rows))

    if (isView && include_upstream && length(rows) > 0L) {
        startNodes <- dat[["nodeId"]][rows]
        keepNodes <- dag_get_reachable_nodes_up(
            dag,
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
#' If other steps depend on the step to be removed, an error is
#' given and the removal is blocked, unless `force` was set to
#' `TRUE`. In force mode, the selected step and all downstream
#' dependent steps are removed together.
#'
#' A view can be passed as well; the step must then be part of the view.
#' Because removing steps drops rows from the pipeline, the row positions
#' that a view selects are shifted. For views, `pip_remove()` therefore
#' remaps the selector to the remaining steps and returns the updated view,
#' so assign the result back: `v <- pip_remove(v, ...)`. Views that are not
#' reassigned, and other views over the same pipeline, keep their old row
#' positions and therefore can become invalid after a step removal.
#'
#' @param x A pipeflow pip or view
#' @param step `string` the name of the step to be removed.
#' @param force `logical` if `TRUE` the step is removed together
#' with all its downstream dependencies.
#' @return The updated pipeline, invisibly. For a view, the view with its
#' remapped selector.
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
#' # If a view is passed as well, the step must be part of the view.
#' v <- pip_view(p, step = "transform")
#' try(pip_remove(v, "load"))  # Error: "load" is not part of the view
#' v <- pip_remove(v, "transform")
#' v[["step"]]              # view is remapped and stays valid
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

    # For views, remember the covered steps so that the view can be remapped.
    isView <- .is_pipeflow_view(x)
    if (isView) {
        viewSteps <- dat[["step"]][.pip_view_rows(x)]
    }

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
    for (nid in rev(nodesToRemove)) {
        ok <- dag_remove_node(pipenv[[".dag"]], nid, force = force)
        if (!ok) {
            stop("failed to remove node ", nid, " from DAG")
        }
    }

    dag_tidy_up(pipenv[[".dag"]])
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

    # Remap the view to the shifted (and potentially dropped) row positions.
    if (isView) {
        remaining <- pipenv[["data"]][["step"]]
        newRows <- as.integer(which(remaining %chin% viewSteps))
        x <- .wrap_pipenv(pipenv, name = x[["name"]], view = newRows)
    }
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
                # Only swap the referenced name to keep the formula's
                # environment (as.formula() would attach this frame).
                fml[[2L]] <- as.name(dep[[arg]])
                newParams[[i]][[arg]] <- fml
            }
        }
    }

    data.table::set(dat, j = "step", value = newSteps)
    data.table::set(dat, j = "depends", value = newDepends)
    data.table::set(dat, j = "params", value = list(newParams))

    stepsToNodes <- env[[".steps_to_nodes"]]
    nodeId <- stepsToNodes[[from]]
    stepsToNodes[[to]] <- nodeId
    rm(list = from, envir = stepsToNodes, inherits = FALSE)

    data.table::setindexv(dat, list("step", "nodeId"))
    invisible(x)
}


#' Replace a step
#'
#' Replaces a step's function while keeping it in the same position in the
#' pipeline. Downstream steps are automatically marked as outdated and will
#' re-run on the next [pip_run()].
#'
#' @param x A pipeflow pipeline or view object.
#' @param step Step name.
#' @param fun Function to execute for the step.
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be adjusted later using `[pip_tag()]`.
#' @param params Optional named list of parameter values, which will be merged
#' with the defaults of `fun` (if overlapping names, the default values in `fun`
#' take precedence). There are two use cases for `params`:
#' 1. Provide param values programmatically when adding steps at runtime
#' 2. Provide extra param values that are defined in pipelines nested in a
#'   step, which ensures that the step (and with that the pipeline in the step)
#'   is re-executed when one of the respective param values change.
#' @param exec Execution mode for this step. One of "auto", "split",
#' "reduce" or "plain".
#' Using execution mode `exec = split`, the output of the step is marked as
#' partitioned output. In this mode, any step that depends on the split step
#' (directly or indirectly) will have its output automatically mapped
#' partition-wise during step execution. The `reduce` mode expects
#' partitioned input and passes it through without mapping, while `plain`
#' mode only accepts non-partitioned input and always intends to execute
#' a single call. In summary:
#' * auto: map if partitioned input appears, otherwise single call
#' * split: always split
#' * reduce: single call, but only valid with partitioned input
#' * plain: single call, only valid with non-partitioned input
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
#' @export
pip_replace <- function(
    x,
    step,
    fun,
    tags = character(0),
    params = list(),
    exec = "auto"
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
    if (!is.function(fun)) {
        stop("fun must be a function")
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
    .assert_exec_mode(exec)

    env <- .pip_pipenv(x)
    data <- env[["data"]]
    iStep <- data.table::chmatch(step, data[["step"]])

    funParams <- .extract_fun_params(fun)
    params[names(funParams)] <- funParams

    # Provide `.self` to the step via a dedicated wrapper environment.
    fun <- .wrap_self(fun, x)

    # Dependencies of the replacement may only point to earlier steps
    prefixSteps <- data[["step"]][seq_len(iStep - 1L)]
    steps <- c(prefixSteps, step)
    depends <- .extract_depends(params = params, steps = steps)
    if (length(depends) > 0L) {
        bad <- depends[!(depends %in% prefixSteps)]
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
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
    if (anyNA(refNodes)) {
        notFound <- Filter(is.na, refNodes)
        stop_no_call(
            "while adding step '",
            step,
            "' - cannot reference unknown steps: ",
            paste0("'", notFound, "'", collapse = ", ")
        )
    }

    # Update the incoming DAG edges of the replaced step (its node and all
    # outgoing edges stay the same).
    d <- env[[".dag"]]
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

    # Reset the step row: new function, params, tags and exec, fresh runtime
    # state (like a freshly added step).
    data.table::set(
        data,
        i = iStep,
        j = c(
            "fun",
            "params",
            "depends",
            "unbound",
            "tags",
            "exec",
            "out",
            "state",
            "time",
            "locked"
        ),
        value = list(
            list(fun),
            list(params),
            list(depends),
            list(setdiff(names(params), names(depends))),
            list(tags),
            exec,
            list(NULL),
            .step_states[["new"]][["name"]],
            Sys.time(),
            FALSE
        )
    )

    # Mark downstream dependent steps as outdated, but keep the replaced
    # step itself as "new".
    downNodes <- .pip_get_reachable_nodes(x, step)
    downNodes <- unique(setdiff(
        as.integer(unlist(downNodes)),
        as.integer(nodeId)
    ))
    if (length(downNodes) > 0L) {
        rowsDown <- data[
            list(downNodes),
            which = TRUE,
            on = "nodeId"
        ]
        if (length(rowsDown) > 0L) {
            data.table::set(
                data,
                i = rowsDown,
                j = "state",
                value = .step_states[["outdated"]][["name"]]
            )
        }
    }

    invisible(x)
}


#' Run a pipeline
#'
#' Executes all pending steps in order. Steps already in state `"done"` are
#' skipped unless `force = TRUE`.
#'
#' @param x A pipeflow pip or view
#' @param lgr A logging function of the form `function(level, msg, ...)`.
#' To suppress logging, you can set `lgr = NULL`.
#' @param force Logical indicating if all steps should be forced to run,
#' regardless of whether they are outdated or not.
#' @param progress Optional callback of the form
#' `function(value, detail)` called before each step.
#' @return The updated pipeline or view, invisibly.
#' @details
#' When `x` is a view, requested rows are run together with required
#' upstream dependencies. If a step fails, the pipeline run state is set to
#' `"failed"` and the error is re-thrown.
#'
#' ## Runtime control flow via restart and stop
#'
#' A running pipeline can be interrupted via the `restart()` and `stop()`
#' functions that are attached to every pipeline object. They are intended for
#' advanced, self-modifying pipelines and are most often called from within a
#' step function via the `.self` argument.
#'
#' - `.self$restart(force = TRUE, times = 1L)`: aborts the current run after the
#'   current step has finished and restarts it from the first step.
#'   The default parameters are `force = TRUE` and `times = 1L`, that is, the
#'   above call is the same as just invoking .self$restart().
#'   To skip steps that are already in state `"done"`, set `force = FALSE`,
#'   and `times` parameter limits the number of consecutive restarts within a
#'   single `pip_run()` call.
#'   If a view is being run, a restart covers the view steps together with
#'   their upstream dependencies.
#' - `p$stop()`: aborts the current run after the current step has finished.
#'
#' In both cases steps that have not been executed until the restart or stop
#' happens are marked as `"outdated"`.
#'
#' @seealso `vignette("v06-self-modify-pipeline", package = "pipeflow")`
#'   for an advanced example of dynamic pipelines.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("square", \(x = ~load) x^2) |>
#'   pip_add("total", \(x = ~square) sum(x))
#'
#' pip_run(p)
#' p
#'
#' # Already-done steps are skipped on a second run
#' pip_run(p) # all steps skipped
#'
#' # lgr = NULL suppresses log output
#' pip_run(p, lgr = NULL)
#'
#' # force = TRUE re-executes every step regardless of state
#' pip_run(p, force = TRUE)
#'
#' # Run only a subset of steps via a view;
#' # upstream dependencies are automatically included
#' v <- pip_view(p, step = "total")
#' pip_run(v)
#'
#' # Stop or restart pipeline at runtime
#' p <- pip_new("restart") |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("check", \(n = ~load) {
#'       if (length(x) > 10L) .self$stop()
#'   }) |>
#'   pip_add("model", \(x = ~load) {
#'       if (length(x) == 3L) .self$restart()
#'       x * 2
#'   })
#' @export
pip_run <- function(
    x,
    lgr = pipeflow_lgr,
    force = FALSE,
    progress = NULL
) {
    .assert_pip_or_view(x)
    if (!.is_single(force, "logical")) {
        stop("force must be a single logical value")
    }
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
    processedSteps <- character()
    restartDelegated <- FALSE
    on.exit({
        # At the end, mark all downstream dependent steps as outdated that
        # were *not* processed, which can happen in two different ways:
        # a) when running a view that does not cover the entire pipeline or
        # b) the run was aborted in the middle (due to an error or manual stop).
        # When the run was restarted, a nested pip_run() has already handled
        # the whole pipeline (including marking), so nothing to do here.
        if (restartDelegated) {
            return(NULL)
        }
        processedNodes <- as.integer(.pip_steps_to_nodes(x, processedSteps))
        outdatedNodes <- .pip_get_reachable_nodes(x, processedSteps) |>
            unlist() |>
            unique() |>
            setdiff(processedNodes)
        if (length(outdatedNodes) > 0L) {
            iOut <- dat[list(outdatedNodes), which = TRUE, on = "nodeId"]
            if (length(iOut) > 0L) {
                data.table::set(dat, i = iOut, j = "state", value = "outdated")
            }
        }
    })

    state <- pipenv[[".run_state"]]
    action <- if (state == "restart") "Restarting" else "Starting"
    log_info(sprintf("%s run of %s '%s'", action, data.class(x), pipname))
    pipenv[[".run_state"]][] <- "running"
    tryCatch(
        {
            for (i in seq_along(rowsToRun)) {
                row <- rowsToRun[[i]]
                step <- dat[["step"]][[row]]
                processedSteps <- c(processedSteps, step)
                if (!is.null(progress)) {
                    progress(value = i, detail = step)
                }
                msg <- if (isView) {
                    marker <- names(rowsToRun)[[i]]
                    sprintf(
                        "Step %i/%i [%s] %s",
                        i,
                        length(rowsToRun),
                        marker,
                        step
                    )
                } else {
                    sprintf("Step %i/%i %s", i, length(rowsToRun), step)
                }

                if (dat[["state"]][[row]] == "done" && !force) {
                    log_info(sprintf("%s - skipping done step", msg))
                    next()
                }
                if (dat[["locked"]][[row]]) {
                    log_info(sprintf("%s - skipping locked step", msg))
                    next()
                }

                # Always pass the full pipeline object (not a view) to the
                # step function, so that it can modify itself if needed.
                self <- .wrap_pipenv(pipenv, pipname, view = NULL)

                log_info(msg)
                .pip_run_row(x = self, i = row, lgr = lgr)
                stateAfterStep <- pipenv[[".run_state"]]

                # Check for restart or stop signals
                if (stateAfterStep == "restart") {
                    log_info("Restarting pipeline execution.")
                    doForce <- pipenv[[".restart_force"]]
                    restartDelegated <- TRUE
                    pip_run(
                        x,
                        lgr = lgr,
                        force = doForce,
                        progress = progress
                    )
                    return(invisible(x))
                }

                if (stateAfterStep == "stop") {
                    log_info("Aborting pipeline execution on manual stop.")
                    break
                }
            }

            log_info(
                sprintf("Finished run of %s '%s'", data.class(x), pipname)
            )
            pipenv[[".run_state"]][] <- "ready"
            pipenv[[".last_run"]] <- Sys.time()
            invisible(x)
        },
        error = function(e) {
            pipenv[[".run_state"]][] <- "failed"
            pipenv[[".last_run"]] <- Sys.time()
            stop_no_call(e$message)
        }
    )
}


#' Reset a pipeline to its initial state
#'
#' Resets all unlocked steps of a pipeline (or a subset of steps defined by a
#' view) to state `"new"` and clears their outputs, so a subsequent
#' [pip_run()] re-executes the cleaned steps from scratch. The run state is
#' reset to `"ready"` and any pending restart counter is cleared. Parameters,
#' tags, and locked flags are left unchanged.
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
#' p[["state"]] # "new", "done"
#' p[["out"]]   # NULL, (x^2 result)
#'
#' pip_unlock(p)
#' pip_reset(p)
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
        j = c("out", "state"),
        value = list(
            rep(list(NULL), length(rowsConsidered)),
            rep(.step_states[["new"]][["name"]], length(rowsConsidered))
        )
    )

    env[[".run_state"]][] <- "ready"
    env[[".restart_count"]] <- 0L
    env[[".last_run"]] <- NULL

    invisible(x)
}


#' Set independent parameters
#'
#' Updates the default values of unbound parameters across the pipeline
#' or a subset of steps defined by a view.
#' Affected steps and their downstream dependents are automatically marked
#' as outdated.
#' @details Parameters of locked steps are never changed and their state
#' remains unchanged.
#' @param x A pipeflow pip or view
#' @param params Named list of parameters to set.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 10) seq_len(n)) |>
#'   pip_add("scale", \(x = ~load, factor = 0.5) x * factor)
#'
#' # See all adjustable parameters before running
#' pip_get_params(p) # list(n = 10, factor = 0.5)
#'
#' # Updating params marks affected steps (and their dependents) outdated
#' pip_set_params(p, params = list(n = 5, factor = 2.0))
#' p
#'
#' pip_run(p)
#' p
#' @export
pip_set_params <- function(x, params = list()) {
    # Input checking
    .assert_pip_or_view(x)
    if (!is.list(params)) {
        stop("params must be a list")
    }
    parNames <- names(params)
    if (length(params) == 0) {
        return(invisible(x))
    }
    allNamed <- length(parNames) == length(params) && all(nzchar(parNames))
    if (!allNamed) {
        stop("All parameters must be named")
    }

    # Narrow down the considered rows
    env <- .pip_pipenv(x)
    dat <- env[["data"]]
    rows <- .pip_view_rows(x)
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

    # Signal parameters that are not defined in any of the affected steps
    used <- unique(unlist(intersects))
    undefined <- setdiff(parNames, used)
    if (length(undefined) > 0L) {
        warning(
            "Trying to set parameters not defined in the target: ",
            toString(undefined)
        )
    }

    if (any(hasOverlap)) {
        # Update parameters in all affected rows
        for (j in seq_along(rowsAffected)) {
            i <- rowsAffected[[j]]
            names <- namesAffected[[j]]
            rowPars <- dat[["params"]][[i]]
            rowPars[names] <- params[names]
            value <- list(list(rowPars)) # need to wrap in list() for call below
            data.table::set(dat, i = i, j = "params", value = value)
        }

        # Update states of affected steps and their downstream steps
        steps <- dat[["step"]][rowsAffected]
        .pip_update_downstream(
            x,
            steps = steps,
            what = "state",
            value = "outdated"
        )
    }

    invisible(x)
}


#' Add tags to selected steps
#'
#' Adds tags to existing tags for all steps of a pipeline or a subset
#' of steps defined by a view.
#' @param x A pipeflow pip or view.
#' @param tags Character vector of tags to add for each selected step.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#'
#' # Tag every step in the pipeline at once
#' pip_tag(p, tags = c("daily", "core"))
#' p[["tags"]] # both steps have c("daily", "core")
#'
#' # Add an extra tag to only one step via a view
#' v <- pip_view(p, step = "fit")
#' pip_tag(v, tags = "model")
#' p[["tags"]] # "fit" also has "model"
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


#' Remove tags from selected steps
#'
#' Removes tags from existing tags for all steps of a pipeline or a subset
#' of steps defined by a view.
#' @param x A pipeflow pip or view.
#' @param tags Character vector of tags to remove for each selected step.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x, tags = c("daily", "core")) |>
#'   pip_add("fit", \(x = ~load) x + 1, tags = c("daily", "model"))
#'
#' # Remove "daily" from all steps
#' pip_untag(p, tags = "daily")
#' # "load" retains "core"; "fit" retains "model"
#' p[["tags"]]
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


#' Lock steps against updates
#'
#' Locks all steps of a pipeline or a subset of steps defined by a view.
#' Locked steps are skipped during [pip_run()] and cannot be modified by
#' [pip_set_params()], [pip_tag()] or [pip_untag()].
#' @param x A pipeflow pip or view.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'     pip_add("x", \(x = 1) x) |>
#'     pip_add("y", \(y = 2) y) |>
#'     pip_add("sum", \(x = 1, y = 2) x + y)
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

# Unlock everything to allow updates again
#' pip_unlock(p)
#' pip_set_params(p, params = list(x = 100, y = 200))
#' (pip_run(p))
#' p[["sum", "out"]] # 300
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


#' Unlock steps
#'
#' Unlocks all steps of a pipeline or a subset of steps defined by a view.
#' @param x A pipeflow pip or view.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x * 2)
#'
#' # Lock all steps, then unlock to restore normal execution
#' pip_lock(p)
#' p[["locked"]] # TRUE, TRUE
#'
#' pip_unlock(p)
#' p[["locked"]] # FALSE, FALSE
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
#' affect only the selected steps.
#'
#' @param x A pipeflow pipeline or view.
#' @param ... Named filters. Supported filter names are `step`, `params`,
#' `depends`, `state`, `tags` and `exec`. Each filter value is a character
#' vector of values to keep, or - if `fixed` is `FALSE` - a regular
#' expression. The `params` filter matches against the actual parameter
#' names of each step.
#' @param join How individual filters are combined: `"intersect"` (the
#' default) keeps steps that match *all* filters, `"union"` keeps steps
#' that match *any* filter. Within a single filter, multiple values are
#' always treated as alternatives (OR).
#' @param fixed If TRUE, values in `...` are treated as fixed strings,
#' otherwise they are treated as regular expressions.
#'
#' @return A `pipeflow_view` object.
#' @export
#' @examples
#'
#' p <- pip_new()
#' pip_add(p, "load_raw", \(x = 1) x,
#'   tags = c("io", "core", "daily")
#' )
#' pip_add(p, "fit_model", \(x = 2) x + 1,
#'   tags = c("model")
#' )
#' pip_add(p, "eval_model", \(x = ~fit_model) x,
#'   tags = c("model", "daily", "report")
#' )
#'
#' # Filter by a fixed column value (one or more states)
#' pip_view(p, state = "new")
#'
#' # Combine filters: step pattern AND state (logical AND)
#' pip_view(p, step = "model", state = "new")
#'
#' # Combine filters as a union (step OR state)
#' pip_view(p, step = "load_raw", state = "done", join = "union")
#'
#' # Filter by tag — keeps steps that have *any* of the given tags
#' pip_view(p, tags = "daily")
#'
#' # Filter by step name
#' pip_view(p, step = c("load_raw", "fit_model"))
#'
#' # Use a regex pattern to match step names
#' pip_view(p, step = "_model$", fixed = FALSE)
#'
#' # Filter by parameter names — steps with any of the given parameters
#' pip_view(p, params = c("x", "n"))
#'
#' # Views are composable: create a view-of-view for progressive narrowing
#' v1 <- pip_view(p, tags = "daily")
#' print(v1) # load_raw, eval_model
#' v2 <- pip_view(v1, tags = "report")
#' print(v2) # eval_model only
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
    .wrap_pipenv(env, name = sprintf("%s view", x[["name"]]), view = rows)
}
