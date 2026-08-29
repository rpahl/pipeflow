# ---------------------
# Pipeline construction
# ---------------------
.empty_pipeline <- function() {
    data.table::data.table(
        step = character(0),
        fun = list(),
        params = list(),
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
    fun,
    params,
    depends,
    .nodeId,
    tags = character(0),
    exec = "auto"
) {
    list(
        step = step,
        fun = list(fun),
        params = list(params),
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


# ---------------
# Type predicates
# ---------------
.is_pipeflow_pip <- function(x) {
    inherits(x, "pipeflow_pip")
}

# A view is a pipeflow_pip whose `rows` field is not NULL.
.is_pipeflow_view <- function(x) {
    inherits(x, "pipeflow_pip") && !is.null(x[["view"]])
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
}

.assert_logger <- function(lgr) {
    if (!is.function(lgr)) {
        stop("lgr must be a function")
    }
    if (!all(c("level", "msg") %in% names(formals(lgr)))) {
        stop("lgr must be a function with arguments 'level' and 'msg'")
    }
}

.assert_pip_or_view <- function(x) {
    if (!.is_pipeflow_pip(x)) {
        stop_no_call("x must be a pipeflow pip or view")
    }
}

# Structural operations require a full pipeline, not a view.
.assert_pip <- function(x) {
    if (!.is_pipeflow_pip(x)) {
        stop_no_call("x must be a pipeflow pip")
    }
    if (.is_pipeflow_view(x)) {
        stop_no_call("x must be a full pipeline, not a view")
    }
}

# The shared inner environment holding all mutable state.
# If `x` is already the inner environment, it is returned unchanged.
.pip_get_pip_env <- function(x) {
    .subset2(x, "pipenv")
}

# A full pipeflow_pip wrapper (view = NULL) around the shared inner
# environment. Used for `.self` inside steps so structural operations such as
# pip_replace() work on the underlying full pipeline.
.pip_full_pip <- function(x) {
    structure(
        list(
            pipenv = .subset2(x, "pipenv"),
            name = .subset2(x, "name"),
            view = NULL
        ),
        class = "pipeflow_pip"
    )
}

# Create a view: a pipeflow_pip sharing the inner environment with the parent
# by reference, selecting only the given absolute row indices.
.pip_make_view <- function(x, rows) {
    structure(
        list(
            pipenv = .subset2(x, "pipenv"),
            name = sprintf("%s view", .subset2(x, "name")),
            view = as.integer(rows)
        ),
        class = "pipeflow_pip"
    )
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

    # Make sure default values are returned as resolved values by evaluating
    # them in the function's environment
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
    if (!is.integer(toPos)) {
        stop_no_call("toPos must be an integer")
    }
    if (toPos > length(steps)) {
        stop_no_call("toPos exceeds number of steps")
    }

    # References to other steps are marked using a formula and can be either
    # referencing earlier steps (e.g. x = ~step1) or using positional indices
    # by pointing backwards a certain number of steps (e.g. x = ~-1)
    depends <- formula_deps(params)
    if (length(depends) == 0) {
        return(character(0))
    }

    # Finally, convert any relative dependencies (those marked with a
    # leading "-") to step names.
    rel_pos_to_step_num <- function(relPos, startPos) {
        if (startPos < 1) {
            stop_no_call("startPos must be at least 1")
        }
        stepNumber <- startPos - relPos
        if (stepNumber < 1) {
            stop_no_call("relative index -", relPos, " points outside pipeline")
        }
        stepNumber
    }
    iRelPos <- which(depends |> startsWith("-"))
    stepNumbers <- depends[iRelPos] |>
        lapply(FUN = \(x) rel_pos_to_step_num(abs(as.integer(x)), toPos))
    depends[iRelPos] <- steps[as.integer(stepNumbers)]

    unlist(depends)
}


# ---------------------
# Partitioned execution
# ---------------------
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

.partition_keys <- function(x) {
    if (!.is_pipeflow_partitioned(x)) {
        stop("x must be a pipeflow_partitioned object")
    }
    names(x)
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
    keys <- .partition_keys(args[[partIdx[[1]]]])
    for (k in partIdx[-1]) {
        kk <- .partition_keys(args[[k]])
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


# --------------------
# Pipeline data access
# --------------------
.pip_data <- function(x) {
    rows <- .pip_view_rows(x)
    .pip_get_pip_env(x)[["data"]][rows, ]
}

# The rows covered by `x`: all pipeline rows for a full pipeline, or the
# view's `rows` selector for a view.
.pip_view_rows <- function(x) {
    if (is.null(.subset2(x, "view"))) {
        seq_len(nrow(.pip_get_pip_env(x)[["data"]]))
    } else {
        as.integer(.subset2(x, "view"))
    }
}

# Internal implementation of [[ for pipeflow_pip objects. Views are pips with
# a `rows` selector, so list fields and inner-env bindings are accessed
# through the same dispatch.
.pip_subset2 <- function(x, i, j = NULL, ...) {
    dat <- .pip_get_pip_env(x)[["data"]]
    rows <- .pip_view_rows(x)

    if (is.null(j)) {
        if (missing(i)) {
            stop("i must be provided")
        }

        # List fields of the wrapper have priority over column names.
        if (is.character(i) && length(i) == 1L && !is.na(i)) {
            if (i %in% c("pipenv", "name", "view")) {
                return(.subset2(x, i))
            }
            # Public inner-env bindings like "data" are next. Hidden
            # internals like ".dag" and ".steps_to_nodes" are deliberately
            # not exposed. They can still be accessed "manually" from the
            # inner environment if needed.
            env <- .pip_get_pip_env(x)
            if (i %in% ls(env)) {
                # ls() by default does not list variables starting with a dot
                return(get(i, envir = env, inherits = FALSE))
            }
        }

        # Column access, restricted to the view's rows for views. Name output
        # vector by step names for easier inspection and post-processing.
        col <- dat[[i]][rows]
        if (!is.null(col)) {
            return(stats::setNames(col, dat[["step"]][rows]))
        }
        return(col)
    }

    # Two-index form extracts a single cell from a single row.
    if (missing(i)) {
        stop("i must be provided")
    }
    if (length(i) != 1L || is.na(i)) {
        stop("i must be a single step name or row index")
    }
    if (!is.character(j) || length(j) != 1L || is.na(j)) {
        stop("j must be a single column name")
    }

    if (is.character(i)) {
        row <- .pip_steps_to_rows(x, i)
        if (!(row %in% rows)) {
            stop("undefined step selected")
        }
    } else {
        if (!is.finite(i) || i != as.integer(i)) {
            stop("row index must be a whole number")
        }
        row <- as.integer(i)
        if (.is_pipeflow_view(x)) {
            if (row < 1L || row > length(rows)) {
                stop("row index out of bounds")
            }
            row <- rows[row]
        } else if (row < 1L || row > nrow(dat)) {
            stop("row index out of bounds")
        }
    }
    dat[[j]][[row]]
}

.pip_filter <- function(x, on, values) {
    x[["data"]][list(values), on = on]
}

.pip_filter_nodes <- function(x, nodes) {
    x[["data"]][list(nodes), on = ".nodeId"]
}


# --------
# Indexing
# -------.
.pip_is_indexed <- function(x) {
    !is.null(data.table::indices(x[["data"]]))
}

.pip_reindex <- function(x) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    data.table::setindexv(x[["data"]], list("step", ".nodeId"))
}


# ---------------------------
# Step lookup & DAG traversal
# ---------------------------
.pip_step_exists <- function(x, step) {
    exists(
        step,
        where = .pip_get_pip_env(x)[[".steps_to_nodes"]],
        inherits = FALSE
    )
}

.pip_steps_to_nodes <- function(x, steps) {
    mget(
        steps,
        envir = .pip_get_pip_env(x)[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
}

.pip_steps_to_rows <- function(x, steps) {
    dat <- x[["data"]]

    if (anyNA(steps)) {
        stop("step names must not contain NA", call. = FALSE)
    }
    if (!all(nzchar(steps))) {
        stop("step names must be non-empty strings", call. = FALSE)
    }

    i <- data.table::chmatch(steps, dat[["step"]])
    if (anyNA(i)) {
        unknown <- unique(steps[is.na(i)])
        stop("Unknown step names: ", toString(unknown), call. = FALSE)
    }
    as.integer(i)
}

.pip_get_reachable_nodes <- function(x, steps, downstream = TRUE) {
    env <- .pip_get_pip_env(x)
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


# -------
# Step execution
# -------

# Wrap a step function so that `.self` is available in its body. The wrapper
# gets its own environment holding the pipeline reference, so the original
# function object is never mutated.
.wrap_self <- function(fun, self) {
    env <- new.env(parent = environment(fun))
    env[[".self"]] <- self
    eval(call("function", formals(fun), body(fun)), envir = env)
}

# Copy a step from another pipeline
.pip_add_from <- function(x, y, step) {
    iStep <- data.table::chmatch(step, y[["data"]][["step"]])
    fun <- y[["data"]][["fun"]][[iStep]]
    tags <- y[["data"]][["tags"]][[iStep]]
    exec <- y[["data"]][["exec"]][[iStep]]
    params <- y[["data"]][["params"]][[iStep]]
    depends <- y[["data"]][["depends"]][[iStep]]
    indeps <- y[["data"]][[".indeps"]][[iStep]]

    # Recreate defaults from stored params/dependencies so pip_add can
    # resolve references and wire DAG updates in the target pipeline.
    fml <- formals(fun)
    for (nm in indeps) {
        fml[[nm]] <- params[[nm]]
    }

    if (length(depends) > 0L) {
        for (arg in names(depends)) {
            fml[[arg]] <- stats::as.formula(paste("~", depends[[arg]]))
        }
    }

    formals(fun) <- fml
    pip_add(x, step = step, fun = fun, tags = tags, exec = exec)
}


.pip_append <- function(x, step, fun, tags, exec = "auto", params = list()) {
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

    funParams <- .extract_fun_params(fun)
    params[names(funParams)] <- funParams

    # Provide `.self` to the step via a dedicated wrapper environment.
    fun <- .wrap_self(fun, x)

    # Determine and verify potential links to existing steps
    env <- .pip_get_pip_env(x)
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
    .nodeId <- as.integer(dag_add_node(d))
    if (length(refNodes) > 0) {
        dag_add_edges_to(d, from = as.integer(refNodes), to = .nodeId)
    }

    # Create and append step
    newStep <- .new_step(
        step = step,
        fun = fun,
        params = params,
        depends = depends,
        tags = tags,
        exec = exec,
        .nodeId = .nodeId
    )

    env[["data"]] <- data.table::rbindlist(list(env[["data"]], newStep))
    env[[".steps_to_nodes"]][[step]] <- .nodeId
    x
}

# Bind two pipelines together by concatenating their steps.
.pip_bind <- function(x, y) {
    out <- pip_clone(x, name = paste0(x[["name"]], "-", y[["name"]]))
    yy <- pip_clone(y)
    yyDat <- yy[["data"]]

    # Resolve all name clashes directly on the cloned source pipeline.
    reserved <- out[["data"]][["step"]]

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

    # Add (potentially renamed) steps from y one by one via .pip_add_from.
    for (k in seq_len(nrow(yyDat))) {
        step <- yyDat[["step"]][[k]]
        .pip_add_from(out, y = yy, step = step)

        # Preserve runtime state from source pipeline.
        iOut <- nrow(out[["data"]])
        data.table::set(
            out[["data"]],
            i = iOut,
            j = c("out", "time", "state", "locked"),
            value = list(
                list(yyDat[["out"]][[k]]),
                yyDat[["time"]][[k]],
                yyDat[["state"]][[k]],
                yyDat[["locked"]][[k]]
            )
        )
    }

    out
}


.pip_run_row <- function(x, i, lgr) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.pip_is_indexed(x)) {
        .pip_reindex(x)
    }

    dat <- x[["data"]]
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
    dat <- x[["data"]]
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
.pip_update_downstream <- function(x, steps, what, value) {
    nodes <- .pip_get_reachable_nodes(x, steps)
    x[["data"]][list(nodes), (what) := value, on = ".nodeId"]

    invisible(x)
}


# ---------------------------
# Exported pipeline functions
# ---------------------------

#' Create a pipeline
#'
#' Creates a new, empty pipeline. Add steps with [pip_add()] and execute
#' them with [pip_run()].
#'
#' @param name Single name used for printing and for derived view names.
#'
#' @return A pipeflow pipeline object.
#' @examples
#' p <- pip_new("demo") |>
#'     pip_add("numbers", \(n = 5) seq_len(n)) |>
#'     pip_add("squared", \(x = ~numbers) x^2) |>
#'     pip_add("total",   \(x = ~squared) sum(x))
#' p
#' str(p)
#' p[["name"]]
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

    # Outer list wrapper: `rows` is NULL for a full pipeline and holds the
    # selected absolute row indices for a view.
    structure(
        list(pipenv = env, name = name, view = NULL),
        class = "pipeflow_pip"
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
#' * split: single call, then mark output as partitioned
#' * reduce: single call, but only valid with partitioned input
#' * plain: single call, only valid with non-partitioned input
#'
#' @details
#' Each step automatically has access to the pipeline object via `.self`,
#' without needing to declare it as a parameter. This is useful for dynamic
#' pipelines, e.g. to call [pip_restart()] or [pip_stop()] from within a
#' step. `.self` is a reserved parameter name and must neither be declared
#' in the step signature nor be passed via `params`.
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
        last <- x[["data"]][["step"]][n]
        if (after != last) {
            pos <- data.table::chmatch(after, x[["data"]][["step"]])
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
        # Step is added at the end (simplest case)
        .pip_append(
            x,
            step = step,
            fun = fun,
            tags = tags,
            exec = exec,
            params = params
        )
        return(invisible(x))
    }

    # The step is inserted in the middle of the pipeline, which would require
    # to re-wire the DAG. Instead of trying to do that in place, we take a
    # simpler approach and just 1) create a new pipeline, 2) copy all steps up
    # to the insertion point, 3) add the new step, and then 4) re-add all
    # remaining steps after that:
    # 1) Copy the pipeline
    src <- pip_clone(x)
    dat <- src[["data"]]
    n <- nrow(dat)

    # 2) Create a new pipeline and copy all steps up to the insertion point
    out <- if (pos > 0L) {
        src[seq_len(pos), view = FALSE]
    } else {
        pip_new(name = src[["name"]])
    }

    # 3) Add the new step at the end of the new pipeline
    pip_add(out, step = step, fun = fun, tags = tags, exec = exec)

    # 4) Add all remaining steps to the end of the new pipeline
    tailRows <- seq.int(pos + 1L, n)
    for (i in tailRows) {
        tailStep <- dat[["step"]][[i]]
        .pip_add_from(out, y = src, step = tailStep)

        iOut <- nrow(out[["data"]])
        data.table::set(
            out[["data"]],
            i = iOut,
            j = c("out", "time", "state", "locked"),
            value = list(
                list(dat[["out"]][[i]]),
                dat[["time"]][[i]],
                dat[["state"]][[i]],
                dat[["locked"]][[i]]
            )
        )
    }

    x[["data"]] <- out[["data"]]
    env <- .pip_get_pip_env(x)
    env[[".dag"]] <- .pip_get_pip_env(out)[[".dag"]]
    env[[".steps_to_nodes"]] <- .pip_get_pip_env(out)[[".steps_to_nodes"]]
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

    out[[".dag"]] <- dag_clone(.pip_get_pip_env(x)[[".dag"]])
    dat <- data.table::copy(x[["data"]])
    out[["data"]] <- dat

    # Clone steps to nodes mapping
    stepsToNodes <- .pip_get_pip_env(out)[[".steps_to_nodes"]]
    for (k in seq_len(nrow(dat))) {
        step <- dat[["step"]][[k]]
        nodeId <- dat[[".nodeId"]][[k]]
        stepsToNodes[[step]] <- nodeId
    }

    out
}


#' Collect step outputs
#'
#' Returns the outputs of all pipeline steps as a flat named list keyed by
#' step name. Use [pip_view()] to narrow the selection before collecting,
#' and compose calls if grouped output is needed.
#' @param x A pipeflow pip or view.
#' @return A named list of outputs, one element per step.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x, tags = "io") |>
#'   pip_add("clean", \(x = ~load) x + 1, tags = "io") |>
#'   pip_add("model", \(x = ~clean) x * 2, tags = "model")
#' pip_run(p)
#'
#' # Flat named list with one entry per step
#' pip_collect_out(p)
#'
#' # Combine with pip_view to collect output for specific tags
#' grouped <- list(
#'   io = pip_view(p, tags = "io") |> pip_collect_out(),
#'   model = pip_view(p, tags = "model") |> pip_collect_out()
#' )
#' grouped
#' @export
pip_collect_out <- function(x) {
    .assert_pip_or_view(x)
    dat <- .pip_data(x)
    if (nrow(dat) == 0) {
        return(list())
    }
    stats::setNames(dat[["out"]], dat[["step"]])
}

#' Get independent parameters
#'
#' Returns the current default values of all tunable (non-dependency)
#' parameters across the pipeline. These are the parameters that can be
#' updated via [pip_set_params()]. Parameters wired to another step's output
#' via `~step_name` are excluded.
#' @param x A pipeflow pip or view
#' @return Named list of tunable parameter values. If the same parameter
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
#' pip_run(p) |> pip_collect_out()
#' @export
pip_get_params <- function(x) {
    .assert_pip_or_view(x)
    dat <- .pip_data(x)

    params <- mapply(
        par = dat[["params"]],
        indeps = dat[[".indeps"]],
        FUN = \(par, indeps) par[indeps],
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
#' graph <- pip_get_graph(p)
#' graph$nodes # data.frame: id, label, shape, color
#' graph$edges # data.frame: from, to, arrows
#'
#' # For a view, include_upstream = TRUE adds upstream deps to the graph
#' v <- pip_view(p, step = "fit")
#' pip_get_graph(v, include_upstream = TRUE)
#'
#' if (require("visNetwork", quietly = TRUE)) {
#'   do.call(what = visNetwork::visNetwork, args = graph)
#' }
pip_get_graph <- function(x, include_upstream = FALSE) {
    .assert_pip_or_view(x)
    if (!.is_single(include_upstream, "logical")) {
        stop("include_upstream must be a single logical value")
    }

    isView <- .is_pipeflow_view(x)
    env <- .pip_get_pip_env(x)
    dat <- env[["data"]]
    dag <- env[[".dag"]]

    rows <- .pip_view_rows(x)
    rows <- sort(unique(rows))

    if (isView && include_upstream && length(rows) > 0L) {
        startNodes <- dat[[".nodeId"]][rows]
        keepNodes <- dag_get_reachable_nodes_up(
            dag,
            as.integer(unique(startNodes))
        )
        rows <- which(dat[[".nodeId"]] %in% keepNodes)
        rows <- sort(unique(as.integer(rows)))
    }

    sub <- dat[rows]

    # Nodes
    colors <- vapply(
        sub[["state"]],
        FUN = \(st) .step_states[[st]][["color"]],
        FUN.VALUE = character(1)
    )

    ids <- as.integer(sub[[".nodeId"]])
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


#' Remove a step
#'
#' If other steps depend on the step to be removed, an error is
#' given and the removal is blocked, unless `force` was set to
#' `TRUE`. In force mode, the selected step and all downstream
#' dependent steps are removed together.
#'
#' @param x A pipeflow pip
#' @param step `string` the name of the step to be removed.
#' @param force `logical` if `TRUE` the step is removed together
#' with all its downstream dependencies.
#' @return The updated pipeline, invisibly.
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
#' # force = TRUE removes the step and all its downstream dependents
#' pip_remove(p, "load", force = TRUE)
#' p                        # pipeline is now empty
#' @export
pip_remove <- function(x, step, force = FALSE) {
    .assert_pip(x)
    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }
    if (is.na(step)) {
        stop("step must not be NA")
    }
    if (!.pip_step_exists(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!is.logical(force) || length(force) != 1L || is.na(force)) {
        stop("force must be a single logical value")
    }

    env <- .pip_get_pip_env(x)
    dat <- env[["data"]]
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
            "' because the following ",
            "steps depend on it: ",
            stepsString
        )
    }

    stepsToRemove <- step
    if (force) {
        downNodes <- .pip_get_reachable_nodes(x, step)
        downNodes <- as.integer(downNodes)
        stepNode <- as.integer(.pip_steps_to_nodes(x, step)[[1]])

        downDeps <- dat[["step"]][
            dat[[".nodeId"]] %in% setdiff(downNodes, stepNode)
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

        stepsToRemove <- dat[["step"]][dat[[".nodeId"]] %in% downNodes]
    }

    nodesToRemove <- as.integer(unname(unlist(
        .pip_steps_to_nodes(x, stepsToRemove)
    )))

    # Remove DAG nodes first to keep node references stable during filtering.
    for (nid in rev(nodesToRemove)) {
        ok <- dag_remove_node(env[[".dag"]], nid, force = force)
        if (!ok) {
            stop("failed to remove node ", nid, " from DAG")
        }
    }
    dag_tidy_up(env[[".dag"]])

    keep <- !(dat[["step"]] %chin% stepsToRemove)
    env[["data"]] <- dat[keep]

    for (s in stepsToRemove) {
        if (
            exists(
                s,
                where = env[[".steps_to_nodes"]],
                inherits = FALSE
            )
        ) {
            rm(
                list = s,
                envir = env[[".steps_to_nodes"]],
                inherits = FALSE
            )
        }
    }

    data.table::setindexv(env[["data"]], list("step", ".nodeId"))
    invisible(x)
}


#' Rename a step
#'
#' Renames the selected step and updates dependency references in
#' downstream steps.
#' @param x A pipeflow pip
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
#' #' # Trying to rename to an existing step name raises an error:
#' try(pip_rename(p, "load_data", to = "s2"))  # step 's2' already exists!
#' @export
pip_rename <- function(x, from, to) {
    .assert_pip(x)

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
    if (.pip_step_exists(x, to)) {
        stop("step '", to, "' already exists")
    }

    env <- .pip_get_pip_env(x)
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
    data.table::set(dat, j = "step", value = newSteps)
    data.table::set(dat, j = "depends", value = newDepends)

    stepsToNodes <- env[[".steps_to_nodes"]]
    nodeId <- stepsToNodes[[from]]
    stepsToNodes[[to]] <- nodeId
    rm(list = from, envir = stepsToNodes, inherits = FALSE)

    data.table::setindexv(dat, list("step", ".nodeId"))
    invisible(x)
}


#' Replace a step
#'
#' Replaces a step's function while keeping it in the same position in the
#' pipeline. Downstream steps are automatically marked as outdated and will
#' re-run on the next [pip_run()].
#'
#' @param x A pipeflow pipeline object.
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
#' @export
pip_replace <- function(
    x,
    step,
    fun,
    tags = character(0),
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
    if (!.pip_step_exists(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!is.function(fun)) {
        stop("fun must be a function")
    }
    .assert_exec_mode(exec)

    src <- pip_clone(x)
    dat <- src[["data"]]
    n <- nrow(dat)
    iStep <- data.table::chmatch(step, dat[["step"]])

    out <- if (iStep > 1L) {
        src[seq_len(iStep - 1L), view = FALSE]
    } else {
        pip_new(name = src[["name"]])
    }

    # Add replacement step at the original position.
    pip_add(
        out,
        step = step,
        fun = fun,
        tags = tags,
        params = params,
        exec = exec
    )

    # Re-append subsequent steps and preserve their runtime state.
    if (iStep < n) {
        tailRows <- seq.int(iStep + 1L, n)
        for (i in tailRows) {
            tailStep <- dat[["step"]][[i]]
            .pip_add_from(out, y = src, step = tailStep)

            iOut <- nrow(out[["data"]])
            data.table::set(
                out[["data"]],
                i = iOut,
                j = c("out", "time", "state", "locked"),
                value = list(
                    list(dat[["out"]][[i]]),
                    dat[["time"]][[i]],
                    dat[["state"]][[i]],
                    dat[["locked"]][[i]]
                )
            )
        }
    }

    # Mark downstream dependent steps as outdated, but keep the replaced
    # step itself as "new".
    downNodes <- .pip_get_reachable_nodes(out, step)
    stepNode <- .pip_steps_to_nodes(out, step)[[1]]
    downNodes <- unique(setdiff(as.integer(unlist(downNodes)), stepNode))
    if (length(downNodes) > 0L) {
        rowsDown <- out[["data"]][
            list(downNodes),
            which = TRUE,
            on = ".nodeId"
        ]
        if (length(rowsDown) > 0L) {
            data.table::set(
                out[["data"]],
                i = rowsDown,
                j = "state",
                value = .step_states[["outdated"]][["name"]]
            )
        }
    }

    x[["data"]] <- out[["data"]]
    env <- .pip_get_pip_env(x)
    env[[".dag"]] <- .pip_get_pip_env(out)[[".dag"]]
    env[[".steps_to_nodes"]] <- .pip_get_pip_env(out)[[".steps_to_nodes"]]
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
#' @details When `x` is a view, requested rows are run together with required
#' upstream dependencies. If a step fails, the pipeline run state is set to
#' `"failed"` and the error is re-thrown.
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
    env <- .pip_get_pip_env(x)
    dat <- env[["data"]]
    rowsToRun <- seq_len(nrow(dat))

    if (isView) {
        requested <- .pip_view_rows(x)
        reqSteps <- dat[["step"]][requested]
        upNodes <- .pip_get_reachable_nodes(
            x,
            reqSteps,
            downstream = FALSE
        )
        upRows <- as.integer(dat[list(upNodes), which = TRUE, on = ".nodeId"])
        rowsToRun <- as.integer(sort(unique(c(requested, upRows))))
        upstreamRows <- setdiff(rowsToRun, requested)
        names(rowsToRun)[match(requested, rowsToRun)] <- "view"
        names(rowsToRun)[match(upstreamRows, rowsToRun)] <- "upstream"
    }
    processedSteps <- character()
    on.exit({
        # At the end, mark all downstream dependent steps as outdated that
        # were *not* processed, which can happen in two different ways:
        # a) when running a view that does not cover the entire pipeline or
        # b) the run was aborted in the middle (due to an error or manual stop).
        processedNodes <- as.integer(.pip_steps_to_nodes(x, processedSteps))
        outdatedNodes <- .pip_get_reachable_nodes(x, processedSteps) |>
            unlist() |>
            unique() |>
            setdiff(processedNodes)
        if (length(outdatedNodes) > 0L) {
            iOut <- dat[list(outdatedNodes), which = TRUE, on = ".nodeId"]
            if (length(iOut) > 0L) {
                data.table::set(dat, i = iOut, j = "state", value = "outdated")
            }
        }
    })

    state <- env[[".run_state"]]
    action <- if (state == "restart") "Restarting" else "Starting"
    log_info(sprintf("%s run of %s '%s'", action, data.class(x), x[["name"]]))
    env[[".run_state"]][] <- "running"
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

                # Run current step - make sure we always pass the full pipeline
                # object (not a view) to the step function, so that it can
                # modify itself if needed.
                log_info(msg)
                self <- .pip_full_pip(x)
                .pip_run_row(x = self, i = row, lgr = lgr)
                stateAfterStep <- env[[".run_state"]]

                # Check for restart or stop signals
                if (stateAfterStep == "restart") {
                    log_info("Restarting pipeline execution.")
                    doForce <- env[[".restart_force"]]
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
                sprintf("Finished run of %s '%s'", data.class(x), x[["name"]])
            )
            env[[".run_state"]][] <- "ready"
            env[[".last_run"]] <- Sys.time()
            invisible(x)
        },
        error = function(e) {
            env[[".run_state"]][] <- "failed"
            env[[".last_run"]] <- Sys.time()
            stop_no_call(e$message)
        }
    )
}

#' Restart a pipeline run
#'
#' Requests a restart of the current [pip_run()] execution. When called from
#' within a step function (via the `.self` argument), the pipeline run is
#' aborted and restarted from the first step. If a view was run, the view run
#' is restarted together with its upstream dependencies.
#'
#' @param x A pipeflow pip or view.
#' @param force Logical indicating if all steps should be forced to run on the
#' restarted run. If `FALSE`, steps that are already in state `"done"` are
#' skipped.
#' @param times Maximum number of restarts to request. Once the pipeline has
#' been restarted `times` times, further calls of `pip_restart()` are ignored
#' until the next run.
#'
#' @return The updated pipeline or view, invisibly.
#' @seealso `vignette("v06-self-modify-pipeline", package = "pipeflow")`
#'   for an advanced example of dynamic pipelines.
#' @examples
#' p <- pip_new("restart") |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("model", \(x = ~load) {
#'     if (length(x) == 3L) {
#'       pip_restart(.self)
#'     }
#'     x * 2
#'   })
#'
#' pip_run(p)
#' p
#' @export
pip_restart <- function(x, force = TRUE, times = 1L) {
    .assert_pip_or_view(x)
    if (!.is_single(force, "logical")) {
        stop("force must be a single logical value")
    }
    if (!.is_single(times, "numeric") || is.na(times) || times < 1L) {
        stop("times must be a single integer value >= 1")
    }

    env <- .pip_get_pip_env(x)

    count <- env[[".restart_count"]]
    if (count >= times) {
        env[[".restart_count"]] <- 0L
        return(invisible(x))
    }

    env[[".run_state"]][] <- "restart"
    env[[".restart_count"]] <- count + 1L
    env[[".restart_force"]] <- force
    invisible(x)
}

#' Stop a pipeline run
#'
#' Aborts the current [pip_run()] execution. When called from within a step
#' function (via the `.self` argument), the pipeline run is stopped after the
#' current step. Steps that were not executed are marked as `"outdated"`.
#'
#' @param x A pipeflow pip or view.
#'
#' @return The updated pipeline or view, invisibly.
#' @seealso `vignette("v06-self-modify-pipeline", package = "pipeflow")`
#'   for an advanced example of dynamic pipelines.
#' @examples
#' p <- pip_new("stop") |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("model", \(x = ~load) {
#'     if (length(x) == 3L) {
#'       pip_stop(.self)
#'     }
#'     x * 2
#'   }) |>
#'   pip_add("report", \(x = ~model) paste("result:", x))
#'
#' pip_run(p)
#' p
#' @export
pip_stop <- function(x) {
    .assert_pip_or_view(x)
    env <- .pip_get_pip_env(x)
    env[[".run_state"]][] <- "stop"
    invisible(x)
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
#' p[["data"]][["state"]] # "done", "done"
#'
#' # Locked steps keep their state and output when resetting
#' pip_lock(pip_view(p, step = "square"))
#' pip_reset(p)
#' p[["data"]][["state"]] # "new", "done"
#' p[["data"]][["out"]]   # NULL, (x^2 result)
#'
#' pip_unlock(p)
#' pip_reset(p)
#' p[["data"]][["state"]] # "new", "new"
#' p[["data"]][["out"]]   # NULL, NULL
#' @export
pip_reset <- function(x) {
    .assert_pip_or_view(x)
    env <- .pip_get_pip_env(x)
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
#' Updates the default values of tunable parameters across the pipeline
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
    env <- .pip_get_pip_env(x)
    dat <- env[["data"]]
    rows <- .pip_view_rows(x)
    rowsConsidered <- setdiff(rows, which(dat[["locked"]]))

    if (length(rowsConsidered) == 0L) {
        message("No steps to update: all selected steps are locked")
        return(invisible(x))
    }

    # Determine which steps/rows are affected, i.e. have intersecting params
    indeps <- dat[[".indeps"]][rowsConsidered] # names of independent params
    intersects <- lapply(indeps, FUN = intersect, y = parNames)
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
#' p[["data"]][["tags"]] # both steps have c("daily", "core")
#'
#' # Add an extra tag to only one step via a view
#' v <- pip_view(p, step = "fit")
#' pip_tag(v, tags = "model")
#' p[["data"]][["tags"]] # "fit" also has "model"
#' @export
pip_tag <- function(x, tags = character()) {
    .assert_pip_or_view(x)
    if (!is.character(tags)) {
        stop("tags must be a character vector")
    }

    env <- .pip_get_pip_env(x)
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
#' p[["data"]][["tags"]]
#' @export
pip_untag <- function(x, tags = character()) {
    .assert_pip_or_view(x)
    if (!is.character(tags)) {
        stop("tags must be a character vector")
    }

    env <- .pip_get_pip_env(x)
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

    env <- .pip_get_pip_env(x)
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
#' p[["data"]][["locked"]] # TRUE, TRUE
#'
#' pip_unlock(p)
#' p[["data"]][["locked"]] # FALSE, FALSE
#' @export
pip_unlock <- function(x) {
    .assert_pip_or_view(x)

    env <- .pip_get_pip_env(x)
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
#' names of each step (both independent and bound / dependency
#' parameters). See examples for usage.
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

    # Resolve each filter name to the column it filters on. "params" is a
    # special case: it matches against the actual parameter names of each
    # step (both independent and bound/dependency parameters).
    for (name in names(filters)) {
        col <- if (name == "params") {
            lapply(sub[["params"]], names)
        } else {
            sub[[name]]
        }
        values <- filters[[name]]
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
    structure(
        list(pipenv = env, name = sprintf("%s view", x[["name"]]), view = rows),
        class = "pipeflow_pip"
    )
}


# ------------------------------------
# Implementation of generic S3 methods
# ------------------------------------

#' Length of a pipeflow pipeline or view
#' @param x A pipeflow pipeline or view
#' @return Number of steps as an integer.
#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1) |>
#'   pip_add("s3", \(x = ~s2) x * 2)
#' length(p) # 3 — total steps in the pipeline
#'
#' # A view reports only the number of selected (visible) steps
#' v <- pip_view(p, step = c("s2", "s3"))
#' length(v) # 2
#' @rdname length.pipeflow
#' @export
length.pipeflow_pip <- function(x) {
    as.integer(length(.pip_view_rows(x)))
}

#' Number of rows of a pipeflow pipeline or view
#'
#' Treats a pipeline as a table of steps: `nrow()` returns the number of
#' steps, the same as [length.pipeflow_pip] / `length()`, and `ncol()`
#' returns the number of columns of the underlying step table. Views report
#' only the number of covered steps as rows.
#' @param x A pipeflow pipeline or view
#' @return `nrow()` returns the number of steps as an integer; `ncol()`
#' returns the number of columns of the step table.
#' @details Base R's `nrow()` is implemented as `dim(x)[1L]`, so the number
#' of rows and columns is provided through a `dim()` method for
#' `pipeflow_pip` objects.
#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1)
#' nrow(p) # 2
#' ncol(p) # number of columns of the step table
#' nrow(p) == length(p) # TRUE
#'
#' v <- pip_view(p, step = "s2")
#' nrow(v) # 1
#' @rdname nrow.pipeflow
#' @export
dim.pipeflow_pip <- function(x) {
    c(as.integer(length(.pip_view_rows(x))), ncol(x[["data"]]))
}

#' Extract or subset a pipeline
#'
#' Selects steps from a pipeline. By default, a lightweight [pip_view()] is
#' returned that references the selected steps without copying them. Set
#' `view = FALSE` to instead get a new, self-contained pipeline that includes
#' all required upstream dependencies.
#'
#' If `i` is omitted, named arguments passed through `...` are forwarded to
#' [pip_view()], so views can be defined directly with the extract operator:
#' `p[state = "new", tags = "io"]`. With no arguments at all, a copy of the
#' pipeline is returned.
#' @param x A pipeflow pipeline object.
#' @param i integer (row indices) or character vector (step names) of steps to
#' select. If omitted, `...` filters are forwarded to [pip_view()].
#' @param view If `TRUE` (default), a view referencing the selected steps is
#' returned. If `FALSE`, a new pipeline is returned that includes the selected
#' steps and all their upstream dependencies.
#' @param ... Named filters forwarded to [pip_view()], used when `i` is
#' omitted. Can be one or more of `step`, `params`, `depends`, `state`,
#' `tags`, `exec`.
#' @return A pipeflow view (if `view = TRUE`) or a new pipeflow pipeline
#' (if `view = FALSE`).
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 5) seq_len(n), tags = "io") |>
#'   pip_add("square", \(x = ~load) x^2) |>
#'   pip_add("total", \(x = ~square) sum(x))
#'
#' # By default, `[` returns a view into the selected steps.
#' sub <- p["total"]
#' sub # pipeflow_view, references the underlying pipeline
#'
#' # With view = FALSE, a self-contained pipeline including all upstream
#' # dependencies is returned instead.
#' sub <- p["total", view = FALSE]
#' sub[["step"]] # "load", "square", "total"
#'
#' # Select a subset of steps by name vector or integer row index
#' p[c("load", "square")][["step"]] # view -> "load", "square"
#' p[1:2, view = FALSE][["step"]]    # pipeline -> "load", "square"
#'
#' # With i omitted, named arguments are forwarded to pip_view()
#' v <- p[state = "new", tags = "io"]
#' v[["step"]] # "load"
#'
#' # No arguments returns a copy of the pipeline
#' length(p[]) # 3
#' @rdname Extract.pipeflow_pip
#' @export
`[.pipeflow_pip` <- function(x, i, view = TRUE, ...) {
    .assert_pip(x)
    n <- length(x)

    if (missing(i)) {
        if (!missing(...)) {
            # Allows views to be defined like x[state = "new", tags = "io"]
            return(pip_view(x, ...))
        } else {
            # We land here if user calls x[]
            return(pip_clone(x))
        }
    }

    if (!.is_single(view, "logical") || is.na(view)) {
        stop("view must be a single logical value")
    }

    if (!(is.numeric(i) || is.character(i))) {
        stop("i must be either numeric row indices or character step names")
    }

    # Verify and resolve row selection
    if (is.character(i)) {
        rows <- sort(unique(.pip_steps_to_rows(x, i)))
    } else {
        if (anyNA(i)) {
            stop("row indices in 'i' must not contain NA")
        }
        if (!all(is.finite(i)) || !all(i == as.integer(i))) {
            stop("numeric indices in 'i' must be whole numbers")
        }
        rows <- sort(unique(as.integer(i)))
        bad <- rows[rows < 1L | rows > n]
        if (length(bad) > 0L) {
            stop("Invalid row indices in 'i': ", toString(bad))
        }
    }

    if (view) {
        return(.pip_make_view(x, rows))
    }

    out <- pip_new(name = x[["name"]])
    if (length(rows) == 0L) {
        return(out)
    }

    # Get all nodes that are reachable from the selected rows via upstream
    env <- .pip_get_pip_env(x)
    dat <- env[["data"]]
    startNodes <- dat[[".nodeId"]][rows]
    keepNodes <- dag_get_reachable_nodes_up(
        env[[".dag"]],
        as.integer(unique(startNodes))
    )
    subsetDat <- dat[dat[[".nodeId"]] %in% keepNodes]
    subsetDat <- data.table::copy(subsetDat)

    # Re-map node ids to a compact sequence and rebuild lookup table
    oldNodeIds <- subsetDat[[".nodeId"]]
    newNodeIds <- seq_along(oldNodeIds) - 1L
    nodeMap <- stats::setNames(newNodeIds, as.character(oldNodeIds))
    subsetDat[[".nodeId"]] <- as.integer(newNodeIds)

    stepsToNodes <- new.env(parent = emptyenv())
    for (k in seq_len(nrow(subsetDat))) {
        stepsToNodes[[subsetDat[["step"]][[k]]]] <- subsetDat[[".nodeId"]][[k]]
    }

    # Build a DAG that matches the extracted rows
    d <- dag_new()
    for (k in seq_len(nrow(subsetDat))) {
        dag_add_node(d)
    }
    for (k in seq_len(nrow(subsetDat))) {
        deps <- subsetDat[["depends"]][[k]]
        if (length(deps) == 0L) {
            next
        }
        from <- as.integer(unname(unlist(mget(
            deps,
            envir = stepsToNodes,
            inherits = FALSE
        ))))
        to <- as.integer(subsetDat[[".nodeId"]][[k]])
        dag_add_edges_to(d, from = from, to = to)
    }

    data.table::setindexv(subsetDat, list("step", ".nodeId"))
    out[["data"]] <- subsetDat
    out[[".dag"]] <- d
    out[[".steps_to_nodes"]] <- stepsToNodes

    nUpstream <- nrow(subsetDat) - length(rows)
    if (nUpstream > 0L) {
        message(sprintf(
            "pulled in %d upstream dependenc%s",
            nUpstream,
            if (nUpstream == 1L) "y" else "ies"
        ))
    }

    out
}


#' Extract values from a pipeline or view
#'
#' A pipeline can be read like a data.frame of steps: `p[[column]]` returns a
#' column of the step table, and `p[[row, column]]` extracts a single cell.
#' In addition, a few meta fields are accessible by name.
#'
#' ## Meta fields
#'
#' The following meta fields are available via `p[["..."]]` (or `p$...`):
#'
#' * `data` — the step table, a `data.table` with one row per step. Columns
#'   include `step`, `fun`, `params`, `depends`, `tags`,
#'   `exec`, `state`, `out`, `time`, and `locked`.
#' * `name` — the name of the pipeline.
#' * `view` — the absolute row indices of the steps covered by a view, or
#'   `NULL` for a full pipeline.
#' * `pipenv` — the shared inner environment holding the pipeline's state.
#'   All views and extracted subsets reference the same environment, so
#'   mutations are shared.
#'
#' ## Step-table columns
#'
#' `p[["column"]]` returns a column of the step table, named by the step
#' names. For views, the column is restricted to the steps covered by the
#' view. Meta fields take priority over columns of the same name. The
#' two-index form `p[[row, column]]` extracts a single cell, where `row` is
#' an integer row index or a step name.
#' @param i integer (row index) or character (step name) of the step to
#' select
#' @param j column name to select
#' @return Extracted value(s), depending on `i` and `j`.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#' pip_run(p)
#'
#' # Meta fields
#' p[["data"]]              # the underlying step table
#' p[["name"]]              # "pipe"
#' p[["view"]]              # NULL — not a view
#' p[["pipenv"]]            # the inner pipeline environment
#'
#' # Column access, named by steps
#' p[["step"]]   # c(load = "load", fit = "fit")
#' p[["out"]]    # c(load = 1, fit = 2)
#'
#' # Single cell: p[[row, column]]
#' p[["fit", "depends"]]    # "load"
#' p[[2, "state"]]          # state of the second step
#'
#' # Views behave analogously:
#' v <- pip_view(p, step = c("load", "fit"))
#' v[["data"]]              # the underlying filtered step table
#' v[["name"]]              # "pipe view"
#' v[["view"]]              # row indices of the covered steps
#'
#' v[["step"]]              # c(load = "load", fit = "fit")
#' v[["out"]]               # c(load = 1, fit = 2)
#' v[["fit", "out"]]        # output of the "fit" step
#' @rdname Extract_value.pipeflow
#' @export
`[[.pipeflow_pip` <- function(x, i, j = NULL, ...) {
    .pip_subset2(x = x, i = i, j = j, ...)
}

# Assignment routes list fields (`pip`, `name`, `rows`) to the wrapper and all
# other bindings to the shared inner environment.
#' @export
`[[<-.pipeflow_pip` <- function(x, i, j = NULL, ..., value) {
    if (i %in% c("pipenv", "name", "view")) {
        unclass(x)[[i]] <- value
    } else {
        env <- .pip_get_pip_env(x)
        env[[i]] <- value
    }
    x
}

#' @rdname Extract_value.pipeflow
#' @export
`$.pipeflow_pip` <- function(x, i) {
    x[[i]]
}

#' @export
`$<-.pipeflow_pip` <- function(x, i, value) {
    x[[i]] <- value
    x
}


#' Print pipeflow objects
#'
#' @param x A pipeflow pipeline or view.
#' @param rows Row indices to be printed. If empty, all rows are printed.
#' @param cols The columns to be printed. Can be either one of
#' `core` or `all` to print the core or all columns, respectively,
#' or an explicit character vector of columns to be printed.
#' @param topn The number of rows to be printed from the beginning
#' and end of tables with more than `nrows` rows.
#' @param nrows The number of rows printed before truncation is enforced.
#' @param class If TRUE, the resulting output will include above each
#' column its storage class (or a self-evident abbreviation thereof).
#' @param row.names If TRUE, row indices will be printed alongside x.
#' @param header If TRUE, a header with the pipeline name and number
#' of steps, and a footer with the run state and the time of the last run,
#' will be printed.
#' @param ...  Other arguments passed to `print.data.table`
#' @return Invisibly returns `x`.
#' @examples
#' p <- pip_new("demo") |>
#'   pip_add("load", \(n = 5) seq_len(n), tags = c("io", "raw")) |>
#'   pip_add("square", \(x = ~load) x^2, tags = "compute") |>
#'   pip_add("total", \(x = ~square) sum(x), tags = "compute")
#'
#' print(p) # core columns: step, depends, tags, out, state
#' print(p, cols = "all") # all non-hidden columns
#' print(p, rows = 2:3) # print only steps 2 and 3
#'
#' v <- pip_view(p, tags = "compute")
#' print(v)
#' @rdname print
#' @export
str.pipeflow_pip <- function(object, ...) {
    str(unclass(object), ...)
}


#' @rdname print
#' @export
print.pipeflow_pip <- function(
    x,
    rows = integer(),
    cols = getOption("pipeflow.print.cols", default = "core"),
    topn = getOption("pipeflow.print.topn", default = 5),
    nrows = getOption("pipeflow.print.nrows", default = 50),
    row.names = getOption("pipeflow.print.rownames", default = TRUE),
    class = getOption("pipeflow.print.class", default = FALSE),
    header = TRUE,
    ...
) {
    dat <- x[["data"]]
    n <- nrow(dat)
    isView <- .is_pipeflow_view(x)

    if (identical(cols, "core")) {
        cols <- c("step", "depends", "out", "state")
        has_tags <- any(lengths(dat[["tags"]]) > 0L)
        if (has_tags) {
            cols <- append(cols, "tags")
        }
        if (any(dat[["exec"]] != "auto")) {
            cols <- append(cols, "exec")
        }
        if (any(dat[["locked"]])) {
            cols <- append(cols, "locked")
        }
    }
    if (identical(cols, "all")) {
        isHidden <- function(name) startsWith(name, ".")
        cols <- Filter(Negate(isHidden), colnames(dat))
    }

    if (header) {
        # Add header
        if (isView) {
            nr <- length(.pip_view_rows(x))
            title <- sprintf(
                "<pipeflow_view> %s (%d of %d step%s)",
                x[["name"]],
                nr,
                n,
                ifelse(n == 1, "", "s")
            )
        } else {
            title <- sprintf(
                "<pipeflow_pip> %s (%d step%s)",
                x[["name"]],
                n,
                ifelse(n == 1, "", "s")
            )
        }
        line <- paste(rep("-", nchar(title)), collapse = "")
        cat(title, line, sep = "\n")
    }

    if (length(rows) == 0) {
        rows <- .pip_view_rows(x)
    }

    if (length(rows) == 0L) {
        cat("Empty pipeline\n")
    } else {
        print(
            dat[rows, cols, with = FALSE],
            topn = topn,
            nrows = nrows,
            row.names = row.names,
            class = class,
            ...
        )
    }

    if (header) {
        # Add footer with run state infos
        runState <- as.character(.pip_get_pip_env(x)[[".run_state"]])
        lastRun <- x[["pipenv"]][[".last_run"]]
        lastRunStr <- if (is.null(lastRun)) {
            "never"
        } else {
            format(lastRun, format = "%Y-%m-%d %H:%M:%S")
        }
        cat(
            line,
            sprintf("<%s> last run: %s", runState, lastRunStr),
            sep = "\n"
        )
    }

    invisible(x)
}


#' Bind pipelines
#'
#' Binds two or more pipelines together by concatenating their steps. If the
#' pipelines have steps with the same name, the step names of later pipelines
#' are automatically adapted to avoid name clashes. A single pipeline is
#' returned unchanged.
#' @param ... Two or more pipeflow pipeline objects.
#' @param deparse.level Not used, for compatibility with the generic
#' `rbind()`.
#' @return A new pipeflow pipeline object representing the bound pipelines.
#' @examples
#' a <- pip_new("a") |>
#'   pip_add("prep", \(x = 1) x * 2) |>
#'   pip_add("fit", \(x = ~prep) x + 10)
#'
#' # "prep" exists in both pipelines; the one from b gets a numeric suffix
#' b <- pip_new("b") |> pip_add("prep", \(x = 5) x * 3)
#'
#' ab <- rbind(a, b)
#' ab[["step"]] # "prep", "fit", "prep2" (step name conflict auto-resolved)
#' ab
#'
#' # Any number of pipelines can be combined
#' abc <- rbind(a, b, b)
#' abc
#' @export
rbind.pipeflow_pip <- function(..., deparse.level = 1) {
    pips <- list(...)
    if (length(pips) == 0L) {
        stop("at least one pipeflow pipeline must be provided")
    }
    for (pip in pips) {
        .assert_pip(pip)
    }

    out <- pips[[1L]]
    if (length(pips) == 1L) {
        return(out)
    }

    for (pip in pips[-1L]) {
        out <- .pip_bind(out, pip)
    }
    out
}
