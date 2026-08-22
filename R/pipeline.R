# ---------------------
# Pipeline construction
# ---------------------
.empty_pipeline <- function() {
    data.table::data.table(
        step = character(0),
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


# ---------------
# Type predicates
# ---------------
.is_pipeflow_pip <- function(x) {
    inherits(x, "pipeflow_pip")
}

.is_pipeflow_view <- function(x) {
    inherits(x, "pipeflow_view")
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
    if (!(.is_pipeflow_pip(x) || .is_pipeflow_view(x))) {
        stop_no_call("x must be a pipeflow pip or view")
    }
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
    depends <- lapply(params, FUN = \(x) trimws(deparse(x))) |>
        Filter(f = \(x) startsWith(x, "~")) |>
        lapply(\(x) substring(x, 2)) |>
        unlist()

    if (length(depends) == 0) {
        return(character(0))
    }

    # Finally, convert any relative dependencies (those marked with a
    # leading "-") to step names.
    iRelPos <- which(depends |> startsWith("-"))
    stepNumbers <- depends[iRelPos] |>
        lapply(FUN = \(x) .rel_pos_to_step_num(abs(as.integer(x)), toPos))
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
    isView <- inherits(x, "pipeflow_view")
    if (isView) {
        rows <- x[["rows"]]
        x[["pip"]][["pipeline"]][rows, ]
    } else {
        x[["pipeline"]]
    }
}

.pip_filter <- function(x, on, values) {
    x[["pipeline"]][list(values), on = on]
}

.pip_filter_nodes <- function(x, nodes) {
    x[["pipeline"]][list(nodes), on = ".nodeId"]
}


# --------
# Indexing
# -------.
.pip_is_indexed <- function(x) {
    !is.null(data.table::indices(x[["pipeline"]]))
}

.pip_reindex <- function(x) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    data.table::setindexv(x[["pipeline"]], list("step", ".nodeId"))
}


# ---------------------------
# Step lookup & DAG traversal
# ---------------------------
.pip_step_exists <- function(x, step) {
    exists(step, where = x[[".steps_to_nodes"]], inherits = FALSE)
}

.pip_steps_to_nodes <- function(x, steps) {
    mget(
        steps,
        envir = x[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
}

.pip_steps_to_rows <- function(x, steps) {
    pip <- if (.is_pipeflow_view(x)) x[["pip"]] else x
    dat <- pip[["pipeline"]]

    if (anyNA(steps)) {
        stop("step names must not contain NA", call. = FALSE)
    }
    if (!all(nzchar(steps))) {
        stop("step names must be non-empty strings", call. = FALSE)
    }

    i <- match(steps, dat[["step"]])
    if (anyNA(i)) {
        unknown <- unique(steps[is.na(i)])
        stop("Unknown step names: ", toString(unknown), call. = FALSE)
    }
    as.integer(i)
}

.pip_get_reachable_nodes <- function(x, steps, downstream = TRUE) {
    known <- intersect(steps, names(x[[".steps_to_nodes"]]))
    if (length(known) == 0L) {
        return(integer(0))
    }

    start_ids <- as.integer(mget(
        known,
        envir = x[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    ))
    start_ids <- start_ids[!is.na(start_ids)]
    if (length(start_ids) == 0L) {
        return(integer(0))
    }

    if (downstream) {
        dag_get_reachable_nodes_down(x[[".dag"]], start_ids)
    } else {
        dag_get_reachable_nodes_up(x[[".dag"]], start_ids)
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
    steps <- c(x[["pipeline"]][["step"]], step)
    depends <- .extract_depends(params = params, steps = steps)
    refNodes <- mget(
        depends,
        envir = x[[".steps_to_nodes"]],
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
    d <- x[[".dag"]]
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

    x[["pipeline"]] <- data.table::rbindlist(list(x[["pipeline"]], newStep))
    x[[".steps_to_nodes"]][[step]] <- .nodeId
    x
}

.pip_run_row <- function(x, i, lgr) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.pip_is_indexed(x)) {
        .pip_reindex(x)
    }

    dat <- x[["pipeline"]]
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
        }
    )

    # Re-read after execution so runtime structural changes are reflected. We
    # update by step name, so if the current step removed itself we simply
    # skip the update instead of writing into a shifted row.
    dat <- x[["pipeline"]]
    rowNow <- match(step, dat[["step"]])
    if (!is.na(rowNow)) {
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
    x[["pipeline"]][list(nodes), (what) := value, on = ".nodeId"]

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
#' # Create a named pipeline
#' p <- pip_new("my_analysis")
#' p[["name"]] # "my_analysis"
#'
#' # Build a simple pipeline and run it
#' pip_add(p, "load", \(n = 5) seq_len(n))
#' pip_add(p, "double", \(x = ~load) x * 2) # x depends on load's output
#' p
#' pip_run(p)
#' p[["out"]] # list of outputs, one per step
#' @export
pip_new <- function(name = "pipe") {
    if (!.is_single(name, "character")) {
        stop("name must be a single string")
    }
    if (is.na(name)) {
        stop("name must not be NA")
    }

    # Main pipeline components
    hash_map <- function() new.env(parent = emptyenv())
    env <- hash_map()
    env[["name"]] <- name
    env[["pipeline"]] <- .empty_pipeline()
    env[[".dag"]] <- dag_new()
    env[[".steps_to_nodes"]] <- hash_map()

    # Pipeline states
    env[[".run_state"]] <- factor(
        "ready",
        levels = c("ready", "restart", "running", "stop")
    )

    # Restart tracking
    env[[".restart_count"]] <- 0L
    env[[".restart_force"]] <- TRUE

    structure(env, class = c("pipeflow_pip", "environment"))
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
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (pip_has_step(x, step)) {
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
        if (!pip_has_step(x, after)) {
            stop("step '", after, "' does not exist")
        }
        # Most of the time the new step is added at the end, so we check that
        # first to avoid the more expensive match() call in the common case.
        pos <- n
        last <- x[["pipeline"]][["step"]][n]
        if (after != last) {
            pos <- match(after, x[["pipeline"]][["step"]])
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
    # simpler approach and just create a new pipeline (1), copy all steps up
    # to the insertion point (2), add the new step (3), and then re-add all
    # remaining steps after that.
    # 1) Clone the pipeline
    src <- pip_clone(x)
    dat <- src[["pipeline"]]
    n <- nrow(dat)

    # 2) Create a new pipeline and copy all steps up to the insertion point
    out <- if (pos > 0L) src[seq_len(pos)] else pip_new(name = src[["name"]])

    # 3) Add the new step at the end of the new pipeline
    pip_add(out, step = step, fun = fun, tags = tags, exec = exec)

    # 4) Add all remaining steps to the end of the new pipeline
    tailRows <- seq.int(pos + 1L, n)
    for (i in tailRows) {
        tailStep <- dat[["step"]][[i]]
        pip_add_from(out, y = src, step = tailStep)

        iOut <- nrow(out[["pipeline"]])
        data.table::set(
            out[["pipeline"]],
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

    x[["pipeline"]] <- out[["pipeline"]]
    x[[".dag"]] <- out[[".dag"]]
    x[[".steps_to_nodes"]] <- out[[".steps_to_nodes"]]
    invisible(x)
}


#' Copy a step from another pipeline
#'
#' Copies one step from pipeline `y` into pipeline `x`, preserving its
#' function, parameters, tags, and dependency links.
#'
#' @param x Target pipeflow pipeline object.
#' @param y Source pipeflow pipeline object.
#' @param step Step name to copy from `y`.
#'
#' @return The updated target pipeline, invisibly.
#' @examples
#' # Build a source pipeline with reusable steps
#' src <- pip_new("source") |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("square", \(x = ~load) x^2)
#'
#' # Copy steps into a new pipeline one at a time.
#' # The dependency of "square" on "load" is re-established automatically.
#' dst <- pip_new("target")
#' pip_add_from(dst, src, "load")
#' pip_add_from(dst, src, "square")
#' pip_run(dst)
#' pip_collect_out(dst)
#' @export
pip_add_from <- function(x, y, step) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.is_pipeflow_pip(y)) {
        stop("y must be a pipeflow pip")
    }
    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }
    if (is.na(step)) {
        stop("step must not be NA")
    }
    if (!nzchar(step)) {
        stop("step must be a non-empty string")
    }

    if (!pip_has_step(y, step)) {
        stop("step '", step, "' does not exist in source pipeline")
    }

    iStep <- match(step, y[["pipeline"]][["step"]])
    fun <- y[["pipeline"]][["fun"]][[iStep]]
    tags <- y[["pipeline"]][["tags"]][[iStep]]
    exec <- y[["pipeline"]][["exec"]][[iStep]]
    params <- y[["pipeline"]][["params"]][[iStep]]
    depends <- y[["pipeline"]][["depends"]][[iStep]]
    indeps <- y[["pipeline"]][[".indeps"]][[iStep]]

    # Recreate defaults from stored params/dependencies so pip_add can
    # resolve references and wire DAG updates in the target pipeline.
    f <- fun
    fml <- formals(f)
    for (nm in indeps) {
        fml[[nm]] <- params[[nm]]
    }

    if (length(depends) > 0L) {
        for (arg in names(depends)) {
            fml[[arg]] <- stats::as.formula(paste("~", depends[[arg]]))
        }
    }

    formals(f) <- fml
    pip_add(x, step = step, fun = f, tags = tags, exec = exec)
}

#' Bind pipelines
#'
#' Bind two pipelines together by concatenating their steps. If both pipelines
#' have steps with the same name, the step names of the second pipeline will be
#' automatically adapted to avoid name clashes.
#' @param x A pipeflow pipeline object.
#' @param y A pipeflow pipeline object.
#' @return A new pipeflow pipeline object representing the bound pipelines.
#' @examples
#' a <- pip_new("a") |>
#'   pip_add("prep", \(x = 1) x * 2) |>
#'   pip_add("fit", \(x = ~prep) x + 10)
#'
#' # "prep" exists in both pipelines; the one from b gets a numeric suffix
#' b <- pip_new("b") |> pip_add("prep", \(x = 5) x * 3)
#'
#' ab <- pip_bind(a, b)
#' ab[["step"]] # "prep", "fit", "prep2" (step name conflict auto-resolved)
#' ab
#' @export
pip_bind <- function(x, y) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.is_pipeflow_pip(y)) {
        stop("y must be a pipeflow pip")
    }

    out <- pip_clone(x, name = paste0(x[["name"]], "-", y[["name"]]))
    yy <- pip_clone(y)
    yyDat <- yy[["pipeline"]]

    # 1) Resolve all name clashes directly on the cloned source pipeline.
    reserved <- out[["pipeline"]][["step"]]
    for (k in seq_len(nrow(yyDat))) {
        step <- yyDat[["step"]][[k]]
        if (step %in% reserved) {
            to <- step
            i <- 2L
            allSteps <- yyDat[["step"]]
            while (to %in% reserved || to %in% allSteps) {
                to <- paste0(step, i)
                i <- i + 1L
            }
            pip_rename(yy, from = step, to = to)
        }
        reserved <- c(reserved, yyDat[["step"]][[k]])
    }

    # 2) Add (potentially renamed) steps from y one by one via pip_add_from.
    for (k in seq_len(nrow(yyDat))) {
        step <- yyDat[["step"]][[k]]
        pip_add_from(out, y = yy, step = step)

        # Preserve runtime state from source pipeline.
        iOut <- nrow(out[["pipeline"]])
        data.table::set(
            out[["pipeline"]],
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
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!is.null(name) && (!.is_single(name, "character") || is.na(name))) {
        stop("name must be a single non-NA string")
    }

    newName <- if (is.null(name)) x[["name"]] else name
    out <- pip_new(name = newName)

    out[[".dag"]] <- dag_clone(x[[".dag"]])
    dat <- data.table::copy(x[["pipeline"]])
    out[["pipeline"]] <- dat

    # Clone steps to nodes mapping
    for (k in seq_len(nrow(dat))) {
        step <- dat[["step"]][[k]]
        nodeId <- dat[[".nodeId"]][[k]]
        out[[".steps_to_nodes"]][[step]] <- nodeId
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
#' v <- pip_view(p, i = "fit")
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
    pip <- if (isView) x[["pip"]] else x
    dat <- pip[["pipeline"]]
    dag <- pip[[".dag"]]

    rows <- if (isView) as.integer(x[["rows"]]) else seq_len(nrow(dat))
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


#' Check whether a step exists
#'
#' @param x A pipeflow pip
#' @param step A step name
#' @return Logical indicating if the step exists
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#'
#' pip_has_step(p, "load") # TRUE
#' pip_has_step(p, "fit") # TRUE
#' pip_has_step(p, "predict") # FALSE — step not yet added
#' @export
pip_has_step <- function(x, step) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }

    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }

    if (is.na(step)) {
        stop("step must not be NA")
    }

    if (!nzchar(step)) {
        stop("step must be a non-empty string")
    }

    .pip_step_exists(x, step)
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
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }
    if (is.na(step)) {
        stop("step must not be NA")
    }
    if (!pip_has_step(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!is.logical(force) || length(force) != 1L || is.na(force)) {
        stop("force must be a single logical value")
    }

    dat <- x[["pipeline"]]
    directDeps <- dat[["step"]][
        vapply(
            dat[["depends"]],
            FUN = \(dep) step %in% dep,
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
        ok <- dag_remove_node(x[[".dag"]], nid, force = force)
        if (!ok) {
            stop("failed to remove node ", nid, " from DAG")
        }
    }
    dag_tidy_up(x[[".dag"]])

    keep <- !(dat[["step"]] %in% stepsToRemove)
    x[["pipeline"]] <- dat[keep]

    for (s in stepsToRemove) {
        if (exists(s, where = x[[".steps_to_nodes"]], inherits = FALSE)) {
            rm(list = s, envir = x[[".steps_to_nodes"]], inherits = FALSE)
        }
    }

    data.table::setindexv(x[["pipeline"]], list("step", ".nodeId"))
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
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }

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

    if (!pip_has_step(x, from)) {
        stop("step '", from, "' does not exist")
    }
    if (pip_has_step(x, to)) {
        stop("step '", to, "' already exists")
    }

    dat <- x[["pipeline"]]
    newSteps <- dat[["step"]]
    newSteps[newSteps %in% from] <- to

    newDepends <- lapply(
        dat[["depends"]],
        FUN = \(dep) {
            if (length(dep) == 0L) {
                return(dep)
            }
            dep[dep %in% from] <- to
            dep
        }
    )
    data.table::set(dat, j = "step", value = newSteps)
    data.table::set(dat, j = "depends", value = newDepends)

    nodeId <- x[[".steps_to_nodes"]][[from]]
    x[[".steps_to_nodes"]][[to]] <- nodeId
    rm(list = from, envir = x[[".steps_to_nodes"]], inherits = FALSE)

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
pip_replace <- function(x, step, fun, tags = character(0)) {
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.is_single(step, "character")) {
        stop("step must be a single string")
    }
    if (is.na(step)) {
        stop("step must not be NA")
    }
    if (!nzchar(step)) {
        stop("step must be a non-empty string")
    }
    if (!pip_has_step(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!is.function(fun)) {
        stop("fun must be a function")
    }

    src <- pip_clone(x)
    dat <- src[["pipeline"]]
    n <- nrow(dat)
    iStep <- match(step, dat[["step"]])

    out <- if (iStep > 1L) {
        src[seq_len(iStep - 1L)]
    } else {
        pip_new(name = src[["name"]])
    }

    # Add replacement step at the original position.
    pip_add(out, step = step, fun = fun, tags = tags)

    # Re-append subsequent steps and preserve their runtime state.
    if (iStep < n) {
        tailRows <- seq.int(iStep + 1L, n)
        for (i in tailRows) {
            tailStep <- dat[["step"]][[i]]
            pip_add_from(out, y = src, step = tailStep)

            iOut <- nrow(out[["pipeline"]])
            data.table::set(
                out[["pipeline"]],
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
        rowsDown <- out[["pipeline"]][
            list(downNodes),
            which = TRUE,
            on = ".nodeId"
        ]
        if (length(rowsDown) > 0L) {
            data.table::set(
                out[["pipeline"]],
                i = rowsDown,
                j = "state",
                value = .step_states[["outdated"]][["name"]]
            )
        }
    }

    x[["pipeline"]] <- out[["pipeline"]]
    x[[".dag"]] <- out[[".dag"]]
    x[[".steps_to_nodes"]] <- out[[".steps_to_nodes"]]
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
#' upstream dependencies.
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
#' v <- pip_view(p, i = "total")
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
    pip <- if (isView) x[["pip"]] else x
    dat <- pip[["pipeline"]]
    rowsToRun <- seq_len(nrow(dat))

    if (isView) {
        requested <- x[["rows"]]
        reqSteps <- dat[["step"]][requested]
        upNodes <- .pip_get_reachable_nodes(pip, reqSteps, downstream = FALSE)
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
        processedNodes <- as.integer(.pip_steps_to_nodes(pip, processedSteps))
        outdatedNodes <- .pip_get_reachable_nodes(pip, processedSteps) |>
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

    state <- pip[[".run_state"]]
    action <- if (state == "restart") "Restarting" else "Starting"
    log_info(sprintf("%s run of %s '%s'", action, data.class(x), x[["name"]]))
    pip[[".run_state"]][] <- "running"
    for (i in seq_along(rowsToRun)) {
        row <- rowsToRun[[i]]
        step <- dat[["step"]][[row]]
        processedSteps <- c(processedSteps, step)
        if (!is.null(progress)) {
            progress(value = i, detail = step)
        }
        msg <- if (isView) {
            marker <- names(rowsToRun)[[i]]
            sprintf("Step %i/%i [%s] %s", i, length(rowsToRun), marker, step)
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

        # Run current step
        log_info(msg)
        .pip_run_row(pip, i = row, lgr = lgr)
        stateAfterStep <- pip[[".run_state"]]

        # Check for restart or stop signals
        if (stateAfterStep == "restart") {
            log_info("Restarting pipeline execution.")
            doForce <- pip[[".restart_force"]]
            pip_run(x, lgr = lgr, force = doForce, progress = progress)
            return(invisible(x))
        }

        if (stateAfterStep == "stop") {
            log_info("Aborting pipeline execution on manual stop.")
            break
        }
    }

    log_info(sprintf("Finished run of %s '%s'", data.class(x), x[["name"]]))
    pip[[".run_state"]][] <- "ready"
    invisible(x)
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

    isView <- .is_pipeflow_view(x)
    pip <- if (isView) x[["pip"]] else x

    count <- pip[[".restart_count"]]
    if (count >= times) {
        pip[[".restart_count"]] <- 0L
        return(invisible(x))
    }

    pip[[".run_state"]][] <- "restart"
    pip[[".restart_count"]] <- count + 1L
    pip[[".restart_force"]] <- force
    invisible(x)
}

#' Stop a pipeline run
#'
#' Aborts the current [pip_run()] execution. When called from within a step
#' function (via the `.self` argument), the pipeline run is stopped after the
#' current step. Steps that were not executed are marked as `"outdated"`. If a
#' view was run, only the steps covered by the view are affected.
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
    isView <- .is_pipeflow_view(x)
    pip <- if (isView) x[["pip"]] else x
    pip[[".run_state"]][] <- "stop"
    invisible(x)
}


#' Set independent parameters
#'
#' Updates the default values of tunable parameters across the pipeline.
#' Affected steps and their downstream dependents are automatically marked
#' as outdated.
#' @details Parameters of locked steps are never changed and their state
#' remains unchanged.
#' @param p A pipeflow pip or view
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
pip_set_params <- function(p, params = list()) {
    # Input checking
    .assert_pip_or_view(p)
    if (!is.list(params)) {
        stop("params must be a list")
    }
    parNames <- names(params)
    if (length(params) == 0) {
        return(invisible(p))
    }
    allNamed <- length(parNames) == length(params) && all(nzchar(parNames))
    if (!allNamed) {
        stop("All parameters must be named")
    }

    # Narrow down the considered rows
    isView <- .is_pipeflow_view(p)
    x <- if (isView) p[["pip"]] else p
    dat <- x[["pipeline"]]
    rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))
    rowsConsidered <- setdiff(rows, which(dat[["locked"]]))

    if (length(rowsConsidered) == 0L) {
        warning("No steps to update: all selected steps are locked")
        return(invisible(p))
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

    invisible(p)
}


#' Add tags to selected steps
#'
#' Adds tags to existing tags for all steps in the pipeline unless `p` is a
#' view, in which case tags are only added for steps covered by the view.
#' Locked steps are skipped and not updated.
#' @param p A pipeflow pip or view.
#' @param tags Character vector of tags to add for each selected step.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#'
#' # Tag every step in the pipeline at once
#' pip_tag(p, tags = c("daily", "core"))
#' p[["pipeline"]][["tags"]] # both steps have c("daily", "core")
#'
#' # Add an extra tag to only one step via a view
#' v <- pip_view(p, i = "fit")
#' pip_tag(v, tags = "model")
#' p[["pipeline"]][["tags"]] # "fit" also has "model"
#' @export
pip_tag <- function(p, tags = character()) {
    .assert_pip_or_view(p)
    if (!is.character(tags)) {
        stop("tags must be a character vector")
    }

    isView <- .is_pipeflow_view(p)
    x <- if (isView) p[["pip"]] else p
    dat <- x[["pipeline"]]
    rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

    if (length(rows) == 0L || length(tags) == 0L) {
        return(invisible(p))
    }

    for (i in rows) {
        if (isTRUE(dat[["locked"]][[i]])) {
            next
        }

        oldTags <- dat[["tags"]][[i]]
        newTags <- unique(c(oldTags, tags))
        data.table::set(dat, i = i, j = "tags", value = list(list(newTags)))
    }

    invisible(p)
}


#' Remove tags from selected steps
#'
#' Removes tags from existing tags for all steps in the pipeline unless `p`
#' is a view, in which case tags are only removed for steps covered by the
#' view. Locked steps are skipped and not updated.
#' @param p A pipeflow pip or view.
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
#' p[["pipeline"]][["tags"]]
#' @export
pip_untag <- function(p, tags = character()) {
    .assert_pip_or_view(p)
    if (!is.character(tags)) {
        stop("tags must be a character vector")
    }

    isView <- .is_pipeflow_view(p)
    x <- if (isView) p[["pip"]] else p
    dat <- x[["pipeline"]]
    rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

    if (length(rows) == 0L || length(tags) == 0L) {
        return(invisible(p))
    }

    for (i in rows) {
        if (isTRUE(dat[["locked"]][[i]])) {
            next
        }

        oldTags <- dat[["tags"]][[i]]
        newTags <- setdiff(oldTags, tags)
        data.table::set(dat, i = i, j = "tags", value = list(list(newTags)))
    }

    invisible(p)
}


#' Lock selected steps against updates
#'
#' Locks all selected steps in the pipeline unless `p` is a view, in which
#' case only steps covered by the view are locked.
#' @param p A pipeflow pip or view.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 10) x) |>
#'   pip_add("fit", \(x = ~load) x * 2)
#' pip_run(p, lgr = NULL)
#'
#' # Lock only "load" via a view so it won't be re-executed or overwritten
#' pip_lock(pip_view(p, i = "load"))
#' p[["pipeline"]][["locked"]] # TRUE, FALSE
#'
#' # Locked steps are silently skipped during pip_run()
#' pip_run(p, lgr = NULL, force = TRUE)
#' p[["pipeline"]][["out"]][[1]] # still 10 — locked, not re-executed
#'
#' pip_unlock(p)
#' p[["pipeline"]][["locked"]] # FALSE, FALSE
#' @export
pip_lock <- function(p) {
    .assert_pip_or_view(p)

    isView <- .is_pipeflow_view(p)
    x <- if (isView) p[["pip"]] else p
    dat <- x[["pipeline"]]
    rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

    if (length(rows) == 0L) {
        return(invisible(p))
    }

    data.table::set(dat, i = rows, j = "locked", value = TRUE)
    invisible(p)
}


#' Unlock selected steps
#'
#' Unlocks all selected steps in the pipeline unless `p` is a view, in which
#' case only steps covered by the view are unlocked.
#' @param p A pipeflow pip or view.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x * 2)
#'
#' # Lock all steps, then unlock to restore normal execution
#' pip_lock(p)
#' p[["pipeline"]][["locked"]] # TRUE, TRUE
#'
#' pip_unlock(p)
#' p[["pipeline"]][["locked"]] # FALSE, FALSE
#' @export
pip_unlock <- function(p) {
    .assert_pip_or_view(p)

    isView <- .is_pipeflow_view(p)
    x <- if (isView) p[["pip"]] else p
    dat <- x[["pipeline"]]
    rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

    if (length(rows) == 0L) {
        return(invisible(p))
    }

    data.table::set(dat, i = rows, j = "locked", value = FALSE)
    invisible(p)
}


#' Create a pipeline view
#'
#' Creates a filtered view showing only a selected subset of steps.
#' A view references the underlying pipeline without copying it, so
#' operations like [pip_run()] and [pip_set_params()] applied to a view
#' affect only the selected steps.
#'
#' @param x A pipeflow pipeline or view.
#' @param i Optional row indices or step names to keep.
#' @param filter A named list of filters to apply. Each element can be a
#' character vector specifying the values to keep for the corresponding
#' property or, if `fixed` is FALSE, a regular expression. See examples
#' for usage.
#' @param tags Tag filter (character). Keeps steps with any matching tag.
#' @param fixed If TRUE, values in `filter` are treated as fixed strings,
#' otherwise they are treated as regular expressions.
#' @param ... further args passed to `grepl` (only in effect when `fixed`
#' is `FALSE`).
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
#' pip_view(p, filter = list(state = "new"))
#'
#' # Combine filters: step pattern AND state
#' pip_view(p, filter = list(step = "model", state = "new"))
#'
#' # Filter by tag — keeps steps that have *any* of the given tags
#' pip_view(p, tags = "daily")
#'
#' # Combine explicit step selection with a filter (intersection)
#' pip_view(p,
#'   i      = c("load_raw", "fit_model"),
#'   filter = list(state = "new")
#' )
#'
#' # Select by integer row indices
#' pip_view(p, i = c(1L, 2L), filter = list(state = "new"))
#'
#' # Use a regex pattern to match step names
#' pip_view(p, filter = list(step = "_model$"), fixed = FALSE)
#'
#' # Views are composable: create a view-of-view for progressive narrowing
#' v1 <- pip_view(p, tags = "daily")
#' print(v1) # load_raw, eval_model
#' v2 <- pip_view(v1, tags = "report")
#' print(v2) # eval_model only
pip_view <- function(
    x,
    i = integer(),
    filter = list(),
    tags = character(),
    fixed = TRUE,
    ...
) {
    .assert_pip_or_view(x)
    isView <- .is_pipeflow_view(x)
    pip <- if (isView) x[["pip"]] else x
    dat <- pip[["pipeline"]]

    # For view-of-view, filter only within parent view rows and map local
    # matches back to absolute row indices of the underlying pipeline.
    parent_rows <- if (isView) as.integer(x[["rows"]]) else seq_len(nrow(dat))
    sub <- dat[parent_rows]
    keep <- rep(TRUE, nrow(sub))

    # Filters
    validFilters <- c("step", "depends", "state", "exec")
    for (name in names(filter)) {
        if (!(name %in% validFilters)) {
            stop(sprintf(
                "Invalid filter name: '%s' - can be one of: %s",
                name,
                paste(validFilters, collapse = ", ")
            ))
        }
        hasMatch <- if (fixed) {
            sapply(sub[[name]], \(e) any(e %in% filter[[name]]))
        } else {
            sapply(sub[[name]], \(e) any(grepl(filter[[name]], x = e, ...)))
        }
        keep <- keep & hasMatch
    }

    # Tags
    if (length(tags) > 0) {
        hasTag <- vapply(
            sub[["tags"]],
            FUN = \(x) any(x %in% tags),
            FUN.VALUE = logical(1)
        )
        keep <- keep & hasTag
    }

    # Rows
    rows <- parent_rows[which(keep)]
    if (length(i) > 0) {
        if (is.character(i)) {
            i <- .pip_steps_to_rows(pip, i)
        }
        if (!is.numeric(i)) {
            stop("i must be numeric row indices or character step names")
        }
        if (any(i < 1L | i > nrow(dat))) {
            stop(
                "Invalid row indices in 'i': ",
                toString(i[i < 1L | i > nrow(dat)])
            )
        }
        rows <- intersect(rows, as.integer(i))
    }

    name <- sprintf("%s view", x[["name"]])
    view <- list(pip = pip, name = name, rows = rows)
    class(view) <- "pipeflow_view"
    view
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
#' v <- pip_view(p, i = c("s2", "s3"))
#' length(v) # 2
#' @rdname length.pipeflow
#' @export
length.pipeflow_pip <- function(x) {
    as.integer(nrow(x[["pipeline"]]))
}

#' @rdname length.pipeflow
#' @export
length.pipeflow_view <- function(x) {
    as.integer(length(x[["rows"]]))
}

#' Extract or subset a pipeline
#'
#' Returns a new pipeline containing selected steps and all required upstream
#' dependencies.
#' @param x A pipeflow pipeline object.
#' @param i integer (row indices) or character vector (step names) of steps to
#' select
#' @param ... not used
#' @return A new pipeflow pipeline object.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 5) seq_len(n)) |>
#'   pip_add("square", \(x = ~load) x^2) |>
#'   pip_add("total", \(x = ~square) sum(x))
#'
#' # Select by step name — upstream deps are pulled in automatically.
#' # Selecting only "total" still includes "load" and "square".
#' sub <- p["total"]
#' sub[["pipeline"]][["step"]] # "load", "square", "total"
#'
#' # Select a subset of steps by name vector
#' p[c("load", "square")][["pipeline"]][["step"]] # "load", "square"
#'
#' # Select by integer row index
#' p[1:2][["pipeline"]][["step"]] # "load", "square"
#' @rdname Extract.pipeflow_pip
#' @export
`[.pipeflow_pip` <- function(x, i, ...) {
    dat <- x[["pipeline"]]
    n <- nrow(dat)

    # Resolve selected rows from either row indices or step names
    if (missing(i)) {
        return(pip_clone(x))
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

    out <- pip_new(name = x[["name"]])
    if (length(rows) == 0L) {
        return(out)
    }

    # Get all nodes that are reachable from the selected rows via upstream
    startNodes <- dat[[".nodeId"]][rows]
    keepNodes <- dag_get_reachable_nodes_up(
        x[[".dag"]],
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
    out[["pipeline"]] <- subsetDat
    out[[".dag"]] <- d
    out[[".steps_to_nodes"]] <- stepsToNodes

    out
}


#' Extract bindings, columns, or row-level values from a pipeline
#'
#' Extracts values from a pipeline using one or two indices.
#' With a single string name, named fields such as `"pipeline"` or `"name"`
#' are returned first; anything else returns the matching step-table column.
#' With two indices (`row`, `column`), a single cell is extracted.
#' @param i integer (row indices) or character vector (step names) of steps to
#' select
#' @param j column names to select
#' @return Extracted value(s), depending on `i` and `j`.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#'
#' # Access internal objects by name
#' p[["pipeline"]] # the full step table
#' p[["name"]] # "pipe"
#'
#' # Shorthand column access (equivalent to p[["pipeline"]][["step"]])
#' p[["step"]]
#'
#' # Two-index form: p[[row, column]] extracts a single cell
#' p[["fit", "depends"]] # "load"
#' p[[2, "state"]] # state of the second step
#' @rdname Extract.pipeflow_pip
#' @export
`[[.pipeflow_pip` <- function(x, i, j, ...) {
    # Keep environment-style extraction for internal bindings, e.g.
    # x[["pipeline"]], x[[".dag"]], x[[".steps_to_nodes"]].
    if (missing(j)) {
        if (missing(i)) {
            stop("i must be provided")
        }

        # Internal bindings have priority over step names/column names.
        # This guarantees p[["name"]] and p[["pipeline"]] behave like
        # environment access even if steps with those names exist.
        if (
            is.character(i) &&
                length(i) == 1L &&
                !is.na(i) &&
                exists(i, where = x, inherits = FALSE)
        ) {
            return(get(i, envir = x, inherits = FALSE))
        }

        # Lightweight fallback: delegate single-argument extraction to the
        # pipeline data.table column extractor.
        return(x[["pipeline"]][[i]])
    }

    col <- x[["pipeline"]][[j]]
    if (missing(i)) {
        return(col)
    }

    if (is.character(i)) {
        i <- .pip_steps_to_rows(x, i)
    }

    res <- col[i]
    if (length(i) == 1) {
        res <- res[[1]]
    }
    res
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
#' of steps will be printed.
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
    dat <- x[["pipeline"]]
    n <- nrow(dat)

    if (identical(cols, "core")) {
        cols <- c("step", "depends", "out", "state")
        has_tags <- any(lengths(dat[["tags"]]) > 0L)
        if (has_tags) {
            cols <- append(cols, "tags")
        }
        has_non_auto_exec <- any(dat[["exec"]] != "auto")
        if (has_non_auto_exec) {
            cols <- append(cols, "exec")
        }
    }
    if (identical(cols, "all")) {
        isHidden <- function(name) startsWith(name, ".")
        cols <- Filter(Negate(isHidden), colnames(dat))
    }

    if (header) {
        title <- sprintf(
            "<pipeflow_pip> %s (%d step%s)",
            x[["name"]],
            n,
            ifelse(n == 1, "", "s")
        )
        line <- paste(rep("-", nchar(title)), collapse = "")
        cat(title, line, sep = "\n")
    }

    if (length(rows) == 0) {
        rows <- seq_len(n)
    }

    print(
        dat[rows, cols, with = FALSE],
        topn = topn,
        nrows = nrows,
        row.names = row.names,
        class = class,
        ...
    )

    invisible(x)
}


#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x, tags = "io") |>
#'   pip_add("s2", \(x = ~s1) x + 1, tags = "model")
#'
#' # A view header shows how many steps are selected out of the total
#' v <- pip_view(p, tags = "model")
#' print(v) # "<pipeflow_view> pipe view (1 of 2 steps)"
#' @rdname print
#' @export
print.pipeflow_view <- function(x, header = TRUE, ...) {
    pip <- x[["pip"]]
    rows <- x[["rows"]]
    nr <- length(rows)
    n <- nrow(pip[["pipeline"]])

    if (header) {
        title <- sprintf(
            "<pipeflow_view> %s (%d of %d step%s)",
            x[["name"]],
            nr,
            n,
            ifelse(n == 1, "", "s")
        )
        line <- paste(rep("-", nchar(title)), collapse = "")
        cat(title, line, sep = "\n")
    }

    if (length(rows) == 0L) {
        return(invisible(x))
    }
    print(pip, rows = rows, header = FALSE, row.names = FALSE, ...)
    invisible(x)
}
