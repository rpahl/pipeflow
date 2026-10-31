# ======================================================================================
# Helpers
# ======================================================================================

# -------
# Asserts
# -------
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


# --------------------
# All-or-nothing edits
# --------------------
#' Edit a pipeline completely or not at all
#'
#' Evaluates `edit`, which changes the pipeline one step at a time, e.g. in
#' `p[i, j] <- value`. If it fails, the pipeline is restored from a snapshot
#' (`.pip_snapshot()`), so that the failed edit changes nothing. The pipeline is
#' restored as soon as the error is signalled, before the handlers outside see it. So a
#' step that edits its own pipeline via `.self` and fails with the edit is still marked
#' `"failed"` by `.pip_run_row()`. An interrupt restores the pipeline on exit.
#'
#' @param x A pipeflow pipeline or view.
#' @param edit The code that edits `x`. It is evaluated in the calling frame, so it can
#' assign to the variables there.
#' @return `NULL`, invisibly.
#' @noRd
.pip_all_or_nothing <- function(x, edit) {
    pipenv <- .pip_pipenv(x)
    snapshot <- .pip_snapshot(pipenv)
    isPending <- TRUE
    restore <- function(...) {
        if (isPending) {
            isPending <<- FALSE
            .pip_restore(pipenv, snapshot)
        }
    }
    on.exit(restore(), add = TRUE)
    withCallingHandlers(edit, error = restore)
    isPending <- FALSE
    invisible(NULL)
}


#' Take a snapshot of a pipeline to undo a failed edit
#'
#' Like `.pip_clone(deep = FALSE)`, the snapshot is a light copy of the step table: its
#' columns are new vectors, but the functions, params and outputs are shared. The step
#' table and the two step maps are restored in place (see `.pip_restore()`), because a
#' running pipeline keeps references to them (`pip_run()`, `.pip_run_row()`).
#'
#' @param pipenv The inner pipeline environment.
#' @return A list with the step table and copies of its columns, a copy of the DAG, the
#' two step maps and copies of their entries, and the kept inner pipelines of the step
#' functions made by [as.function.pipeflow()], which a rename moves (see
#' `.fun_rename()`).
#' @noRd
.pip_snapshot <- function(pipenv) {
    dat <- pipenv[["data"]]
    mapNames <- c(".steps_to_nodes", ".steps_with_current_out")
    maps <- mget(mapNames, envir = pipenv)
    caches <- Filter(Negate(is.null), lapply(dat[["fun"]], .fun_cache))
    list(
        data = dat,
        columns = as.list(dat[seq_len(nrow(dat))]),
        dag = dag_clone(.pip_dag(pipenv)),
        maps = maps,
        entries = lapply(maps, as.list, all.names = TRUE),
        caches = caches,
        slots = lapply(caches, \(cache) cache[["slots"]])
    )
}


#' Restore a pipeline from a snapshot
#'
#' @param pipenv The inner pipeline environment.
#' @param snapshot The result of `.pip_snapshot(pipenv)`.
#' @return `pipenv`, invisibly. The snapshot is written into `pipenv`, so the views of
#' the pipeline see the restored pipeline as well.
#' @noRd
.pip_restore <- function(pipenv, snapshot) {
    # The step table that was edited in place, or replaced by a new one if steps were
    # removed
    dat <- snapshot[["data"]]
    columns <- snapshot[["columns"]]
    data.table::set(dat, j = names(columns), value = columns)
    pipenv[["data"]] <- dat

    # Nothing keeps the DAG across a step call (see .pip_dag()), so it can be replaced
    pipenv[[".dag"]] <- snapshot[["dag"]]

    for (name in names(snapshot[["maps"]])) {
        map <- snapshot[["maps"]][[name]]
        rm(list = ls(map, all.names = TRUE), envir = map)
        list2env(snapshot[["entries"]][[name]], envir = map)
        pipenv[[name]] <- map
    }

    caches <- snapshot[["caches"]]
    for (k in seq_along(caches)) {
        assign("slots", snapshot[["slots"]][[k]], envir = caches[[k]])
    }
    invisible(pipenv)
}


# ----------
# Compaction
# ----------
#' Compact a pipeline to a subset of steps
#'
#' Helper to build new pipeline from the steps of `x` whose `nodeId` is contained in
#' `keepNodes`. The kept rows get a compact node id sequence, and the DAG and the
#' step->node lookup are re-built from the `depends` values. For a small number of kept
#' steps, the DAG is built node by node in R. If roughly a third or more of the steps
#' are kept, it is faster to clone the existing DAG, remove the excluded nodes in place,
#' and compact the ids in C++ via dag_rebuild().
#'
#' @param x A pipeflow pipeline.
#' @param keepNodes Integer vector of node ids to keep.
#' @param rebuildThresh Fraction of steps to keep above which the DAG is rebuilt. The
#' default value of 0.3 was determined empirically, that is, at this fraction both
#' methods take roughly the same time.
#' @return A new pipeflow pipeline with the selected steps and a compact node id
#' sequence.
#' @noRd
.pip_compact <- function(x, keepNodes, rebuildThresh = 0.3) {
    pipenv <- .pip_pipenv(x)
    data <- pipenv[["data"]]
    out <- pip_new(name = x[["name"]])
    keepNodes <- as.integer(keepNodes)
    rows <- which(data[["nodeId"]] %in% keepNodes)
    if (length(rows) == 0L) {
        return(out)
    }

    subDat <- data.table::copy(data[rows])
    useRebuild <- length(rows) / nrow(data) >= rebuildThresh

    if (useRebuild) {
        # Clone the existing DAG, drop all nodes that are not kept, and let the C++ side
        # compact the node ids. Nodes that are still alive correspond to the current
        # rows of `data`.
        d <- dag_clone(.pip_dag(pipenv))
        dead <- setdiff(data[["nodeId"]], keepNodes)
        for (id in dead) {
            dag_remove_node(d, id, force = TRUE)
        }
        oldOrder <- dag_rebuild(d)
        subDat[["nodeId"]] <- match(subDat[["nodeId"]], oldOrder) - 1L
    } else {
        # Re-map node ids to a compact sequence
        subDat[["nodeId"]] <- seq_along(subDat[["nodeId"]]) - 1L
    }

    # Rebuild the step->node lookup table
    nodeIds <- subDat[["nodeId"]]
    stepsToNodes <- list2env(
        as.list(stats::setNames(nodeIds, subDat[["step"]])),
        envir = new.env(parent = emptyenv())
    )

    if (!useRebuild) {
        # Build a new DAG from scratch that matches the kept rows
        d <- dag_new()
        depends <- subDat[["depends"]]
        for (k in seq_along(nodeIds)) {
            dag_add_node(d)
            # A step can refer to the same step with several arguments, but there is
            # only one edge
            deps <- unique(depends[[k]])
            if (length(deps) == 0L) {
                next
            }
            from <- as.integer(unname(unlist(mget(
                deps,
                envir = stepsToNodes,
                inherits = FALSE
            ))))
            to <- as.integer(nodeIds[[k]])
            dag_add_edges_to(d, from = from, to = to)
        }
    }

    data.table::setindexv(subDat, list("step", "nodeId"))
    env <- .pip_pipenv(out)
    env[["data"]] <- subDat
    env[[".dag"]] <- d
    env[[".steps_to_nodes"]] <- stepsToNodes
    out
}


# -------------
# Row selection
# -------------
# The data.table operators that are available in the boolean filters of `[.pipeflow`,
# also when data.table is not attached.
.filter_ops <- list(
    `%like%` = data.table::`%like%`,
    `%ilike%` = data.table::`%ilike%`,
    `%flike%` = data.table::`%flike%`,
    `%plike%` = data.table::`%plike%`,
    `%chin%` = data.table::`%chin%`,
    `%between%` = data.table::`%between%`,
    `%inrange%` = data.table::`%inrange%`,
    `%notin%` = data.table::`%notin%`
)

#' Resolve the `i` selector of `[.pipeflow` to rows of the step table
#'
#' `i_expr` is evaluated with the columns of the covered rows (those of the view, or all
#' rows of a pipeline) as variables, so a filter like `state == "done"` only sees the
#' steps of a view. What it evaluates to selects the rows:
#' * logical: a filter over the covered rows. A single value is recycled, otherwise it
#'   needs one value per covered row and no `NA`.
#' * numeric: positions among the covered rows, so `1` is the first step of a view, not
#'   of the pipeline. Negative positions exclude rows, as in base R.
#' * character: step names, which must be covered by the view.
#'
#' @param x A pipeflow pipeline or view.
#' @param i_expr The unevaluated `i` expression.
#' @param enclos The enclosing environment of the evaluation, e.g. the caller of `[`.
#' The data.table filter operators (`.filter_ops`) are put in between, so `%like%` and
#' others work without attaching data.table.
#' @param keep_order If `FALSE`, positions and step names are sorted and made unique, so
#' the rows come in pipeline order. If `TRUE`, they are kept in the given order, with
#' repeats. Logical filters always give rows in pipeline order.
#' @return Integer vector of absolute row indices of the step table.
#' @noRd
.pip_select_rows <- function(x, i_expr, enclos, keep_order = FALSE) {
    dat <- .pip_pipenv(x)[["data"]]
    view <- .is_pipeflow_view(x)
    rows <- .pip_view_rows(x)
    sub <- if (view) dat[rows] else dat

    value <- eval(
        i_expr,
        envir = sub,
        enclos = list2env(.filter_ops, parent = enclos)
    )

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
        paste(
            "`i` must evaluate to row indices, step names, or a logical",
            "filter, not %s"
        ),
        typeof(value)
    ))
}

#' Get the rows of the step table of steps
#'
#' @param x A pipeflow pipeline or view. A view does not restrict the steps.
#' @param steps Character vector of step names.
#' @return Integer vector of absolute row indices, in the order of `steps`. Signals an
#' error for `NA`, empty or unknown step names.
#' @noRd
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


# -----------
# Run control
# -----------
#'
#' Does the work of `.self$restart()`, called from within a step (see the `$` method).
#' Sets the run state to `"restart"`, so that `pip_run()` starts over after the step.
#' `times` limits the number of restarts: once the pipeline has restarted `times` times,
#' the call does not restart it again but resets the count, so the run goes on.
#'
#' @param pipenv The inner pipeline environment.
#' @param force The `force` of the restarted run, see [pip_run()].
#' @param times The maximum number of restarts.
#' @return `NULL`, invisibly.
#' @noRd
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

#' Ask a running pipeline to stop after the current step
#'
#' Does the work of `.self$halt()`, called from within a step (see the `$` method). Sets
#' the run state to `"halted"`, so that `pip_run()` runs no further steps.
#'
#' @param pipenv The inner pipeline environment.
#' @return `NULL`, invisibly.
#' @noRd
.pip_halt <- function(pipenv) {
    pipenv[[".run_state"]][] <- .run_states[["halted"]][["name"]]
    invisible()
}


# --------------
# Element access
# --------------
#' The virtual methods of a pipeline
#'
#' Maps method names to the functions that implement them, so that `[[` and `$` can
#' offer `p$add(...)` as a shorthand for `pip_add(p, ...)`. The map is built on the
#' first call, since the `pip_*` functions are not all defined yet when this file is
#' sourced.
#'
#' @return An environment: method name -> function whose first argument is the pipeline
#' or view.
#' @noRd
.pip_method_table <- local({
    table <- NULL
    function() {
        if (is.null(table)) {
            # Build table
            table <<- new.env(parent = emptyenv())
            values <- list(
                add = pip_add,
                remove = pip_remove,
                rename = pip_rename,
                replace = pip_replace,
                run = pip_run,
                reset = pip_reset,
                tag = pip_tag,
                untag = pip_untag,
                lock = pip_lock,
                unlock = pip_unlock,
                set_params = pip_set_params,
                get_params = pip_get_params,
                collect = pip_collect,
                clone = pip_clone,
                graph = pip_graph,
                data = pip_data,
                restart = function(x, force = TRUE, times = 1L) {
                    .pip_restart(
                        .pip_pipenv(x),
                        force = force,
                        times = times
                    )
                },
                halt = function(x) .pip_halt(.pip_pipenv(x))
            )
            for (nm in names(values)) {
                table[[nm]] <- values[[nm]]
            }
        }
        table
    }
})


#' Extract a field, method, column or cell of a pipeline or view
#'
#' Does the work of `[[.pipeflow`. With `j = NULL`, `i` is looked up in this order:
#' 1. `"view"`: the steps covered by a view, `NULL` for a pipeline.
#' 2. `"pipenv"` and the other fields of the wrapper (`"name"`).
#' 3. A virtual method (see `.pip_method_table()`), returned as a function with `x`
#'    bound as its first argument.
#' 4. A column of the step table, by name or position, with its values of the covered
#'    rows named by step, or `NULL` for an unknown column.
#'
#' @param x A pipeflow pipeline or view.
#' @param i A single field, method or column name or column position, or, with `j`, a
#' single step name or row position. Row positions count among the rows covered by `x`.
#' @param j `NULL`, or a single column name to extract one cell.
#' @return The extracted value. A cell keeps the class of its column, see `.cell()`.
#' @noRd
.pip_subset2 <- function(x, i, j = NULL) {
    if (missing(i)) {
        stop("i must be provided")
    }
    if (length(i) != 1L || is.na(i)) {
        stop("i must be a single step name or row index")
    }
    if (is.null(j)) {
        # The `view` field holds node ids, so the covered steps are returned.
        if (identical(i, "view")) {
            if (!.is_pipeflow_view(x)) {
                return(NULL)
            }
            return(.pip_pipenv(x)[["data"]][["step"]][.pip_view_rows(x)])
        }
        # Via .pip_pipenv(), so that the step table of a saved pipeline gets its missing
        # columns also when it is read directly.
        if (identical(i, "pipenv")) {
            return(.pip_pipenv(x))
        }

        # List fields of the wrapper have priority over column names.
        if (is.character(i) && length(i) == 1L && !is.na(i)) {
            if (i %in% names(x)) {
                return(.subset2(x, i))
            }
        }

        # Virtual methods (an O(1) lookup followed by a single closure that binds the
        # backend to the current object).
        if (is.character(i) && length(i) == 1L && !is.na(i)) {
            fn <- get0(i, envir = .pip_method_table(), inherits = FALSE)
            if (!is.null(fn)) {
                return(function(...) fn(x, ...))
            }
        }

        # case x[[col]]; only the column is read, not the whole step table
        data <- .pip_pipenv(x)[["data"]]
        col <- data[[i]]
        if (is.null(col)) {
            return(NULL)
        }
        rows <- .pip_view_rows(x)
        return(stats::setNames(col[rows], data[["step"]][rows]))
    }

    # Two-index form extracts a single cell from a single row.
    if (!is.character(j) || length(j) != 1L || is.na(j)) {
        stop("j must be a single column name")
    }

    data <- .pip_pipenv(x)[["data"]]
    rows <- .pip_view_rows(x)
    if (is.character(i)) {
        # case x[[stepName, col]]; the row is relative to the view's rows
        row <- data.table::chmatch(i, data[["step"]][rows])
        if (is.na(row)) {
            .pip_steps_to_rows(x, steps = i) # signals unknown steps
            stop("selected step not part of view: ", i)
        }
    } else {
        # case x[[i, col]]
        row <- as.integer(i)
        if (row < 1L || row > length(rows)) {
            stop("row index out of bounds")
        }
    }

    .cell(data[[j]], rows[[row]])
}


#' Get one element of a step table column
#'
#' Unlike `[[`, which drops the class of an atomic vector unless it has its own `[[`
#' method (as `POSIXct` has, but `difftime` hasn't), the element of an atomic column
#' keeps its class, e.g. a `duration` stays a `difftime`.
#'
#' @param col A column of the step table.
#' @param row A single row index.
#' @return The element of `col` at `row`.
#' @noRd
.cell <- function(col, row) {
    if (is.list(col)) col[[row]] else col[row]
}


# -------------------
# Cross-pipeline copy
# -------------------
#' Copy steps of one pipeline or view onto selected steps of another
#'
#' Does the work of `x[i] <- q` when `q` is a pipeline or view. The k-th selected row of
#' `x` gets the step name, function, params, exec mode, output, state, times, tags and
#' lock of the k-th covered row of `q`. The derived columns (`nodeId`, `depends`,
#' `unbound`) are not copied but follow from the copied function and params. The
#' references in the params of `q` are given by name, so a relative reference (`~-1`)
#' keeps pointing to the step that it pointed to in `q`, which must exist in `x`. If a
#' row fails, the rows copied before are undone (see `.pip_all_or_nothing()`). `q` may
#' be `x` itself.
#'
#' @param x A pipeflow pipeline or view.
#' @param i The unevaluated `i` of `[<-`, see `.pip_select_rows()`. Its rows are taken
#' in the given order.
#' @param q A pipeflow pipeline or view with as many covered rows as `i` selects.
#' @param enclos The environment to evaluate `i` in.
#' @return `x`.
#' @noRd
.pip_copy_from <- function(x, i, q, enclos) {
    rowsP <- .pip_select_rows(x, i, enclos, keep_order = TRUE)
    n <- length(rowsP)
    if (n == 0L) {
        return(x)
    }

    envQ <- .pip_pipenv(q)
    dataQ <- envQ[["data"]]
    rowsQ <- .pip_view_rows(q)
    if (length(rowsQ) != n) {
        stop(sprintf(
            "cannot assign from a pipeline with %d step%s to %d selected row%s",
            length(rowsQ),
            if (length(rowsQ) == 1L) "" else "s",
            n,
            if (n == 1L) "" else "s"
        ))
    }

    dataP <- .pip_pipenv(x)[["data"]]
    viewRowsP <- .pip_view_rows(x)
    iSelP <- if (.is_pipeflow_view(x)) match(rowsP, viewRowsP) else rowsP

    props <- c(
        "step",
        "fun",
        "params",
        "exec",
        "out",
        "state",
        "time",
        "time_start",
        "duration",
        "tags",
        "locked"
    )

    # Since the source can be `x` itself, we will work on a snapshot to prevent that any
    # writes below would alter rows that are still to be copied (e.g. pip_replace()
    # marking downstream steps as outdated).
    qSnapshot <- dataQ[rowsQ, c(props, "depends"), with = FALSE]

    # If a row fails, the rows copied before are undone, so that `x` is left as it was.
    .pip_all_or_nothing(x, {
        for (k in seq_len(n)) {
            pRow <- rowsP[[k]]

            qVals <- lapply(props, function(prop) .cell(qSnapshot[[prop]], k))
            names(qVals) <- props

            # 1) Step name (a no-op when both pipelines use the same names).
            x[[dataP[["step"]][[pRow]], "step"]] <- qVals[["step"]]

            # 2) Function, params and exec mode. The exec mode is set here, because
            #    setting a new one in 3) would outdate the copied state. The params of q
            #    override the defaults of the function. Their references are pointed to
            #    the steps that the step of q depends on, by name, because a relative
            #    reference (~-1) is stale once steps were inserted or removed before it.
            #    pip_replace() then resolves them against the steps of `x`.
            params <- qVals[["params"]]
            depends <- qSnapshot[["depends"]][[k]]
            for (arg in intersect(names(depends), names(params))) {
                params[[arg]] <- formula_set_ref(params[[arg]], depends[[arg]])
            }
            # An unbound param can still have a formula as value: the function made by
            # as.function() sets its arguments as values, also formulas, and a step can
            # expose that pipeline, e.g. via pip_clone(.self). pip_replace() would take
            # it as a reference, so it is replaced with a placeholder and its value is
            # put back below.
            isValue <- !(names(params) %in% names(depends)) &
                vapply(params, is_one_sided_formula, logical(1))
            values <- params[isValue]
            params[isValue] <- list(NULL)
            pip_replace(
                x,
                step = qVals[["step"]],
                fun = qVals[["fun"]],
                params = params,
                exec = qVals[["exec"]]
            )
            if (length(values) > 0L) {
                dataX <- .pip_pipenv(x)[["data"]]
                iRow <- data.table::chmatch(qVals[["step"]], dataX[["step"]])
                stored <- dataX[["params"]][[iRow]]
                stored[names(values)] <- values
                data.table::set(
                    dataX,
                    i = iRow,
                    j = "params",
                    value = list(list(stored))
                )
            }

            # 3) Runtime state and annotation columns.
            for (prop in setdiff(props, c("step", "fun", "params", "exec"))) {
                x[[iSelP[[k]], prop]] <- qVals[[prop]]
            }
        }
    })

    x
}


# ----------------
# Pipeline binding
# ----------------
#' Bind two pipelines by appending the steps of one to the other
#'
#' Does the work of `rbind()` for two pipelines. A step of `y` whose name is taken (by a
#' step of `x` or an earlier step of `y`) is renamed by adding the first free number
#' from 2 on, e.g. `a` becomes `a2`, and references to it in `y` follow. Relative
#' references of `y` become references by name, so they keep pointing to the steps of
#' `y`. The current params of the steps of `y` become the defaults of their functions.
#' Outputs, states, times and locks of the steps of `y` are kept.
#'
#' @param x,y Pipeflow pipelines (not views). Neither is changed.
#' @return A new pipeline named `"<name of x>-<name of y>"`.
#' @noRd
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

    # Append all (potentially renamed) steps of y in one pass: transform each row
    # (re-create the formula dependencies after renaming), allocate a fresh node id,
    # wire the DAG edges, and collect the rows for a single rbindlist.
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

        # Point the formula dependencies to the (renamed) steps of y, which also turns
        # relative references into step names. References wrapped in try() stay wrapped.
        for (arg in intersect(names(depends), names(params))) {
            params[[arg]] <- formula_set_ref(params[[arg]], depends[[arg]])
        }
        # Fold current parameter values into the defaults of the existing function
        # arguments. `[<-` with list() keeps a NULL value, which `[[<-` would drop
        # together with the argument.
        fml <- formals(fun)
        for (nm in intersect(names(params), setdiff(names(fml), "..."))) {
            fml[nm] <- list(params[[nm]])
        }
        formals(fun) <- fml

        # Update DAG: allocate the node, register the step, add edges to its upstream
        # steps (which are either in `out` or already appended).
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
        j = c("out", "state", "time", "time_start", "duration", "locked"),
        value = list(
            yyDat[["out"]],
            yyDat[["state"]],
            yyDat[["time"]],
            yyDat[["time_start"]],
            yyDat[["duration"]],
            yyDat[["locked"]]
        )
    )
    outEnv[["data"]] <- data.table::rbindlist(
        list(outEnv[["data"]], appRows),
        use.names = TRUE
    )
    .pip_warn_param_conflicts(
        out,
        steps = yyDat[["step"]],
        others = .pip_pipenv(x)[["data"]][["step"]]
    )
    out
}


# ======================================================================================
# S3 methods
# ======================================================================================

# ----------
# Dimensions
# ----------
#' @name dim.pipeflow
#' @rdname dim.pipeflow
#' @export
length.pipeflow <- function(x) {
    as.integer(length(.pip_view_rows(x)))
}


#' Dimensions of a pipeflow pipeline or view
#'
#' Treats a pipeline as a table of steps: `length()` and `nrow()` return the number of
#' steps, `ncol()` returns the number of columns of the underlying step table, and
#' `dim()` returns both. Views report only the steps covered by the view.
#' @param x A pipeflow pipeline or view
#' @return
#' * `length()`, `nrow()`: the number of steps
#' * `ncol()`: the number of columns of the step table
#' * `dim()`: vector with the number of steps and columns.
#'
#' @seealso [base::dim()], [base::nrow()], [base::length()]
#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1) |>
#'   pip_add("s3", \(x = ~s2) x * 2)
#'
#' dim(p)
#' nrow(p)
#' ncol(p)
#' length(p)
#' length(p) == nrow(p) # TRUE
#'
#' # A view reports only the number of selected (visible) steps
#' v <- pip_view(p, step = c("s2", "s3"))
#' length(v) # 2
#' nrow(v) # 2
#' @rdname dim.pipeflow
#' @export
dim.pipeflow <- function(x) {
    c(
        as.integer(length(.pip_view_rows(x))),
        ncol(
            .pip_pipenv(x)[["data"]]
        )
    )
}


# -------------------
# Extract and replace
# -------------------
#' Extract or replace parts of a pipeline
#'
#' A pipeline can be subset, read from and written to with the usual extract and replace
#' operators, treating it as a table of steps. The extraction operators mirror base R,
#' and the replacement operators update steps (or their properties) in place.
#'
#' @details
#' ## Extract or subset a pipeline (`x[i, j, view = TRUE]`)
#'
#' `p[...]` selects steps from a pipeline. By default, a lightweight [pip_view()] is
#' returned that references the selected steps without copying them. Set `view = FALSE`
#' to instead get a new, self-contained pipeline that includes all required upstream
#' dependencies.
#'
#' Three forms of row selection are supported:
#' * **row indices** (relative to the current selection), e.g. `p[2:3]`,
#' * **step names**, e.g. `p[c("load", "fit")]`,
#' * a **boolean filter** that is evaluated in the context of the pipeline's step table,
#'   e.g. `p[state == "new"]` or `p[step %in% c("load", "fit") & tags %like% "io"]`.
#'
#' Boolean filters may reference the columns of the step table (`step`, `fun`, `params`,
#' `depends`, `tags`, `state`, ...) directly as variables. The filter operators of
#' \pkg{data.table} (`%like%`, `%ilike%`, `%flike%`, `%plike%`, `%chin%`, `%between%`,
#' `%inrange%` and `%notin%`) are available in the filter without attaching
#' \pkg{data.table}. `p[]` returns a copy of the pipeline.
#'
#' Note that `tags` is a list column, so `%like%` matches the pattern against the tags
#' of each step converted to a single string, e.g. `"character(0)"` for a step without
#' tags and `'c("io", "daily")'` for a step with two tags. A pattern can therefore also
#' match untagged steps (e.g. `tags %like% "a"`), and anchored patterns such as `"^io$"`
#' miss steps with several tags. To match tags exactly, use the `tags` argument of
#' [pip_view()], e.g. `pip_view(p, tags = "io")`.
#'
#' When `j` is provided, the selected steps are returned as a step-table extraction
#' rather than a pipeline. `j` must be a character vector of column names: `p[, j]`
#' keeps those columns for all steps and `p[i, j]` first selects the rows and then keeps
#' those columns. The result in this case is a `data.table`.
#'
#' If done on a view, row selection is relative to the covered steps, and step names
#' must be part of the view. For validated, programmatic filters with dedicated
#' arguments (such as matching *any* of a set of tags) use [pip_view()] instead.
#' @param x A pipeflow pipeline or view object.
#' @param i The row or field selector. For `[` and `[<-`, the rows to select: integer
#' row indices, character step names, or a boolean filter expression evaluated in the
#' context of the step table. For `[[`, a step-table column name, a meta field, or — in
#' the two-index form — a step name or integer row index. For `[[<-`, a step name, an
#' integer row index, or one of the meta fields `"name"` and `"view"`. For a view,
#' indices are relative to the covered steps and step names must be part of the view.
#' @param j For `[`, an optional character vector of step-table column names to extract.
#' For `[<-`, the single step property to assign. For `[[` and `[[<-`, the column or
#' step property to extract or assign.
#' @param view For `[`, if `TRUE` (default), a view referencing the selected steps is
#' returned. If `FALSE`, a new pipeline is returned that includes the selected steps and
#' all their upstream dependencies. Ignored when `j` is provided. With no selection,
#' `v[, view = FALSE]` turns a view into a standalone pipeline of all its covered steps,
#' which is also the way to get a copy of a view, as [pip_clone()] requires a full
#' pipeline.
#' @param value The value to assign (`[[<-`, `[<-`).
#' @return For `[`, a pipeflow view (if `view = TRUE`) or a new pipeflow pipeline (if
#' `view = FALSE`); if `j` is provided, a `data.table` with the selected rows and
#' columns. For `[[`, the extracted step-table column, single cell, or meta field. For
#' the assignment forms, the updated pipeline, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(n = 5) seq_len(n), tags = c("io", "daily")) |>
#'   pip_add("square", \(x = ~load) x^2, tags = "model") |>
#'   pip_add("total", \(x = ~square) sum(x), tags = c("model", "report"))
#'
#' # By default, `[` returns a view into the selected steps.
#' p["total"] # view with a single step
#'
#' # Select by name vector or integer row index
#' p[c("load", "square")][["step"]]   # view -> "load", "square"
#' p[1:2, view = FALSE][["step"]]     # pipeline -> "load", "square"
#'
#' # Boolean filters are evaluated against the step table
#' p[tags %like% "model"][["step"]]  # "square", "total"
#'
#' # view = FALSE extracts a pipeline with all upstream dependencies
#' p[tags %like% "report", view = FALSE][["step"]] # "load", "square", "total"
#'
#' # No arguments returns a copy of the pipeline
#' p2 <- p[]
#' pip_run(p2)
#' p
#' p2
#'
#' # Two-index extraction selects step-table columns by name
#' p[, "step"]                     # one-column data.table
#' p[c("load", "square"), "out"]   # selected rows, one column
#'
#' # Works with views - selection is relative to the covered steps
#' v <- pip_view(p, tags = "model")
#' v[1L, "out"]
#' v[, "step"]
#' @name Extract.pipeflow
#' @rdname Extract.pipeflow
#' @export
`[.pipeflow` <- function(x, i, j, view = TRUE) {
    .assert_pip_or_view(x)

    if (!.is_single(view, "logical") || is.na(view)) {
        stop("view must be a single logical value")
    }

    # Two-index form p[i, j] / p[, j]: select step-table columns by name.
    if (!missing(j)) {
        if (!missing(view)) {
            warning("'view' is ignored when 'j' is specified")
        }
        if (!is.character(j)) {
            stop("j must be a character vector of column names")
        }
        dat <- .pip_pipenv(x)[["data"]]
        rows <- if (missing(i)) {
            .pip_view_rows(x)
        } else {
            .pip_select_rows(x, substitute(i), parent.frame())
        }
        return(dat[rows, j, with = FALSE])
    }

    pipenv <- .pip_pipenv(x)
    name <- x[["name"]]
    dat <- pipenv[["data"]]

    if (missing(i)) {
        # x[] returns a copy of the pipeline, or the view itself. On a view, x[, view =
        # FALSE] materializes all covered steps instead.
        if (!.is_pipeflow_view(x)) {
            return(pip_clone(x))
        }
        if (view) {
            return(x)
        }
        rows <- .pip_view_rows(x)
    } else {
        rows <- .pip_select_rows(x, substitute(i), parent.frame())
    }

    if (view) {
        # Return a view on the selected rows
        selector <- .pip_rows_to_view(pipenv, rows)
        return(.wrap_pipenv(pipenv, name = name, view = selector))
    }

    # Resolve all nodes reachable from the selected rows via upstream edges, then build
    # a self-contained, compact pipeline from them.
    keepNodes <- dag_get_reachable_nodes_up(
        .pip_dag(pipenv),
        as.integer(unique(dat[["nodeId"]][rows]))
    )
    out <- .pip_compact(x, keepNodes)

    nUpstream <- nrow(.pip_pipenv(out)[["data"]]) - length(rows)
    if (nUpstream > 0L) {
        message(sprintf(
            "pulled in %d upstream dependenc%s",
            nUpstream,
            if (nUpstream == 1L) "y" else "ies"
        ))
    }
    out
}


#' @details
#' ## Extract values from a pipeline or view (`x[[i, j]]`)
#'
#' A pipeline can be read like a data.frame of steps: `p[[column]]` returns a column of
#' the step table, and `p[[row, column]]` extracts a single cell. In addition, a few
#' meta fields are accessible by name.
#'
#' ### Meta fields
#'
#' The following meta fields are available via `p[["..."]]` (or `p$...`):
#'
#' * `name`: the name of the pipeline.
#' * `view`: the names of the steps covered by a view, or `NULL` for a full pipeline.
#' * `pipenv`: the shared inner environment holding the pipeline's state. All views
#'   and extracted subsets reference the same environment, so mutations are shared. The
#'   step table is available as `p[["pipenv"]][["data"]]` (see also [pip_data()]).
#'
#' ### Virtual methods
#'
#' All pipeline functions are exposed as *virtual* methods via `p$...` (or
#' `p[["..."]]`). For example, `p$add(...)` is shorthand for `pip_add(p, ...)` and
#' returns the updated pipeline.
#'
#' ### Step-table columns
#'
#' `p[["column"]]` returns the raw values of a column of the step table, named by the
#' step names. For views, the column is restricted to the steps covered by the view. The
#' two-index form `p[[row, column]]` extracts a single cell, where `row` is an integer
#' row index or a step name.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#' pip_run(p)
#'
#' # Meta fields
#' p[["name"]]              # "pipe"
#' p[["view"]]              # NULL - not a view
#' p[["pipenv"]]            # the inner pipeline environment
#' p[["pipenv"]][["data"]]  # the underlying step table
#'
#' # Virtual methods
#' p$add("s3", \(x = ~fit) x * 10)
#' p$run()
#' p$restart   # a function; call p$restart() to request a restart
#' p$halt      # a function; call p$halt() to halt the current run
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
#' v[["name"]]              # "pipe"
#' v[["view"]]              # "load", "fit" - the covered steps
#'
#' v[["step"]]              # c(load = "load", fit = "fit")
#' v[["out"]]               # c(load = 1, fit = 2)
#' v[["fit", "out"]]        # output of the "fit" step
#' @rdname Extract.pipeflow
#' @export
`[[.pipeflow` <- function(x, i, j = NULL) {
    .pip_subset2(x = x, i = i, j = j)
}


#' @details
#' ## Assign values to meta fields or step properties (`x[[i, j]] <- value`)
#'
#' ### Assigning meta fields
#' * `p[["name"]] <- value` -- sets the name of the pipeline
#' * `p[["view"]] <- steps` -- allows to define the pipeline view explicitly via a
#'    character vector of step names, which is equivalent to `p[steps]` or `p[steps, ]`.
#'    Assigning `p[["view"]] <- NULL` clears the view and returns a full pipeline.
#'
#' ### Assigning step properties
#' The two-index form `p[[step, property]] <- value` provides interactive shortcuts for
#' modifying a single step, where `step` is a step name or an integer row index and
#' `property` selects what to update:
#'
#' * `p[[step, "step"]] <- newName` -- rename the step; same as
#'   `pip_rename(p, step, newName)`).
#'   * `p[[step, "step"]] <- NULL` -- remove the step, together with its downstream
#'      dependencies; same as `pip_remove(p, step, force = TRUE)`
#' * `p[[step, "fun"]] <- fun` -- replace the step's function; same as [pip_replace()],
#'    that is, tags and execution mode are kept
#' * `p[[step, "params"]] <- list(...)` -- update the step's parameters; same as
#'   `pip_set_params(pip_view(p, step = step), list(...), scope = "view")`
#' * `p[[step, "tags"]] <- tags` -- set the step's tags explicitly to `tags` (a
#'   character vector); in contrast, [pip_tag()] will just add tags.
#'    * `p[[step, "tags"]] <- NULL` clears all tags
#' * `p[[step, "locked"]] <- TRUE|FALSE` -- lock or unlock the step; same as
#'   [pip_lock()] / [pip_unlock()]
#' * `p[[step, "exec"]] <- mode` -- set the step's execution mode. A new mode marks the
#'   step and its downstream steps as `"outdated"`.
#' * `p[[step, "state"]] <- state` -- set the step's state.
#' * `p[[step, "time"]] <- time` -- set the step's time stamp (a single `POSIXct`
#'   value).
#' * `p[[step, "time_start"]] <- time` -- set the start time of the step's last run (a
#'   single `POSIXct` value, which can be `NA` for a step that never ran).
#' * `p[[step, "duration"]] <- duration` -- set the duration of the step's last run (a
#'   single `difftime` value, which can be `NA` for a step that never ran).
#' * `p[[step, "out"]] <- value` -- set the step's stored output (mostly useful for
#'    debugging). The downstream steps are marked as `"outdated"`, so that the next run
#'    computes them from the new output.
#'
#' Assigning to any other step-table column, or to a meta field other than `name` and
#' `view`, is not supported.
#'
#' Views are supported as well. For a view, `i` is interpreted relative to the steps
#' covered by the view: an integer refers to the n-th visible step, and a step name must
#' be part of the view. Because views share the pipeline environment, step properties
#' are written through to the originating pipeline, while `name` only renames the view
#' itself.
#'
#' ### Which assignments outdate steps
#' Results that may no longer match the steps are marked as `"outdated"`, so that the
#' next [pip_run()] computes them again. As always, steps that never ran stay `"new"`.
#' * `fun`: the step becomes `"new"` and its downstream steps `"outdated"`.
#' * `params` and `exec`: the step and its downstream steps, if a value changes.
#' * `out`: the downstream steps, while the step keeps its state.
#' * `step` (a new name), `state`, `time`, `time_start`, `duration`, `tags` and `locked`
#'   don't change any result and outdate no step.
#'
#' This holds for all forms of assignment: `[[<-`, `[<-` (which assigns row by row) and
#' assignments through a view, which also outdate the downstream steps that are not part
#' of the view.
#' @examples
#' p <- pip_new("pipe") |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load, k = 2) x * k) |>
#'   pip_add("report", \(x = ~fit) x)
#'
#' # Assign the pipeline name via its meta field
#' p[["name"]] <- "demo"
#'
#' # Replace a step's function (tags and exec mode are kept)
#' p[["fit", "fun"]] <- \(x = ~load, k = 2) x * k
#'
#' # Update parameters, tags, locking and the execution mode
#' p[["fit", "params"]] <- list(k = 5)
#' p[["fit", "tags"]] <- c("model", "daily")
#' p[["fit", "locked"]] <- TRUE
#' p[["fit", "locked"]] <- FALSE
#' p[["fit", "exec"]] <- "plain"
#'
#' # Rename a step; dependent steps are updated as well
#' p[["load", "step"]] <- "read"
#'
#' # Assign by row index to set the state, time stamp or stored output
#' p[[2, "state"]] <- "outdated"
#' p[[2, "time"]] <- Sys.time() - 3600
#' p[[2, "out"]] <- 42
#' p
#'
#' # Restrict to a view covering selected steps, then clear it again
#' p[["view"]] <- c("read", "fit")
#' p[["step"]]                  # view -> "read", "fit"
#' p[["view"]] <- NULL
#' p[["step"]]                  # full pipeline again
#' @rdname Extract.pipeflow
#' @export
`[[<-.pipeflow` <- function(x, i, j, value) {
    if (missing(i)) {
        stop("i must be provided")
    }
    if (length(i) != 1L) {
        stop("i must be of length 1")
    }
    if (identical(i, "name")) {
        if (!.is_single(value, "character") || is.na(value) || !nzchar(value)) {
            stop("name must be a non-empty string")
        }
        return(.wrap_pipenv(
            .pip_pipenv(x),
            name = value,
            view = .subset2(x, "view")
        ))
    }
    if (identical(i, "view")) {
        # Build a full wrapper over the shared environment and, if requested, derive a
        # new view from it.
        x <- .wrap_pipenv(
            .pip_pipenv(x),
            name = x[["name"]],
            view = NULL
        )
        if (is.null(value)) {
            return(x)
        }
        return(pip_view(x, step = value))
    }

    env <- .pip_pipenv(x)
    data <- env[["data"]]
    rows <- .pip_view_rows(x)

    # Determine the absolute row index of the step to modify. For a view, `i` is
    # interpreted relative to the covered rows and step names must be part of the view.
    if (is.character(i)) {
        step <- i
        if (!.pip_step_exists(x, step)) {
            stop("element or step '", step, "' does not exist")
        }
        i <- data.table::chmatch(step, data[["step"]])
        if (!i %in% rows) {
            stop("step '", step, "' is not part of the view")
        }
    } else if (is.numeric(i)) {
        i <- as.integer(i)
        if (i < 1L || i > length(rows)) {
            fmt <- "row index %d out of bounds [%d, %d]"
            stop(sprintf(fmt, i, 1, length(rows)))
        }
        i <- rows[[i]]
        step <- data[["step"]][[i]]
    } else {
        stop("i must be a step name or row index")
    }

    if (missing(j)) {
        stop("j must be provided when assigning to a step property")
    }
    if (length(j) != 1L) {
        stop("j must be a single step property name")
    }

    # Dispatch the step modification function based on the property name.
    if (j == "step") {
        if (is.null(value)) {
            x <- pip_remove(x, step = step, force = TRUE)
        } else {
            pip_rename(x, from = step, to = value)
        }
    } else if (j == "fun") {
        pip_replace(x, step = step, fun = value)
    } else if (j == "params") {
        pip_set_params(pip_view(x, step = step), params = value, scope = "view")
    } else if (j == "out") {
        # wrap value in list of list to enable arbitrary objects (e.g. a data.frame) to
        # be stored as-is.
        data.table::set(data, i = i, j = "out", value = list(list(value)))
        # An output set from outside does not come from the step's last attempt, so a
        # retry runs all keys (see .pip_run_row()).
        env[[".steps_with_current_out"]][[step]] <- FALSE
        # The downstream steps ran with the old output. The step itself keeps its state.
        .pip_outdate_downstream(x, steps = step, keep = step)
    } else if (j == "state") {
        .assert_state(value)
        data.table::set(data, i = i, j = "state", value = value)
    } else if (j == "tags") {
        if (!is.null(value) && !is.character(value)) {
            stop("tags must be a character vector")
        }
        value <- list(as.character(value))
        data.table::set(data, i = i, j = "tags", value = value)
    } else if (j == "locked") {
        if (!.is_single(value, "logical") || is.na(value)) {
            stop("locked must be a single logical value")
        }
        data.table::set(data, i = i, j = "locked", value = value)
    } else if (j == "exec") {
        .assert_exec_mode(value)
        # The exec mode decides how the step is called, so a new mode outdates its
        # output and the outputs of its downstream steps.
        if (!identical(value, data[["exec"]][[i]])) {
            data.table::set(data, i = i, j = "exec", value = value)
            .pip_outdate_downstream(x, steps = step)
        }
    } else if (j == "time") {
        if (!.is_single(value, "POSIXct") || is.na(value)) {
            stop("time must be a single POSIXct value")
        }
        data.table::set(data, i = i, j = "time", value = value)
    } else if (j == "time_start") {
        if (identical(value, NA)) {
            value <- as.POSIXct(NA)
        }
        if (!.is_single(value, "POSIXct")) {
            stop("time_start must be a single POSIXct value or NA")
        }
        data.table::set(data, i = i, j = "time_start", value = value)
    } else if (j == "duration") {
        if (identical(value, NA)) {
            value <- .as_duration(NA)
        }
        if (!.is_single(value, "difftime")) {
            stop("duration must be a single difftime value or NA")
        }
        data.table::set(
            data,
            i = i,
            j = "duration",
            value = .as_duration(value)
        )
    } else {
        if (j %in% .derived_cols) {
            stop("direct assignment to column '", j, "' is not supported.")
        } else {
            stop("unknown step property: ", j)
        }
    }
    x
}


#' @details
#' ## Bulk assignment of step properties (`x[i, j] <- value`)
#'
#' `p[i, j] <- value` assigns the step property `j` to the selected steps (see
#' `[[<-.pipeflow` above for the supported properties), mirroring the row and column
#' selection of the extraction form `p[i, j]`: `i` selects rows (negative indices select
#' all rows but the excluded ones), `j` must be a single property name, and
#' `p[, j] <- value` selects all steps.
#'
#' * With a *single selected row*, `value` is stored *as-is* (like
#'   `p[[i, j]] <- value`).
#' * With *more than one selected row*, a value of length 1 is *replicated* to all rows
#'   and a value with one element per row is assigned *element-wise*
#'   (`p[i, j] <- value` behaves like `p[[i[k], j]] <- value[[k]]`). As in base R,
#'   longer values are *recycled* if the number of rows is a multiple of the value
#'   length, otherwise an error is raised.
#' * To assign the *same list value* (e.g. a set of tags) to all rows, wrap it in
#'   `list(...)`.
#' * For the `params` property, the value of each row must *itself be a list*, e.g.
#'   `p[1:2, "params"] <- list(list(a = 1), list(b = 2))`.
#'
#' The selected steps are assigned one after another, but the assignment changes all of
#' them or none: if it fails for one step, the pipeline is restored as it was.
#'
#' If `value` is a `pipeflow` pipeline or view, the writable step properties of its
#' steps are copied to the selected rows: `p[i, ] <- q[i, ]` copies the steps of `q`
#' selected by `i` into the steps of `p` selected by `i`. In this form, `i` is required
#' and `j` must be omitted.
#'
#' * The rows of both sides are *aligned by position* and must have the same length.
#' * The step is *renamed* to the source name (a no-op when it is unchanged), and its
#'   function, params and execution mode are *replaced* with those of the source (same
#'   as [pip_replace()] with the params and `exec` of the source).
#' * The runtime state and annotation columns (`out`, `state`, `time`, `time_start`,
#'   `duration`, `tags`, `locked`) are *overwritten*, locked steps included.
#' * A copied step refers to the steps that the source step depends on *by name*, also
#'   if it was defined with a relative reference such as `~-1`, and these steps are
#'   *looked up in `p`*.
#' * The structural columns `nodeId`, `depends` and `unbound` are not copied but
#'   *recomputed* against the steps of `p`.
#' * If a step can't be copied (e.g. because it refers to a step that is not in `p`),
#'   `p` is *left as it was*.
#'
#' Instead of `p[i, j] <- q`, use `p[i, j] <- q[i, j]` to copy the values of a single
#' property; the one-column table returned by the extraction form is unwrapped and
#' assigned element-wise.
#' @examples
#' p <- pip_new("pipe") |>
#'   pip_add("load", \(n = 5) seq_len(n)) |>
#'   pip_add("fit", \(x = ~load, k = 2) x * k) |>
#'   pip_add("report", \(x = ~fit) x)
#'
#' # Assign a property element-wise or replicated to selected steps
#' p[c("load", "fit"), "state"] <- c("outdated", "done")
#' p[1:2, "tags"] <- c("io", "model")
#' p[1:2, "tags"] <- list(c("new", "tag"))  # same tags for both steps
#'
#' # All steps at once; read-only columns are rejected
#' p[, "state"] <- "outdated"
#' try(p[c("load", "fit"), "depends"] <- "load")  # read-only
#'
#' # Copy the writable properties from another pipeline
#' q <- pip_clone(p)
#' q[c("load", "fit"), "out"] <- list(1:5, 6:10)
#' p[c("load", "fit"), ] <- q[c("load", "fit"), ]
#' p[c("load", "fit"), "tags"] <- q[c("load", "fit"), "tags"]
#' @rdname Extract.pipeflow
#' @export
`[<-.pipeflow` <- function(x, i, j, value) {
    .assert_pip_or_view(x)

    # Case x[i, ] <- q[i, ] - basically a cross-pipeline copy from q to x
    if (inherits(value, "pipeflow")) {
        if (missing(i)) {
            stop("i must be provided when assigning from a pipeline")
        }
        if (!missing(j)) {
            stop(
                "j must not be provided when assigning from a pipeline - ",
                "use p[i, j] <- q[i, j] to copy a single property"
            )
        }
        return(.pip_copy_from(x, substitute(i), value, parent.frame()))
    }

    if (missing(j)) {
        stop("j must be provided when assigning step properties")
    }
    if (!.is_single(j, "character") || is.na(j) || !nzchar(j)) {
        stop("j must be a single step property name")
    }

    rows <- if (missing(i)) {
        .pip_view_rows(x)
    } else {
        .pip_select_rows(x, substitute(i), parent.frame(), keep_order = TRUE)
    }

    n <- length(rows)
    if (n == 0L) {
        return(x)
    }

    # Convert absolute row indices to the view-relative indexing used by `[[<-`
    viewRows <- .pip_view_rows(x)
    iSel <- if (.is_pipeflow_view(x)) match(rows, viewRows) else rows

    hasOneColumnTable <- is.data.frame(value) &&
        ncol(value) == 1L &&
        identical(colnames(value), j)

    if (n == 1L) {
        # If we get here, i and j are single entries
        if (hasOneColumnTable) {
            # case p[i, j] <- q[i, j] => double-unwrap q[[1]][[1]]
            value <- value[[1L]][[1L]]
        }

        # case p[i, j] <- some_value => no unwrap
        x[[iSel, j]] <- value
        return(x)
    }

    if (hasOneColumnTable) {
        # If we get here, i is multi-row, e.g. value == q[1:3, "state"], so we unwrap
        # and assign element-wise to the selected rows.
        value <- value[[1L]]
    }

    # Determine the value for each selected row. An element of an atomic vector is taken
    # with `[`, which keeps its class (e.g. `difftime`), unlike `[[` and as.list().
    element <- function(v, m) {
        if (is.atomic(v)) unname(v[m]) else v[[m]]
    }
    if (is.null(value)) {
        values <- rep(list(NULL), n)
    } else {
        k <- length(value)
        if (k == 1L) {
            val <- if (is.atomic(value) || is.list(value)) {
                element(value, 1L)
            } else {
                value
            }
            values <- rep(list(val), n)
        } else if (k == n) {
            values <- lapply(seq_len(n), \(m) element(value, m))
        } else if (k > 1L && n %% k == 0L) {
            value <- rep(value, length.out = n)
            values <- lapply(seq_len(n), \(m) element(value, m))
        } else {
            stop(sprintf("replacement has length %d, data has %d", k, n))
        }
    }

    # The step names are resolved up front: removing steps shifts the row indices, and
    # force-removing one step may already remove its downstream steps.
    stepNames <- if (j == "step") {
        .pip_pipenv(x)[["data"]][["step"]][rows]
    } else {
        character(0)
    }

    # If a row fails, the rows assigned before are undone, so that `x` is left as it
    # was.
    .pip_all_or_nothing(x, {
        for (k in seq_len(n)) {
            if (j == "step") {
                if (.pip_step_exists(x, stepNames[[k]])) {
                    x[[stepNames[[k]], j]] <- values[[k]]
                }
            } else {
                x[[iSel[[k]], j]] <- values[[k]]
            }
        }
    })
    x
}


#' @details
#' ## Extract and assign via $ (`x$i / x$i <- value`)
#'
#' `$` is a shortcut for the one-index form of `[[` / `[[<-`: `p$name` is equivalent to
#' `p[["name"]]`, and `p$name <- value` to `p[["name"]] <- value`. This works for meta
#' fields, step-table columns, and the virtual methods (e.g. `p$add(...)` or `p$run()`).
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit", \(x = ~load) x + 1)
#'
#' p$name                # same as p[["name"]]
#' p$step                # same as p[["step"]]
#' p$name <- "renamed"   # same as p[["name"]] <- "renamed"
#'
#' p$run                 # a virtual method; call p$run() to run the pipeline
#' @rdname Extract.pipeflow
#' @export
`$.pipeflow` <- function(x, i) {
    x[[i]]
}


#' @rdname Extract.pipeflow
#' @export
`$<-.pipeflow` <- function(x, i, value) {
    x[[i]] <- value
    x
}


# ---------
# Structure
# ---------
#' Print pipeflow objects
#'
#' @param x A pipeflow pipeline or view.
#' @param object A pipeflow pipeline or view, for [utils::str()].
#' @param rows Row indices to be printed. If empty, all rows are printed.
#' @param cols The columns to be printed. Can be either one of `core` or `all` to print
#' the core or all columns, respectively, or an explicit character vector of columns to
#' be printed. Some columns are shown in short form:
#' * `params`: the names of the step's parameters.
#' * `out`: a condition (the output of a step that failed in a run with
#'   `on_error = "continue"`) as `<kind/class>`, e.g. `<error/simpleError>`; a matrix,
#'   array or table as `<class[dims]>`, e.g. `<matrix[4x2]>`; a function or environment
#'   (e.g. an R6 object) as `<class>`; strings longer than 40 characters are cut off and
#'   marked with `"..."`.
#' * `duration`: e.g. `0.251 s` or `2 min 5 s`.
#' @param topn The number of rows to be printed from the beginning and end of tables
#' with more than `nrows` rows.
#' @param nrows The number of rows printed before truncation is enforced.
#' @param class If TRUE, the resulting output will include above each column its storage
#' class (or a self-evident abbreviation thereof).
#' @param row.names If TRUE, row indices will be printed alongside x.
#' @param header If TRUE, a header with the pipeline name and number of steps, and a
#' footer will be printed. The footer shows the run state, the time and the duration of
#' the last run, and how many of the printed steps are in each state, e.g.
#' ```
#' <ready> last run: 2026-10-25 14:03:12 (0.251 s)
#' steps: 3 done, 1 failed, 2 new
#' ```
#' @param ...  Other arguments passed to `print.data.table`
#' @return Invisibly returns `x`.
#' @examples
#' p <- pip_new("demo") |>
#'   pip_add("load", \(n = 5) seq_len(n), tags = c("io", "raw")) |>
#'   pip_add("square", \(x = ~load) x^2, tags = "compute") |>
#'   pip_add("total", \(x = ~square) sum(x), tags = "compute")
#'
#' print(p) # core columns: step, params, depends, state, tags
#' print(p, cols = "all") # all step-table columns
#' print(p, rows = 2:3) # print only steps 2 and 3
#'
#' v <- pip_view(p, tags = "compute")
#' print(v)
#' @name print.pipeflow
#' @rdname print
#' @export
str.pipeflow <- function(object, ...) {
    utils::str(unclass(object), ...)
}


# -------
# Binding
# -------
#' Bind pipelines
#'
#' Binds two or more pipelines together by concatenating their steps. If the pipelines
#' have steps with the same name, the step names of later pipelines are automatically
#' adapted to avoid name clashes. A single pipeline is returned unchanged. A warning is
#' signalled if an unbound param of a later pipeline has another value than the same
#' param in the earlier ones (see Params with the same name in [pip_add()]).
#'
#' To feed steps of the second pipeline with the output of steps of the first one, bind
#' their params with [pip_set_params()] afterwards, e.g.
#' `pip_set_params(ab, list(data = ~prep))`. This binds the param in all steps that have
#' it unbound, so to bind only the steps of the second pipeline, select them with a
#' view, e.g. `ab[-seq_along(a)]`. Locked steps are not bound: unlock them before and
#' lock them again afterwards, e.g. with [pip_unlock()] and [pip_lock()] on the same
#' view.
#' @param ... Two or more pipeflow pipeline objects.
#' @param deparse.level Not used, for compatibility with the generic `rbind()`.
#' @return A new pipeflow pipeline object representing the bound pipelines.
#' @examples
#' a <- pip_new("a") |>
#'   pip_add("prep", \(x = 1) x * 2) |>
#'   pip_add("fit", \(x = ~prep) x + 10)
#'
#' # "prep" exists in both pipelines; the one from b gets a numeric suffix
#' b <- pip_new("b") |> pip_add("prep", \(y = 5) y * 3)
#'
#' ab <- rbind(a, b)
#' ab[["step"]] # "prep", "fit", "prep2" (step name conflict auto-resolved)
#' ab
#'
#' # Any number of pipelines can be combined
#' abc <- rbind(a, b, b)
#' abc
#'
#' # Feed the param `data` of the second pipeline with the output of "prep"
#' d <- pip_new("d") |> pip_add("scale", \(data = 1, k = 3) data * k)
#' ad <- rbind(a, d)
#' pip_set_params(ad[-seq_along(a)], list(data = ~prep))
#' ad[["scale", "depends"]]
#' pip_get_params(ad) # `data` is no longer a param
#' pip_run(ad)
#' ad[["scale", "out"]]
#' @export
rbind.pipeflow <- function(..., deparse.level = 1) {
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
