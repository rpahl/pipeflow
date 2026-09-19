# ------
# Helper
# ------

# Copy step properties of the selected rows of one pipeline/view to
# the selected rows of another. The rows of `x` and `q` are aligned by
# position and must have equal length. The structural columns
# (`nodeId`, `depends`, `unbound`) are never copied:
# - `depends` is recomputed when the function is replaced and
# - `nodeId`/`unbound` are also derived.
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
        "out",
        "state",
        "time",
        "tags",
        "locked",
        "exec"
    )

    # Since the source can be `x` itself, we will work on a snapshot to
    # prevent that any writes below would alter rows that are still to be
    # copied (e.g. pip_replace() marking downstream steps as outdated).
    qSnapshot <- dataQ[rowsQ, props, with = FALSE]

    for (k in seq_len(n)) {
        pRow <- rowsP[[k]]

        qVals <- lapply(props, function(prop) qSnapshot[[prop]][[k]])
        names(qVals) <- props

        # 1) Step name (a no-op when both pipelines use the same names).
        x[[dataP[["step"]][[pRow]], "step"]] <- qVals[["step"]]

        # 2) Function. pip_replace() resets the params to the function
        #    defaults and recomputes `depends` against the steps of `x`.
        x[[iSelP[[k]], "fun"]] <- qVals[["fun"]]

        # 3) Take over all unbound param values from q. Bound (formula)
        #    params are pipeline-context specific and stay as recomputed.
        parsP <- x[[iSelP[[k]], "params"]]
        dep <- x[[iSelP[[k]], "depends"]]
        unboundP <- setdiff(names(parsP), names(dep))
        pars <- qVals[["params"]][
            intersect(names(qVals[["params"]]), unboundP)
        ]
        if (length(pars) > 0L) {
            x[[iSelP[[k]], "params"]] <- pars
        }

        # 4) Runtime state and annotation columns.
        for (prop in c("out", "state", "time", "tags", "locked", "exec")) {
            x[[iSelP[[k]], prop]] <- qVals[[prop]]
        }
    }

    x
}


#' Compact a pipeline to a subset of steps
#'
#' Helper to build new pipeline from the steps of `x` whose `nodeId` is
#' contained in `keepNodes`. The kept rows get a compact node id sequence,
#' and the DAG and the step->node lookup are re-built from the `depends`
#' values.
#' For a small number of kept steps, the DAG is built node by node in R. If
#' roughly a third or more of the steps are kept, it is faster to clone the
#' existing DAG, remove the excluded nodes in place, and compact the ids in
#' C++ via dag_rebuild().
#'
#' @param x A pipeflow pipeline.
#' @param keepNodes Integer vector of node ids to keep.
#' @param rebuildThresh Fraction of steps to keep above which the DAG is
#' rebuilt. The default value of 0.3 was determined empirically, that is,
#' at this fraction both methods take roughly the same time.
#' @return A new pipeflow pipeline with the selected steps and a compact
#' node id sequence.
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
        # Clone the existing DAG, drop all nodes that are not kept, and let
        # the C++ side compact the node ids. Nodes that are still alive
        # correspond to the current rows of `data`.
        d <- dag_clone(pipenv[[".dag"]])
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
    stepsToNodes <- new.env(parent = emptyenv())
    for (k in seq_len(nrow(subDat))) {
        stepsToNodes[[subDat[["step"]][[k]]]] <- subDat[["nodeId"]][[k]]
    }

    if (!useRebuild) {
        # Build a new DAG from scratch that matches the kept rows
        d <- dag_new()
        for (k in seq_len(nrow(subDat))) {
            dag_add_node(d)
            deps <- subDat[["depends"]][[k]]
            if (length(deps) == 0L) {
                next
            }
            from <- as.integer(unname(unlist(mget(
                deps,
                envir = stepsToNodes,
                inherits = FALSE
            ))))
            to <- as.integer(subDat[["nodeId"]][[k]])
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

# The method registry maps method names to the (unbound) backend functions and
# is built lazily, so dispatch is an O(1) hash lookup followed by binding
# exactly one closure to the current object.
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
                collect_out = pip_collect_out,
                clone = pip_clone,
                graph = pip_graph,
                restart = function(x, force = TRUE, times = 1L) {
                    .pip_restart(
                        .pip_pipenv(x),
                        force = force,
                        times = times
                    )
                },
                stop = function(x) .pip_stop(.pip_pipenv(x))
            )
            for (nm in names(values)) {
                table[[nm]] <- values[[nm]]
            }
        }
        table
    }
})

# Internal implementation of [[ for pipeflow objects.
.pip_subset2 <- function(x, i, j = NULL) {
    if (missing(i)) {
        stop("i must be provided")
    }
    if (length(i) != 1L || is.na(i)) {
        stop("i must be a single step name or row index")
    }
    if (is.null(j)) {
        # List fields of the wrapper have priority over column names.
        if (is.character(i) && length(i) == 1L && !is.na(i)) {
            if (i %in% names(x)) {
                return(.subset2(x, i))
            }
        }

        # Virtual methods (an O(1) lookup followed by a single closure that
        # binds the backend to the current object).
        if (is.character(i) && length(i) == 1L && !is.na(i)) {
            fn <- get0(i, envir = .pip_method_table(), inherits = FALSE)
            if (!is.null(fn)) {
                return(function(...) fn(x, ...))
            }
        }

        # case x[[col]]
        data <- .pip_view_data(x)
        col <- data[[i]]
        if (is.null(col)) {
            return(NULL)
        }
        return(stats::setNames(col, data[["step"]]))
    }

    # Two-index form extracts a single cell from a single row.
    if (!is.character(j) || length(j) != 1L || is.na(j)) {
        stop("j must be a single column name")
    }

    data <- .pip_view_data(x)
    if (is.character(i)) {
        # case x[[stepName, col]]
        row <- .pip_steps_to_rows(x, steps = i)
        if (row > nrow(data)) {
            stop("selected step not part of view: ", i)
        }
    } else {
        # case x[[i, col]]
        row <- as.integer(i)
        if (row < 1L || row > nrow(data)) {
            stop("row index out of bounds")
        }
    }

    data[[j]][[row]]
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
length.pipeflow <- function(x) {
    as.integer(length(.pip_view_rows(x)))
}

#' Number of rows of a pipeflow pipeline or view
#'
#' Treats a pipeline as a table of steps: `nrow()` returns the number of
#' steps, the same as [length.pipeflow] / `length()`, and `ncol()`
#' returns the number of columns of the underlying step table. Views report
#' only the number of covered steps as rows.
#' @param x A pipeflow pipeline or view
#' @return `nrow()` returns the number of steps as an integer; `ncol()`
#' returns the number of columns of the step table.
#' @details Base R's `nrow()` is implemented as `dim(x)[1L]`, so the number
#' of rows and columns is provided through a `dim()` method for
#' `pipeflow` objects.
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
dim.pipeflow <- function(x) {
    c(
        as.integer(length(.pip_view_rows(x))),
        ncol(
            .pip_pipenv(x)[["data"]]
        )
    )
}


#' Extract or subset a pipeline
#'
#' Selects steps from a pipeline. By default, a lightweight [pip_view()] is
#' returned that references the selected steps without copying them. Set
#' `view = FALSE` to instead get a new, self-contained pipeline that includes
#' all required upstream dependencies.
#'
#' Three forms of row selection are supported:
#' * **row indices** (relative to the current selection), e.g. `p[2:3]`,
#' * **step names**, e.g. `p[c("load", "fit")]`,
#' * a **boolean filter** that is evaluated in the context of the pipeline's
#'   step table, e.g. `p[state == "new"]` or
#'   `p[step %in% c("load", "fit") & tags %like% "io"]`.
#'
#' Boolean filters may reference the columns of the step table (`step`, `fun`,
#' `params`, `depends`, `tags`, `state`, ...) directly as variables, and the
#' [data.table filter operators][pipeflow-operators] re-exported by pipeflow
#' (e.g. `%like%`, `%chin%`, `%between%`) are available. `p[]` returns a copy
#' of the pipeline.
#'
#' When `j` is provided, the selected steps are returned as a step-table
#' extraction rather than a pipeline. `j` must be a character vector of column
#' names: `p[, j]` keeps those columns for all steps and `p[i, j]` first
#' selects the rows and then keeps those columns. The result in this case is a
#' `data.table`.
#'
#' If done on a view, row selection is relative to the covered steps, and step
#' names must be part of the view. For validated,
#' programmatic filters with dedicated arguments (such as matching *any* of a
#' set of tags) use [pip_view()] instead.
#' @param x A pipeflow pipeline or view object.
#' @param i Row selection: integer row indices, character step names, or a
#' boolean filter expression evaluated in the context of the step table.
#' Negative row indices select all rows but the excluded ones, like in base
#' R (e.g. `p[-2]`), and are relative to the covered steps for a view. For a
#' view, indices are relative to the covered steps and step names must be
#' part of the view.
#' @param j Optional character vector of step-table column names to extract.
#' @param view If `TRUE` (default), a view referencing the selected steps is
#' returned. If `FALSE`, a new pipeline is returned that includes the selected
#' steps and all their upstream dependencies. Ignored when `j` is provided.
#' @return A pipeflow view (if `view = TRUE`) or a new pipeflow pipeline
#' (if `view = FALSE`). If `j` is provided, a `data.table` with the selected
#' rows and columns.
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

    # x[] returns a copy of the pipeline (or the view itself)
    if (missing(i)) {
        if (.is_pipeflow_view(x)) {
            return(x)
        }
        return(pip_clone(x))
    }

    pipenv <- .pip_pipenv(x)
    name <- x[["name"]]
    dat <- pipenv[["data"]]
    rows <- .pip_select_rows(x, substitute(i), parent.frame())

    if (view) {
        # Return a view on the selected rows
        return(.wrap_pipenv(pipenv, name = paste(name, "view"), view = rows))
    }

    # Resolve all nodes reachable from the selected rows via upstream edges,
    # then build a self-contained, compact pipeline from them.
    keepNodes <- dag_get_reachable_nodes_up(
        pipenv[[".dag"]],
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
#' * `name` — the name of the pipeline.
#' * `view` — the absolute row indices of the steps covered by a view, or
#'   `NULL` for a full pipeline.
#' * `pipenv` — the shared inner environment holding the pipeline's state.
#'   All views and extracted subsets reference the same environment, so
#'   mutations are shared. The step table is available as
#'   `p[["pipenv"]][["data"]]`.
#'
#' ## Virtual methods
#'
#' All pipeline functions are exposed as *virtual* methods via `p$...`
#' (or `p[["..."]]`). For example, `p$add(...)` is shorthand for
#' `pip_add(p, ...)` and returns the updated pipeline, which allows
#' chaining: `p$add("s1", ...) |> p$add("s2", ...)`.
#'
#' ## Step-table columns
#'
#' `p[["column"]]` returns a column of the step table, named by the step
#' names. For views, the column is restricted to the steps covered by the
#' view. Meta fields take priority over columns of the same name. The
#' two-index form `p[[row, column]]` extracts a single cell, where `row` is
#' an integer row index or a step name.
#' @param x A pipeflow pipeline or view.
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
#' p[["name"]]              # "pipe"
#' p[["view"]]              # NULL — not a view
#' p[["pipenv"]]            # the inner pipeline environment
#' p[["pipenv"]][["data"]]  # the underlying step table
#'
#' # Virtual methods
#' p$add("s3", \(x = ~fit) x * 10)
#' p$run()
#' p$restart   # a function; call p$restart() to request a restart
#' p$stop      # a function; call p$stop() to stop the current run
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
#' v[["name"]]              # "pipe view"
#' v[["view"]]              # row indices of the covered steps
#'
#' v[["step"]]              # c(load = "load", fit = "fit")
#' v[["out"]]               # c(load = 1, fit = 2)
#' v[["fit", "out"]]        # output of the "fit" step
#' @rdname Extract_value.pipeflow
#' @export
`[[.pipeflow` <- function(x, i, j = NULL) {
    .pip_subset2(x = x, i = i, j = j)
}

#' Assign values to pipeline meta fields or step properties
#'
#' `p[["name"]] <- value` sets the name of the pipeline, where `value` must be
#' a non-empty string, and `p[["view"]] <- steps` restricts the pipeline to a
#' view covering `steps` (a character vector of step names). Assigning
#' `p[["view"]] <- NULL` clears the view and returns a full pipeline. The
#' two-index form `p[[step, property]] <- value` provides interactive
#' shortcuts for modifying a single step, where `step` is a step name or an
#' integer row index and `property` selects what to update:
#'
#' * `p[[step, "step"]] <- newName` — rename the step ([pip_rename()]);
#'   references in dependent steps are updated as well. Assigning `NULL`
#'   removes the step, together with its downstream steps
#'   ([pip_remove()] with `force = TRUE`).
#' * `p[[step, "fun"]] <- fun` — replace the step's function
#'   ([pip_replace()]); the tags and the execution mode are kept and the
#'   downstream steps are marked as outdated.
#' * `p[[step, "params"]] <- list(...)` — update the step's parameters
#'   ([pip_set_params()]).
#' * `p[[step, "tags"]] <- tags` — set the step's tags to exactly `tags`
#'   (a character vector); `NULL` clears all tags.
#' * `p[[step, "locked"]] <- TRUE|FALSE` — lock or unlock the step
#'   ([pip_lock()] / [pip_unlock()]).
#' * `p[[step, "exec"]] <- mode` — set the step's execution mode.
#' * `p[[step, "state"]] <- state` — set the step's state.
#' * `p[[step, "time"]] <- time` — set the step's time stamp (a single
#'   `POSIXct` value).
#' * `p[[step, "out"]] <- value` — set the step's stored output.
#'
#' Assigning to any other step-table column, or to a meta field other than
#' `name` and `view`, is not supported. To remove a step, use [pip_remove()].
#'
#' Views are supported as well. For a view, `i` is interpreted relative to the
#' steps covered by the view: an integer refers to the n-th visible step, and a
#' step name must be part of the view. Because views share the pipeline
#' environment, step properties are written through to the originating
#' pipeline, while `name` only renames the view itself.
#' @param x A pipeflow pipeline or view.
#' @param i `"name"` or `"view"` to assign the respective meta field, or a
#' step name or integer row index to select the step to modify. For a view, the
#' row index is relative to the covered steps and the step name must be part of
#' the view.
#' @param j The step property to assign; see 'Details'.
#' @param value The value to assign.
#' @return The updated pipeline, invisibly.
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
#' @rdname Extract_value.pipeflow
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
        # Build a full wrapper over the shared environment and, if requested,
        # derive a new view from it.
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

    # Determine the absolute row index of the step to modify. For a view, `i`
    # is interpreted relative to the covered rows and step names must be part
    # of the view.
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
        tags <- data[["tags"]][[i]]
        exec <- data[["exec"]][[i]]
        pip_replace(x, step = step, fun = value, tags = tags, exec = exec)
    } else if (j == "params") {
        pip_set_params(pip_view(x, step = step), params = value)
    } else if (j == "out") {
        # wrap value in list of list to enable arbitrary objects
        # (e.g. a data.frame) to be stored as-is.
        data.table::set(data, i = i, j = "out", value = list(list(value)))
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
        data.table::set(data, i = i, j = "exec", value = value)
    } else if (j == "time") {
        if (!.is_single(value, "POSIXct") || is.na(value)) {
            stop("time must be a single POSIXct value")
        }
        data.table::set(data, i = i, j = "time", value = value)
    } else {
        if (j %in% .derived_cols) {
            stop("direct assignment to column '", j, "' is not supported.")
        } else {
            stop("unknown step property: ", j)
        }
    }
    x
}


#' Bulk assignment of step properties
#'
#' `p[i, j] <- value` assigns the step property `j` to the selected steps
#' (see [`[[<-.pipeflow`] for the supported properties), mirroring the row
#' and column selection of the extraction form `p[i, j]`. `i` selects rows
#' like in the extraction form (including negative row indices, which
#' select all rows but the excluded ones) and `j` must be a single
#' property name; `p[, j] <- value` selects all steps. With more than one
#' selected row, a value of length 1 is replicated to all selected rows and
#' a value whose length equals the number of selected rows is assigned
#' element-wise (`p[i, j] <- value` behaves like `p[[i[k], j]] <- value[[k]]`).
#' As in base R, longer values are recycled if the number of selected rows
#' is a multiple of the value length, otherwise an error is raised. To
#' assign the same list value (e.g. a set of tags) to all rows, wrap it in
#' `list(...)`. With a single selected row, `value` is stored as-is (like
#' `p[[i, j]] <- value`). For the `params` property, the value of each row
#' must itself be a list (e.g. `p[1:2, "params"] <- list(list(a = 1),
#' list(b = 2))`). Assigning to a read-only column such as `depends`,
#' `nodeId` or `unbound` raises an error.
#'
#' If `value` is a `pipeflow` pipeline or view, the writable step properties
#' of its steps are copied to the selected rows: `p[i, ] <- q[i, ]` copies
#' the steps of `q` selected by `i` into the steps of `p` selected by `i`
#' (the rows of both sides are aligned by position and must have the same
#' length). In this form, `i` is required and `j` must be omitted. The
#' properties are copied in the order `step`, `fun`, `params`, `out`,
#' `state`, `time`, `tags`, `locked`, `exec`: the step is renamed to the
#' source name (a no-op when it is unchanged), the function is replaced with
#' the source function ([pip_replace()]; its dependencies are recomputed
#' against the steps of `p`), the unbound parameters of the source are
#' applied on top of the new function defaults, and the runtime state and
#' annotation columns are overwritten (locked steps included). The
#' structural columns `nodeId`, `depends` and `unbound` are not copied.
#' Instead of `p[i, j] <- q`, use `p[i, j] <- q[i, j]` to copy the values of
#' a single property; the one-column table returned by the extraction form
#' is unwrapped and assigned element-wise.
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
#' @rdname Extract_value.pipeflow
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
        # If we get here, i is multi-row, e.g. value == q[1:3, "state"], so we
        # unwrap and assign element-wise to the selected rows.
        value <- value[[1L]]
    }

    # Determine the value for each selected row.
    if (is.null(value)) {
        values <- rep(list(NULL), n)
    } else {
        k <- length(value)
        if (k == 1L) {
            val <- if (is.atomic(value) || is.list(value)) {
                value[[1L]]
            } else {
                value
            }
            values <- rep(list(val), n)
        } else if (k == n) {
            values <- as.list(value)
        } else if (k > 1L && n %% k == 0L) {
            values <- as.list(rep(value, length.out = n))
        } else {
            stop(sprintf("replacement has length %d, data has %d", k, n))
        }
    }

    # The step names are resolved up front: removing steps shifts the row
    # indices, and force-removing one step may already remove its
    # downstream steps.
    stepNames <- if (j == "step") {
        .pip_pipenv(x)[["data"]][["step"]][rows]
    } else {
        character(0)
    }

    for (k in seq_len(n)) {
        if (j == "step") {
            if (.pip_step_exists(x, stepNames[[k]])) {
                x[[stepNames[[k]], j]] <- values[[k]]
            }
        } else {
            x[[iSel[[k]], j]] <- values[[k]]
        }
    }
    x
}

#' @rdname Extract_value.pipeflow
#' @export
`$.pipeflow` <- function(x, i) {
    x[[i]]
}

#' @export
`$<-.pipeflow` <- function(x, i, value) {
    x[[i]] <- value
    x
}


#' Print pipeflow objects
#'
#' @param x A pipeflow pipeline or view.
#' @param object A pipeflow pipeline or view, for [utils::str()].
#' @param rows Row indices to be printed. If empty, all rows are printed.
#' @param cols The columns to be printed. Can be either one of
#' `core` or `all` to print the core or all columns, respectively,
#' or an explicit character vector of columns to be printed. The `params`
#' column is shown in a compact `name=value` form, abbreviating long values
#' by their class.
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
#' print(p) # core columns: step, params, depends, state, tags
#' print(p, cols = "all") # all step-table columns
#' print(p, rows = 2:3) # print only steps 2 and 3
#'
#' v <- pip_view(p, tags = "compute")
#' print(v)
#' @rdname print
#' @export
str.pipeflow <- function(object, ...) {
    utils::str(unclass(object), ...)
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


# ---------------------------------------------------------------------------
# Re-exports: data.table row-filter operators
# ---------------------------------------------------------------------------

`%like%` <- data.table::`%like%`
`%ilike%` <- data.table::`%ilike%`
`%flike%` <- data.table::`%flike%`
`%plike%` <- data.table::`%plike%`
`%chin%` <- data.table::`%chin%`
`%between%` <- data.table::`%between%`
`%inrange%` <- data.table::`%inrange%`
`%notin%` <- data.table::`%notin%`

#' Row-filter operators re-exported from data.table
#'
#' pipeflow re-exports the row-filter operators of \pkg{data.table} so that
#' they are available after attaching pipeflow and can be used in boolean
#' filters passed to `[.pipeflow`, e.g. `p[tags %like% "daily"]`. They behave
#' exactly as in \pkg{data.table}; see its documentation for details.
#'
#' @name pipeflow-operators
#' @aliases %like% %ilike% %flike% %plike% %chin% %between% %inrange% %notin%
#' @usage NULL
#' @keywords internal
#' @rawNamespace export("%like%")
#' @rawNamespace export("%ilike%")
#' @rawNamespace export("%flike%")
#' @rawNamespace export("%plike%")
#' @rawNamespace export("%chin%")
#' @rawNamespace export("%between%")
#' @rawNamespace export("%inrange%")
#' @rawNamespace export("%notin%")
NULL
