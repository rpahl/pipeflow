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
    c(as.integer(length(.pip_view_rows(x))), ncol(x[["data"]]))
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
#' of the pipeline. For validated, programmatic filters with dedicated
#' arguments (such as matching *any* of a set of tags) use [pip_view()]
#' instead.
#' @param x A pipeflow pipeline object.
#' @param i Row selection: integer row indices, character step names, or a
#' boolean filter expression evaluated in the context of the step table.
#' @param view If `TRUE` (default), a view referencing the selected steps is
#' returned. If `FALSE`, a new pipeline is returned that includes the selected
#' steps and all their upstream dependencies.
#' @param ... Not used.
#' @return A pipeflow view (if `view = TRUE`) or a new pipeflow pipeline
#' (if `view = FALSE`).
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
#' p[c("load", "square")][["step"]]  # view -> "load", "square"
#' p[1:2, view = FALSE][["step"]]     # pipeline -> "load", "square"
#'
#' # Boolean filters are evaluated against the step table
#' p[tags %like% "model"][["step"]]  # "square", "total"
#'
#' # view = FALSE extracts a pipeline with all upstream dependencies
#' p[tags %like% "report", view = FALSE][["step"]] # "load", "square", "total"
#'
#' # No arguments returns a copy of the pipeline
#' length(p[]) # 3
#' @rdname Extract.pipeflow
#' @export
`[.pipeflow` <- function(x, i, view = TRUE, ...) {
    .assert_pip(x)

    if (!missing(...)) {
        stop(
            "`...` is not supported in `[`. Select rows, step names, or a ",
            "boolean expression, or use pip_view() for named filters."
        )
    }
    if (!.is_single(view, "logical") || is.na(view)) {
        stop("view must be a single logical value")
    }

    # x[] returns a copy of the pipeline
    if (missing(i)) {
        return(pip_clone(x))
    }

    pipenv <- .pip_get_pipenv(x)
    name <- x[["name"]]

    # The step table provides the scope for boolean filters (data.table-style
    # non-standard evaluation); `i` is evaluated against it, falling back to
    # the calling environment for ordinary variables.
    dat <- pipenv[["data"]]
    i_expr <- substitute(i)
    value <- eval(i_expr, envir = dat, enclos = parent.frame())

    if (is.logical(value)) {
        if (length(value) == 1L) {
            value <- rep(value, nrow(dat))
        }
        if (length(value) != nrow(dat)) {
            stop(sprintf(
                "logical filter has length %d but the pipeline has %d rows",
                length(value),
                nrow(dat)
            ))
        }
        if (anyNA(value)) {
            stop("logical filter must not contain NA")
        }
        rows <- which(value)
    } else if (is.numeric(value)) {
        if (anyNA(value)) {
            stop("row indices in 'i' must not contain NA")
        }
        if (!all(is.finite(value)) || !all(value == as.integer(value))) {
            stop("numeric indices in 'i' must be whole numbers")
        }
        rows <- sort(unique(as.integer(value)))
        bad <- rows[rows < 1L | rows > nrow(dat)]
        if (length(bad) > 0L) {
            stop("Invalid row indices in 'i': ", toString(bad))
        }
    } else if (is.character(value)) {
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
        rows <- sort(unique(m))
    } else {
        stop(sprintf(
            "`i` must evaluate to row indices, step names, or a logical ",
            "filter, not %s",
            typeof(value)
        ))
    }

    if (view) {
        # Return a view on the selected rows
        return(.wrap_pipenv(pipenv, name = paste(name, view), view = rows))
    }

    data <- pipenv[["data"]]
    out <- pip_new(name = name)
    if (length(rows) == 0L) {
        return(out)
    }

    # Get all nodes that are reachable from the selected rows via upstream
    startNodes <- data[[".nodeId"]][rows]
    keepNodes <- dag_get_reachable_nodes_up(
        pipenv[[".dag"]],
        as.integer(unique(startNodes))
    )
    subsetDat <- data[data[[".nodeId"]] %in% keepNodes]
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
#' @param x A pipeflow pipeline or view.
#' @param i integer (row index) or character (step name) of the step to
#' select
#' @param j column name to select
#' @param ... Not used.
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
`[[.pipeflow` <- function(x, i, j = NULL, ...) {
    .pip_subset2(x = x, i = i, j = j, ...)
}

# Assignment routes list fields (`pip`, `name`, `rows`) to the wrapper and all
# other bindings to the shared inner environment.
#' @export
`[[<-.pipeflow` <- function(x, i, j = NULL, ..., value) {
    if (i %in% c("pipenv", "name", "view")) {
        unclass(x)[[i]] <- value
    } else {
        env <- .pip_get_pipenv(x)
        env[[i]] <- value
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
