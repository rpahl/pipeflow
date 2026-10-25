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
#' Node colors reflect the step state (`new`, `done`, `outdated` or
#' `failed`). Hovering over a node shows its `title`: the state, the exec
#' mode, the tags, whether it is locked, the start of its last run (and its
#' duration if it succeeded), the failed keys of a mapped step, its
#' parameters and the steps it depends on.
#'
#' An edge is dashed if the step refers to the other step only with
#' `~try()`, i.e. if the step also runs when the other step failed (see
#' [pip_run()]).
#'
#' @param x A pipeflow pip or view.
#' @param include_upstream Logical. Only relevant for views. If `TRUE`, add
#' all upstream dependencies of selected steps.
#'
#' @return A named list with two `data.frame`s: `nodes` (columns `id`,
#' `label`, `shape`, `color` and `title`) and `edges` (columns `from`, `to`,
#' `arrows` and `dashes`).
#' @export
#' @examples
#' p <- pip_new()
#' pip_add(p, "load", \(x = 1) x, tags = "io")
#' pip_add(p, "clean", \(x = ~load) x + 1, tags = "io")
#' pip_add(p, "fit", \(x = ~clean) x * 2, tags = "model")
#' pip_add(p, "report", \(fit = ~try(fit)) class(fit))
#'
#' graph <- pip_graph(p)
#' graph$nodes # data.frame: id, label, shape, color, title
#' graph$edges # data.frame: from, to, arrows, dashes
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

    rows <- .pip_view_rows(x)
    rows <- sort(unique(rows))

    if (isView && include_upstream && length(rows) > 0L) {
        startNodes <- dat[["nodeId"]][rows]
        keepNodes <- dag_get_reachable_nodes_up(
            .pip_dag(env),
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
    titles <- vapply(
        seq_len(nrow(sub)),
        FUN = \(k) .pip_graph_title(sub, k),
        FUN.VALUE = character(1)
    )
    nodes <- data.frame(
        id = ids,
        label = sub[["step"]],
        shape = shape,
        color = unname(colors),
        title = titles
    )

    # Edges from direct dependencies only (no transitive links).
    stepToId <- stats::setNames(ids, sub[["step"]])
    edgeRows <- lapply(seq_len(nrow(sub)), FUN = \(k) {
        refs <- .pip_graph_refs(
            params = sub[["params"]][[k]],
            depends = sub[["depends"]][[k]]
        )
        fromIds <- as.integer(stepToId[refs[["step"]]])
        isIn <- !is.na(fromIds)
        if (!any(isIn)) {
            return(NULL)
        }

        data.frame(
            from = fromIds[isIn],
            to = ids[[k]],
            arrows = "to",
            dashes = refs[["try"]][isIn]
        )
    })
    edgeRows <- Filter(f = Negate(is.null), x = edgeRows)

    edges <- if (length(edgeRows) == 0L) {
        data.frame(
            from = integer(0),
            to = integer(0),
            arrows = character(0),
            dashes = logical(0)
        )
    } else {
        do.call(what = rbind, args = edgeRows)
    }

    list(nodes = nodes, edges = edges)
}


#' Escape the HTML special characters of strings
#'
#' @param x Character vector.
#' @return `x` with `&`, `<`, `>`, `"` and `'` replaced by HTML entities.
#' @noRd
.html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    gsub("'", "&#39;", x, fixed = TRUE)
}

#' Format a value in short form for the graph
#'
#' A language object or a short atomic vector is deparsed, any other value
#' is shown as `<class>`, so that a large value (e.g. a data set) is not
#' deparsed. The result is cut off after `maxChars` characters.
#'
#' @param x Any value.
#' @param maxChars The maximum number of characters.
#' @return A single character string.
#' @noRd
.pip_graph_format_value <- function(x, maxChars = 30L) {
    isShort <- is.null(x) ||
        is.language(x) ||
        (is.atomic(x) && length(x) <= 10L && is.null(dim(x)))
    s <- if (isShort) deparse1(x) else sprintf("<%s>", class(x)[[1L]])
    if (nchar(s) > maxChars) {
        s <- paste0(substr(s, 1L, maxChars), "...")
    }
    s
}

#' Collapse strings to a short list
#'
#' @param x Character vector.
#' @param maxItems The maximum number of items shown. If `x` is longer, the
#' rest is marked with `"..."`.
#' @return A single character string with the items separated by `", "`.
#' @noRd
.pip_graph_format_list <- function(x, maxItems = 5L) {
    if (length(x) > maxItems) {
        x <- c(x[seq_len(maxItems)], "...")
    }
    paste(x, collapse = ", ")
}

#' Get the step references of a step for the graph
#'
#' @param params The step's parameters.
#' @param depends The step's dependencies, i.e. the referenced step names,
#' named by parameter.
#' @return A `data.frame` with one row per referenced step and columns
#' `step` and `try`. `try` is `TRUE` if all parameters that reference the
#' step are `~try()` references (see `formula_try_args()`), i.e. if the
#' step also runs if the referenced step failed.
#' @noRd
.pip_graph_refs <- function(params, depends) {
    isTry <- names(depends) %in% formula_try_args(params)
    steps <- as.character(unique(unname(depends)))
    data.frame(
        step = steps,
        try = vapply(
            steps,
            FUN = \(s) all(isTry[depends == s]),
            FUN.VALUE = logical(1),
            USE.NAMES = FALSE
        )
    )
}

#' Build the hover text of a step for the graph
#'
#' Lists the state, the exec mode and, if set, the tags, the lock, the last
#' run (its start and, if it succeeded, its duration), the failed keys of a
#' mapped step, the unbound parameters and the referenced steps (a `~try()`
#' reference as `try(step)`).
#'
#' @param dat The step table.
#' @param k The row of the step.
#' @return A single string in HTML, with all values escaped.
#' @noRd
.pip_graph_title <- function(dat, k) {
    state <- dat[["state"]][[k]]
    params <- dat[["params"]][[k]]
    unbound <- params[dat[["unbound"]][[k]]]
    refs <- .pip_graph_refs(params, depends = dat[["depends"]][[k]])
    tags <- dat[["tags"]][[k]]
    failedKeys <- .pip_failed_keys(dat[["out"]][[k]])
    timeStart <- dat[["time_start"]][[k]]
    time <- dat[["time"]][[k]]

    lines <- c(state = state, exec = dat[["exec"]][[k]])
    if (length(tags) > 0L) {
        lines[["tags"]] <- .pip_graph_format_list(tags)
    }
    if (isTRUE(dat[["locked"]][[k]])) {
        lines[["locked"]] <- "yes"
    }
    if (!is.na(timeStart)) {
        lastRun <- format(timeStart, "%Y-%m-%d %H:%M:%S")
        if (state == "done" && time >= timeStart) {
            duration <- signif(difftime(time, timeStart), digits = 3L)
            lastRun <- sprintf("%s (%s)", lastRun, format(duration))
        }
        lines[["last run"]] <- lastRun
    }
    if (length(failedKeys) > 0L) {
        lines[["failed keys"]] <- .pip_graph_format_list(failedKeys)
    }
    if (length(unbound) > 0L) {
        values <- vapply(
            unbound,
            FUN = .pip_graph_format_value,
            FUN.VALUE = character(1)
        )
        lines[["params"]] <- .pip_graph_format_list(
            paste(names(unbound), "=", values)
        )
    }
    if (nrow(refs) > 0L) {
        steps <- refs[["step"]]
        steps[refs[["try"]]] <- sprintf("try(%s)", steps[refs[["try"]]])
        lines[["depends"]] <- .pip_graph_format_list(steps)
    }

    paste0(
        "<b>",
        .html_escape(dat[["step"]][[k]]),
        "</b><br>",
        paste0(names(lines), ": ", .html_escape(lines), collapse = "<br>")
    )
}
