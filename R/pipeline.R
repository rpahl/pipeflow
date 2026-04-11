# -------
# Helpers
# -------

.pip_get_last_step <- function(x)
{
    utils::tail(x[["pipeline"]][["step"]], n = 1)
}

.pip_is_indexed <- function(x)
{
    !is.null(data.table::indices(x[["pipeline"]]))
}

.pip_is_pipeflow_pip <- function(x)
{
    inherits(x, "pipeflow_pip")
}

.pip_is_pipeflow_view <- function(x)
{
    inherits(x, "pipeflow_view")
}

.pip_filter_nodes <- function(x, nodes)
{
    x[["pipeline"]][list(nodes), on = ".nodeId"]
}

.pip_filter <- function(x, on, values)
{
    x[["pipeline"]][list(values), on = on]
}

.pip_reindex <- function(x)
{
    data.table::setindexv(x[["pipeline"]], list("step", ".nodeId"))
}

.pip_run_row <- function(x, i, lgr)
{
    if (!.pip_is_indexed(x)) {
        .pip_reindex(x)
    }

    dat <- x[["pipeline"]]
    fun <- dat[["fun"]][[i]]
    args <- dat[["fargs"]][[i]]
    refs <- dat[["refs"]][[i]]

    # If calculation depends on results of earlier steps, get them from
    # respective referenced output slots of the pipeline.
    if (length(refs) > 0) {
        refsOut <- .pip_filter(x, on = "step", values = refs)[["out"]]
        args[names(refs)] <- refsOut
    }

    step <- dat[["step"]][[i]]
    context <- sprintf("step %i ('%s')", i, step)

    out <- withCallingHandlers(
        do.call(fun, args = args),
        error = function(e) {
            data.table::set(dat,
                i = i, j = "state",
                value = .step_states[["failed"]][["name"]]
            )
            lgr(level = "error", msg = e$message, context = context)
            stop_no_call(e$message)
        },
        warning = function(w) {
            lgr(level = "warn", msg = w$message, context = context)
        }
    )

    data.table::set(dat,
        i = i, j = c("out", "time", "state"),
        value = list(list(out), Sys.time(), .step_states[["done"]][["name"]])
    )

    # TODO: not needed here - we will rather update states when changing
    # step parameters
    # .pip_update_downstream(
    #     x,
    #     fromStep = step,
    #     what = "state",
    #     value = .step_states[["outdated"]][["name"]]
    # )

    invisible(x)
}

.pip_step_exists <- function(x, step)
{
    exists(step, where = x[[".steps_to_nodes"]], inherits = FALSE)
}

.pip_steps_to_nodes <- function(x, steps)
{
    mget(steps,
        envir = x[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
}

.pip_update_downstream <- function(x, fromStep, what, value)
{
    startNode <- get(fromStep,
        envir = x[[".steps_to_nodes"]],
        inherits = FALSE
    )
    nodes <- dag_get_reachable_nodes_down(x[[".dag"]], startNode)[-1]
    x[["pipeline"]][list(nodes), (what) := value, on = ".nodeId"]

    invisible(x)
}


# ---------------------------
# Exported pipeline functions
# ---------------------------

#' Create new pipeline
#'
#' @param name Pipeline name.
#'
#' @return A pipeflow pipeline object.
#' @export
pip_new <- function(name = "pipe")
{
    if (!.is_single(name, "character")) {
        "name must be a single string"
    }
    if (is.na(name)) {
        "name must not be NA"
    }

    env <- new.env(parent = emptyenv())
    env[["name"]] <- name
    env[["pipeline"]] <- .empty_pipeline()
    env[[".dag"]] <- dag_new()
    env[[".steps_to_nodes"]] <- new.env(hash = TRUE, parent = emptyenv())
    env[[".steps_downstream_nodes"]] <- new.env(hash = TRUE, parent = emptyenv())

    structure(env, class = c("pipeflow_pip", "environment"))
    env
}

pip_add <- function(x, step, fun, group = step, tags = character(0))
{
    if (pip_has_step(x, step)) {
        stop("step '", step, "' already exists in the pipeline")
    }
    if (!is.function(fun)) {
        stop("fun must be a function")
    }
    if (!.is_single(group, "character") || is.na(group) || !nzchar(group)) {
        stop("group must be a non-empty valid string")
    }

    # Determine and verify potential links to existing steps
    fargs <- .extract_fun_args(fun)
    if (".self" %in% names(fargs)) {
        fargs[[".self"]] <- x
    }
    steps <- c(x[["pipeline"]][["step"]], step)
    refs <- .extract_refs_to_steps(fargs = fargs, steps = steps)
    refNodes <- mget(refs,
        envir = x[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
    if (anyNA(refNodes)) {
        notFound <- Filter(is.na, refNodes)
        stop_no_call(
            "while adding step '", step, "' - cannot reference unknown steps: ",
            paste0("'", names(notFound), "'", collapse = ", ")
        )
    }

    # Update DAG
    d <- x[[".dag"]]
    .nodeId <- dag_add_node(d)
    for (refNode in refNodes) {
        dag_add_edge(d, from = refNode, to = .nodeId)
        # TODO: implement dag_add_edges allowing vector args
    }

    # Create and append step
    newStep <- .new_step(
        step = step, group = group, fun = fun, fargs = fargs, refs = refs,
        .nodeId = .nodeId, tags = tags
    )
    x[["pipeline"]] <- data.table::rbindlist(list(x[["pipeline"]], newStep))
    x[[".steps_to_nodes"]][[step]] <- .nodeId
    invisible(x)
}


pip_bind <- function(x, y, fix.names = TRUE, fix.sep = "_")
{
    if (!.pip_is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.pip_is_pipeflow_pip(y)) {
        stop("y must be a pipeflow pip")
    }
    if (!.is_single(fix.names, "logical")) {
        stop("fix.names must be a single logical value")
    }
    if (!.is_single(fix.sep, "character")) {
        stop("fix.sep must be a single character string")
    }

    stop("to be implemented")
}


pip_collect_out <- function(x, grouped = TRUE)
{
    isView <- inherits(x, "pipeflow_view")
    if (!(isView || .pip_is_pipeflow_pip(x))) {
        stop("x must be a pipeflow pip or view")
    }
    if (!.is_single(grouped, "logical")) {
        stop("grouped must be a single logical value")
    }

    dat <- if (isView) {
        rows <- x[["rows"]]
        x[["pip"]][["pipeline"]][rows, ]
    } else {
        x[["pipeline"]]
    }

    if (nrow(dat) == 0) {
        return(list())
    }

    steps <- dat[["step"]]
    out <- dat[["out"]]

    # Straight-forward step-by-step output
    if (!grouped) {
        return(stats::setNames(out, steps))
    }

    # Grouped output
    groups <- dat[["group"]]
    groupLabels <- unique(groups)
    res <- vector(mode = "list", length = length(groupLabels))
    names(res) <- groupLabels
    for (k in seq_along(groupLabels)) {
        lab <- groupLabels[[k]]
        idx <- which(groups == lab)

        # Group output if at least two steps have the same group label
        if (length(idx) > 1L) {
            grp <- out[idx]
            names(grp) <- steps[idx]
            res[[k]] <- grp
        } else {
            res[[k]] <- out[[idx]]
        }
    }
    res
}


pip_has_step <- function(x, step)
{
    if (!.pip_is_pipeflow_pip(x)) {
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

pip_length <- function(x)
{
    as.integer(nrow(x[["pipeline"]]))
}

pip_run <- function(
    x,
    force = FALSE,
    lgr = pipeflow_lgr,
    progress = NULL
) {
    if (!.pip_is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.is_single(force, "logical")) {
        stop("force must be a single logical value")
    }
    if (!is.null(progress) && !is.function(progress)) {
        stop("progress must be a function")
    }
    if (!is.null(lgr) && !is.function(lgr)) {
        stop("lgr must be a function")
    }
    log_info <- if (is.null(lgr)) {
        function(msg) {}
    } else {
        function(msg, ...) lgr(level = "info", msg = msg, ...)
    }

    dat <- x[["pipeline"]]
    to <- nrow(dat)

    log_info(sprintf("Start run of '%s' pipeline:", x[["name"]]))

    for (i in seq(from = 1, to = to)) {
        step <- dat[["step"]][[i]]
        if (!is.null(progress)) {
            progress(value = i, detail = step)
        }
        info <- sprintf("Step %i/%i %s", i, to, step)

        if (identical(dat[["state"]][[i]], "done") && !force) {
            log_info(sprintf("%s - skipping done step", info))
            next()
        }
        if (dat[["locked"]][[i]]) {
            log_info(sprintf("%s - skipping locked step", info))
            next()
        }

        log_info(info)
        .pip_run_row(x, i, lgr)
    }

    log_info(sprintf("Finished run of '%s' pipeline:", x[["name"]]))
    invisible(x)
}


#' Pipeline view
#'
#' Create a filtered view of a pipeline
#'
#' @param x A pipeflow pipeline.
#' @param i Row indices to keep.
#' @param filter A named list of filters to apply. Each element can be a
#' character vector specifying the values to keep for the corresponding
#' property or, if `fixed` is FALSE, a regular expression. See examples
#' for usage.
#' @param tags Tag filter (character). Keeps steps with any matching tag.
#' @param fixed If TRUE, any values in `filter` are treated as fixed strings,
#' otherwise they are treated as regular expressions.
#' @param ... further args passed to `grepl` (only in effect when `fixed`
#' is `FALSE`).
#'
#' @return A `pipeflow_view` object.
#' @export
#' @examples
#'
#' # Character columns with fixed matching
#' p <- pip_new()
#' pip_add(p, "load", \(x = 1) x, group = "io")
#' pip_add(p, "fit", \(x = ~-1) x + 1, group = "model")
#' pip_add(p, "eval", \(x = ~fit) x, group = "model")
#' pip_run_step(p, "fit")
#' pip_view(p, filter = list(group = "model", state = "done"))
#'
#' # Filter by tags
#' p <- pip_new()
#' pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
#' pip_add(p, "s2", \(x = ~-1) x + 1, tags = "model")
#' pip_add(p, "s3", \(x = ~-1) x, tags = c("daily", "report"))
#'
#' # Filter by regex
#' p <- pip_new()
#' pip_add(p, "load_raw", \(x = 1) x, group = "io")
#' pip_add(p, "fit_model", \(x = ~-1) x + 1, group = "model")
#' pip_add(p, "eval_model", \(x = ~fit_model) x, group = "model")
#'
#' # Filter using explicit row indices
#' p <- pip_new()
#' pip_add(p, "a1", \(x = 1) x, group = "g1")
#' pip_add(p, "a2", \(x = ~-1) x, group = "g2")
#' pip_add(p, "a3", \(x = ~-1) x, group = "g2")
#'
pip_view <- function(
    x,
    i = integer(),
    filter = list(),
    tags = character(),
    fixed = TRUE,
    ...
) {
    dat <- x[["pipeline"]]
    keep <- rep(TRUE, nrow(dat))

    # Filters
    availFilters <- names(which(sapply(dat, is.character)))
    for (name in names(filter)) {
        if (!(name %in% availFilters)) {
            stop(sprintf(
                "Invalid filter name: '%s' - can be one of: %s",
                name, paste(availFilters, collapse = ", ")
            ))
        }
        if (fixed) {
            keep <- keep & (dat[[name]] %in% filter[[name]])
        } else {
            keep <- keep & grepl(pattern = filter[[name]], x = dat[[name]], ...)
        }
    }

    # Tags
    if (length(tags) > 0) {
        hasTag <- vapply(dat[["tags"]],
            FUN = \(x) any(x %in% tags),
            FUN.VALUE = logical(1)
        )
        keep <- keep & hasTag
    }

    # Rows
    rows <- which(keep)
    if (length(i) > 0) {
        if (any(i < 1L | i > nrow(dat))) {
            stop(
                "Invalid row indices in 'i': ",
                paste(i[i < 1L | i > nrow(dat)], collapse = ", ")
            )
        }
        rows <- intersect(rows, i)
    }

    view <- list(pip = x, rows = rows)
    class(view) <- "pipeflow_view"
    view
}

#' Print a pipeflow pipeline
#'
#' @param x A pipeflow pipeline.
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
#' @export
print.pipeflow_pip <- function(x,
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
        cols <- if (identical(dat[["step"]], dat[["group"]])) {
            c("step", "signature", "out", "state", "time")
        } else {
            c("step", "group", "signature", "out", "state", "time")
        }
    }
    if (identical(cols, "all")) {
        cols <- names(dat)
    }

    if (header) {
        cat(sprintf(
            "<pipeflow_pip> %s (%d step%s)\n---------------\n",
            x[["name"]], n, ifelse(n == 1, "", "s")
        ))
    }

    if (length(rows) == 0) {
        rows <- seq_len(n)
    }

    print(dat[rows, cols, with = FALSE],
        topn = topn,
        nrows = nrows,
        row.names = row.names,
        class = class,
        ...
    )

    invisible(x)
}


#' Print a pipeflow view
#'
#' @param x A pipeflow view.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.pipeflow_view <- function(x, header = TRUE, ...)
{
    pip <- x[["pip"]]
    rows <- x[["rows"]]
    nr <- length(rows)
    n <- nrow(pip[["pipeline"]])

    if (header) {
        cat(
            sprintf(
                "<pipeflow_view> %s (%d of %d step%s)\n---------------\n",

                pip[["name"]], nr, n, ifelse(n == 1, "", "s")
            )
        )
    }
    print(pip, rows = rows, header = FALSE, row.names = FALSE, ...)
    invisible(x)
}
