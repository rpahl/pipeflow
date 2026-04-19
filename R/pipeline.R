# -------
# Helpers
# -------
.assert_pip_or_view <- function(x)
{
    if (!(.is_pipeflow_pip(x) || .is_pipeflow_view(x))) {
        stop_no_call("x must be a pipeflow pip or view")
    }
}
.is_pipeflow_view <- function(x)
{
    inherits(x, "pipeflow_view")
}
.is_pipeflow_pip <- function(x)
{
    inherits(x, "pipeflow_pip")
}

.pip_data <- function(x)
{
    isView <- inherits(x, "pipeflow_view")
    if (isView) {
        rows <- x[["rows"]]
        x[["pip"]][["pipeline"]][rows, ]
    } else {
        x[["pipeline"]]
    }
}

.pip_filter_nodes <- function(x, nodes)
{
    x[["pipeline"]][list(nodes), on = ".nodeId"]
}

.pip_filter <- function(x, on, values)
{
    x[["pipeline"]][list(values), on = on]
}

.pip_is_indexed <- function(x)
{
    !is.null(data.table::indices(x[["pipeline"]]))
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
    args <- dat[["params"]][[i]]
    depends <- dat[["depends"]][[i]]

    # If calculation depends on results of earlier steps, get them from
    # respective referenced output slots of the pipeline.
    if (length(depends) > 0) {
        refsOut <- .pip_filter(x, on = "step", values = depends)[["out"]]
        args[names(depends)] <- refsOut
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

.pip_update_downstream <- function(x, steps, what, value)
{
    start_ids <- mget(steps,
        envir = x[[".steps_to_nodes"]],
        inherits = FALSE
    )
    nodes <- dag_get_reachable_nodes_down(x[[".dag"]], as.integer(start_ids))
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

#' Add a new step to the pipeline
#'
#' @param x A pipeflow pipeline object.
#' @param step Step name.
#' @param fun Function to execute for the step.
#' @param group Step group name.
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be set later (see)
#' @return The updated pipeflow pipeline object.
#' @export
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
    params <- .extract_fun_params(fun)
    if (".self" %in% names(params)) {
        params[[".self"]] <- x
    }
    steps <- c(x[["pipeline"]][["step"]], step)
    depends <- .extract_depends(params = params, steps = steps)
    refNodes <- mget(depends,
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
    .nodeId <- as.integer(dag_add_node(d))
    if (length(refNodes) > 0) {
        dag_add_edges_to(d, from = as.integer(refNodes), to = .nodeId)
    }

    # Create and append step
    newStep <- .new_step(
        step = step,
        group = group,
        fun = fun,
        params = lapply(params, eval),
        depends = depends,
        tags = tags,
        .nodeId = .nodeId
    )
    x[["pipeline"]] <- data.table::rbindlist(list(x[["pipeline"]], newStep))
    x[[".steps_to_nodes"]][[step]] <- .nodeId
    invisible(x)
}


pip_bind <- function(x, y, fix.names = TRUE, fix.sep = "_")
{
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (!.is_pipeflow_pip(y)) {
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


#' Collect pipeline output
#'
#' @param x A pipeflow pip or view
#' @param grouped Logical indicating if the output should be grouped by step
#' groups
#' @return A list of pipeline outputs
#' @export
pip_collect_out <- function(x, grouped = TRUE)
{
    .assert_pip_or_view(x)
    if (!.is_single(grouped, "logical")) {
        stop("grouped must be a single logical value")
    }

    dat <- .pip_data(x)
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
            res[[k]] <- stats::setNames(out[idx], nm = steps[idx])
        } else {
            res[[k]] <- out[[idx]]
        }
    }
    res
}

#' Extract all independent pipeline parameters
#'
#' Independent parameters are those that are not dependent on any other steps
#' in the pipeline.
#' @param x A pipeflow pip or view
#' @return Unique list of all independent pipeline parameters
#' @export
pip_get_params <- function(x)
{
    .assert_pip_or_view(x)
    dat <- .pip_data(x)

    params <- mapply(
        par = dat[["params"]], indeps = dat[[".indeps"]],
        FUN = \(par, indeps) par[indeps],
        SIMPLIFY = FALSE
    ) |>
        Filter(f = \(x) length(x) > 0)

    parNames <- unlist(sapply(params, FUN = names))
    parValues <- stats::setNames(unlist1(params), parNames)
    as.list(parValues[!duplicated(names(parValues))])
}


#' Check if a step exists in the pipeline
#'
#' @param x A pipeflow pip
#' @param step A step name
#' @return Logical indicating if the step exists
#' @export
pip_has_step <- function(x, step)
{
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


#' Run pipeline
#'
#' @param x A pipeflow pip
#' @param lgr A logging function of the form `function(level, msg, ...)`.
#' To suppress logging, you can set `lgr = NULL`.
#' @param force Logical indicating if all steps should be forced to run,
#' regardless of whether they are outdated or not.
#' @param progress A progress function
#' @return The updated pipeline
#' @export
pip_run <- function(
    x,
    lgr = pipeflow_lgr,
    force = FALSE,
    progress = NULL
) {
    # TODO: change implementation to work with pip and view
    if (!.is_pipeflow_pip(x)) {
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


#' Set independent pipeline parameters
#'
#' Independent parameters are those that are not dependent on any other steps
#' in the pipeline.
#' @details Each step for which one or more parameters are set will have its
#' state as well as the state of any downstream dependent steps updated to
#' "outdated". The only exception is if a step is locked - parameters of
#' locked steps are never changed and so their state remains unchanged.
#' @param p A pipeflow pip or view
#' @param params list of parameters to set
#' @param warnUnused Logical indicating if a warning should be issued for
#' unused parameters.
#' @return The updated pipeline
#' @export
pip_set_params <- function(p, params = list(), warnUnused = FALSE)
{
    # Input checking
    .assert_pip_or_view(p)
    if (!.is_single(warnUnused, "logical")) {
        stop("warnUnused must be a single logical value")
    }
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
    considered_rows <- setdiff(rows, which(dat[["locked"]]))

    if (length(considered_rows) == 0L) {
        if (warnUnused) {
            warning(
                "Trying to set parameters not defined in the target: ",
                toString(parNames)
            )
        }
        return(invisible(p))
    }

    # Determine which steps/rows are affected (i.e. have overlapping parameters)
    overlaps <- lapply(dat[[".indeps"]][considered_rows],
        FUN = \(indep) intersect(indep, parNames)
    )

    if (warnUnused) {
        used <- unique(unlist(overlaps))
        unused <- setdiff(parNames, used)
        if (length(unused) > 0L) {
            warning(
                "Trying to set parameters not defined in the target: ",
                toString(unused)
            )
        }
    }

    hasOverlap <- lengths(overlaps) > 0
    if (any(hasOverlap)) {
        # Set new parameters at all steps that are affected
        set <- data.table::set
        changedRows <- considered_rows[hasOverlap]
        Map(
            f = \(i, ov) {
                rowPars <- dat[["params"]][[i]]
                rowPars[ov] <- params[ov]
                set(dat, i = i, j = "params", value = list(list(rowPars)))
                set(dat, i = i, j = "state", value = "outdated")
            },
            i = changedRows, ov = overlaps[hasOverlap]
        )

        # Update states of changed steps and their downstream steps
        steps <- dat[["step"]][changedRows]
        .pip_update_downstream(
            x, steps = steps, what = "state", value = "outdated"
        )
    }

    invisible(p)
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


# ------------------------------------
# Implementation of generic S3 methods
# ------------------------------------

#' @param x A pipeflow pipeline.
#' @export
length.pipeflow_pip <- function(x)
{
    as.integer(nrow(x[["pipeline"]]))
}

#' @param x A pipeflow view.
#' @export
length.pipeflow_view <- function(x)
{
    as.integer(length(x[["rows"]]))
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
            c("step", "signature", "out", "state")
        } else {
            c("step", "group", "signature", "out", "state")
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
