# -------
# Helpers
# -------

.pip_get_last_step <- function(pip)
{
    utils::tail(pip[["pipeline"]][["step"]], n = 1)
}

.pip_is_indexed <- function(pip)
{
    !is.null(data.table::indices(pip[["pipeline"]]))
}

.pip_is_pipeflow_pipe <- function(pip)
{
    inherits(pip, "pipeflow_pipe")
}

.pip_filter_on_nodes <- function(pip, nodes)
{
    pip[["pipeline"]][list(nodes), on = ".nodeId"]
}

.pip_filter_on <- function(pip, on, values)
{
    pip[["pipeline"]][list(values), on = on]
}

.pip_reindex <- function(pip)
{
    data.table::setindexv(pip[["pipeline"]], list("step", ".nodeId"))
}

.pip_run_row <- function(pip, i, lgr)
{
    if (!.pip_is_indexed(pip)) {
        .pip_reindex(pip)
    }

    dt <- pip[["pipeline"]]
    fun <- dt[["fun"]][[i]]
    args <- dt[["fargs"]][[i]]
    refs <- dt[["refs"]][[i]]

    # If calculation depends on results of earlier steps, get them from
    # respective referenced output slots of the pipeline.
    if (length(refs) > 0) {
        refsOut <- .pip_filter_on(pip, on = "step", values = refs)[["out"]]
        args[names(refs)] <- refsOut
    }

    step <- dt[["step"]][[i]]
    context <- sprintf("step %i ('%s')", i, step)

    out <- withCallingHandlers(
        do.call(fun, args = args),
        error = function(e) {
            data.table::set(dt,
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

    data.table::set(dt,
        i = i, j = c("out", "time", "state"),
        value = list(list(out), Sys.time(), .step_states[["done"]][["name"]])
    )

    # TODO: not needed here - we will rather update states when changing
    # step parameters
    # .pip_update_downstream(
    #     pip,
    #     fromStep = step,
    #     what = "state",
    #     value = .step_states[["outdated"]][["name"]]
    # )

    invisible(pip)
}

.pip_step_exists <- function(pip, step)
{
    exists(step, where = pip[[".steps_to_nodes"]], inherits = FALSE)
}

.pip_steps_to_nodes <- function(pip, steps)
{
    mget(steps,
        envir = pip[[".steps_to_nodes"]],
        ifnotfound = NA_integer_,
        inherits = FALSE
    )
}

.pip_update_downstream <- function(pip, fromStep, what, value)
{
    startNode <- get(fromStep,
        envir = pip[[".steps_to_nodes"]],
        inherits = FALSE
    )
    nodes <- dag_get_reachable_nodes_down(pip[[".dag"]], startNode)[-1]
    pip[["pipeline"]][list(nodes), (what) := value, on = ".nodeId"]

    invisible(pip)
}


# ---------------------------
# Exported pipeline functions
# ---------------------------

#' Create new pipeline
#'
#' @param name Pipeline name.
#'
#' @return A pipeflow pipeline object.
#' @noRd
pipe_new <- function(name = "pipe")
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

    structure(env, class = c("pipeflow_pipe", "environment"))
    env
}

pipe_add <- function(pip, step, fun, group = step)
{
    if (pipe_has_step(pip, step)) {
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
        fargs[[".self"]] <- pip
    }
    steps <- c(pip[["pipeline"]][["step"]], step)
    refs <- .extract_refs_to_steps(fargs = fargs, steps = steps)
    refNodes <- mget(refs,
        envir = pip[[".steps_to_nodes"]],
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
    d <- pip[[".dag"]]
    nodeId <- dag_add_node(d)
    for (refNode in refNodes) {
        dag_add_edge(d, from = refNode, to = nodeId)
        # TODO: implement dag_add_edges allowing vector args
    }

    # Create and append step
    newStep <- .new_step(step, fun, fargs, refs, group, .nodeId = nodeId)
    pip[["pipeline"]] <- data.table::rbindlist(list(pip[["pipeline"]], newStep))
    pip[[".steps_to_nodes"]][[step]] <- nodeId

    invisible(pip)
}


pipe_has_step <- function(pip, step)
{
    if (!.pip_is_pipeflow_pipe(pip)) {
        stop("pip must be a pipeflow pipeline")
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

    .pip_step_exists(pip, step)
}

pipe_length <- function(pip)
{
    as.integer(nrow(pip[["pipeline"]]))
}

pipe_run <- function(
    pip,
    force = FALSE,
    lgr = pipeflow_lgr,
    progress = NULL
) {
    if (!.pip_is_pipeflow_pipe(pip)) {
        stop("pip must be a pipeflow pipeline")
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

    dt <- pip[["pipeline"]]
    to <- nrow(dt)

    log_info(sprintf("Start run of '%s' pipeline:", pip[["name"]]))

    for (i in seq(from = 1, to = to)) {
        step <- dt[["step"]][[i]]
        if (!is.null(progress)) {
            progress(value = i, detail = step)
        }
        info <- sprintf("Step %i/%i %s", i, to, step)

        if (identical(dt[["state"]][[i]], "done") && !force) {
            log_info(sprintf("%s - skipping done step", info))
            next()
        }
        if (dt[["skip"]][[i]]) {
            log_info(sprintf("%s - skipping step marked for skip", info))
            next()
        }
        if (dt[["lock"]][[i]]) {
            log_info(sprintf("%s - skipping locked step", info))
            next()
        }

        log_info(info)
        .pip_run_row(pip, i, lgr)
    }

    log_info(sprintf("Finished run of '%s' pipeline:", pip[["name"]]))
    invisible(pip)
}


#' Print a pipeflow pipeline
#'
#' @param x A pipeflow pipeline.
#' @param cols The columns to be printed. Can be either one of
#' "main" or "all" to print the most main or all columns,
#' respectively, or a character vector of columns to be printed.
#' @param topn The number of rows to be printed from the beginning
#' and end of tables with more than `nrows` rows.
#' @param nrows The number of rows printed before truncation is enforced.
#' @param class If TRUE, the resulting output will include above each
#' column its storage class (or a self-evident abbreviation thereof).
#' @param row.names If TRUE, row indices will be printed alongside x.
#' @param ...  Other arguments passed to `print.data.table`
#' @export
print.pipeflow_pipe <- function(x,
    cols = getOption("pipeflow.print.cols", default = "main"),
    topn = getOption("pipeflow.print.topn", default = 5),
    nrows = getOption("pipeflow.print.nrows", default = 100),
    row.names = getOption("pipeflow.print.rownames", default = TRUE),
    class = getOption("pipeflow.print.class", default = FALSE),
    ...
) {
    stopifnot(inherits(x, "pipeflow_pipe"))

    dt <- x[["pipeline"]]
    n <- nrow(dt)

    main <- c("step", "signature", "out", "state", "time")
    if (length(cols) == 1) {
        cols <- switch(cols,
            main = main,
            all = names(dt),
            stop("Invalid value for 'cols': ", cols)
        )
    }

    header <- sprintf(
        "<pipeflow_pipe> %s (%d step%s)\n", x[["name"]],
        n, ifelse(n == 1, "", "s")
    )
    cat(header)

    print(dt[, cols, with = FALSE],
        topn = topn,
        nrows = nrows,
        row.names = row.names,
        class = class,
        ...
    )

    invisible(x)
}
