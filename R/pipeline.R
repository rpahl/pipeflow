
#' Create new pipeline
#'
#' @param name Pipeline name.
#'
#' @return A pipeflow pipeline object.
#' @noRd
pipe_new <- function(name = "pipe")
{
    stopifnot(
        "name must be a single string" = .is_single(name, "character"),
        "name must not be NA" = !is.na(name)
    )

    env <- new.env(parent = emptyenv())
    env[["name"]] <- name
    env[["pipeline"]] <- .empty_pipeline()
    env[[".dag"]] <- dag_new()
    env[[".steps"]] <- new.env(hash = TRUE, parent = emptyenv())

    structure(env, class = c("pipeflow_pipe", "environment"))
    env
}

# -------
# Helpers
# -------

.pip_get_last_step <- function(pip)
{
    utils::tail(pip[["pipeline"]][["step"]], n = 1)
}

.pip_is_pipeflow_pipe <- function(pip)
{
    inherits(pip, "pipeflow_pipe")
}

.pip_nodes_to_steps <- function(pip, nodes)
{
    pip[["pipeline"]][list(nodes), on = ".nodeId"][["step"]]
}

.pip_reindex <- function(pip)
{
    data.table::setindexv(pip[["pipeline"]], list("step", ".nodeId"))
}

.pip_run_row <- function(pip, i, lgr)
{
    pipi <- unlist1(pip[["pipeline"]][i])
    fun <- pipi[["fun"]]
    args <- pipi[["args"]]
    refs <- pipi[["refs"]]

    # If calculation depends on results of earlier steps, get them from
    # respective referenced output slots of the pipeline.
    if (length(refs) > 0) {
        # out <- pip[refs][["out"]]
        # depdendentOut <- private$.extract_dependent_out(refs, out)
        args[refs] <- pip[["pipeline"]][refs][["out"]]
    }

    step <- pipi[["step"]]
    context <- sprintf("step %i ('%s')", i, step)

    out <- withCallingHandlers(
        do.call(fun, args = args),
        error = function(e) {
            pip[["pipeline"]][i, "state"] <- "failed"
            lgr(level = "error", msg = e$message, context = context)
            stop_no_call(e$message)
        },
        warning = function(w) {
            lgr(level = "warn", msg = w$message, context = context)
        }
    )

    pip[["pipeline"]][i, "time"] <- Sys.time()
    pip[["pipeline"]][i, "out"] <- list(out)
    pip[["pipeline"]][i, "state"] <- "done"

    #.pip_update_states_downstream(step, "outdated")

    invisible(ok)
}

.pip_step_exists <- function(pip, step)
{
    exists(step, envir = pip[[".steps"]], inherits = FALSE)
}

.pip_steps_to_nodes <- function(pip, steps)
{
    pip[["pipeline"]][list(steps), on = "step"][[".nodeId"]]
}


# ----------------------
# Pipeline functionality
# ----------------------

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
    for (ref in refs) {
        if (!.pip_step_exists(pip, ref)) {
            stop("step '", step, "': dependency '", ref, "' not found")
        }
    }

    # Update DAG
    d <- pip[[".dag"]]
    nodeId <- dag_add_node(d)
    for (ref in refs) {
        from <- pip[["pipeline"]][list(ref), on = "step"][[".nodeId"]]
        dag_add_edge(d, from = from, to = nodeId)
    }

    # Create and append step
    newStep <- .new_step(step, fun, fargs, refs, group, .nodeId = nodeId)
    pip[["pipeline"]] <- data.table::rbindlist(list(pip[["pipeline"]], newStep))
    pip[[".steps"]][[step]] <- TRUE

    invisible(.pip_reindex(pip))
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
    progress = function(value, detail) {},
    lgr = pipeflow_lgr
) {
    stopifnot(
        "pip must be a pipeflow pipeline" = .pip_is_pipeflow_pipe(pip),
        "force must be a single logical value" = .is_single(force, "logical"),
        "progress must be a function" = is.function(progress),
        "lgr must be a function" = is.function(lgr)
    )
    log_info <- function(msg, ...) lgr(level = "info", msg = msg, ...)

    sprintf("Start run of '%s' pipeline:", pip[["name"]]) |> log_info()

    to <- pipe_length(pip)
    for (i in seq(from = 1, to = to)) {
        pipi <- unlist1(pip[["pipeline"]][i])
        step <- as.character(pipi[["step"]])
        progress(value = i, detail = step)
        info <- sprintf("Step %i/%i %s", i, to, step)

        if (pipi[["state"]] == "done" && !force) {
            paste0(info, " - skipping done step") |> log_info()
            next()
        }
        if (pipi[["skip"]]) {
            paste0(info, " - skipping step marked for skip") |> log_info()
            next()
        }
        if (pipi[["lock"]]) {
            paste0(info, " - skipping locked step") |> log_info()
            next()
        }

        log_info(info)
        .pip_run_row(pip, i)
    }

    sprintf("Finished run of '%s' pipeline:", pip[["name"]]) |> log_info()
    invisible(self)
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
