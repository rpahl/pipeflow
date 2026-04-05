
#' Create new pipeline
#'
#' @param name Pipeline name.
#'
#' @return A pipeflow pipeline object.
#' @noRd
pipe_new <- function(name = "pipe")
{
    stopifnot(
        "name must be a single string" = is_string(name),
        "name must not be NA" = !is.na(name)
    )

    env <- new.env(parent = emptyenv())
    env[["name"]] <- name
    env[["pipeline"]] <- .empty_pipeline()
    env[[".dag"]] <- dag_new()
    env[[".step_to_node"]] <- new.env(hash = TRUE, parent = emptyenv())
    env[[".step_to_row"]] <- new.env(hash = TRUE, parent = emptyenv())
    # node_to_row = new.env(hash = TRUE, parent = emptyenv())

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

.pip_step_exists <- function(pip, step)
{
    exists(step, envir = pip[[".step_to_row"]], inherits = FALSE)
    # !is.null(pip[[".step_to_row"]][[step]])
}

.pip_step_row <- function(pip, step)
{
    pip[[".step_to_row"]][[step]]
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
    if (!is_string(group) || is.na(group) || !nzchar(group)) {
        stop("group must be a non-empty valid string")
    }

    # Determine and verify potential links to existing steps
    fargs <- .extract_fun_args(fun)
    steps <- pip[["pipeline"]][["step"]]
    refs <- .extract_refs_to_steps(fargs = fargs, steps = steps)
    for (ref in refs) {
        if (!.pip_step_exists(pip, ref)) {
            stop("step '", step, "': dependency '", ref, "' not found")
        }
    }
    newStep <- .new_step(step, fun, fargs, refs, group)
    pip[["pipeline"]] <- data.table::rbindlist(list(pip[["pipeline"]], newStep))

    # Update internal pipeline state
    d <- pip[[".dag"]]
    nodeId <- dag_add_node(d)
    pip[[".step_to_node"]][[step]] <- nodeId
    pip[[".step_to_row"]][[step]] <- nrow(pip[["pipeline"]])
    for (ref in refs) {
        dag_add_edge(d, from = pip[[".step_to_node"]][[ref]], to = nodeId)
    }

    invisible(pip)
}


pipe_get_step_names <- function(pip)
{
    if (!.pip_is_pipeflow_pipe(pip)) {
        stop("pip must be a pipeflow pipeline")
    }

    pip$pipeline[["step"]]
}


pipe_get_step_number <- function(pip, step)
{
    if (!.pip_is_pipeflow_pipe(pip)) {
        stop("pip must be a pipeflow pipeline")
    }

    .pip_step_row(pip, step)
}


pipe_has_step <- function(pip, step)
{
    if (!.pip_is_pipeflow_pipe(pip)) {
        stop("pip must be a pipeflow pipeline")
    }

    if (!is_string(step)) {
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
    nrow(pip[["pipeline"]])
}

#' Print a pipeflow pipeline
#'
#' @param x A pipeflow pipeline.
#' @param cols The columns to be printed. Can be either one of
#' "important" or "all" to print the most important or all columns,
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
    cols = getOption("pipeflow.print.cols", default = "important"),
    topn = getOption("pipeflow.print.topn", default = 5),
    nrows = getOption("pipeflow.print.nrows", default = 100),
    row.names = getOption("pipeflow.print.rownames", default = TRUE),
    class = getOption("pipeflow.print.class", default = FALSE),
    ...
) {
    stopifnot(inherits(x, "pipeflow_pipe"))

    dt <- x[["pipeline"]]
    n <- nrow(dt)

    important <- c("step", "depends", "out", "group", "state")
    if (length(cols) == 1) {
        cols <- switch(cols,
            important = important,
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
