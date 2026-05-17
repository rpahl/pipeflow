# -------
# Helpers
# -------
.assert_logger <- function(lgr)
{
    if (!is.function(lgr)) {
        stop("lgr must be a function")
    }
    if (!all(c("level", "msg") %in% names(formals(lgr)))) {
        stop("lgr must be a function with arguments 'level' and 'msg'")
    }
}

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

.pip_append <- function(x, step, fun, group, tags)
{
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
    x
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

.pip_get_downstream_nodes <- function(x, steps)
{
    start_ids <- mget(steps,
        envir = x[[".steps_to_nodes"]],
        inherits = FALSE
    )
    dag_get_reachable_nodes_down(x[[".dag"]], as.integer(start_ids))
}

.pip_is_indexed <- function(x)
{
    !is.null(data.table::indices(x[["pipeline"]]))
}

.pip_reindex <- function(x)
{
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    x[["pipeline"]][, .rowId := .I]
    data.table::setindexv(x[["pipeline"]], list("step", ".nodeId"))
}

.pip_run_row <- function(x, i, lgr)
{
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

    # If calculation depends on results of earlier steps, get them from
    # respective referenced output slots of the pipeline.
    if (length(depends) > 0) {
        refsOut <- .pip_filter(x, on = "step", values = depends)[["out"]]
        args[names(depends)] <- refsOut
    }

    step <- dat[["step"]][[i]]

    out <- withCallingHandlers(
        do.call(fun, args = args),
        error = function(e) {
            data.table::set(dat,
                i = i, j = "state",
                value = .step_states[["failed"]][["name"]]
            )
            lgr(level = "error", msg = e$message)
            stop_no_call(e$message)
        },
        warning = function(w) {
            lgr(level = "warn", msg = w$message)
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
    nodes <- .pip_get_downstream_nodes(x, steps)
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

    hash_map <- function() new.env(parent = emptyenv())
    env <- hash_map()
    env[["name"]] <- name
    env[["pipeline"]] <- .empty_pipeline()
    env[[".dag"]] <- dag_new()
    env[[".steps_to_nodes"]] <- hash_map()

    structure(env, class = c("pipeflow_pip", "environment"))
    env
}

#' Add new step to the pipeline
#'
#' Adds a new step to the pipeline, by default at the end.
#' If `after` was specified, the new step will be inserted after the given
#' step or position. Be aware that in contrast to adding a step at the end,
#' inserting a step in the middle is a rather expensive operation as it
#' requires re-wiring parts of the internal pipeline structure, especially
#' if the new step is inserted at an early position.
#' @param x A pipeflow pipeline object.
#' @param step Step name.
#' @param fun Function to execute for the step.
#' @param group Step group name.
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be set later (see TODO: pip_set_tags or something similar).
#' @param after Optional position after which the new step should be inserted
#' (defaults to last position). Can be a step name or an integer index. If
#' set to 0, the new step will be inserted at the beginning of the pipeline.
#' @return The updated pipeflow pipeline object.
#' @export
pip_add <- function(
    x, step, fun,
    group = step,
    tags = character(0),
    after = length(x)
)
{
    if (!.is_pipeflow_pip(x)) {
        stop("x must be a pipeflow pip")
    }
    if (pip_has_step(x, step)) {
        stop("step '", step, "' already exists in the pipeline")
    }
    if (!is.function(fun)) {
        stop("fun must be a function")
    }
    if (!.is_single(group, "character") || is.na(group) || !nzchar(group)) {
        stop("group must be a non-empty valid string")
    }

    n <- length(x)

    if (is.character(after)) {
        if (!.is_single(after, "character") || is.na(after) || !nzchar(after)) {
            stop("after must be a non-empty step name or integer index")
        }
        if (!pip_has_step(x, after)) {
            stop("step '", after, "' does not exist")
        }
        pos <- match(after, x[["pipeline"]][["step"]])
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

    if (pos == length(x)) {
        return(.pip_append(
            x, step = step, fun = fun, group = group, tags = tags
        ))
    }

    src <- pip_clone(x)
    dat <- src[["pipeline"]]
    n <- nrow(dat)

    out <- if (pos > 0L) src[seq_len(pos)] else pip_new(name = src[["name"]])
    pip_add(out, step = step, fun = fun, group = group, tags = tags)

    if (pos < n) {
        tailRows <- seq.int(pos + 1L, n)
        for (i in tailRows) {
            tailStep <- dat[["step"]][[i]]
            pip_add_from(out, step = tailStep, y = src)

            iOut <- nrow(out[["pipeline"]])
            data.table::set(
                out[["pipeline"]],
                i = iOut,
                j = c("out", "time", "state", "locked", "meta"),
                value = list(
                    list(dat[["out"]][[i]]),
                    dat[["time"]][[i]],
                    dat[["state"]][[i]],
                    dat[["locked"]][[i]],
                    list(dat[["meta"]][[i]])
                )
            )
        }
    }

    x[["pipeline"]] <- out[["pipeline"]]
    x[[".dag"]] <- out[[".dag"]]
    x[[".steps_to_nodes"]] <- out[[".steps_to_nodes"]]
    invisible(x)
}


#' Add step from another pipeline
#'
#' Add one existing step definition from pipeline `y` to pipeline `x`.
#' The step is added via [pip_add()] so dependency checks and DAG updates
#' are applied consistently.
#'
#' @param x Target pipeflow pipeline object.
#' @param step Step name to copy from `y`.
#' @param y Source pipeflow pipeline object.
#'
#' @return The updated target pipeflow pipeline object.
#' @export
pip_add_from <- function(x, step, y)
{
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
    group <- y[["pipeline"]][["group"]][[iStep]]
    tags <- y[["pipeline"]][["tags"]][[iStep]]
    params <- y[["pipeline"]][["params"]][[iStep]]
    depends <- y[["pipeline"]][["depends"]][[iStep]]
    indeps <- y[["pipeline"]][[".indeps"]][[iStep]]

    # Recreate defaults from stored params/dependencies so pip_add can
    # resolve references and wire DAG updates in the target pipeline.
    f <- fun
    fml <- formals(f)

    for (nm in indeps) {
        if (identical(nm, ".self")) {
            next
        }
        fml[[nm]] <- params[[nm]]
    }

    if (length(depends) > 0L) {
        for (arg in names(depends)) {
            fml[[arg]] <- stats::as.formula(paste("~", depends[[arg]]))
        }
    }

    formals(f) <- fml
    pip_add(x, step = step, fun = f, group = group, tags = tags)
}

#' Bind two pipelines together
#'
#' Bind two pipelines together by concatenating their steps. If both pipelines
#' have steps with the same name, the step names of the second pipeline will be
#' automatically adapted to avoid name clashes.
#' @param x A pipeflow pipeline object.
#' @param y A pipeflow pipeline object.
#' @return A new pipeflow pipeline object representing the bound pipelines.
#' @export
pip_bind <- function(x, y)
{
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
        pip_add_from(out, step = step, y = yy)

        # Preserve runtime metadata/state from source pipeline.
        iOut <- nrow(out[["pipeline"]])
        data.table::set(
            out[["pipeline"]],
            i = iOut,
            j = c("out", "time", "state", "locked", "meta"),
            value = list(
                list(yyDat[["out"]][[k]]),
                yyDat[["time"]][[k]],
                yyDat[["state"]][[k]],
                yyDat[["locked"]][[k]],
                list(yyDat[["meta"]][[k]])
            )
        )
    }

    out
}


#' Clone pipeline
#'
#' @param x A pipeflow pipeline object.
#' @param name Optional name for the cloned pipeline. If `NULL`, the original
#' name is used.
#'
#' @return A cloned pipeflow pipeline object.
#' @export
pip_clone <- function(x, name = NULL)
{
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

    # Re-point explicit self references to the cloned pipeline
    for (k in seq_len(nrow(dat))) {
        pars <- dat[["params"]][[k]]
        if (".self" %in% names(pars) && identical(pars[[".self"]], x)) {
            pars[[".self"]] <- out
            data.table::set(dat, i = k, j = "params", value = list(list(pars)))
        }
    }

    out[["pipeline"]] <- dat

    # Clone steps to nodes mapping
    for (k in seq_len(nrow(dat))) {
        step <- dat[["step"]][[k]]
        nodeId <- dat[[".nodeId"]][[k]]
        out[[".steps_to_nodes"]][[step]] <- nodeId
    }

    out
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


#' Remove a step from the pipeline
#'
#' If other steps depend on the step to be removed, an error is
#' given and the removal is blocked, unless `recursive` was set to
#' `TRUE`.
#' @param x A pipeflow pip
#' @param step `string` the name of the step to be removed.
#' @param recursive `logical` if `TRUE` the step is removed together
#' with all its downstream dependencies.
#' @return The updated pipeflow pipeline object.
#' @export
pip_remove <- function(x, step, recursive = FALSE)
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
    if (!pip_has_step(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
        stop("recursive must be a single logical value")
    }

    dat <- x[["pipeline"]]
    directDeps <- dat[["step"]][
        vapply(
            dat[["depends"]],
            FUN = \(dep) step %in% dep,
            FUN.VALUE = logical(1)
        )
    ]

    if (length(directDeps) > 0L && !recursive) {
        stepsString <- paste0("'", directDeps, "'", collapse = ", ")
        stop(
            "cannot remove step '", step, "' because the following ",
            "steps depend on it: ",
            stepsString
        )
    }

    stepsToRemove <- step
    if (recursive) {
        downNodes <- .pip_get_downstream_nodes(x, step)
        downNodes <- unique(as.integer(unlist(downNodes)))
        stepNode <- as.integer(.pip_steps_to_nodes(x, step)[[1]])

        recursiveDeps <- dat[["step"]][
            dat[[".nodeId"]] %in% setdiff(downNodes, stepNode)
        ]
        if (length(recursiveDeps) > 0L) {
            stepsString <- paste0("'", recursiveDeps, "'", collapse = ", ")
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
        ok <- dag_remove_node(x[[".dag"]], nid, force = recursive)
        if (!ok) {
            stop("failed to remove node ", nid, " from DAG")
        }
    }
    dag_tidy_up(x[[".dag"]])

    if (".rowId" %in% names(dat)) {
        data.table::set(dat, j = ".rowId", value = NULL)
    }

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


#' Rename a step in the pipeline
#' @param x A pipeflow pip
#' @param from Existing step name
#' @param to New step name
#' @return The updated pipeline
#' @export
pip_rename <- function(x, from, to)
{
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


#' Replace a step in the pipeline
#'
#' @param x A pipeflow pipeline object.
#' @param step Step name.
#' @param fun Function to execute for the step.
#' @param group Step group name.
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be set later (see TODO: pip_set_tags or something similar).
#' @return The updated pipeflow pipeline object.
#' @export
pip_replace <- function(x, step, fun, group = step, tags = character(0))
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
    if (!pip_has_step(x, step)) {
        stop("step '", step, "' does not exist")
    }
    if (!is.function(fun)) {
        stop("fun must be a function")
    }
    if (!.is_single(group, "character") || is.na(group) || !nzchar(group)) {
        stop("group must be a non-empty valid string")
    }

    src <- pip_clone(x)
    if (".rowId" %in% names(src[["pipeline"]])) {
        data.table::set(src[["pipeline"]], j = ".rowId", value = NULL)
    }
    dat <- src[["pipeline"]]
    n <- nrow(dat)
    iStep <- match(step, dat[["step"]])

    out <- if (iStep > 1L) {
        src[seq_len(iStep - 1L)]
    } else {
        pip_new(name = src[["name"]])
    }
    if (".rowId" %in% names(out[["pipeline"]])) {
        data.table::set(out[["pipeline"]], j = ".rowId", value = NULL)
    }

    # Add replacement step at the original position.
    pip_add(out, step = step, fun = fun, group = group, tags = tags)

    # Re-append subsequent steps and preserve their runtime metadata.
    if (iStep < n) {
        tailRows <- seq.int(iStep + 1L, n)
        for (i in tailRows) {
            tailStep <- dat[["step"]][[i]]
            pip_add_from(out, step = tailStep, y = src)

            iOut <- nrow(out[["pipeline"]])
            data.table::set(
                out[["pipeline"]],
                i = iOut,
                j = c("out", "time", "state", "locked", "meta"),
                value = list(
                    list(dat[["out"]][[i]]),
                    dat[["time"]][[i]],
                    dat[["state"]][[i]],
                    dat[["locked"]][[i]],
                    list(dat[["meta"]][[i]])
                )
            )
        }
    }

    # Mark downstream dependent steps as outdated, but keep the replaced
    # step itself as "new".
    downNodes <- .pip_get_downstream_nodes(out, step)
    stepNode <- .pip_steps_to_nodes(out, step)[[1]]
    downNodes <- unique(setdiff(as.integer(unlist(downNodes)), stepNode))
    if (length(downNodes) > 0L) {
        out[["pipeline"]][
            list(downNodes),
            state := .step_states[["outdated"]][["name"]],
            on = ".nodeId"
        ]
    }

    x[["pipeline"]] <- out[["pipeline"]]
    x[[".dag"]] <- out[[".dag"]]
    x[[".steps_to_nodes"]] <- out[[".steps_to_nodes"]]
    invisible(x)
}


#' Run pipeline
#'
#' @param x A pipeflow pip or view
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
    .assert_pip_or_view(x)
    if (!.is_single(force, "logical")) {
        stop("force must be a single logical value")
    }
    if (!is.null(progress) && !is.function(progress)) {
        stop("progress must be a function")
    }
    if (is.null(lgr)) lgr <- function(...) {} else .assert_logger(lgr)
    log_info <- function(msg) lgr(level = "info", msg = msg)

    isView <- .is_pipeflow_view(x)
    pip <- if (isView) x[["pip"]] else x
    dat <- pip[["pipeline"]]
    rowsToRun <- if (isView) x[["rows"]] else seq_len(nrow(dat))
    if (isView) {
        # Add rows of upstream dependencies not yet covered by the view.
        deps <- unique(unlist(dat[["depends"]][rowsToRun]))
        if (length(deps) > 0L) {
            depRows <- which(dat[["step"]] %in% deps)
            rowsToRun <- sort(unique(c(rowsToRun, depRows)))
        }
    }
    processedSteps <- character()
    on.exit({
        # At the end, mark all downstream dependent steps as outdated that
        # were *not* processed, which can happen in two different ways:
        # a) when running a view that does not cover the entire pipeline or
        # b) the run aborted in the middle due to an error
        processedNodes <- as.integer(.pip_steps_to_nodes(pip, processedSteps))
        outdatedNodes <- .pip_get_downstream_nodes(pip, processedSteps) |>
            unlist() |> unique() |> setdiff(processedNodes) # nolint
        if (length(outdatedNodes) > 0L) {
            dat[list(outdatedNodes), state := "outdated", on = ".nodeId"]
        }
    })

    log_info(sprintf("Start run of %s '%s':", data.class(x), x[["name"]]))
    for (i in seq_along(rowsToRun)) {
        row <- rowsToRun[[i]]
        step <- dat[["step"]][[row]]
        processedSteps <- c(processedSteps, step)
        if (!is.null(progress)) {
            progress(value = i, detail = step)
        }
        msg <- sprintf("Step %i/%i %s", i, length(rowsToRun), step)

        if (identical(dat[["state"]][[row]], "done") && !force) {
            log_info(sprintf("%s - skipping done step", msg))
            next()
        }
        if (dat[["locked"]][[row]]) {
            log_info(sprintf("%s - skipping locked step", msg))
            next()
        }

        log_info(msg)
        .pip_run_row(pip, i = row, lgr = lgr)
    }


    log_info(sprintf("Finished run of %s '%s':", data.class(x), x[["name"]]))
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
#' @return The updated pipeline
#' @export
pip_set_params <- function(p, params = list())
{
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
    considered_rows <- setdiff(rows, which(dat[["locked"]]))

    if (length(considered_rows) == 0L) {
        warning(
            "Trying to set parameters not defined in the target: ",
            toString(parNames)
        )
        return(invisible(p))
    }

    # Determine which steps/rows are affected (i.e. have overlapping parameters)
    overlaps <- lapply(dat[[".indeps"]][considered_rows],
        FUN = \(indep) intersect(indep, parNames)
    )

    used <- unique(unlist(overlaps))
    unused <- setdiff(parNames, used)
    if (length(unused) > 0L) {
        warning(
            "Trying to set parameters not defined in the target: ",
            toString(unused)
        )
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


#' Set step properties
#'
#' Sets tags, meta information, and lock status for all steps in the pipeline
#' unless `p` is a view, in which case the properties will only be set for
#' the steps covered by the view.
#' @param p A pipeflow pip or view
#' @param tags Character vector of tags to set for each step.
#' @param meta List of metadata to set for each step.
#' @param lock Logical indicating if the steps should be locked. Locked
#' steps are skipped during pipeline runs and none of their properties can be
#' changed until they are unlocked again.
#' @export
pip_set_props <- function(
    p,
    tags = character(),
    meta = list(),
    lock = NULL
)
{
    .assert_pip_or_view(p)

    setTags <- !missing(tags)
    setMeta <- !missing(meta)
    setLock <- !missing(lock)

    if (setTags && !is.character(tags)) {
        stop("tags must be a character vector")
    }
    if (setMeta && !is.list(meta)) {
        stop("meta must be a list")
    }
    if (setLock) {
        if (!.is_single(lock, "logical") || is.na(lock)) {
            stop("lock must be a single logical value")
        }
    }

    isView <- .is_pipeflow_view(p)
    x <- if (isView) p[["pip"]] else p
    dat <- x[["pipeline"]]
    rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

    if (length(rows) == 0L || !(setTags || setMeta || setLock)) {
        return(invisible(p))
    }

    for (i in rows) {
        isLocked <- isTRUE(dat[["locked"]][[i]])
        isUnlocking <- setLock && identical(lock, FALSE)

        # Locked steps are immutable unless this call explicitly unlocks them.
        if (isLocked && !isUnlocking) {
            next
        }

        if (setTags) {
            data.table::set(dat, i = i, j = "tags", value = list(list(tags)))
        }
        if (setMeta) {
            data.table::set(dat, i = i, j = "meta", value = list(list(meta)))
        }
        if (setLock) {
            data.table::set(dat, i = i, j = "locked", value = lock)
        }
    }

    invisible(p)
}


#' Pipeline view
#'
#' Create a filtered view of a pipeline (or view).
#'
#' @param x A pipeflow pipeline or view.
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
#' pip_view(p, tags = "daily")
#'
#' # Filter by regex
#' p <- pip_new()
#' pip_add(p, "load_raw", \(x = 1) x, group = "io")
#' pip_add(p, "fit_model", \(x = ~-1) x + 1, group = "model")
#' pip_add(p, "eval_model", \(x = ~fit_model) x, group = "model")
#' pip_view(p, filter = list(step = "_model$"), fixed = FALSE)
#'
#' # Filter using explicit row indices
#' p <- pip_new()
#' pip_add(p, "a1", \(x = 1) x, group = "g1")
#' pip_add(p, "a2", \(x = ~-1) x, group = "g2")
#' pip_add(p, "a3", \(x = ~-1) x, group = "g2")
#' pip_view(p, i = c(1L, 2L), filter = list(group = "g2"))
#'
#' # Chain filters by create view of view
#' p <- pip_new()
#' pip_add(p, "s1", \(x = 1) x, tags = c("core", "daily"))
#' pip_add(p, "s2", \(x = ~-1) x + 1, tags = "model")
#' pip_add(p, "s3", \(x = ~-1) x, tags = c("daily", "report"))
#' v1 <- pip_view(p, tags = "daily")
#' print(v1)
#' v2 <- pip_view(v1, tags = "report")
#' print(v2)
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

    dat <- if (isView) {
        .pip_reindex(x[["pip"]])
        x[["pip"]][["pipeline"]]
    } else {
        x[["pipeline"]]
    }
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
                toString(i[i < 1L | i > nrow(dat)])
            )
        }
        rows <- intersect(rows, i)
    }

    if (isView) {
        rows <- dat[[".rowId"]][rows]
    }

    pip <- if (isView) x[["pip"]] else x
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
#' @rdname S3generics
#' @export
length.pipeflow_pip <- function(x)
{
    as.integer(nrow(x[["pipeline"]]))
}

#' @rdname S3generics
#' @export
length.pipeflow_view <- function(x)
{
    as.integer(length(x[["rows"]]))
}

#' Extract part of a pipeflow pipeline
#' @param i integer (row indices) or character vector (step names) of steps to
#' select
#' @param ... further arguments passed to \code{data.table::`[.data.table`}
#' @rdname S3generics
#' @export
`[.pipeflow_pip` <- function(x, i, ...)
{
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
        if (anyNA(i)) {
            stop("step names in 'i' must not contain NA")
        }
        if (!all(nzchar(i))) {
            stop("step names in 'i' must be non-empty strings")
        }
        stepRows <- match(i, dat[["step"]])
        if (anyNA(stepRows)) {
            unknown <- unique(i[is.na(stepRows)])
            stop("Unknown step names in 'i': ", toString(unknown))
        }
        rows <- sort(unique(as.integer(stepRows)))
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

    # Re-point explicit self references to the extracted pipeline copy
    for (k in seq_len(nrow(subsetDat))) {
        pars <- subsetDat[["params"]][[k]]
        if (".self" %in% names(pars) && identical(pars[[".self"]], x)) {
            pars[[".self"]] <- out
            subsetDat[["params"]][[k]] <- pars
        }
    }

    data.table::setindexv(subsetDat, list("step", ".nodeId"))
    out[["pipeline"]] <- subsetDat
    out[[".dag"]] <- d
    out[[".steps_to_nodes"]] <- stepsToNodes

    out
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
        isHidden <- function(name) startsWith(name, ".")
        cols <- Filter(Negate(isHidden), colnames(dat))
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
                x[["name"]], nr, n, ifelse(n == 1, "", "s")
            )
        )
    }
    print(pip, rows = rows, header = FALSE, row.names = FALSE, ...)
    invisible(x)
}
