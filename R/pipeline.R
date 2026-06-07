# -------
# Helpers
# -------
.assert_exec_mode <- function(exec) {
  if (!.is_single(exec, "character") || is.na(exec)) {
    stop("exec must be a single string")
  }
  allowed <- c("auto", "split", "reduce", "plain")
  if (!(exec %in% allowed)) {
    stop("exec must be one of: ", toString(allowed))
  }
}

.assert_logger <- function(lgr) {
  if (!is.function(lgr)) {
    stop("lgr must be a function")
  }
  if (!all(c("level", "msg") %in% names(formals(lgr)))) {
    stop("lgr must be a function with arguments 'level' and 'msg'")
  }
}

.assert_pip_or_view <- function(x) {
  if (!(.is_pipeflow_pip(x) || .is_pipeflow_view(x))) {
    stop_no_call("x must be a pipeflow pip or view")
  }
}

.as_pipeflow_partitioned <- function(x) {
  if (!is.list(x)) {
    stop("split mode requires step output to be a list")
  }

  nm <- names(x)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(nm))) {
    stop("split output must be a named list with non-empty keys")
  }

  if (anyDuplicated(nm) > 0L) {
    stop("split output keys must be unique")
  }

  class(x) <- c(class(x), "pipeflow_partitioned")
  x
}

.is_pipeflow_view <- function(x) {
  inherits(x, "pipeflow_view")
}

.is_pipeflow_partitioned <- function(x) {
  inherits(x, "pipeflow_partitioned")
}

.is_pipeflow_pip <- function(x) {
  inherits(x, "pipeflow_pip")
}

.partition_keys <- function(x) {
  if (!.is_pipeflow_partitioned(x)) {
    stop("x must be a pipeflow_partitioned object")
  }
  names(x)
}

.pip_execute_step_call <- function(fun, args, exec) {
  partIdx <- which(vapply(args,
    FUN = .is_pipeflow_partitioned,
    FUN.VALUE = logical(1)
  ))

  if (identical(exec, "split")) {
    out <- do.call(fun, args = args)
    return(.as_pipeflow_partitioned(out))
  }

  if (identical(exec, "plain") && length(partIdx) > 0L) {
    stop("plain mode does not accept partitioned inputs")
  }

  if (identical(exec, "reduce") && length(partIdx) == 0L) {
    stop("reduce mode requires at least one partitioned input")
  }

  if (length(partIdx) == 0L || identical(exec, "plain") ||
    identical(exec, "reduce")) {
    return(do.call(fun, args = args))
  }

  # Auto-map over partition keys.
  keys <- .partition_keys(args[[partIdx[[1]]]])
  for (k in partIdx[-1]) {
    kk <- .partition_keys(args[[k]])
    if (!identical(kk, keys)) {
      stop("partitioned arguments must share identical keys")
    }
  }

  out <- stats::setNames(vector(mode = "list", length = length(keys)), keys)
  for (key in keys) {
    keyArgs <- args
    for (idx in partIdx) {
      keyArgs[[idx]] <- args[[idx]][[key]]
    }

    out[[key]] <- tryCatch(
      expr = do.call(fun, args = keyArgs),
      error = function(e) {
        stop_no_call("key '", key, "': ", e$message)
      }
    )
  }

  .as_pipeflow_partitioned(out)
}

.pip_append <- function(x, step, fun, group, tags, exec = "auto") {
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
    exec = exec,
    .nodeId = .nodeId
  )

  x[["pipeline"]] <- data.table::rbindlist(list(x[["pipeline"]], newStep))
  x[[".steps_to_nodes"]][[step]] <- .nodeId
  x
}

.pip_data <- function(x) {
  isView <- inherits(x, "pipeflow_view")
  if (isView) {
    rows <- x[["rows"]]
    x[["pip"]][["pipeline"]][rows, ]
  } else {
    x[["pipeline"]]
  }
}

.pip_filter_nodes <- function(x, nodes) {
  x[["pipeline"]][list(nodes), on = ".nodeId"]
}

.pip_filter <- function(x, on, values) {
  x[["pipeline"]][list(values), on = on]
}

.pip_get_downstream_nodes <- function(x, steps) {
  known <- intersect(steps, names(x[[".steps_to_nodes"]]))
  if (length(known) == 0L) {
    return(integer(0))
  }

  start_ids <- mget(known,
    envir = x[[".steps_to_nodes"]],
    ifnotfound = NA_integer_,
    inherits = FALSE
  )
  start_ids <- start_ids[!is.na(start_ids)]
  if (length(start_ids) == 0L) {
    return(integer(0))
  }

  dag_get_reachable_nodes_down(x[[".dag"]], as.integer(start_ids))
}

.pip_is_indexed <- function(x) {
  !is.null(data.table::indices(x[["pipeline"]]))
}

.pip_reindex <- function(x) {
  if (!.is_pipeflow_pip(x)) {
    stop("x must be a pipeflow pip")
  }
  data.table::setindexv(x[["pipeline"]], list("step", ".nodeId"))
}

.pip_run_row <- function(x, i, lgr) {
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
  exec <- dat[["exec"]][[i]]

  # If calculation depends on results of earlier steps, get them from
  # respective referenced output slots of the pipeline.
  if (length(depends) > 0) {
    refsOut <- .pip_filter(x, on = "step", values = depends)[["out"]]
    args[names(depends)] <- refsOut
  }

  step <- dat[["step"]][[i]]

  out <- withCallingHandlers(
    .pip_execute_step_call(fun = fun, args = args, exec = exec),
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

  # Re-read after execution so runtime structural changes are reflected. We
  # update by step name, so if the current step removed itself we simply
  # skip the update instead of writing into a shifted row.
  dat <- x[["pipeline"]]
  rowNow <- match(step, dat[["step"]])
  if (!is.na(rowNow)) {
    data.table::set(dat,
      i = rowNow, j = c("out", "time", "state"),
      value = list(list(out), Sys.time(), .step_states[["done"]][["name"]])
    )
  }

  out
}

.pip_step_exists <- function(x, step) {
  exists(step, where = x[[".steps_to_nodes"]], inherits = FALSE)
}

.pip_steps_to_nodes <- function(x, steps) {
  mget(steps,
    envir = x[[".steps_to_nodes"]],
    ifnotfound = NA_integer_,
    inherits = FALSE
  )
}

.pip_get_recursive_depth <- function(x) {
  if (!.is_pipeflow_pip(x)) {
    stop("x must be a pipeflow pip")
  }

  depth <- x[[".recursive_depth"]]
  if (is.null(depth)) {
    return(0L)
  }

  as.integer(depth)
}

.pip_steps_to_rows <- function(x, steps) {
  pip <- if (.is_pipeflow_view(x)) x[["pip"]] else x
  dat <- pip[["pipeline"]]

  if (anyNA(steps)) {
    stop("step names must not contain NA", call. = FALSE)
  }
  if (!all(nzchar(steps))) {
    stop("step names must be non-empty strings", call. = FALSE)
  }

  i <- match(steps, dat[["step"]])
  if (anyNA(i)) {
    unknown <- unique(steps[is.na(i)])
    stop("Unknown step names: ", toString(unknown), call. = FALSE)
  }
  as.integer(i)
}

.pip_update_downstream <- function(x, steps, what, value) {
  nodes <- .pip_get_downstream_nodes(x, steps)
  x[["pipeline"]][list(nodes), (what) := value, on = ".nodeId"]

  invisible(x)
}


# ---------------------------
# Exported pipeline functions
# ---------------------------

#' Create a pipeline
#'
#' Creates a new, empty pipeline. Add steps with [pip_add()] and execute
#' them with [pip_run()].
#'
#' @param name Single name used for printing and for derived view names.
#'
#' @return A pipeflow pipeline object.
#' @examples
#' # Create a named pipeline
#' p <- pip_new("my_analysis")
#' p[["name"]]  # "my_analysis"
#'
#' # Build a simple two-step pipeline and run it
#' pip_add(p, "load", \(n = 5) seq_len(n))
#' pip_add(p, "double", \(x = ~load) x * 2)  # x depends on load's output
#' p
#' pip_run(p)
#' p[["out"]]  # list of outputs, one per step
#' @export
pip_new <- function(name = "pipe") {
  if (!.is_single(name, "character")) {
    stop("name must be a single string")
  }
  if (is.na(name)) {
    stop("name must not be NA")
  }

  hash_map <- function() new.env(parent = emptyenv())
  env <- hash_map()
  env[["name"]] <- name
  env[["pipeline"]] <- .empty_pipeline()
  env[[".dag"]] <- dag_new()
  env[[".steps_to_nodes"]] <- hash_map()
  env[[".recursive_depth"]] <- 0L

  structure(env, class = c("pipeflow_pip", "environment"))
}


#' Add a step
#'
#' Adds a named step to the pipeline. Each step is a function whose parameters
#' either hold constant defaults or reference the output of a prior step using
#' formula notation (`~step_name`). Dependencies are validated when the step
#' is added.
#'
#' @param x A pipeflow pipeline object.
#' @param step Unique step name.
#' @param fun Function to execute for the step. Each function parameter must
#' have a default value. Default values that are simple constants are resolved
#' immediately. Default values that are formulas like `~other_step` are
#' treated as dependencies to those steps and resolved to the respective output
#' values at runtime once the step is executed.
#' @param group Optional character label used for grouping output collections -
#' see also `[pip_collect_out()]`.
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be adjusted later using `[pip_tag()]`.
#' @param after Optional position after which the new step should be inserted
#' (defaults to last position). Can be a step name or an integer index. If
#' set to 0, the new step will be inserted at the beginning of the pipeline.
#' @param exec Execution mode for this step. One of "auto", "split",
#' "reduce" or "plain".
#' Using execution mode `exec = split`, the output of the step is marked as
#' partitioned output. In this mode, any step that depends on the split step
#' (directly or indirectly) will have its output automatically mapped
#' partition-wise during step execution. The `reduce` mode expects
#' partitioned input and passes it through without mapping, while `plain`
#' mode only accepts non-partitioned input and always intends to execute
#' a single call. In summary:
#' * auto: map if partitioned input appears, otherwise single call
#' * split: single call, then mark output as partitioned
#' * reduce: single call, but only valid with partitioned input
#' * plain: single call, only valid with non-partitioned input
#'
#' @details
#' If `after` was specified, the new step will be inserted after the given
#' step or position. Be aware that in contrast to adding a step at the end,
#' inserting a step in the middle is a rather expensive operation as it
#' requires re-wiring parts of the internal pipeline structure, especially
#' if the new step is inserted at an early position.
#'
#' @return The updated pipeline, invisibly.
#' @examples
# p <- pip_new("demo")

# # Constant defaults become independent parameters (adjustable later)
# pip_add(p, "load", \(n = 5) seq_len(n), group = "io", tags = "raw")

# # Use ~step_name to declare a dependency on another step's output;
# # "square" will automatically receive the output of "load" at runtime
# pip_add(p, "square", \(x = ~load) x^2,
#     group = "trans", tags = c("core", "daily")
# )

# # Insert between two existing steps by giving a step name to `after`
# pip_add(p, "scale", \(x = ~load, factor = 2) x * factor, after = "load")

# # after = 0 inserts a step at the very beginning of the pipeline
# pip_add(p, "init", \(x = NULL) x, after = 0L)

# p
# pip_run(p)
# pip_collect_out(p)
#' @export
pip_add <- function(
  x, step, fun,
  group = step,
  tags = character(0),
  after = length(x),
  exec = "auto"
) {
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
  .assert_exec_mode(exec)

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
    .pip_append(x,
      step = step, fun = fun, group = group, tags = tags, exec = exec
    )
    return(invisible(x))
  }

  src <- pip_clone(x)
  dat <- src[["pipeline"]]
  n <- nrow(dat)

  out <- if (pos > 0L) src[seq_len(pos)] else pip_new(name = src[["name"]])
  pip_add(out,
    step = step, fun = fun, group = group, tags = tags, exec = exec
  )

  if (pos < n) {
    tailRows <- seq.int(pos + 1L, n)
    for (i in tailRows) {
      tailStep <- dat[["step"]][[i]]
      pip_add_from(out, y = src, step = tailStep)

      iOut <- nrow(out[["pipeline"]])
      data.table::set(
        out[["pipeline"]],
        i = iOut,
        j = c("out", "time", "state", "locked"),
        value = list(
          list(dat[["out"]][[i]]),
          dat[["time"]][[i]],
          dat[["state"]][[i]],
          dat[["locked"]][[i]]
        )
      )
    }
  }

  x[["pipeline"]] <- out[["pipeline"]]
  x[[".dag"]] <- out[[".dag"]]
  x[[".steps_to_nodes"]] <- out[[".steps_to_nodes"]]
  invisible(x)
}


#' Copy a step from another pipeline
#'
#' Copies one step from pipeline `y` into pipeline `x`, preserving its
#' function, parameters, group, tags, and dependency links.
#'
#' @param x Target pipeflow pipeline object.
#' @param y Source pipeflow pipeline object.
#' @param step Step name to copy from `y`.
#'
#' @return The updated target pipeline, invisibly.
#' @examples
#' # Build a source pipeline with reusable steps
#' src <- pip_new("source") |>
#'   pip_add("load", \(n = 3) seq_len(n)) |>
#'   pip_add("square", \(x = ~load) x^2)
#'
#' # Copy steps into a new pipeline one at a time.
#' # The dependency of "square" on "load" is re-established automatically.
#' dst <- pip_new("target")
#' pip_add_from(dst, src, "load")
#' pip_add_from(dst, src, "square")
#' pip_run(dst)
#' pip_collect_out(dst, grouped = FALSE)
#' @export
pip_add_from <- function(x, y, step) {
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
  exec <- y[["pipeline"]][["exec"]][[iStep]]
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
  pip_add(
    x,
    step = step,
    fun = f,
    group = group,
    tags = tags,
    exec = exec
  )
}

#' Bind pipelines
#'
#' Bind two pipelines together by concatenating their steps. If both pipelines
#' have steps with the same name, the step names of the second pipeline will be
#' automatically adapted to avoid name clashes.
#' @param x A pipeflow pipeline object.
#' @param y A pipeflow pipeline object.
#' @return A new pipeflow pipeline object representing the bound pipelines.
#' @examples
#' a <- pip_new("a") |>
#'   pip_add("prep", \(x = 1) x * 2) |>
#'   pip_add("fit", \(x = ~prep) x + 10)
#'
#' # "prep" exists in both pipelines; the one from b gets a numeric suffix
#' b <- pip_new("b") |>
#'   pip_add("prep", \(x = 5) x * 3)
#'
#' z <- pip_bind(a, b)
#' z[["step"]]  # "prep", "fit", "prep2" (conflict resolved)
#' length(z)  # 3 steps total
#' @export
pip_bind <- function(x, y) {
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
    pip_add_from(out, y = yy, step = step)

    # Preserve runtime state from source pipeline.
    iOut <- nrow(out[["pipeline"]])
    data.table::set(
      out[["pipeline"]],
      i = iOut,
      j = c("out", "time", "state", "locked"),
      value = list(
        list(yyDat[["out"]][[k]]),
        yyDat[["time"]][[k]],
        yyDat[["state"]][[k]],
        yyDat[["locked"]][[k]]
      )
    )
  }

  out
}


#' Clone a pipeline
#'
#' Creates an independent copy of the pipeline. Changes to the clone do not
#' affect the original, and vice versa.
#'
#' @param x A pipeflow pipeline object.
#' @param name Optional name for the cloned pipeline. If `NULL`, the original
#' name is used.
#'
#' @return A cloned pipeflow pipeline object.
#' @examples
#' p <- pip_new("original") |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1)
#'
#' # Clone produces a fully independent copy
#' cp <- pip_clone(p, name = "copy")
#' pip_add(cp, "s3", \(x = ~s2) x * 10)
#'
#' length(cp)  # 3 — clone has the new step
#' length(p)   # 2 — original is unchanged
#' cp[["pipeline"]][["step"]]
#' @export
pip_clone <- function(x, name = NULL) {
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


#' Collect step outputs
#'
#' Returns the outputs of all pipeline steps as a named list, optionally
#' grouped by step group label.
#' @param x A pipeflow pip or view
#' @param grouped Logical indicating if the output should be grouped by step
#' groups
#' @return A named list of outputs. If `grouped = TRUE`, groups with more than
#' one step are returned as nested named lists.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load",  \(x = 1) x,       group = "io") |>
#'   pip_add("clean", \(x = ~load) x + 1, group = "io") |>
#'   pip_add("model", \(x = ~clean) x * 2, group = "model")
#' pip_run(p)
#'
#' # grouped = TRUE (default): steps sharing a group become a nested list
#' out <- pip_collect_out(p)
#' out$io     # list(load = 1, clean = 2)  — two steps, so nested
#' out$model  # 4  — single step, returned directly (not wrapped)
#'
#' # grouped = FALSE: flat named list with one entry per step
#' pip_collect_out(p, grouped = FALSE)
#' @export
pip_collect_out <- function(x, grouped = TRUE) {
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

#' Get independent parameters
#'
#' Returns the current default values of all tunable (non-dependency)
#' parameters across the pipeline. These are the parameters that can be
#' updated via [pip_set_params()]. Parameters wired to another step's output
#' via `~step_name` are excluded.
#' @param x A pipeflow pip or view
#' @return Named list of tunable parameter values. If the same parameter
#' name appears in multiple steps, the first occurrence in pipeline order
#' is returned.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load",  \(n = 100, seed = 42) seq_len(n)) |>
#'   pip_add("model", \(x = ~load, lambda = 0.1) x * lambda)
#'
#' # ~load is a dependency — only non-dependency params are returned
#' pip_get_params(p)  # list(n = 100, seed = 42, lambda = 0.1)
#'
#' # Useful as a guide for pip_set_params()
#' pip_set_params(p, params = list(n = 20, lambda = 0.5))
#' pip_run(p)
#' pip_collect_out(p, grouped = FALSE)
#' @export
pip_get_params <- function(x) {
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


#' Build pipeline graph data
#'
#' Builds graph data (nodes and edges) describing the pipeline's step
#' structure, suitable for visualisation with [visNetwork::visNetwork()].
#'
#' @details
#' Node shapes reflect execution mode: hexagon for `auto`/`plain`, star for
#' `split`, dot for `reduce`.
#'
#' @param x A pipeflow pip or view.
#' @param include_upstream Logical. Only relevant for views. If `TRUE`, add
#' all upstream dependencies of selected steps.
#'
#' @return A named list with two `data.frame`s: `nodes` and `edges`.
#' @export
#' @examples
#' p <- pip_new()
#' pip_add(p, "load",  \(x = 1) x,       group = "io")
#' pip_add(p, "clean", \(x = ~load) x + 1, group = "io")
#' pip_add(p, "fit",   \(x = ~clean) x * 2, group = "model")
#'
#' graph <- pip_get_graph(p)
#' graph$nodes  # data.frame: id, label, group, shape, color
#' graph$edges  # data.frame: from, to, arrows
#'
#' # For a view, include_upstream = TRUE adds upstream deps to the graph
#' v <- pip_view(p, i = "fit")
#' pip_get_graph(v, include_upstream = TRUE)
#'
#' if (require("visNetwork", quietly = TRUE)) {
#'   do.call(what = visNetwork::visNetwork, args = graph)
#' }
pip_get_graph <- function(x, include_upstream = FALSE) {
  .assert_pip_or_view(x)
  if (!.is_single(include_upstream, "logical")) {
    stop("include_upstream must be a single logical value")
  }

  isView <- .is_pipeflow_view(x)
  pip <- if (isView) x[["pip"]] else x
  dat <- pip[["pipeline"]]
  dag <- pip[[".dag"]]

  rows <- if (isView) as.integer(x[["rows"]]) else seq_len(nrow(dat))
  rows <- sort(unique(rows))

  if (isView && include_upstream && length(rows) > 0L) {
    startNodes <- dat[[".nodeId"]][rows]
    keepNodes <- dag_get_reachable_nodes_up(
      dag,
      as.integer(unique(startNodes))
    )
    rows <- which(dat[[".nodeId"]] %in% keepNodes)
    rows <- sort(unique(as.integer(rows)))
  }

  sub <- dat[rows]

  # Nodes
  colors <- vapply(sub[["state"]],
    FUN = \(st) .step_states[[st]][["color"]],
    FUN.VALUE = character(1)
  )

  ids <- as.integer(sub[[".nodeId"]])
  shape <- rep("hexagon", nrow(sub))
  if ("exec" %in% names(sub)) {
    shape[sub[["exec"]] %in% "split"] <- "star"
    shape[sub[["exec"]] %in% "reduce"] <- "dot"
  }
  nodes <- data.frame(
    id = ids,
    label = sub[["step"]],
    group = sub[["group"]],
    shape = shape,
    color = colors
  )

  # Edges from direct dependencies only (no transitive links).
  stepToId <- stats::setNames(ids, sub[["step"]])
  edgeRows <- lapply(seq_len(nrow(sub)), FUN = \(k) {
    depSteps <- unname(sub[["depends"]][[k]])
    if (length(depSteps) == 0L) {
      return(NULL)
    }

    fromIds <- as.integer(stepToId[depSteps])
    fromIds <- fromIds[!is.na(fromIds)]
    if (length(fromIds) == 0L) {
      return(NULL)
    }

    data.frame(
      from = fromIds,
      to = rep.int(ids[[k]], length(fromIds)),
      stringsAsFactors = FALSE
    )
  })
  edgeRows <- Filter(f = Negate(is.null), x = edgeRows)

  edges <- if (length(edgeRows) == 0L) {
    data.frame(
      from = integer(0),
      to = integer(0),
      arrows = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    edges <- do.call(what = rbind, args = edgeRows)
    edges <- unique(edges)
    cbind(edges, "arrows" = "to")
  }

  list(nodes = nodes, edges = edges)
}


#' Check whether a step exists
#'
#' @param x A pipeflow pip
#' @param step A step name
#' @return Logical indicating if the step exists
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit",  \(x = ~load) x + 1)
#'
#' pip_has_step(p, "load")     # TRUE
#' pip_has_step(p, "fit")      # TRUE
#' pip_has_step(p, "predict")  # FALSE — step not yet added
#' @export
pip_has_step <- function(x, step) {
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


#' Remove a step
#'
#' If other steps depend on the step to be removed, an error is
#' given and the removal is blocked, unless `recursive` was set to
#' `TRUE`.
#' In recursive mode, the selected step and all downstream dependent steps are
#' removed together.
#' @param x A pipeflow pip
#' @param step `string` the name of the step to be removed.
#' @param recursive `logical` if `TRUE` the step is removed together
#' with all its downstream dependencies.
#' @return The updated pipeline, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load",      \(x = 1) x) |>
#'   pip_add("transform", \(x = ~load) x * 2) |>
#'   pip_add("model",     \(x = ~transform) x + 10)
#'
#' # Removing a leaf step (nothing depends on it) works directly
#' pip_remove(p, "model")
#' p[["pipeline"]][["step"]]  # "load", "transform"
#'
#' # Trying to remove a step that others depend on raises an error:
#' # pip_remove(p, "load")  # Error!
#'
#' # recursive = TRUE removes the step and all its downstream dependents
#' pip_remove(p, "load", recursive = TRUE)
#' p[["pipeline"]][["step"]]  # character(0) — pipeline is now empty
#' @export
pip_remove <- function(x, step, recursive = FALSE) {
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


#' Rename a step
#'
#' Renames the selected step and updates dependency references in downstream
#' steps.
#' @param x A pipeflow pip
#' @param from Existing step name
#' @param to New step name
#' @return The updated pipeline, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1)  # "s2" depends on "s1"
#'
#' # Downstream dependency references are updated automatically
#' pip_rename(p, from = "s1", to = "load_data")
#' p[["pipeline"]][["step"]]           # "load_data", "s2"
#' p[["pipeline"]][["depends"]][[2]]   # "load_data" (was "s1")
#' @export
pip_rename <- function(x, from, to) {
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


#' Replace a step
#'
#' Replaces a step's function while keeping it in the same position in the
#' pipeline. Downstream steps are automatically marked as outdated and will
#' re-run on the next [pip_run()].
#'
#' @param x A pipeflow pipeline object.
#' @param step Step name.
#' @param fun Function to execute for the step.
#' @param group Step group name.
#' @param tags Optional character vector of tags belonging to the step.
#' Can also be adjusted later using `[pip_tag()]`.
#' @return The updated pipeline, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load",   \(n = 5) seq_len(n)) |>
#'   pip_add("double", \(x = ~load) x * 2)
#' pip_run(p)
#'
#' # Replace "load" — downstream steps are automatically marked "outdated"
#' pip_replace(p, "load", \(n = 3) seq_len(n))
#' p[["pipeline"]][["state"]]  # "new", "outdated"
#'
#' # Re-run to bring everything up to date
#' pip_run(p)
#' pip_collect_out(p, grouped = FALSE)
#' @export
pip_replace <- function(x, step, fun, group = step, tags = character(0)) {
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
  dat <- src[["pipeline"]]
  n <- nrow(dat)
  iStep <- match(step, dat[["step"]])

  out <- if (iStep > 1L) {
    src[seq_len(iStep - 1L)]
  } else {
    pip_new(name = src[["name"]])
  }

  # Add replacement step at the original position.
  pip_add(out, step = step, fun = fun, group = group, tags = tags)

  # Re-append subsequent steps and preserve their runtime state.
  if (iStep < n) {
    tailRows <- seq.int(iStep + 1L, n)
    for (i in tailRows) {
      tailStep <- dat[["step"]][[i]]
      pip_add_from(out, y = src, step = tailStep)

      iOut <- nrow(out[["pipeline"]])
      data.table::set(
        out[["pipeline"]],
        i = iOut,
        j = c("out", "time", "state", "locked"),
        value = list(
          list(dat[["out"]][[i]]),
          dat[["time"]][[i]],
          dat[["state"]][[i]],
          dat[["locked"]][[i]]
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
    rowsDown <- out[["pipeline"]][list(downNodes), which = TRUE, on = ".nodeId"]
    if (length(rowsDown) > 0L) {
      data.table::set(
        out[["pipeline"]],
        i = rowsDown,
        j = "state",
        value = .step_states[["outdated"]][["name"]]
      )
    }
  }

  # Rebind any explicit .self references from the temporary clone back to
  # the actual pipeline object that is being mutated at runtime.
  datOut <- out[["pipeline"]]
  for (k in seq_len(nrow(datOut))) {
    pars <- datOut[["params"]][[k]]
    selfRef <- pars[[".self"]]
    if (".self" %in% names(pars) &&
      inherits(selfRef, "pipeflow_pip") &&
      !identical(selfRef, x)
    ) {
      pars[[".self"]] <- x
      data.table::set(datOut,
        i = k, j = "params", value = list(list(pars))
      )
    }
  }

  x[["pipeline"]] <- datOut
  x[[".dag"]] <- out[[".dag"]]
  x[[".steps_to_nodes"]] <- out[[".steps_to_nodes"]]
  invisible(x)
}


#' Run a pipeline
#'
#' Executes all pending steps in order. Steps already in state `"done"` are
#' skipped unless `force = TRUE`.
#'
#' @param x A pipeflow pip or view
#' @param lgr A logging function of the form `function(level, msg, ...)`.
#' To suppress logging, you can set `lgr = NULL`.
#' @param force Logical indicating if all steps should be forced to run,
#' regardless of whether they are outdated or not.
#' @param progress Optional callback of the form
#' `function(value, detail)` called before each step.
#' @param recursive If `TRUE` and a step returns a pipeline object, the
#' current run is aborted and continues from the returned pipeline. Useful
#' for dynamic or self-modifying pipelines.
#' @return The updated pipeline or view, invisibly.
#' @details When `x` is a view, requested rows are run together with required
#' upstream dependencies.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load",  \(n = 3) seq_len(n)) |>
#'   pip_add("square", \(x = ~load) x^2) |>
#'   pip_add("total",  \(x = ~square) sum(x))
#'
#' pip_run(p)
#' pip_collect_out(p, grouped = FALSE)
#'
#' # Already-done steps are skipped on a second run
#' pip_run(p)          # all steps skipped
#'
#' # lgr = NULL suppresses log output
#' pip_run(p, lgr = NULL)
#'
#' # force = TRUE re-executes every step regardless of state
#' pip_run(p, lgr = NULL, force = TRUE)
#'
#' # Run only a subset of steps via a view;
#' # upstream dependencies are automatically included
#' v <- pip_view(p, i = "total")
#' pip_run(v, lgr = NULL)
#' @export
pip_run <- function(
  x,
  lgr = pipeflow_lgr,
  force = FALSE,
  progress = NULL,
  recursive = FALSE
) {
  .assert_pip_or_view(x)
  if (!.is_single(force, "logical")) {
    stop("force must be a single logical value")
  }
  if (!.is_single(recursive, "logical")) {
    stop("recursive must be a single logical value")
  }
  if (!is.null(progress) && !is.function(progress)) {
    stop("progress must be a function")
  }
  if (is.null(lgr)) {
    lgr <- function(level, msg, ...) {}
  } else {
    .assert_logger(lgr)
  }
  log_info <- function(msg) lgr(level = "info", msg = msg)

  isView <- .is_pipeflow_view(x)
  pip <- if (isView) x[["pip"]] else x
  dat <- pip[["pipeline"]]
  rowsToRun <- if (isView) x[["rows"]] else seq_len(nrow(dat))
  rowsRequested <- unique(as.integer(rowsToRun))
  upstreamRows <- integer(0)
  if (isView) {
    # Add rows of upstream dependencies not yet covered by the view.
    deps <- unique(unlist(dat[["depends"]][rowsToRun]))
    if (length(deps) > 0L) {
      depRows <- which(dat[["step"]] %in% deps)
      rowsToRun <- sort(unique(c(rowsToRun, depRows)))
    }
    rowsToRun <- as.integer(rowsToRun)
    upstreamRows <- setdiff(rowsToRun, rowsRequested)
  }
  processedSteps <- character()
  on.exit({
    # At the end, mark all downstream dependent steps as outdated that
    # were *not* processed, which can happen in two different ways:
    # a) when running a view that does not cover the entire pipeline or
    # b) the run aborted in the middle due to an error
    processedNodes <- as.integer(.pip_steps_to_nodes(pip, processedSteps))
    outdatedNodes <- .pip_get_downstream_nodes(pip, processedSteps) |>
      unlist() |>
      unique() |>
      setdiff(processedNodes) # nolint
    if (length(outdatedNodes) > 0L) {
      rowsOutdated <- dat[list(outdatedNodes), which = TRUE, on = ".nodeId"]
      if (length(rowsOutdated) > 0L) {
        data.table::set(dat, i = rowsOutdated, j = "state", value = "outdated")
      }
    }
  })

  log_info(sprintf("Start run of %s '%s'", data.class(x), x[["name"]]))
  for (i in seq_along(rowsToRun)) {
    row <- rowsToRun[[i]]
    step <- dat[["step"]][[row]]
    processedSteps <- c(processedSteps, step)
    if (!is.null(progress)) {
      progress(value = i, detail = step)
    }
    marker <- ""
    if (isView) {
      marker <- if (row %in% rowsRequested) "[view]" else "[upstream]"
    }
    msg <- if (isView) {
      sprintf("Step %i/%i %s %s", i, length(rowsToRun), marker, step)
    } else {
      sprintf("Step %i/%i %s", i, length(rowsToRun), step)
    }

    if (identical(dat[["state"]][[row]], "done") && !force) {
      log_info(sprintf("%s - skipping done step", msg))
      next()
    }
    if (dat[["locked"]][[row]]) {
      log_info(sprintf("%s - skipping locked step", msg))
      next()
    }

    log_info(msg)
    res <- .pip_run_row(pip, i = row, lgr = lgr)

    if (.is_pipeflow_pip(res)) {
      if (recursive) {
        current_depth <- as.integer(x[[".recursive_depth"]])
        max_depth <- getOption("pipeflow_max_recursive_depth", 10L)
        if (is.na(max_depth) || max_depth < 0L) {
          max_depth <- 10L
        }

        if (current_depth >= max_depth) {
          sprintf(
            paste(
              "Maximum recursive pipeline restarts exceeded (%i).",
              "Set options(pipeflow_max_recursive_depth = <n>) to",
              "increase the limit."
            ),
            max_depth
          ) |> stop(call. = FALSE)
        }

        res[[".recursive_depth"]] <- current_depth + 1L

        log_info(
          "Abort pipeline execution and restart on returned pipeline."
        )
        pip_run(
          x = res,
          lgr = lgr,
          force = TRUE,
          progress = progress,
          recursive = TRUE
        )
        return(invisible(res))
      }

      log_info(
        "Abort pipeline execution on returned pipeline."
      )
      return(invisible(res))
    }
  }

  log_info(sprintf("Finished run of %s '%s'", data.class(x), x[["name"]]))
  invisible(x)
}


#' Set independent parameters
#'
#' Updates the default values of tunable parameters across the pipeline.
#' Affected steps and their downstream dependents are automatically marked
#' as outdated.
#' @details Parameters of locked steps are never changed and their state
#' remains unchanged.
#' @param p A pipeflow pip or view
#' @param params Named list of parameters to set.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load",  \(n = 10) seq_len(n)) |>
#'   pip_add("scale", \(x = ~load, factor = 0.5) x * factor)
#'
#' # See all adjustable parameters before running
#' pip_get_params(p)  # list(n = 10, factor = 0.5)
#'
#' # Updating params marks affected steps (and their dependents) outdated
#' pip_set_params(p, params = list(n = 5, factor = 2.0))
#' p[["pipeline"]][["state"]]  # "outdated", "outdated"
#'
#' pip_run(p, lgr = NULL)
#' pip_collect_out(p, grouped = FALSE)
#' @export
pip_set_params <- function(p, params = list()) {
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
      x,
      steps = steps, what = "state", value = "outdated"
    )
  }

  invisible(p)
}


#' Add tags to selected steps
#'
#' Adds tags to existing tags for all steps in the pipeline unless `p` is a
#' view, in which case tags are only added for steps covered by the view.
#' Locked steps are skipped and not updated.
#' @param p A pipeflow pip or view.
#' @param tags Character vector of tags to add for each selected step.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit",  \(x = ~load) x + 1)
#'
#' # Tag every step in the pipeline at once
#' pip_tag(p, tags = c("daily", "core"))
#' p[["pipeline"]][["tags"]]  # both steps have c("daily", "core")
#'
#' # Add an extra tag to only one step via a view
#' v <- pip_view(p, i = "fit")
#' pip_tag(v, tags = "model")
#' p[["pipeline"]][["tags"]]  # "fit" also has "model"
#' @export
pip_tag <- function(p, tags = character()) {
  .assert_pip_or_view(p)
  if (!is.character(tags)) {
    stop("tags must be a character vector")
  }

  isView <- .is_pipeflow_view(p)
  x <- if (isView) p[["pip"]] else p
  dat <- x[["pipeline"]]
  rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

  if (length(rows) == 0L || length(tags) == 0L) {
    return(invisible(p))
  }

  for (i in rows) {
    if (isTRUE(dat[["locked"]][[i]])) {
      next
    }

    oldTags <- dat[["tags"]][[i]]
    newTags <- unique(c(oldTags, tags))
    data.table::set(dat, i = i, j = "tags", value = list(list(newTags)))
  }

  invisible(p)
}


#' Remove tags from selected steps
#'
#' Removes tags from existing tags for all steps in the pipeline unless `p`
#' is a view, in which case tags are only removed for steps covered by the
#' view. Locked steps are skipped and not updated.
#' @param p A pipeflow pip or view.
#' @param tags Character vector of tags to remove for each selected step.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x, tags = c("daily", "core")) |>
#'   pip_add("fit",  \(x = ~load) x + 1, tags = c("daily", "model"))
#'
#' # Remove "daily" from all steps
#' pip_untag(p, tags = "daily")
#' # "load" retains "core"; "fit" retains "model"
#' p[["pipeline"]][["tags"]]
#' @export
pip_untag <- function(p, tags = character()) {
  .assert_pip_or_view(p)
  if (!is.character(tags)) {
    stop("tags must be a character vector")
  }

  isView <- .is_pipeflow_view(p)
  x <- if (isView) p[["pip"]] else p
  dat <- x[["pipeline"]]
  rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

  if (length(rows) == 0L || length(tags) == 0L) {
    return(invisible(p))
  }

  for (i in rows) {
    if (isTRUE(dat[["locked"]][[i]])) {
      next
    }

    oldTags <- dat[["tags"]][[i]]
    newTags <- setdiff(oldTags, tags)
    data.table::set(dat, i = i, j = "tags", value = list(list(newTags)))
  }

  invisible(p)
}


#' Lock selected steps against updates
#'
#' Locks all selected steps in the pipeline unless `p` is a view, in which
#' case only steps covered by the view are locked.
#' @param p A pipeflow pip or view.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 10) x) |>
#'   pip_add("fit",  \(x = ~load) x * 2)
#' pip_run(p, lgr = NULL)
#'
#' # Lock only "load" via a view so it won't be re-executed or overwritten
#' pip_lock(pip_view(p, i = "load"))
#' p[["pipeline"]][["locked"]]  # TRUE, FALSE
#'
#' # Locked steps are silently skipped during pip_run()
#' pip_run(p, lgr = NULL, force = TRUE)
#' p[["pipeline"]][["out"]][[1]]  # still 10 — locked, not re-executed
#'
#' pip_unlock(p)
#' p[["pipeline"]][["locked"]]  # FALSE, FALSE
#' @export
pip_lock <- function(p) {
  .assert_pip_or_view(p)

  isView <- .is_pipeflow_view(p)
  x <- if (isView) p[["pip"]] else p
  dat <- x[["pipeline"]]
  rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

  if (length(rows) == 0L) {
    return(invisible(p))
  }

  data.table::set(dat, i = rows, j = "locked", value = TRUE)
  invisible(p)
}


#' Unlock selected steps
#'
#' Unlocks all selected steps in the pipeline unless `p` is a view, in which
#' case only steps covered by the view are unlocked.
#' @param p A pipeflow pip or view.
#' @return The updated pipeline or view, invisibly.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit",  \(x = ~load) x * 2)
#'
#' # Lock all steps, then unlock to restore normal execution
#' pip_lock(p)
#' p[["pipeline"]][["locked"]]  # TRUE, TRUE
#'
#' pip_unlock(p)
#' p[["pipeline"]][["locked"]]  # FALSE, FALSE
#' @export
pip_unlock <- function(p) {
  .assert_pip_or_view(p)

  isView <- .is_pipeflow_view(p)
  x <- if (isView) p[["pip"]] else p
  dat <- x[["pipeline"]]
  rows <- if (isView) p[["rows"]] else seq_len(nrow(dat))

  if (length(rows) == 0L) {
    return(invisible(p))
  }

  data.table::set(dat, i = rows, j = "locked", value = FALSE)
  invisible(p)
}


#' Create a pipeline view
#'
#' Creates a filtered view showing only a selected subset of steps.
#' A view references the underlying pipeline without copying it, so
#' operations like [pip_run()] and [pip_set_params()] applied to a view
#' affect only the selected steps.
#'
#' @param x A pipeflow pipeline or view.
#' @param i Optional row indices or step names to keep.
#' @param filter A named list of filters to apply. Each element can be a
#' character vector specifying the values to keep for the corresponding
#' property or, if `fixed` is FALSE, a regular expression. See examples
#' for usage.
#' @param tags Tag filter (character). Keeps steps with any matching tag.
#' @param fixed If TRUE, values in `filter` are treated as fixed strings,
#' otherwise they are treated as regular expressions.
#' @param ... further args passed to `grepl` (only in effect when `fixed`
#' is `FALSE`).
#'
#' @return A `pipeflow_view` object.
#' @export
#' @examples
#'
#' p <- pip_new()
#' pip_add(p, "load_raw",  \(x = 1) x,
#'   group = "io",    tags = c("core", "daily")
#' )
#' pip_add(p, "fit_model",  \(x = 2) x + 1,
#'   group = "model", tags = "model"
#' )
#' pip_add(p, "eval_model", \(x = ~fit_model) x,
#'   group = "model", tags = c("daily", "report")
#' )
#'
#' # Filter by a fixed column value (one or more groups)
#' pip_view(p, filter = list(group = "model"))
#'
#' # Combine filters: group AND state
#' pip_view(p, filter = list(group = "model", state = "new"))
#'
#' # Filter by tag — keeps steps that have *any* of the given tags
#' pip_view(p, tags = "daily")
#'
#' # Combine explicit step selection with a filter (intersection)
#' pip_view(p,
#'   i      = c("load_raw", "fit_model"),
#'   filter = list(group = "model")
#' )
#'
#' # Select by integer row indices
#' pip_view(p, i = c(1L, 2L), filter = list(group = "model"))
#'
#' # Use a regex pattern to match step names
#' pip_view(p, filter = list(step = "_model$"), fixed = FALSE)
#'
#' # Views are composable: create a view-of-view for progressive narrowing
#' v1 <- pip_view(p, tags = "daily")
#' print(v1)  # load_raw, eval_model
#' v2 <- pip_view(v1, tags = "report")
#' print(v2)  # eval_model only
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
  pip <- if (isView) x[["pip"]] else x
  dat <- pip[["pipeline"]]

  # For view-of-view, filter only within parent view rows and map local
  # matches back to absolute row indices of the underlying pipeline.
  parent_rows <- if (isView) as.integer(x[["rows"]]) else seq_len(nrow(dat))
  sub <- dat[parent_rows]
  keep <- rep(TRUE, nrow(sub))

  # Filters
  validFilters <- c("step", "depends", "group", "state", "exec")
  for (name in names(filter)) {
    if (!(name %in% validFilters)) {
      stop(sprintf(
        "Invalid filter name: '%s' - can be one of: %s",
        name, paste(validFilters, collapse = ", ")
      ))
    }
    hasMatch <- if (fixed) {
      sapply(sub[[name]], \(e) any(e %in% filter[[name]]))
    } else {
      sapply(sub[[name]], \(e) any(grepl(filter[[name]], x = e, ...)))
    }
    keep <- keep & hasMatch
  }

  # Tags
  if (length(tags) > 0) {
    hasTag <- vapply(sub[["tags"]],
      FUN = \(x) any(x %in% tags),
      FUN.VALUE = logical(1)
    )
    keep <- keep & hasTag
  }

  # Rows
  rows <- parent_rows[which(keep)]
  if (length(i) > 0) {
    if (is.character(i)) {
      i <- .pip_steps_to_rows(pip, i)
    }
    if (!is.numeric(i)) {
      stop("i must be numeric row indices or character step names")
    }
    if (any(i < 1L | i > nrow(dat))) {
      stop(
        "Invalid row indices in 'i': ",
        toString(i[i < 1L | i > nrow(dat)])
      )
    }
    rows <- intersect(rows, as.integer(i))
  }

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
#' @return Number of steps as an integer.
#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x) |>
#'   pip_add("s2", \(x = ~s1) x + 1) |>
#'   pip_add("s3", \(x = ~s2) x * 2)
#' length(p)    # 3 — total steps in the pipeline
#'
#' # A view reports only the number of selected (visible) steps
#' v <- pip_view(p, i = c("s2", "s3"))
#' length(v)    # 2
#' @rdname length.pipeflow
#' @export
length.pipeflow_pip <- function(x) {
  as.integer(nrow(x[["pipeline"]]))
}

#' @rdname length.pipeflow
#' @export
length.pipeflow_view <- function(x) {
  as.integer(length(x[["rows"]]))
}

#' Extract or subset a pipeline
#'
#' Returns a new pipeline containing selected steps and all required upstream
#' dependencies.
#' @param x A pipeflow pipeline object.
#' @param i integer (row indices) or character vector (step names) of steps to
#' select
#' @param ... not used
#' @return A new pipeflow pipeline object.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load",   \(n = 5) seq_len(n)) |>
#'   pip_add("square", \(x = ~load) x^2) |>
#'   pip_add("total",  \(x = ~square) sum(x))
#'
#' # Select by step name — upstream deps are pulled in automatically.
#' # Selecting only "total" still includes "load" and "square".
#' sub <- p["total"]
#' sub[["pipeline"]][["step"]]   # "load", "square", "total"
#'
#' # Select a subset of steps by name vector
#' p[c("load", "square")][["pipeline"]][["step"]]  # "load", "square"
#'
#' # Select by integer row index
#' p[1:2][["pipeline"]][["step"]]  # "load", "square"
#' @rdname Extract.pipeflow_pip
#' @export
`[.pipeflow_pip` <- function(x, i, ...) {
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
    rows <- sort(unique(.pip_steps_to_rows(x, i)))
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


#' Extract bindings, columns, or row-level values from a pipeline
#'
#' Extracts values from a pipeline using one or two indices.
#' With a single string name, named fields such as `"pipeline"` or `"name"`
#' are returned first; anything else returns the matching step-table column.
#' With two indices (`row`, `column`), a single cell is extracted.
#' @param i integer (row indices) or character vector (step names) of steps to
#' select
#' @param j column names to select
#' @return Extracted value(s), depending on `i` and `j`.
#' @examples
#' p <- pip_new() |>
#'   pip_add("load", \(x = 1) x) |>
#'   pip_add("fit",  \(x = ~load) x + 1)
#'
#' # Access internal objects by name
#' p[["pipeline"]]  # the full step table
#' p[["name"]]      # "pipe"
#'
#' # Shorthand column access (equivalent to p[["pipeline"]][["step"]])
#' p[["step"]]
#'
#' # Two-index form: p[[row, column]] extracts a single cell
#' p[["fit", "depends"]]  # "load"
#' p[[2, "state"]]        # state of the second step
#' @rdname Extract.pipeflow_pip
#' @export
`[[.pipeflow_pip` <- function(x, i, j, ...) {
  # Keep environment-style extraction for internal bindings, e.g.
  # x[["pipeline"]], x[[".dag"]], x[[".steps_to_nodes"]].
  if (missing(j)) {
    if (missing(i)) {
      stop("i must be provided")
    }

    # Internal bindings have priority over step names/column names.
    # This guarantees p[["name"]] and p[["pipeline"]] behave like
    # environment access even if steps with those names exist.
    if (is.character(i) && length(i) == 1L && !is.na(i) &&
      exists(i, where = x, inherits = FALSE)) {
      return(get(i, envir = x, inherits = FALSE))
    }

    # Lightweight fallback: delegate single-argument extraction to the
    # pipeline data.table column extractor.
    return(x[["pipeline"]][[i]])
  }

  col <- x[["pipeline"]][[j]]
  if (missing(i)) {
    return(col)
  }

  if (is.character(i)) {
    i <- .pip_steps_to_rows(x, i)
  }

  res <- col[i]
  if (length(i) == 1) {
    res <- res[[1]]
  }
  res
}


#' Print pipeflow objects
#'
#' @param x A pipeflow pipeline or view.
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
#' @return Invisibly returns `x`.
#' @examples
#' p <- pip_new("demo") |>
#'   pip_add("load",   \(n = 5) seq_len(n), group = "io",   tags = "raw") |>
#'   pip_add("square", \(x = ~load) x^2,   group = "compute") |>
#'   pip_add("total",  \(x = ~square) sum(x), group = "compute")
#'
#' print(p)              # core columns: step, group, depends, tags, out, state
#' print(p, cols = "all")  # all non-hidden columns
#' print(p, rows = 2:3)    # print only steps 2 and 3
#' @rdname print
#' @export
print.pipeflow_pip <- function(
  x,
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
    cols <- c("step", "depends", "out", "state")
    hasOtherGrous <- !identical(dat[["step"]], dat[["group"]])
    if (hasOtherGrous) {
      cols <- append(cols, "group", after = 1L)
    }
    has_tags <- any(lengths(dat[["tags"]]) > 0L)
    if (has_tags) {
      cols <- append(cols, "tags")
    }
    has_non_auto_exec <- any(dat[["exec"]] != "auto")
    if (has_non_auto_exec) {
      cols <- append(cols, "exec")
    }
  }
  if (identical(cols, "all")) {
    isHidden <- function(name) startsWith(name, ".")
    cols <- Filter(Negate(isHidden), colnames(dat))
  }

  if (header) {
    title <- sprintf(
      "<pipeflow_pip> %s (%d step%s)",
      x[["name"]], n, ifelse(n == 1, "", "s")
    )
    line <- paste(rep("-", nchar(title)), collapse = "")
    cat(title, line, sep = "\n")
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


#' @examples
#' p <- pip_new() |>
#'   pip_add("s1", \(x = 1) x, group = "io") |>
#'   pip_add("s2", \(x = ~s1) x + 1, group = "model")
#'
#' # A view header shows how many steps are selected out of the total
#' v <- pip_view(p, filter = list(group = "model"))
#' print(v)  # "<pipeflow_view> pipe view (1 of 2 steps)"
#' @rdname print
#' @export
print.pipeflow_view <- function(x, header = TRUE, ...) {
  pip <- x[["pip"]]
  rows <- x[["rows"]]
  nr <- length(rows)
  n <- nrow(pip[["pipeline"]])

  if (header) {
    title <- sprintf(
      "<pipeflow_view> %s (%d of %d step%s)",
      x[["name"]], nr, n, ifelse(n == 1, "", "s")
    )
    line <- paste(rep("-", nchar(title)), collapse = "")
    cat(title, line, sep = "\n")
  }

  if (length(rows) == 0L) {
    return(invisible(x))
  }
  print(pip, rows = rows, header = FALSE, row.names = FALSE, ...)
  invisible(x)
}
