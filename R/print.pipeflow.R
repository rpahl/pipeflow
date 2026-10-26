#' Format a condition in short form
#'
#' Formats a condition as `<kind/class>`, where `kind` is `"error"`, `"warning"`,
#' `"message"` or `"condition"`, and `class` is the first class of the condition, e.g.
#' `<error/simpleError>`. If the first class is the kind itself, only `<kind>` is shown.
#'
#' @param x A condition.
#' @return A single character string.
#' @noRd
.format_condition <- function(x) {
    kinds <- c("error", "warning", "message")
    kind <- kinds[vapply(kinds, \(k) inherits(x, k), logical(1))]
    kind <- if (length(kind) > 0L) kind[[1L]] else "condition"
    cls <- class(x)[[1L]]
    if (identical(cls, kind)) {
        return(sprintf("<%s>", kind))
    }
    sprintf("<%s/%s>", kind, cls)
}


#' Format a step output for printing
#'
#' Formats the outputs that data.table doesn't print well in a list column in short
#' form:
#' * a condition as `<kind/class>` (see `.format_condition()`),
#' * a partitioned output with failed keys (see `.pip_failed_keys()`) as
#'   `<partitioned: failed/all failed>`, e.g. `<partitioned: 1/3 failed>`,
#' * an atomic object with dimensions (matrix, array, table) as `<class[dims]>`, e.g.
#'   `<matrix[4x2]>`, similar to how data.table prints data frames
#' * a function or an environment (e.g. an R6 object) as `<class>`.
#'
#' Strings longer than `maxChars` characters are cut off after `maxChars` characters and
#' marked with `"..."`, so that a long output (e.g. a path or a query) doesn't widen the
#' whole table.
#'
#' Other outputs are returned unchanged and left to data.table.
#'
#' @param x A step output.
#' @param maxChars The maximum number of characters shown of each string.
#' @return A single character string, a character vector with the cut off strings, or
#' `x` unchanged.
#' @noRd
.format_out <- function(x, maxChars = 40L) {
    if (inherits(x, "condition")) {
        return(.format_condition(x))
    }
    failedKeys <- .pip_failed_keys(x)
    if (length(failedKeys) > 0L) {
        return(sprintf(
            "<partitioned: %d/%d failed>",
            length(failedKeys),
            length(x)
        ))
    }
    if (is.atomic(x) && !is.null(dim(x))) {
        dims <- paste(dim(x), collapse = "x")
        return(sprintf("<%s[%s]>", class(x)[[1L]], dims))
    }
    if (is.function(x) || is.environment(x)) {
        return(sprintf("<%s>", class(x)[[1L]]))
    }
    if (is.character(x)) {
        isLong <- !is.na(x) & nchar(x) > maxChars
        if (any(isLong)) {
            x <- as.character(x)
            x[isLong] <- paste0(substr(x[isLong], 1L, maxChars), "...")
        }
    }
    x
}


#' Format the number of steps per state
#'
#' Counts the steps per state, in the order `done`, `outdated`, `failed`, `new`, and
#' leaves out the states without steps, e.g. `"steps: 3 done, 1 failed, 2 new"`.
#'
#' @param states A character vector with the state of each step.
#' @return A single character string.
#' @noRd
.format_state_counts <- function(states) {
    order <- c("done", "outdated", "failed", "new")
    counts <- table(factor(states, levels = order))
    counts <- counts[counts > 0L]
    paste("steps:", toString(paste(counts, names(counts))))
}


#' @rdname print
#' @export
print.pipeflow <- function(
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
    data <- .pip_pipenv(x)[["data"]]
    n <- nrow(data)
    isView <- .is_pipeflow_view(x)

    if (identical(cols, "core")) {
        cols <- c("step", "params", "depends", "state")
        if (any(lengths(data[["out"]]) > 0L)) {
            cols <- append(cols, "out")
        }
        if (any(lengths(data[["tags"]]) > 0L)) {
            cols <- append(cols, "tags")
        }
        if (any(data[["exec"]] != "auto")) {
            cols <- append(cols, "exec")
        }
        if (any(data[["locked"]])) {
            cols <- append(cols, "locked")
        }
    }
    if (identical(cols, "all")) {
        cols <- colnames(data)
    }

    if (header) {
        # Add header
        if (isView) {
            nr <- length(.pip_view_rows(x))
            title <- sprintf(
                "<pipeflow_view> %s (%d of %d step%s)",
                x[["name"]],
                nr,
                n,
                ifelse(n == 1, "", "s")
            )
        } else {
            title <- sprintf(
                "<pipeflow> %s (%d step%s)",
                x[["name"]],
                n,
                ifelse(n == 1, "", "s")
            )
        }
        line <- paste(rep("-", nchar(title)), collapse = "")
        cat(title, line, sep = "\n")
    }

    if (length(rows) == 0) {
        rows <- .pip_view_rows(x)
    }

    if (length(rows) == 0L) {
        cat("Empty pipeline\n")
    } else {
        dat <- data[rows, cols, with = FALSE]
        if ("params" %in% names(dat)) {
            # We print the parameter names of each step as a list column, so
            # that data.table renders and truncates them like `depends`.
            params_names <- lapply(dat[["params"]], \(p) as.character(names(p)))
            data.table::set(dat, j = "params", value = params_names)
        }
        if ("duration" %in% names(dat)) {
            # A difftime is printed with many digits, each with its unit
            secs <- as.numeric(dat[["duration"]], units = "secs")
            duration <- vapply(
                secs,
                \(d) if (is.na(d)) NA_character_ else .formatted_duration(d),
                character(1)
            )
            data.table::set(dat, j = "duration", value = duration)
        }
        if ("out" %in% names(dat)) {
            # Fix outputs that data.table prints poorly (e.g. matrices, or
            # the condition of a failed step) in short form.
            out <- lapply(dat[["out"]], .format_out)
            data.table::set(dat, j = "out", value = out)
        }
        print(
            dat,
            topn = topn,
            nrows = nrows,
            row.names = row.names,
            class = class,
            ...
        )
    }

    if (header) {
        # Add footer with run state infos
        pipenv <- .pip_pipenv(x)
        runState <- as.character(pipenv[[".run_state"]])
        lastRun <- pipenv[[".last_run"]]
        lastRunStr <- if (is.null(lastRun)) {
            "never"
        } else {
            format(lastRun, format = "%Y-%m-%d %H:%M:%S")
        }
        # Pipelines saved with an older version have no run duration
        lastRunDuration <- pipenv[[".last_run_duration"]]
        if (!is.null(lastRun) && !is.null(lastRunDuration)) {
            lastRunStr <- sprintf(
                "%s (%s)",
                lastRunStr,
                .formatted_duration(lastRunDuration)
            )
        }
        cat(
            line,
            sprintf("<%s> last run: %s", runState, lastRunStr),
            sep = "\n"
        )
        if (length(rows) > 0L) {
            cat(.format_state_counts(data[["state"]][rows]), sep = "\n")
        }
    }

    invisible(x)
}
