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
        runState <- as.character(.pip_pipenv(x)[[".run_state"]])
        lastRun <- x[["pipenv"]][[".last_run"]]
        lastRunStr <- if (is.null(lastRun)) {
            "never"
        } else {
            format(lastRun, format = "%Y-%m-%d %H:%M:%S")
        }
        cat(
            line,
            sprintf("<%s> last run: %s", runState, lastRunStr),
            sep = "\n"
        )
    }

    invisible(x)
}
