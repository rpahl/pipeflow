.class_abb <- function(x) {
    # Abbreviations for common classes as also defined in the data.table package
    classes <- vapply(x, data.class, character(1))
    class_abb <- c(
        character = "<char>",
        complex = "<cplx>",
        Date = "<Date>",
        expression = "<expr>",
        factor = "<fctr>",
        IDate = "<IDat>",
        integer = "<int>",
        integer64 = "<i64>",
        list = "<list>",
        logical = "<lgcl>",
        numeric = "<num>",
        ordered = "<ord>",
        POSIXct = "<POSc>",
        raw = "<raw>"
    )

    abbs <- unname(class_abb[classes])

    if (length(idx <- which(is.na(abbs)))) {
        # If not in above abbreviation list, use original class name
        abbs[idx] <- paste0("<", classes[idx], ">")
    }

    abbs
}

.param_list_to_string <- function(
    x,
    maxchar = getOption("pipeflow.print.param.maxchar", default = 6)
) {
    chars <- trimws(sapply(x, deparse1))
    if (length(idx <- which(nchar(chars) > maxchar))) {
        chars[idx] <- .class_abb(x[idx])
    }

    toString(paste0(names(x), "=", chars))
}

.format_pip_data_table <- function(
    dat,
    strwidth = getOption("pipeflow.prettyprint.strwidth", default = 50L)
) {
    charCols <- setdiff(
        names(Filter(f = is.character, x = dat)),
        c("state", "exec")
    )
    for (col in charCols) {
        nchars <- nchar(dat[[col]])
        if (length(idx <- which(nchars > strwidth))) {
            vals <- dat[[col]]
            vals[idx] <- paste0(strtrim(vals[idx], width = strwidth), "...")
            dat[[col]] <- vals
        }
    }
    dat
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
    data <- x[["data"]]
    n <- nrow(data)
    isView <- .is_pipeflow_view(x)

    if (identical(cols, "core")) {
        cols <- c("step", "depends", "state")
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
        isHidden <- function(name) startsWith(name, ".")
        cols <- Filter(Negate(isHidden), colnames(data))
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
        sig <- data.table::data.table(
            signature = vapply(
                data[["params"]][rows],
                FUN = .param_list_to_string,
                character(1)
            )
        )
        dat2print <- data.table::cbindlist(list(dat[, 1], sig, dat[, -1]))
        print(
            .format_pip_data_table(dat2print),
            topn = topn,
            nrows = nrows,
            row.names = row.names,
            class = class,
            ...
        )
    }

    if (header) {
        # Add footer with run state infos
        runState <- as.character(.pip_get_pipenv(x)[[".run_state"]])
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
