describe(".param_list_to_string", {
    it("returns the parameter names as a comma separated string", {
        params <- list(a = 1, b = ~s2, c = list(a = 1, b = 2))
        expect_equal(.param_list_to_string(params), "a, b, c")
    })

    it("returns an empty string for an empty parameter list", {
        expect_equal(.param_list_to_string(list()), "")
    })

    it("returns long parameter names as-is", {
        params <- list(data = ~data_prep, xVar = "Temp.Celsius")
        expect_equal(.param_list_to_string(params), "data, xVar")
    })
})

describe(".format_pip_data_table", {
    make_dt <- function(strwidth = 50L) {
        options(pipeflow.prettyprint.strwidth = strwidth)
        data.table::data.table(
            step = c("s1", "s2"),
            params = c(paste(rep("a", 60), collapse = ""), "short"),
            state = c(paste(rep("x", 60), collapse = ""), "new"),
            exec = c(paste(rep("y", 60), collapse = ""), "auto"),
            tags = list(rep("t", 60), "s2")
        )
    }

    it("truncates over-long values in character columns", {
        op <- options(pipeflow.prettyprint.strwidth = 50L)
        on.exit(options(op))

        dt <- make_dt()
        out <- .format_pip_data_table(dt)

        expect_equal(
            out[["params"]],
            c(paste0(paste(rep("a", 50), collapse = ""), "..."), "short")
        )
    })

    it("leaves short character values untouched", {
        op <- options(pipeflow.prettyprint.strwidth = 50L)
        on.exit(options(op))

        dt <- make_dt()
        out <- .format_pip_data_table(dt)

        expect_equal(out[["params"]][[2]], "short")
        expect_equal(out[["step"]], c("s1", "s2"))
    })

    it("does not truncate the 'state' and 'exec' columns", {
        op <- options(pipeflow.prettyprint.strwidth = 50L)
        on.exit(options(op))

        dt <- make_dt()
        out <- .format_pip_data_table(dt)

        expect_equal(out[["state"]], dt[["state"]])
        expect_equal(out[["exec"]], dt[["exec"]])
    })

    it("leaves non-character columns untouched", {
        op <- options(pipeflow.prettyprint.strwidth = 50L)
        on.exit(options(op))

        dt <- make_dt()
        out <- .format_pip_data_table(dt)

        expect_identical(out[["tags"]], dt[["tags"]])
    })

    it("respects a custom strwidth option", {
        op <- options(pipeflow.prettyprint.strwidth = 10L)
        on.exit(options(op))

        dt <- make_dt(strwidth = 10L)
        out <- .format_pip_data_table(dt)

        expect_equal(
            out[["params"]],
            c(paste0(paste(rep("a", 10), collapse = ""), "..."), "short")
        )
    })

    it("returns the table unchanged when no value exceeds strwidth", {
        op <- options(pipeflow.prettyprint.strwidth = 50L)
        on.exit(options(op))

        dt <- data.table::data.table(
            step = c("s1", "s2"),
            params = c("x=1", "x=~s1"),
            state = c("new", "new")
        )
        out <- .format_pip_data_table(dt)

        expect_identical(out, dt)
    })
})

describe("print.pipeflow", {
    get_print_header <- function(x, ...) {
        out <- capture.output(print(x, ...))
        iHeader <- which(grepl("^\\s*step\\b", out))[1]
        trimws(out[[iHeader]]) |> strsplit("\\s+") |> unlist()
    }

    get_step_line <- function(x, step, ...) {
        out <- capture.output(print(x, ...))
        pattern <- sprintf("^\\s*\\d+:\\s+%s\\b", step)
        out[grepl(pattern, out)][1]
    }

    it("shows params after step and hides 'out' without results", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)

        expect_equal(
            get_print_header(p),
            c("step", "params", "depends", "state")
        )
    })

    it("shows the nodeId column only in the printed 'all' view", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)

        expect_false("nodeId" %in% get_print_header(p))
        expect_equal(
            get_print_header(p, cols = "all")[seq_len(4L)],
            c("step", "fun", "params", "out")
        )
    })

    it("shows the 'out' column only when a step has a result", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)

        expect_false("out" %in% get_print_header(p))

        pip_run(p, lgr = NULL)
        expect_equal(
            get_print_header(p),
            c("step", "params", "depends", "state", "out")
        )
    })

    it("prints the parameter names of each step", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1, params = list(y = "hi"))

        expect_true(grepl("^\\s*\\d+:\\s+s1\\s+x\\s", get_step_line(p, "s1")))
        expect_true(grepl(
            "^\\s*\\d+:\\s+s2\\s+y, x\\s",
            get_step_line(p, "s2")
        ))
    })

    it("shows tags when at least one step has tags", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe")
        pip_add(p, "s1", \(x = 1) x)
        pip_add(p, "s2", \(x = ~s1) x + 1, tags = "model")

        header <- get_print_header(p)

        expect_equal(
            header,
            c("step", "params", "depends", "state", "tags")
        )
    })

    it("prints the tags column last when any step defines tags", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        header <- get_print_header(p)

        expect_equal(
            header,
            c("step", "params", "depends", "state", "tags")
        )
    })

    it("prints the exec column last when any step defines non-auto exec", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2", exec = "split")

        header <- get_print_header(p)

        expect_equal(
            header,
            c("step", "params", "depends", "state", "tags", "exec")
        )
    })

    it("prints the locked column when any step is locked", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)

        pip_lock(p["s1", ])
        header <- get_print_header(p)

        expect_equal(
            header,
            c("step", "params", "depends", "state", "locked")
        )
    })

    it("prints a footer with the run state and last run time", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)

        out <- capture.output(print(p))
        expect_true(any(grepl("<ready> last run: never", out)))

        pip_run(p, lgr = NULL)
        out <- capture.output(print(p))
        expect_true(any(grepl("<ready> last run: \\d{4}-\\d{2}-\\d{2}", out)))
        expect_false(is.null(p[["pipenv"]][[".last_run"]]))

        env <- p[["pipenv"]]
        env[[".run_state"]] <- factor(
            "failed",
            levels = c("ready", "restart", "running", "halted", "failed")
        )
        out <- capture.output(print(p))
        expect_true(any(grepl("<failed> last run:", out)))
    })

    it("resets the last run time", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x)
        pip_run(p, lgr = NULL)
        expect_false(is.null(p[["pipenv"]][[".last_run"]]))

        pip_reset(p)

        expect_null(p[["pipenv"]][[".last_run"]])
        out <- capture.output(print(p))
        expect_true(any(grepl("last run: never", out)))
    })

    it("prints 'Empty pipeline' for empty pipelines and views", {
        p <- pip_new("demo")
        out <- capture.output(print(p))
        expect_true(any(grepl("Empty pipeline", out)))
        expect_false(any(grepl("Empty data.table", out)))

        v <- pip_view(p)
        out <- capture.output(print(v))
        expect_true(any(grepl("Empty pipeline", out)))

        p2 <- pip_new() |> pip_add("s1", \(x = 1) x)
        out <- capture.output(print(pip_view(p2, step = "nope")))
        expect_true(any(grepl("Empty pipeline", out)))
    })
})


describe("print.pipeflow_view", {
    get_view_header <- function(x, ...) {
        out <- capture.output(print(x, ...))
        iHeader <- which(grepl("^\\s*step\\b", out))[1]
        trimws(out[[iHeader]]) |> strsplit("\\s+") |> unlist()
    }

    it("prints a view header and uses the pipeline column layout", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "io") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "model")

        v <- pip_view(p, tags = "model")
        out <- capture.output(print(v))

        expect_true(any(grepl("<pipeflow_view>", out)))
        expect_equal(
            get_view_header(v),
            c("step", "params", "depends", "state", "tags")
        )
    })

    it("prints the tags column last for tagged views", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        v <- pip_view(p, tags = "core")

        expect_equal(
            get_view_header(v),
            c("step", "params", "depends", "state", "tags")
        )
    })

    it("prints the exec column last when any step defines non-auto exec", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, exec = "split", tags = "core") |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        v <- pip_view(p, tags = "core")

        expect_equal(
            get_view_header(v),
            c("step", "params", "depends", "state", "tags", "exec")
        )
    })

    it("prints the params for the selected steps", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1) |>
            pip_add("s3", \(x = ~s2) x + 1)

        v <- pip_view(p, step = c("s2", "s3"))
        out <- capture.output(print(v))

        stepLines <- grep("^\\s*\\d+:", out, value = TRUE)
        expect_equal(length(stepLines), 2L)
        expect_true(any(grepl("s2\\s+x\\s+s1", stepLines)))
        expect_true(any(grepl("s3\\s+x\\s+s2", stepLines)))
    })
})
