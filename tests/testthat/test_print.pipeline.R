describe(".class_abb", {
    it("abbreviates known classes", {
        expect_equal(
            .class_abb(list(
                1,
                1L,
                TRUE,
                1 + 1i,
                "x",
                as.Date("2020-01-01"),
                factor("a"),
                ordered("b"),
                as.POSIXct("2020-01-01"),
                as.raw(1),
                list(1),
                expression(1)
            )),
            c(
                "<num>",
                "<num>",
                "<lgcl>",
                "<cplx>",
                "<char>",
                "<Date>",
                "<fctr>",
                "<ord>",
                "<POSc>",
                "<raw>",
                "<list>",
                "<expr>"
            )
        )
    })

    it("wraps classes without an abbreviation in angle brackets", {
        expect_equal(
            .class_abb(list(structure(1, class = "myclass"))),
            "<myclass>"
        )
    })

    it("returns one abbreviation per element, unnamed", {
        x <- c(a = 1, b = 2)
        abbs <- .class_abb(x)
        expect_length(abbs, 2L)
        expect_null(names(abbs))
    })
})

describe(".param_list_to_string", {
    it("converts a param list to a comma separated string", {
        params <- list(a = 1, b = ~s2, c = list(a = 1, b = 2))
        expect_equal(
            .param_list_to_string(params),
            "a=1, b=~s2, c=<list>"
        )
    })

    it("abbreviates params whose string exceeds maxchar", {
        params <- list(data = 1:100000, label = "some long label")
        expect_equal(
            .param_list_to_string(params),
            "data=<num>, label=<char>"
        )
    })

    it("keeps params whose string does not exceed maxchar", {
        params <- list(a = 1, b = TRUE, c = "hi")
        expect_equal(
            .param_list_to_string(params),
            "a=1, b=TRUE, c=\"hi\""
        )
    })

    it("respects a custom maxchar", {
        params <- list(a = 1)
        expect_equal(.param_list_to_string(params, maxchar = 1), "a=1")
        expect_equal(.param_list_to_string(params, maxchar = 0), "a=<num>")
    })

    it("uses the class abbreviation of the value, not the deparsed string", {
        params <- list(x = data.frame(a = 1:2), f = factor(letters[1:3]))
        expect_equal(
            .param_list_to_string(params),
            "x=<data.frame>, f=<fctr>"
        )
    })
})

describe(".format_pip_data_table", {
    make_dt <- function(strwidth = 50L) {
        options(pipeflow.prettyprint.strwidth = strwidth)
        data.table::data.table(
            step = c("s1", "s2"),
            signature = c(paste(rep("a", 60), collapse = ""), "short"),
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
            out[["signature"]],
            c(paste0(paste(rep("a", 50), collapse = ""), "..."), "short")
        )
    })

    it("leaves short character values untouched", {
        op <- options(pipeflow.prettyprint.strwidth = 50L)
        on.exit(options(op))

        dt <- make_dt()
        out <- .format_pip_data_table(dt)

        expect_equal(out[["signature"]][[2]], "short")
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
            out[["signature"]],
            c(paste0(paste(rep("a", 10), collapse = ""), "..."), "short")
        )
    })

    it("returns the table unchanged when no value exceeds strwidth", {
        op <- options(pipeflow.prettyprint.strwidth = 50L)
        on.exit(options(op))

        dt <- data.table::data.table(
            step = c("s1", "s2"),
            signature = c("x=1", "x=~s1"),
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

    it("shows signature after step and hides 'out' without results", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)

        expect_equal(
            get_print_header(p),
            c("step", "signature", "depends", "state")
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
            c("step", "signature", "depends", "state", "out")
        )
    })

    it("prints one signature per step", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1, params = list(y = "hi"))

        expect_true(grepl("x=1", get_step_line(p, "s1")))
        expect_true(grepl("y=\"hi\", x=~s1", get_step_line(p, "s2")))
    })

    it("abbreviates long signatures with the maxchar option", {
        op <- options(width = 1000L, pipeflow.print.param.maxchar = 4L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = list(a = 1, b = 2)) x)

        expect_true(grepl("x=<list>", get_step_line(p, "s1")))
    })

    it("shows the class abbreviation for long signatures", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = data.frame(a = 1:2)) x)

        expect_true(grepl("x=<data.frame>", get_step_line(p, "s1")))
    })

    it("truncates over-long signatures in the printed table", {
        op <- options(
            width = 1000L,
            pipeflow.prettyprint.strwidth = 10L,
            pipeflow.print.param.maxchar = 100L
        )
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = list(alpha = 1, beta = 2, gamma = 3)) x)

        expect_true(grepl("x=list\\(alp\\.\\.\\.", get_step_line(p, "s1")))
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
            c("step", "signature", "depends", "state", "tags")
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
            c("step", "signature", "depends", "state", "tags")
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
            c("step", "signature", "depends", "state", "tags", "exec")
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
            c("step", "signature", "depends", "state", "locked")
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
            levels = c("ready", "restart", "running", "stop", "failed")
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
            c("step", "signature", "depends", "state", "tags")
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
            c("step", "signature", "depends", "state", "tags")
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
            c("step", "signature", "depends", "state", "tags", "exec")
        )
    })

    it("prints a signature for the selected steps", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1) |>
            pip_add("s3", \(x = ~s2) x + 1)

        v <- pip_view(p, step = c("s2", "s3"))
        out <- capture.output(print(v))

        expect_true(any(grepl("x=~s1", out)))
        expect_true(any(grepl("x=~s2", out)))
        expect_false(any(grepl("^s1\\b", out)))
    })
})
