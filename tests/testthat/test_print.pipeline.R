describe(".class_abb", {
    it("abbreviates known classes", {
        expect_equal(
            .class_abb(list(
                1, 1L, TRUE, 1 + 1i, "x", as.Date("2020-01-01"),
                factor("a"), ordered("b"), as.POSIXct("2020-01-01"),
                as.raw(1), list(1), expression(1)
            )),
            c(
                "<num>", "<num>", "<lgcl>", "<cplx>", "<char>", "<Date>",
                "<fctr>", "<ord>", "<POSc>", "<raw>", "<list>", "<expr>"
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
            "a = 1, b = ~s2, c = <char>"
        )
    })

    it("abbreviates params whose string exceeds maxchar", {
        params <- list(data = 1:100000, label = "some long label")
        expect_equal(
            .param_list_to_string(params),
            "data = <char>, label = <char>"
        )
    })

    it("keeps params whose string does not exceed maxchar", {
        params <- list(a = 1, b = TRUE, c = "hi")
        expect_equal(
            .param_list_to_string(params),
            "a = 1, b = TRUE, c = \"hi\""
        )
    })

    it("respects a custom maxchar", {
        params <- list(a = 1)
        expect_equal(.param_list_to_string(params, maxchar = 1), "a = 1")
        expect_equal(.param_list_to_string(params, maxchar = 0), "a = <char>")
    })
})

describe("print.pipeflow_pip", {
    get_print_header <- function(x, ...) {
        out <- capture.output(print(x, ...))
        iHeader <- which(grepl("^\\s*step\\b", out))[1]
        trimws(out[[iHeader]]) |> strsplit("\\s+") |> unlist()
    }

    it("shows step/depends/out/state plus tags when steps have tags", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1, tags = "s2")

        header <- get_print_header(p)

        expect_equal(
            header,
            c("step", "depends", "out", "state", "tags")
        )
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
            c("step", "depends", "out", "state", "tags")
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
            c("step", "depends", "out", "state", "tags")
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
            c("step", "depends", "out", "state", "tags", "exec")
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
            c("step", "depends", "out", "state", "locked")
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
            c("step", "depends", "out", "state", "tags")
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
            c("step", "depends", "out", "state", "tags")
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
            c("step", "depends", "out", "state", "tags", "exec")
        )
    })
})
