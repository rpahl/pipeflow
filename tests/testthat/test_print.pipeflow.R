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

    it("shows the 'continued' run state in the footer", {
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) stop("boom"))
        suppressWarnings(pip_run(p, lgr = NULL, on_error = "continue"))

        out <- capture.output(print(p))
        expect_match(out[[length(out)]], "^<continued> last run: ")
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

    it("shows conditions in the 'out' column in short form", {
        skip_if_not_installed("rlang")
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) stop("boom")) |>
            pip_add("s3", \(x = ~s1) {
                rlang::abort("rlang boom", class = "my_error")
            })
        expect_warning(
            pip_run(p, lgr = NULL, on_error = "continue"),
            class = "pipeflow_run_failed"
        )

        out <- capture.output(print(p))
        expect_match(get_step_line(p, "s1"), "\\s1$")
        expect_match(get_step_line(p, "s2"), "\\s<error/simpleError>$")
        expect_match(get_step_line(p, "s3"), "\\s<error/my_error>$")
        expect_false(any(grepl("Backtrace", out)))

        # The stored outputs are not changed
        expect_s3_class(p[["s3", "out"]], "rlang_error")
    })

    it("shows matrices in the 'out' column in short form", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \() as.matrix(data.frame(a = 1:4, b = 1:4))) |>
            pip_add("s2", \(x = ~s1) as.data.frame(x))
        pip_run(p, lgr = NULL)

        expect_match(get_step_line(p, "s1"), "\\s<matrix\\[4x2\\]>$")
        expect_match(get_step_line(p, "s2"), "\\s<data.frame\\[4x2\\]>$")
        expect_true(is.matrix(p[["s1", "out"]]))
    })

    it("cuts off long strings in the 'out' column", {
        op <- options(width = 1000L)
        on.exit(options(op))

        long <- strrep("x", 100)
        p <- pip_new("pipe") |>
            pip_add("s1", \(x = long) x)
        pip_run(p, lgr = NULL)

        expected <- paste0(strrep("x", 40), "...")
        expect_match(get_step_line(p, "s1"), paste0("\\s", expected, "$"))
        expect_equal(p[["s1", "out"]], long)
    })

    it("prints the parameter names of each step", {
        op <- options(width = 1000L)
        on.exit(options(op))

        p <- pip_new("pipe") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1, params = list(y = "hi"))

        expect_true(grepl("^\\s*\\d+:\\s+s1\\s+x\\s", get_step_line(p, "s1")))
        expect_true(grepl(
            "^\\s*\\d+:\\s+s2\\s+x,y\\s",
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
        env[[".run_state"]][] <- "failed"
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

        expect_true(any(grepl(
            "<pipeflow_view> pipe (1 of 2 steps)",
            out,
            fixed = TRUE
        )))
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

describe(".format_condition", {
    it("shows the kind and the first class of a condition", {
        expect_equal(
            .format_condition(simpleError("e")),
            "<error/simpleError>"
        )
        expect_equal(
            .format_condition(simpleWarning("w")),
            "<warning/simpleWarning>"
        )
        expect_equal(
            .format_condition(simpleMessage("m")),
            "<message/simpleMessage>"
        )
        expect_equal(
            .format_condition(simpleCondition("c")),
            "<condition/simpleCondition>"
        )
    })

    it("shows the leading class of rlang errors", {
        skip_if_not_installed("rlang")
        expect_equal(
            .format_condition(rlang::error_cnd("my_error", message = "e")),
            "<error/my_error>"
        )
    })

    it("shows only the kind if it is the first class", {
        cnd <- structure(
            class = c("error", "condition"),
            list(message = "e", call = NULL)
        )
        expect_equal(.format_condition(cnd), "<error>")
    })
})

describe(".format_out", {
    it("formats conditions via .format_condition()", {
        expect_equal(.format_out(simpleError("e")), "<error/simpleError>")
    })

    it("shows atomic objects with dimensions as <class[dims]>", {
        expect_equal(.format_out(matrix(1:8, 4)), "<matrix[4x2]>")
        expect_equal(.format_out(matrix(letters[1:4], 2)), "<matrix[2x2]>")
        expect_equal(.format_out(array(1:24, c(2, 3, 4))), "<array[2x3x4]>")
        expect_equal(.format_out(table(c("a", "b", "a"))), "<table[2]>")
        expect_equal(
            .format_out(table(c("a", "b"), c("x", "y"))),
            "<table[2x2]>"
        )
        expect_equal(
            .format_out(ts(matrix(1:20, 10), frequency = 1)),
            "<mts[10x2]>"
        )
    })

    it("shows functions and environments as <class>", {
        expect_equal(.format_out(function(x) x), "<function>")
        expect_equal(.format_out(sum), "<function>")
        expect_equal(.format_out(new.env()), "<environment>")

        r6Like <- structure(new.env(), class = c("Foo", "R6"))
        expect_equal(.format_out(r6Like), "<Foo>")
    })

    it("cuts off strings longer than maxChars characters", {
        long <- strrep("a", 41)
        expect_equal(.format_out(long), paste0(strrep("a", 40), "..."))
        expect_equal(.format_out(strrep("a", 40)), strrep("a", 40))
        expect_equal(.format_out("abcdef", maxChars = 3L), "abc...")

        # Each string of a vector is cut off on its own; NA stays NA
        expect_equal(
            .format_out(c("abcdef", "ab", NA), maxChars = 3L),
            c("abc...", "ab", NA)
        )

        # Classed strings are only changed if they are cut off
        json <- structure("{\"a\":1}", class = "json")
        expect_identical(.format_out(json), json)
        expect_equal(.format_out(json, maxChars = 3L), "{\"a...")
    })

    it("returns other outputs unchanged", {
        values <- list(
            1:3,
            c(a = 1, b = 2),
            "text",
            factor(c("x", "y")),
            Sys.Date(),
            NULL,
            list(1, 2),
            data.frame(a = 1:2),
            y ~ x
        )
        for (value in values) {
            expect_identical(.format_out(value), value)
        }
    })
})
