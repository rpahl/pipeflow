# ------------------------------------
# Implementation of generic S3 methods
# ------------------------------------

describe("length", {
    it("returns an integer value", {
        p <- pip_new()
        expect_true(is.integer(length(p)))
    })

    it("returns the expected value", {
        p <- pip_new()
        expect_equal(length(p), 0L)
        pip_add(p, "s1", \(a = 1) a)
        pip_add(p, "s2", \(a = 1) a)
        expect_equal(length(p), 2L)

        v <- pip_view(p, step = "s1")
        expect_equal(length(v), 1L)
    })
})

describe("nrow", {
    it("matches length for pipelines and views", {
        p <- pip_new() |>
            pip_add("s1", \(a = 1) a) |>
            pip_add("s2", \(a = ~s1) a)
        pip_add(p, "s3", \(a = 1) a)

        expect_equal(nrow(p), length(p))
        expect_equal(nrow(p), 3L)
        expect_equal(ncol(p), ncol(p[["data"]]))

        v <- pip_view(p, step = c("s1", "s3"))
        expect_equal(nrow(v), length(v))
        expect_equal(nrow(v), 2L)
        expect_equal(ncol(v), ncol(p[["data"]]))
    })

    it("returns zero rows for an empty pipeline", {
        p <- pip_new()
        expect_equal(nrow(p), 0L)
        expect_equal(ncol(p), ncol(p[["data"]]))
    })
})

describe("extract operator [", {
    test_pip <- function() {
        pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
    }

    it("returns a view into the selected steps by default", {
        p <- test_pip()
        v <- p[5L]

        expect_true(.is_pipeflow_view(v))
        expect_equal(v[["view"]], 5L)
        expect_identical(v[["data"]], p[["data"]])
    })

    it("returns a view by step names by default", {
        p <- test_pip()
        v <- p[c("a2", "b2")]

        expect_true(.is_pipeflow_view(v))
        expect_equal(v[["view"]], c(2L, 5L))
    })

    it("returns a pipeline including upstream dependencies with view = FALSE", {
        p <- test_pip()
        suppressMessages(sub <- p[5L, view = FALSE])

        expect_true(.is_pipeflow(sub))
        expect_equal(sub[["data"]][["step"]], c("b1", "b2"))
    })

    it("returns a pipeline by step names with view = FALSE", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2", "b2"), view = FALSE])

        expect_true(.is_pipeflow(sub))
        expect_equal(sub[["data"]][["step"]], c("a1", "a2", "b1", "b2"))
    })

    it("returns the full pipeline when i is missing", {
        p <- test_pip()
        sub <- p[]

        expect_equal(sub[["data"]][["step"]], p[["data"]][["step"]])
    })

    it("returns an empty pipeline for empty selectors with view = FALSE", {
        p <- test_pip()

        expect_equal(length(p[integer(), view = FALSE]), 0L)
        expect_equal(length(p[character(), view = FALSE]), 0L)
    })

    it("signals invalid row indices", {
        p <- test_pip()

        expect_error(p[c(0, 1)], "Invalid row indices in 'i'")
        expect_error(p[99], "Invalid row indices in 'i'")
        expect_error(p[c(1, NA)], "row indices in 'i' must not contain NA")
        expect_error(p[c(1.1, 2)], "must be whole numbers")
    })

    it("signals invalid step names", {
        p <- test_pip()

        expect_error(p[c("a1", "")], "must be non-empty strings")
        expect_error(p[c("a1", NA)], "must not contain NA")
        expect_error(p[c("a1", "unknown")], "Unknown step names")
    })

    it("returns an independent copy with view = FALSE", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2"), view = FALSE])

        sub[["data"]][["state"]][1] <- "done"
        expect_equal(p[["data"]][["state"]][1], "new")
    })

    it("copies DAG edges for the extracted subset", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2", "b2"), view = FALSE])

        nodes_a1 <- .pip_get_reachable_nodes(sub, "a1")
        steps_a1 <- .pip_filter_nodes(sub, nodes_a1)[["step"]]
        expect_setequal(steps_a1, c("a1", "a2"))

        nodes_b1 <- .pip_get_reachable_nodes(sub, "b1")
        steps_b1 <- .pip_filter_nodes(sub, nodes_b1)[["step"]]
        expect_setequal(steps_b1, c("b1", "b2"))
    })

    it("uses an independent DAG copy in the extracted subset", {
        p <- test_pip()
        suppressMessages(sub <- p[c("a2", "b2"), view = FALSE])

        from <- as.integer(.pip_steps_to_nodes(sub, "a2")[[1]])
        to <- as.integer(.pip_steps_to_nodes(sub, "b1")[[1]])
        dag_add_edges_to(sub[["pipenv"]][[".dag"]], from = from, to = to)

        sub_steps <- .pip_filter_nodes(
            sub,
            .pip_get_reachable_nodes(sub, "a1")
        )[["step"]]
        expect_setequal(sub_steps, c("a1", "a2", "b1", "b2"))

        original_steps <- .pip_filter_nodes(
            p,
            .pip_get_reachable_nodes(p, "a1")
        )[["step"]]
        expect_setequal(original_steps, c("a1", "a2", "a3"))
    })

    it("signals the number of pulled-in upstream dependencies", {
        p <- test_pip()

        expect_message(p[5L, view = FALSE], "pulled in 1 upstream")
        expect_message(p[c("a2", "b2"), view = FALSE], "pulled in 2 upstream")
        expect_silent(p[c("a1", "b1"), view = FALSE])
    })

    describe("filter forwarding", {
        it("can use custom filters as in pip_view", {
            p <- test_pip()
            res <- p["depends" = "a1", "state" = "new"]

            expect_true(.is_pipeflow_view(res))
            expect_equal(res[["view"]], unname(which(p[["depends"]] == "a1")))
        })

        it("forwards a single filter to pip_view", {
            p <- test_pip()
            v1 <- p[state = "new"]
            v2 <- pip_view(p, state = "new")

            expect_true(.is_pipeflow_view(v1))
            expect_equal(v1[["view"]], v2[["view"]])
            expect_equal(v1[["name"]], v2[["name"]])
        })

        it("combines multiple filters by intersection", {
            p <- test_pip()
            v1 <- p[state = "new", depends = "a1"]
            v2 <- pip_view(p, state = "new", depends = "a1")

            expect_equal(v1[["view"]], v2[["view"]])
            expect_equal(v1[["view"]], c(2L, 4L))
        })

        it("forwards join and fixed arguments to pip_view", {
            p <- test_pip()
            v1 <- p[step = "a", fixed = FALSE]
            v2 <- pip_view(p, step = "a", fixed = FALSE)

            expect_equal(v1[["view"]], v2[["view"]])
            expect_equal(v1[["view"]], c(1L, 2L, 4L))

            v3 <- p[step = "a1", state = "new", join = "union"]
            expect_equal(
                v3[["view"]],
                pip_view(
                    p,
                    step = "a1",
                    state = "new",
                    join = "union"
                )[["view"]]
            )
        })

        it("filters by tags", {
            p <- pip_new() |>
                pip_add("load", \(x = 1) x, tags = c("io", "raw")) |>
                pip_add("report", \(x = ~load) x, tags = c("io", "report")) |>
                pip_add("other", \(x = 1) x, tags = "misc")

            v <- p[tags = "io"]

            expect_true(.is_pipeflow_view(v))
            expect_equal(v[["step"]], c(load = "load", report = "report"))
        })

        it("returns a copy of the pipeline when no arguments are given", {
            p <- test_pip()
            res <- p[]

            expect_true(.is_pipeflow(res))
            expect_false(.is_pipeflow_view(res))
            expect_equal(res[["data"]][["step"]], p[["data"]][["step"]])
        })
    })
})


describe("extract operator [[", {
    test_pip <- function() {
        pip_new("pipe") |>
            pip_add("s1", \(x = 1) x, tags = "init") |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("keeps environment-style extraction for internal bindings", {
        p <- test_pip()
        expect_equal(p[["name"]], "pipe")
        expect_true(data.table::is.data.table(p[["data"]]))
        expect_false(is.null(p[["pipenv"]][[".dag"]]))
    })

    it("extracts full columns when j is missing", {
        p <- test_pip()
        expect_equal(p[["step"]], c(s1 = "s1", s2 = "s2"))
        expect_equal(p[[1]], c(s1 = "s1", s2 = "s2"))
        expect_null(p[["unknown"]])
    })

    it("extracts a single cell by row selector and column selector", {
        p <- test_pip()

        expect_equal(p[[2L, "step"]], "s2")
        expect_equal(p[["s1", "state"]], "new")
    })

    it("extracts list-column values for a single row", {
        p <- test_pip() |> pip_run(lgr = NULL)

        expect_equal(p[[1L, "tags"]], "init")
        expect_equal(p[[2L, "tags"]], character(0))
        expect_equal(p[[1L, "params"]], list(x = 1))
        expect_named(p[[2L, "params"]], "x")
        expect_equal(p[[1L, "depends"]], character(0))
        expect_equal(p[[2L, "depends"]], c(x = "s1"))
        expect_equal(p[[1L, "out"]], 1)
        expect_equal(p[[2L, "out"]], 2)
    })

    it("signals multi-step column access and points to pip_view", {
        p <- test_pip()

        expect_error(
            p[[c(1L, 2L), "state"]],
            "i must be a single step name or row index"
        )
        expect_error(
            p[[c("s1", "s2"), "step"]],
            "i must be a single step name or row index"
        )
    })

    it(
        paste(
            "can distinguish between steps called 'name' and 'pipeline'",
            "and the internal 'name' and 'pipeline' elements"
        ),
        {
            p <- pip_new("pipe") |>
                pip_add("name", \(x = "hello") x) |>
                pip_add("data", \(x = ~name, y = "world") paste(x, y))

            expect_equal(p[["name"]], "pipe")
            expect_equal(p[["data"]], p$data)

            expect_equal(p[["name", "params"]], p[[1, "params"]])
            expect_equal(p[["data", "params"]], p[[2, "params"]])
        }
    )

    it("signals invalid row and column selectors", {
        p <- test_pip()

        expect_error(
            p[[c("s1", ""), "step"]],
            "i must be a single step name or row index"
        )
        expect_error(
            p[[c("s1", "unknown"), "step"]],
            "i must be a single step name or row index"
        )
        expect_null(p[[1, "unknown"]])
        expect_error(
            p[[1, c("step", "state")]],
            "j must be a single column name"
        )
    })

    it("extracts internal bindings and columns from a view", {
        p <- test_pip() |> pip_run(lgr = NULL)
        v <- pip_view(p, step = c("s1", "s2"))

        expect_identical(v[["data"]], p[["data"]])
        expect_equal(v[["view"]], c(1L, 2L))
        expect_equal(v[["step"]], c(s1 = "s1", s2 = "s2"))
        expect_equal(v[["out"]], list(s1 = 1, s2 = 2))
    })

    it("extracts a single cell from a view by step name or row index", {
        p <- test_pip() |> pip_run(lgr = NULL)
        v <- pip_view(p, step = c("s1", "s2"))

        expect_equal(v[["s2", "out"]], 2)
        expect_equal(v[[2, "step"]], "s2")
    })

    it("signals steps not covered by the view", {
        p <- test_pip() |> pip_run(lgr = NULL)
        v <- pip_view(p, step = "s1")

        expect_error(
            v[["s2", "out"]],
            "selected step not part of view: s2"
        )
    })

    it("signals out-of-bounds row indices for pipelines and views", {
        p <- test_pip()
        v <- pip_view(p, step = "s1")

        expect_error(p[[0L, "step"]], "row index out of bounds")
        expect_error(
            p[[nrow(p[["data"]]) + 1L, "step"]],
            "row index out of bounds"
        )
        expect_error(v[[0L, "step"]], "row index out of bounds")
        expect_error(v[[2L, "step"]], "row index out of bounds")
        expect_error(v[[length(v) + 1L, "step"]], "row index out of bounds")
    })

    it("signals non-whole-number row indices for pipelines and views", {
        p <- test_pip()
        v <- pip_view(p, step = "s1")

        expect_error(p[[1.5, "step"]], "row index must be a whole number")
        expect_error(v[[1.5, "step"]], "row index must be a whole number")
    })
})


describe("benchmarking", {
    skip("benchmarking tests are skipped by default")
    v <- c("hello", "world")
    w <- c("hello", "this", "is", "my", "world")
    grepv(pattern = v, x = w)
    `%chin%` <- data.table::`%chin%`

    N <- 1e3
    u <- as.character(as.hexmode(1:10000))
    y <- sample(u, N, replace = TRUE)
    x <- sample(u, 100)
    system.time(x %in% y)
    system.time(x %chin% y)

    microbenchmark(
        y %in% x,
        y %chin% x,
        times = 1000
    )

    # Please type 'example(chmatch)' to run this and see timings on your machine

    microbenchmark(x %in% y, x %chin% y, times = 100)
    system.time(a <- x %in% y) #  4.5s
    system.time(b <- x %chin% y) #  1.7s
    identical(a, b)

    # Different example with more unique strings ...
    u <- as.character(as.hexmode(1:(N / 10)))
    y <- sample(u, N, replace = TRUE)
    x <- sample(u, N, replace = TRUE)
    system.time(a <- match(x, y)) # 46s
    system.time(b <- chmatch(x, y)) # 16s
    identical(a, b)
})


describe("rbind", {
    test_pip <- function(name = "p") {
        pip_new(name) |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s2", \(x = ~s1) x + 1)
    }

    it("signals invalid inputs", {
        p <- test_pip()
        expect_error(rbind(1, p), "x must be a pipeflow pip")
        expect_error(rbind(p, 1), "x must be a pipeflow pip")
    })

    it("binds pipelines without mutating inputs", {
        p1 <- test_pip("left")
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 3) x) |>
            pip_add("t2", \(x = ~t1) x + 2)

        out <- rbind(p1, p2)
        expect_true(.is_pipeflow(out))
        expect_equal(out[["name"]], "left-right")
        expect_equal(out[["data"]][["step"]], c("s1", "s2", "t1", "t2"))

        pip_add(out, "extra", \(x = ~t2) x)
        expect_false("extra" %in% p1[["data"]][["step"]])
        expect_false("extra" %in% p2[["data"]][["step"]])
    })

    it("binds any number of pipelines and returns a single one unchanged", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")

        out <- rbind(p1, p2, p2)
        expect_equal(out[["name"]], "left-right-right")
        steps <- out[["data"]][["step"]]
        expect_true(all(c("s1", "s2", "s12", "s22", "s13", "s23") %in% steps))
        expect_identical(anyDuplicated(steps), 0L)

        expect_identical(rbind(p1), p1)
    })

    it("auto-renames duplicated step names from second pipeline", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")

        out <- rbind(p1, p2)
        steps <- out[["data"]][["step"]]
        expect_identical(anyDuplicated(steps), 0L)
        expect_true(all(c("s1", "s2", "s12", "s22") %in% steps))

        dep_new_s2 <- out[["data"]][step == "s22", depends][[1]]
        expect_equal(unname(dep_new_s2), "s12")
    })

    it("handles collisions of auto-fixed names", {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1) x) |>
            pip_add("s12", \(x = 2) x)
        p2 <- pip_new("right") |>
            pip_add("s1", \(x = 3) x)

        out <- rbind(p1, p2)
        expect_true("s13" %in% out[["data"]][["step"]])
    })

    it("rebuilds DAG and keeps dependencies valid in result", {
        p1 <- test_pip("left")
        p2 <- test_pip("right")
        out <- rbind(p1, p2)

        nodes <- .pip_get_reachable_nodes(out, "s1")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s1", "s2"))

        nodes <- .pip_get_reachable_nodes(out, "s12")
        steps <- .pip_filter_nodes(out, nodes)[["step"]]
        expect_setequal(steps, c("s12", "s22"))
    })

    it("rebinds .self references to the bound pipeline", {
        p1 <- pip_new("left") |>
            pip_add("s1", \(x = 1) .self[["name"]])
        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 1) .self[["name"]])

        out <- rbind(p1, p2)
        pip_run(out, lgr = NULL)
        expect_equal(
            out[["data"]][["out"]],
            list("left-right", "left-right")
        )
    })

    it("preserves runtime state from both source pipelines", {
        p1 <- test_pip("left")
        data.table::set(
            p1[["data"]],
            i = 1L,
            j = "out",
            value = list(10)
        )
        data.table::set(
            p1[["data"]],
            i = 1L,
            j = "state",
            value = "done"
        )
        data.table::set(
            p1[["data"]],
            i = 1L,
            j = "locked",
            value = TRUE
        )

        p2 <- pip_new("right") |>
            pip_add("t1", \(x = 3) x) |>
            pip_add("t2", \(x = ~t1) x + 2)

        data.table::set(
            p2[["data"]],
            j = "out",
            value = list(7, 9)
        )
        data.table::set(
            p2[["data"]],
            j = "state",
            value = c("done", "outdated")
        )
        data.table::set(
            p2[["data"]],
            j = "locked",
            value = c(FALSE, TRUE)
        )

        out <- rbind(p1, p2)
        actualOut <- out[["data"]][["out"]]
        expectedOut <- list(10, NULL, 7, 9)
        expect_equal(actualOut, expectedOut)
        expect_equal(
            out[["data"]][["state"]],
            c("done", "new", "done", "outdated")
        )
        expect_equal(
            out[["data"]][["locked"]],
            c(TRUE, FALSE, FALSE, TRUE)
        )
    })
})
