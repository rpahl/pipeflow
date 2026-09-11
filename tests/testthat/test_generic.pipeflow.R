# ------
# Helper
# ------

describe(".pip_compact", {
    it("builds a compact self-contained pipeline from the kept nodes", {
        p <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x * 2) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("b2", \(x = ~b1) x + 1)
        pip_run(p, lgr = NULL)

        c <- .pip_compact(p, keepNodes = c(0L, 1L))

        expect_equal(unname(c[["data"]][["step"]]), c("a1", "a2"))
        expect_equal(c[["data"]][[".nodeId"]], 0:1)
        expect_equal(unname(c[["data"]][["depends"]][[2]]), "a1")
        expect_setequal(
            .pip_filter_nodes(
                c,
                .pip_get_reachable_nodes(c, "a1")
            )[["step"]],
            c("a1", "a2")
        )

        # runtime state is preserved and re-running yields the same outputs
        expect_equal(c[["data"]][["state"]], c("done", "done"))
        suppressMessages(pip_run(c, lgr = NULL))
        expect_equal(c[["data"]][["out"]], p[["data"]][["out"]][1:2])
    })

    it("returns an empty pipeline when no nodes are kept", {
        p <- pip_new() |>
            pip_add("s1", \(x = 1) x)
        c <- .pip_compact(p, keepNodes = integer())

        expect_equal(nrow(c[["data"]]), 0L)
    })

    it("keeps a small subset via the node-by-node path", {
        p <- pip_new() |>
            pip_add("hub", \(x = 1) x)
        for (i in 1:9) {
            pip_add(p, sprintf("f%d", i), function(x = ~hub) x + 1)
        }
        # 2 of 10 steps -> below the rebuild fraction, from-scratch path
        c <- .pip_compact(p, keepNodes = c(0L, 9L))

        expect_equal(unname(c[["data"]][["step"]]), c("hub", "f9"))
        expect_equal(c[["data"]][[".nodeId"]], 0:1)
        expect_equal(unname(c[["data"]][["depends"]][[2]]), "hub")
    })

    it("compacts node ids also when the source has gaps", {
        p <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
        pip_remove(p, "a2") # leaves .nodeId with a gap

        c <- .pip_compact(p, keepNodes = p[["data"]][[".nodeId"]])

        expect_equal(unname(c[["data"]][["step"]]), c("a1", "b1", "b2"))
        expect_equal(c[["data"]][[".nodeId"]], 0:2)
        expect_equal(unname(c[["data"]][["depends"]][[3]]), "b1")
    })

    it("gives identical results for any rebuildFrac", {
        check_equal_compact <- function(a, b) {
            expect_equal(
                unname(a[["data"]][["step"]]),
                unname(b[["data"]][["step"]])
            )
            expect_equal(a[["data"]][[".nodeId"]], b[["data"]][[".nodeId"]])
            expect_equal(a[["data"]][["depends"]], b[["data"]][["depends"]])
            expect_equal(a[["data"]][["unbound"]], b[["data"]][["unbound"]])
            expect_equal(a[["data"]][["params"]], b[["data"]][["params"]])
            expect_equal(a[["data"]][["out"]], b[["data"]][["out"]])
            expect_equal(a[["data"]][["state"]], b[["data"]][["state"]])
            expect_equal(a[["data"]][["locked"]], b[["data"]][["locked"]])

            # DAG equivalence via downstream reachability per step
            for (s in a[["data"]][["step"]]) {
                ra <- .pip_filter_nodes(
                    a,
                    .pip_get_reachable_nodes(a, s)
                )[["step"]]
                rb <- .pip_filter_nodes(
                    b,
                    .pip_get_reachable_nodes(b, s)
                )[["step"]]
                expect_setequal(ra, rb)
            }

            # step -> node lookup is identical
            nodesA <- a[["pipenv"]][[".steps_to_nodes"]]
            nodesB <- b[["pipenv"]][[".steps_to_nodes"]]
            expect_setequal(ls(nodesA), ls(nodesB))
            expect_equal(
                unname(unlist(mget(ls(nodesA), envir = nodesA))),
                unname(unlist(mget(ls(nodesA), envir = nodesB)))
            )
        }

        p <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x * 2) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a3", \(x = ~a1) x + 10) |>
            pip_add("b2", \(x = ~b1) x + 1)
        pip_run(p, lgr = NULL)

        keepSets <- list(
            c(0L, 1L), # small subset (from-scratch path)
            c(0L, 1L, 3L), # intermediate subset (both paths)
            0:4 # full set (rebuild path)
        )
        fracs <- c(0, 0.3, 0.6, 1, 2)

        for (keep in keepSets) {
            ref <- .pip_compact(p, keepNodes = keep, rebuildThresh = fracs[1])
            for (frac in fracs[-1]) {
                got <- .pip_compact(p, keepNodes = keep, rebuildThresh = frac)
                check_equal_compact(ref, got)
            }
        }

        # also on a pipeline whose node ids have gaps
        q <- pip_new() |>
            pip_add("a1", \(x = 1) x) |>
            pip_add("b1", \(x = 1) x) |>
            pip_add("a2", \(x = ~a1) x) |>
            pip_add("b2", \(x = ~b1) x)
        pip_remove(q, "a2")
        keep <- q[["data"]][[".nodeId"]]

        ref <- .pip_compact(q, keepNodes = keep, rebuildThresh = fracs[1])
        for (frac in fracs[-1]) {
            got <- .pip_compact(q, keepNodes = keep, rebuildThresh = frac)
            check_equal_compact(ref, got)
        }
    })
})

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

    describe("boolean expression filtering", {
        filter_pip <- function() {
            pip_new("pipe") |>
                pip_add("load", \(x = 1) x, tags = c("io", "daily")) |>
                pip_add("fit", \(x = ~load) x, tags = c("model", "daily")) |>
                pip_add("eval", \(x = ~fit) x, tags = c("model", "report"))
        }

        set_state <- function(p, step, state = "done") {
            p[["data"]][["state"]][p[["data"]][["step"]] == step] <- state
            p
        }

        it("selects steps matching a boolean expression", {
            p <- set_state(filter_pip(), "load")
            v <- p[state == "done"]

            expect_true(.is_pipeflow_view(v))
            expect_equal(unname(v[["step"]]), "load")
        })

        it("combines conditions with & and |", {
            p <- set_state(filter_pip(), "load")
            v <- p[tags %like% "model" & state == "new"]

            expect_equal(unname(v[["step"]]), c("fit", "eval"))

            v2 <- p[state == "done" | step %like% "^eval$"]
            expect_equal(unname(v2[["step"]]), c("load", "eval"))
        })

        it("supports %in% set membership", {
            p <- filter_pip()
            v <- p[step %in% c("load", "eval")]

            expect_equal(v[["view"]], c(1L, 3L))
        })

        it("mirrors the data.table filter on the step table", {
            p <- set_state(filter_pip(), "load")
            v <- p[tags %like% "model"]

            raw <- p[["data"]][tags %like% "model"]
            expect_equal(unname(v[["step"]]), raw[["step"]])
        })

        it("recycles scalar logical filters", {
            p <- filter_pip()

            expect_equal(length(p[TRUE]), 3L)
            expect_equal(length(p[FALSE]), 0L)
        })

        it("accepts a logical vector variable", {
            p <- filter_pip()
            keep <- p[["step"]] %in% c("load", "eval")
            v <- p[keep]

            expect_equal(v[["view"]], c(1L, 3L))
        })

        it("extracts upstream dependencies with view = FALSE", {
            p <- set_state(filter_pip(), "load")
            suppressMessages(sub <- p[tags %like% "report", view = FALSE])

            expect_equal(sub[["data"]][["step"]], c("load", "fit", "eval"))
        })

        it("is restricted to full pipelines (use pip_view for views)", {
            p <- filter_pip()
            v <- pip_view(p, step = c("fit", "eval"))

            expect_error(v[state == "new"], "full pipeline")
            expect_error(v[1L], "full pipeline")
        })

        it("rejects named filters and stray arguments", {
            p <- filter_pip()

            expect_error(p[state = "new"], "unused argument")
            expect_error(p[tags = "io"], "unused argument")
        })

        it("signals invalid logical filters", {
            p <- filter_pip()

            expect_error(p[c(TRUE, FALSE)], "logical filter has length")
            expect_error(p[c(TRUE, NA, FALSE)], "must not contain NA")
        })

        it("signals unknown columns in boolean expressions", {
            p <- filter_pip()

            expect_error(p[not_a_column == 1], "not_a_column")
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
